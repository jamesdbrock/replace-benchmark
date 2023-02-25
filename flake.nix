{
  description = "replace-benchmark";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nix-filter.url = "github:numtide/nix-filter";
  inputs.replace-megaparsec-src = {
    url = "github:jamesdbrock/replace-megaparsec";
    flake = false;
  };
  inputs.replace-attoparsec-src = {
    url = "github:jamesdbrock/replace-attoparsec";
    flake = false;
  };

  outputs = inputs:
    let
      nix-filter = import inputs.nix-filter;
      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = hfinal: hprev:
            prev.haskell.packageOverrides hfinal hprev // {
              replace-megaparsec = hfinal.callCabal2nix "replace-megaparsec" inputs.replace-megaparsec-src {};
              replace-attoparsec = hfinal.callCabal2nix "replace-attoparsec" inputs.replace-attoparsec-src {};
              replace-benchmark = hfinal.callCabal2nix "" (nix-filter {root = ./.; include = ["src" ./replace-benchmark.cabal ./LICENSE ];}) {};
            };
        };
      };
      perSystem = system:
        let
          pkgs = import inputs.nixpkgs { inherit system; overlays = [ overlay ]; };
          # hspkgs = pkgs.haskellPackages;
          hspkgs = pkgs.haskell.packages.ghc944;
          input-dense = pkgs.stdenv.mkDerivation {
            name = "input-dense";
            src = "";
            phases = ["installPhase"];
            installPhase = ''
              # Make a 10MB dense test file.
              for ((i=1;i<=5000000;i++)); \
              do \
                echo -n "x " >> $out; \
              done;
              '';
          };
          input-sparse = pkgs.stdenv.mkDerivation {
            name = "input-sparse";
            src = "";
            phases = ["installPhase"];
            installPhase = ''
              # Make a 10MB sparse test file.
              for ((i=1;i<=5000000;i++)); \
              do \
                echo -n " " >> $out; \
              done; \
              echo -n "x" >> $out; \
              for ((i=1;i<=4999999;i++)); \
              do \
                echo -n " " >> $out; \
              done
              '';
          };
        in
        {

          apps = {
            default =
              let
                perfstat = "${pkgs.linuxPackages.perf}/bin/perf stat --field-separator , --event=task-clock";
                sed = "${pkgs.gnused}/bin/sed";
                python = "${pkgs.python3}/bin/python";
                perl = "${pkgs.perl}/bin/perl";
                this = "${hspkgs.replace-benchmark}/bin";
                oneline = "${pkgs.coreutils}/bin/tr -d '\n'";
                mscut = "${pkgs.coreutils}/bin/cut --delimiter=, --fields=1 | ${oneline}";
                benchmark-run = pkgs.writeScriptBin "benchmark" ''

                  export LC_ALL=C.UTF-8

                  # Function replacement

									echo -n '| '
                  ${python} --version | ${oneline}
                  echo -n ' [`re.sub`](https://docs.python.org/3/library/re.html#re.sub) *repl* function'
                  echo -n " | "
                  ${perfstat} ${python} replace-python-fn.py < ${input-dense} 2>&1 1>/dev/null | ${mscut}
                  echo -n " | "
                  ${perfstat} ${python} replace-python-fn.py < ${input-sparse} 2>&1 1>/dev/null | ${mscut}
                  echo ' |'

									echo -n '| '
                  echo -n "Perl  "
                  ${perl} --version | ${pkgs.gnugrep}/bin/grep -o "v[0-9]*\.[0-9]*\.[0-9]*" | ${oneline}
                  echo -n ' [`s///ge`](https://perldoc.perl.org/functions/s.html)'
                  echo -n " function | "
                  ${perfstat} ${perl} replace-perl.pl < ${input-dense} 2>&1 1>/dev/null | ${mscut}
                  echo -n " | "
                  ${perfstat} ${perl} replace-perl.pl < ${input-sparse} 2>&1 1>/dev/null | ${mscut}
                  echo ' |'

									echo -n '| '
                  echo -n '[`Replace.Megaparsec.streamEdit`](https://hackage.haskell.org/package/replace-megaparsec/docs/Replace-Megaparsec.html#v:streamEdit) `String` | '
                  ${perfstat} ${this}/megaparsec-string < ${input-dense} 2>&1 1>/dev/null | ${mscut}
                  echo -n " | "
                  ${perfstat} ${this}/megaparsec-string < ${input-sparse} 2>&1 1>/dev/null | ${mscut}
                  echo ' |'

									echo -n '| '
                  echo -n '[`Replace.Megaparsec.streamEdit`](https://hackage.haskell.org/package/replace-megaparsec/docs/Replace-Megaparsec.html#v:streamEdit) `ByteString` | '
                  ${perfstat} ${this}/megaparsec-bytestring < ${input-dense} 2>&1 1>/dev/null | ${mscut}
                  echo -n " | "
                  ${perfstat} ${this}/megaparsec-bytestring < ${input-sparse} 2>&1 1>/dev/null | ${mscut}
                  echo ' |'

									echo -n '| '
                  echo -n '[`Replace.Megaparsec.streamEdit`](https://hackage.haskell.org/package/replace-megaparsec/docs/Replace-Megaparsec.html#v:streamEdit) `Text` | '
                  ${perfstat} ${this}/megaparsec-text < ${input-dense} 2>&1 1>/dev/null | ${mscut}
                  echo -n " | "
                  ${perfstat} ${this}/megaparsec-text < ${input-sparse} 2>&1 1>/dev/null | ${mscut}
                  echo ' |'

									echo -n '| '
                  echo -n '[`Replace.Attoparsec.ByteString.streamEdit`](https://hackage.haskell.org/package/replace-attoparsec/docs/Replace-Attoparsec-ByteString.html#v:streamEdit) | '
                  ${perfstat} ${this}/attoparsec-bytestring < ${input-dense} 2>&1 1>/dev/null | ${mscut}
                  echo -n " | "
                  ${perfstat} ${this}/attoparsec-bytestring < ${input-sparse} 2>&1 1>/dev/null | ${mscut}
                  echo ' |'

									echo -n '| '
                  echo -n '[`Replace.Attoparsec.Text.streamEdit`](https://hackage.haskell.org/package/replace-attoparsec/docs/Replace-Attoparsec-Text.html#v:streamEdit) | '
                  ${perfstat} ${this}/attoparsec-text < ${input-dense} 2>&1 1>/dev/null | ${mscut}
                  echo -n " | "
                  ${perfstat} ${this}/attoparsec-text < ${input-sparse} 2>&1 1>/dev/null | ${mscut}
                  echo ' |'

									echo -n '| '
                  echo -n '[`Text.Regex.Applicative.replace`](http://hackage.haskell.org/package/regex-applicative/docs/Text-Regex-Applicative.html#v:replace) `String` | '
                  ${perfstat} ${this}/regex-applicative-string < ${input-dense} 2>&1 1>/dev/null | ${mscut}
                  echo -n " | "
                  ${perfstat} ${this}/regex-applicative-string < ${input-sparse} 2>&1 1>/dev/null | ${mscut}
                  echo ' |'

									echo -n '| '
                  echo -n '[`Text.Regex.PCRE.Heavy.gsub`](http://hackage.haskell.org/package/pcre-heavy/docs/Text-Regex-PCRE-Heavy.html#v:gsub) `Text` | '
                  echo -n "∞"
                  echo -n " | "
                  ${perfstat} ${this}/pcre-heavy-text < ${input-sparse} 2>&1 1>/dev/null | ${mscut}
                  echo ' |'

									echo -n '| '
                  echo -n '[`Control.Lens.Regex.ByteString.match`](https://hackage.haskell.org/package/lens-regex-pcre/docs/Control-Lens-Regex-ByteString.html#v:match) | '
                  echo -n "∞"
                  echo -n " | "
                  ${perfstat} ${this}/lens-regex-text < ${input-sparse} 2>&1 1>/dev/null | ${mscut}
                  echo ' |'

									echo -n '| '
                  echo -n '[`Control.Lens.Regex.Text.match`](https://hackage.haskell.org/package/lens-regex-pcre/docs/Control-Lens-Regex-Text.html#v:match) | '
                  echo -n "∞"
                  echo -n " | "
                  ${perfstat} ${this}/lens-regex-bytestring < ${input-sparse} 2>&1 1>/dev/null | ${mscut}
                  echo ' |'



                  # Constant replacement

									echo -n '| '
                  echo -n "GNU sed "
                  ${pkgs.gnused}/bin/sed --version | ${pkgs.coreutils}/bin/head -1 | ${pkgs.coreutils}/bin/cut --delimiter=' ' --fields=4 | ${oneline}
                  echo -n " | "
                  ${perfstat} ${sed} 's/x/oo/g' < ${input-dense} 2>&1 1>/dev/null | ${mscut}
                  echo -n " | "
                  ${perfstat} ${sed} 's/x/oo/g' < ${input-sparse} 2>&1 1>/dev/null | ${mscut}
                  echo ' |'

									echo -n '| '
                  ${python} --version | ${oneline}
                  echo -n ' [`re.sub`](https://docs.python.org/3/library/re.html#re.sub) *repl* string'
                  echo -n " | "
                  ${perfstat} ${python} replace-python.py < ${input-dense} 2>&1 1>/dev/null | ${mscut}
                  echo -n " | "
                  ${perfstat} ${python} replace-python.py < ${input-sparse} 2>&1 1>/dev/null | ${mscut}
                  echo ' |'

									echo -n '| '
                  echo -n "Perl  "
                  ${perl} --version | ${pkgs.gnugrep}/bin/grep -o "v[0-9]*\.[0-9]*\.[0-9]*" | ${oneline}
                  echo -n ' [`s///g`](https://perldoc.perl.org/functions/s.html)'
                  echo -n " | "
                  ${perfstat} ${perl} -0777 -pe 's/x/oo/g' ${input-dense} 2>&1 1>/dev/null | ${mscut}
                  echo -n " | "
                  ${perfstat} ${perl} -0777 -pe 's/x/oo/g' ${input-sparse} 2>&1 1>/dev/null | ${mscut}
                  echo ' |'

									echo -n '| '
                  echo -n '[`Data.ByteString.Search.replace`](http://hackage.haskell.org/package/stringsearch/docs/Data-ByteString-Search.html#v:replace) | '
                  ${perfstat} ${this}/stringsearch-bytestring < ${input-dense} 2>&1 1>/dev/null | ${mscut}
                  echo -n " | "
                  ${perfstat} ${this}/stringsearch-bytestring < ${input-sparse} 2>&1 1>/dev/null | ${mscut}
                  echo ""
                  '';
              in
              {
                type = "app";
                program = "${benchmark-run}/bin/benchmark";
              };
          };
        };
    in
    { inherit overlay; } // inputs.flake-utils.lib.eachDefaultSystem perSystem;
}
