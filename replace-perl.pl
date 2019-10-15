# https://www.perlmonks.org/?node_id=1172179

use strict;
use warnings;

my $str = <STDIN>;

$str =~ s/x/repl($1)/ge;

print $str;

sub repl {
    return "oo";
}
