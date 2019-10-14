import re
import sys

pattern = re.compile('x')

input = sys.stdin.read()

def repl(matchobj):
    return 'oo'

sys.stdout.write(pattern.sub(repl, input))
