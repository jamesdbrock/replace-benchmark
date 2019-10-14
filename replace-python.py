import re
import sys

pattern = re.compile('x')

input = sys.stdin.read()

sys.stdout.write(pattern.sub('oo', input))
