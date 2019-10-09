import re
import sys

pattern = re.compile('fnord')

input = sys.stdin.read()

sys.stdout.write(pattern.sub('bar', input))
