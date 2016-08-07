import sys

def syls(word, n):
    for i in range(0, len(line) - n):
        print word[i:i+n].encode('utf-8')

for l in sys.stdin:
    line = l.decode('utf-8')
    syls(line, 3)
    syls(line, 4)
    syls(line, 5)
    syls(line, 6)
