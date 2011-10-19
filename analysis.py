#!/usr/bin/python

import sys

file = open(sys.argv[1])
text = file.read()

i = 0
isNewLine = False

for j in range(len(text)):

    if text[j] == '[':
        i = i+1
        sys.stdout.write('\n')
        isNewLine = True
    if text[j] == '{':
        i = i+1
        sys.stdout.write('\n')
        isNewLine = True


    if isNewLine:
        sys.stdout.write(" "*i)
        isNewLine = False

    sys.stdout.write(text[j])

    if text[j] == '}':
        i = i-1
        sys.stdout.write('\n')
        isNewLine = True
    if text[j] == ']':
        i = i-1
        sys.stdout.write('\n')
        isNewLine = True
        
