import os
import string
import sys

regex_string = ''

if len(sys.argv) > 1:
    words = sys.argv[1:]
    length = len(words)
    for arg in range(0, length):
        for index in range(0, len(words[arg])):
            regex_string += '[' + words[arg][index].lower() + '|' + words[arg][index].upper() + ']'
        if arg+1 != length:
            regex_string += ' '
    print regex_string