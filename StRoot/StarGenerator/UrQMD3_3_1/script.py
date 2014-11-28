#!/usr/bin/python

# just a quick parsing script
# This parses a few bits of text which I then copy and paste into other code.

# John Novak
# October 22, 2012
# 9:50 am

import sys
import os

def main():
    includes=open("includes.txt",'r')
    defines=open("defines.txt",'w')
    typeofcalls=open("typeofcalls.txt",'w')
    if not includes:
        print "cant seem to open includes.txt...?"
        exit(1)
    for line in includes:
        defines.write("#define "+line[:-1]+" F77_NAME("+line[:-1].lower()+","+line[:-1].upper()+")\n")
        typeofcalls.write("extern \"C\" void type_of_call "+line[:-1]+"();\n")
    includes.close()
    defines.close()
    typeofcalls.close()

if __name__ == '__main__':
    main()
