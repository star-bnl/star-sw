#!/usr/bin/python

# UrQMD_rename.py
# John Novak
# Sunda, October 28, 2012, 12:00pm

#==================| How to use this: |==================
# Download UrQMD: http://urqmd.org => "Download UrQMD"
# Untar (unzip) the file.
# copy this script into the directory with the UrQMD files.
# run it with "python UrQMD_rename.py".
# Fix the mess I have made for you.

# If you have made any changes to any of the original files, or if you are attached to them in any way BACK THEM UP BEFORE USING THIS SCRIPT. If it is a brand spanking new install, keep the tar file on hand, just in case.

# This is a python script which attempts to speed up the process of modifying UrQMD so that it can be compiled by cons.
# A number of things have to be changed, and this script will attempt to take care of the repetitive dumb things.
# When this script is finished, you will probably NOT have a version of UrQMD which will compile happily with cons. Most likely, you will still get a small handfull of errors which you will need to address. The point of this script is to make it so that you can spend 15 minutes chasing down a few simple bugs, instead of spending 4 hour retyping include statments and renaming files, then spend 15 minutes chasing down bugs.

#================================ What this script does: ================================
# -renames all the files *.f -> *.F
# -renames the included files (*.f||*.F) -> *.inc
# -replaces all of the FORTRAN include statments with C include statments for the C-preprocessor

#======= Possible errors in compiling UrQMD with cons after running this script: ========
# -cons will try to build every file which end in *.F, even if they weren't built by the original makefile. This can cause problems if there are duplicates of functions defined. For example: if the code shipped with multiple random number generators.

#========================== What you will still have to do: =============================
# -handle the erros from cons
# -expose the common block
# -make the common blocks accessible as C structs
# -write the library files 
# -implement the STAR random number generator

import os
import sys
import re

def main():
    global dic
    # First get a list of all the *.f files
    os.system("ls -1 *.f > filelist.txt")
    filelist = list(open("filelist.txt",'r').read().split("\n"))[:-1]

    # replace the FORTRAN include statements
    replaceinclude(filelist)

    # rename the files to *.F
    renameAll(filelist)

    # rename the *.inc files
    renameInc()


# This function runs through all the files in 'filelist' and replaces the include functions
def replaceinclude(filelist):
    global dic
    for i in filelist:
        # In a perfect world I would make the newfile end in just '.F', but, for some reason the bash shell doesn't seem to distinguish between 'a.f' and 'a.F'
        output = file(i.split('.')[0]+'.NEWF','w')
        inputf = file(i,'r')
        for line in inputf:
            match = re.search(r"include [\"|']([\w.]+).f",line,re.IGNORECASE) # I am worried that in some cases this regular expression will pick up other things than include statments... It seems to work though
            if match:
                # comment out previous include statement and put in the new one
                output.write("C"+line)
                output.write("#include \""+match.group(1).split('.')[0]+".inc\"\n")
                # keep track of which files are included
                dic['incfiles'].append(match.group(1).split('.')[0])
            else:
                # if it's not an include statement just dump it back out
                output.write(line)
        output.close()
        inputf.close()
    # by making this a set then back to a list, we drop duplicates
    dic['incfiles'] = list(set(dic['incfiles']))


# This function moves all the new files over the old ones
def renameAll(filelist):
    for i in filelist:
        os.system("rm "+i)
        os.system("mv "+i.split('.')[0]+'.NEWF'+" "+i.split('.')[0]+'.F')


# This function renames all of the include files
def renameInc():
    global dic
    for i in dic['incfiles']:
        os.system("mv "+i+".F "+i+".inc")


if __name__ == "__main__":
    global dic
    dic = {'incfiles':[]}
    main()
