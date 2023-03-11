#!/usr/bin32/python

## \file
# Read millepede binary file and print records
#
# \author Claus Kleinwort, DESY, 2009 (Claus.Kleinwort@desy.de)
#
#  \copyright
#  Copyright (c) 2009 - 2018 Deutsches Elektronen-Synchroton,
#  Member of the Helmholtz Association, (DESY), HAMBURG, GERMANY \n\n
#  This library is free software; you can redistribute it and/or modify
#  it under the terms of the GNU Library General Public License as
#  published by the Free Software Foundation; either version 2 of the
#  License, or (at your option) any later version. \n\n
#  This library is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU Library General Public License for more details. \n\n
#  You should have received a copy of the GNU Library General Public
#  License along with this program (see the file COPYING.LIB for more
#  details); if not, write to the Free Software Foundation, Inc.,
#  675 Mass Ave, Cambridge, MA 02139, USA.
#
# Hardcoded defaults can be replaced by command line arguments for
#    -  Name of binary file
#    -  Number of records to print (-1: all; <-1: all, record headers only)
#    -  Number of records to skip (optional)
#    -  Mininum value to print derivative
#
# Description of the output from readMilleBinary.py
#    -  Records (tracks) start with \c '===' followed by record number and length 
#       (<0 for binary files containing doubles)
#    -  Measurements: A measurement with global derivatives is called a 'global measurement', 
#       otherwise 'local measurement'. Usually the real measurements from the detectors are 'global'
#       ones and virtual measurements e.g. to describe multiple scattering are 'local'.
#    -  'Global' measurements start with \c '-g-' followed by measurement number, first global label,
#       number of local and global derivatives, measurement value and error. The next lines contain 
#       local and global labels (array('i')) and derivatives (array('f') or array('d')).
#    -  'Local' measurements start with \c '-l-' followed by measurement number, first local label, 
#       number of local and global derivatives, measurement value and error. The next lines contain
#       local labels (array('i')) and derivatives (array('f') or array('d')).
#
# Tested with SL4, SL5, SL6

import array, sys

# ############### read millepede binary file #################
#  
## Binary file type (C or Fortran)
Cfiles = 1  # Cfiles
#Cfiles = 0 # Fortran files
# 
## Integer format 
intfmt = 'i'  # SL5, gcc-4
#intfmt = 'l' # SL4, gcc-3
#
## Binary file name
fname = "milleBinaryISN.dat"
#
## number of records (tracks) to show
mrec = 10
## number of records (track) to skip before 
skiprec = 0
## minimum value to print derivatives
minval = None  # allows for NaNs
#
# ## C. Kleinwort - DESY ########################

# ## use command line arguments ?
narg = len(sys.argv)
if narg > 1:
  if narg < 3:
    print " usage: readMilleBinary.py <file name> <number of records> [<number of records to skip> <minimum value to print derivative>]"
    sys.exit(2)
  else:
    fname = sys.argv[1]
    mrec = int(sys.argv[2])
    if narg > 3:
      skiprec = int(sys.argv[3])
    if narg > 4:
      minval = float(sys.argv[4])

#print " input ", fname, mrec, skiprec

f = open(fname, "rb")

nrec = 0
try:
    while (nrec < mrec + skiprec) or (mrec < 0):
# read 1 record
        nr = 0
        if (Cfiles == 0):
            lenf = array.array(intfmt)
            lenf.fromfile(f, 2)

        length = array.array(intfmt)
        length.fromfile(f, 1)
        nr = abs(length[0] / 2)
        nrec += 1

        if length[0] > 0:
            glder = array.array('f')
        else:
            glder = array.array('d')
        glder.fromfile(f, nr)

        inder = array.array(intfmt)
        inder.fromfile(f, nr)

        if (Cfiles == 0):
            lenf = array.array(intfmt)
            lenf.fromfile(f, 2)

        if (nrec <= skiprec):  # must be after last fromfile
            continue

        print " === NR ", nrec, length[0] / 2

        # no details, only header
        if (mrec < -1):
            continue

        i = 0
        nh = 0
        ja = 0
        jb = 0
        jsp = 0
        nsp = 0
        while (i < (nr - 1)):
            i += 1
            while (i < nr) and (inder[i] != 0): i += 1
            ja = i
            i += 1
            while (i < nr) and (inder[i] != 0): i += 1
            jb = i
            i += 1
            # special data ?
            if (ja + 1 == jb) and (glder[jb] < 0.):
                jsp = jb
                nsp = int(-glder[jb])
                i += nsp - 1
                print ' ### spec. ', nsp, inder[jsp + 1:i + 1], glder[jsp + 1:i + 1]
                continue
            while (i < nr) and (inder[i] != 0): i += 1
            i -= 1
            nh += 1
            if (jb < i):
# measurement with global derivatives
                print ' -g- meas. ', nh, inder[jb + 1], jb - ja - 1, i - jb, glder[ja], glder[jb]
            else:
# measurement without global derivatives
                print ' -l- meas. ', nh, inder[ja + 1], jb - ja - 1, i - jb, glder[ja], glder[jb]
            if (ja + 1 < jb):
                lab = []
                val = []
                for k in range(ja + 1, jb):
                    if minval is None:
                        lab.append(inder[k])
                        val.append(glder[k])
                    elif abs(glder[k]) >= minval:
                        lab.append(inder[k])
                        val.append(glder[k])
                print " local  ", lab
                print " local  ", val
            if (jb + 1 < i + 1):
                lab = []
                val = []
                for k in range(jb + 1, i + 1):
                    if minval is None:
                        lab.append(inder[k])
                        val.append(glder[k]) 
                    elif abs(glder[k]) >= minval:
                        lab.append(inder[k])
                        val.append(glder[k])
                print " global ", lab
                print " global ", val

except EOFError:
    print
    if (nr > 0):
        print " >>> error: end of file before end of record", nrec
    else:
        print " end of file after", nrec, "records"

f.close()
