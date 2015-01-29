#!/usr/bin32/python
#
import array
### read millepede binary file #################
#   print information (tested with SL4)
#   for C files
Cfiles = 1
#   or Fortran files
#Cfiles = 0 
# SL5, gcc-4
intfmt = 'i'
# SL4, gcc-3
#intfmt = 'l'
#
# input file
f = open("milleBinaryISN.dat","rb")
#
# number of records (tracks) to show
mrec = 10
# number of records (track) to skip before 
skiprec = 0
#
### C. Kleinwort - DESY ########################

nrec=0
try:
    while (nrec<mrec+skiprec):
# read 1 record    
        if (Cfiles == 0): 
           lenf=array.array(intfmt)
           lenf.fromfile(f,2)
           
        len=array.array(intfmt)
        len.fromfile(f,1)
        nr=len[0]/2
        nrec+=1

        glder=array.array('f')
        glder.fromfile(f,nr)

        inder=array.array(intfmt)
        inder.fromfile(f,nr)
        
        if (Cfiles == 0): 
           lenf=array.array(intfmt)
           lenf.fromfile(f,2)

        if (nrec < skiprec): # must be after last fromfile
            continue

        print " === NR ", nrec, nr

        i=0
        nh=0
        ja=0
        jb=0
        jsp=0
        nsp=0
        while (i<(nr-1)):
            i+=1
            while (i<nr) and (inder[i] != 0): i+=1
            ja=i
            i+=1
            while (i<nr) and (inder[i] != 0): i+=1
            jb=i
            i+=1
            while (i<nr) and (inder[i] != 0): i+=1
            i-=1
# special data ?
            if (ja+1 == jb) and (glder[jb] < 0.):
               jsp=jb
               nsp=int(-glder[jb])
               i+=nsp
               print ' ### spec. ', nsp, inder[jsp+1:i+1], glder[jsp+1:i+1]
               continue
            nh+=1           
            if (jb<i):
# measurement with global derivatives
               print ' -g- meas. ', nh, inder[jb+1], jb-ja-1, i-jb, glder[ja], glder[jb]
            else:
# measurement without global derivatives
               print ' -l- meas. ', nh, inder[ja+1], jb-ja-1, i-jb, glder[ja], glder[jb]            
            if (ja+1<jb):
               print " local  ",inder[ja+1:jb]
               print " local  ",glder[ja+1:jb]
            if (jb+1<i+1):
               print " global ",inder[jb+1:i+1]
               print " global ",glder[jb+1:i+1]

               
except EOFError:
     pass
#    print "end of file"

f.close()
