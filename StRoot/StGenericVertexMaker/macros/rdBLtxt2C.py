#!/usr/bin/python
# od Ziu:
# http://docs.python.org/library/csv.html

import os
import time

inpFname='/star/u/balewski/2012-Wana/runQC_2012_long_pp510_ver3b.csv' ;  outFname='run2012_long_xxx' ; headLine=5


kFill=0; kRun=1; kPol=2;  kStat=3; kSec=4; kL2WBeve=7; kL2WEeve=8; kL2WBid=17;kL2WEid=18; kZdc=11;kLum=6;kStartT=16; kBnlStart=9

linesMaster =  [line.split(',') for line in  open(inpFname).readlines()]
print inpFname
k=0
for x in linesMaster[headLine]: 
   print k,"="+x,",  ",
   if k%5==4:
      print
   k+=1
print


linesBL =[line.split() for line in  open('beamLineB.txt').readlines()]
print '\n run BL info:'
print linesBL [0]

linesRootC =[line for line in  open('vertexSeed.20120101.000001.C').readlines()]
print '\n RootC code is in'
#print linesRootC 


localtime = time.asctime( time.localtime(time.time()) )
print "Local current time :", localtime


#------------------------------
def  writeRootC(beamL,dateString):
   outFile='out/vertexSeed.'+dateString+'.C'
   print 'write=',outFile, 'new beamlin=',beamL
   nameA=['x0_fix','y0_fix','dxdz_fix','dydz_fix',
          'x0Err_fix','y0Err_fix','dxdzErr_fix','dydzErr_fix','chisq_fix','tracks_fix','Rrun_fix']
   fdOut=open(outFile,'w')
   for x in linesRootC:
       y=x
       for k in range(len(nameA)):
           y=y.replace(nameA[k],beamL[k]) 
       fdOut.write(y)
   fdOut.close()
   return


#------------------------------
#   MAIN   PROGRAM (after initialization)
#------------------------------



nRunOk=0; 
nLine=0 ; 

for line in linesMaster:  
    
    nLine+=1
    if line[kRun][0]!='R' : #run line
        continue

    
    if 'Xx'  in line[kStat]:
        continue

    type=0
    # .... search for the beamLine
    for x in linesBL:
        #print x
        Rrun=x[2].split('_')[1]
        if Rrun!=line[kRun]:
            continue        
        #print 'found', Rrun,x,x[10]
        beamL=[x[5],x[6],x[7],x[8], '0.001', '0.002', '0.003', '0.004','0.005',x[10],Rrun]
        type+=1
        break
    if type<1 :
        continue

    tStart=line[kStartT]
    dateString=time.strftime("%Y%m%d.%H%M%S",time.gmtime(float(tStart)-10.))
    print Rrun,beamL,line[kBnlStart],dateString

    writeRootC(beamL,dateString)
    
    nRunOk+=1
    #break
    # reformating of records is completed
   
   
   
print 'nRunOk=',nRunOk
print 'output=', outFname
