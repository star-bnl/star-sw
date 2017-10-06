#!/usr/bin/env python3
''' needs those module
on Cori:
  module load python/3.5-anaconda
on PDSF:
  module load python/3.4.3

'''
import sys
import os
(va,vb,vc,vd,ve)=sys.version_info ;
if va!=3:
   print('needs python3, have you executed\n\n module load python/3.5-anaconda \n\n aborting .py script')
   exit(1)
assert(va==3)  # needes Python3

import argparse
parser = argparse.ArgumentParser()

import datetime

#----------------------------------------------------------
def setup_parser():
   # parser.add_argument("-x", dest="x",help="x")
   parser.add_argument('-trigger','-t', action='append', dest="trigIdL", help="trigId, accepts many")
   parser.add_argument("-trg", dest="triggerSetName",help="data trigger set name")

   parser.add_argument("-lib", dest="starLib",help="STAR library version")
   parser.add_argument("-production", dest="starProdID",help="star production ID")
   parser.add_argument("-mixer", dest="mixerScript",help="mixer macro")
   parser.add_argument("-prodname", dest="prodName",help="production name")
   parser.add_argument("-r", dest="embedRequestID",help="embedding request ID")

   parser.add_argument("-geantid", dest="geantPID",help="geant PID")
   parser.add_argument("-particle", dest="particleName",help="geant particle name")
   parser.add_argument("-mode", dest="ptMode",help="pt mode")


   parser.add_argument("-z", dest="zVertMax",help="z vertex max")
   parser.add_argument("-vrcut", dest="vrcut",help="r-vertex cut")


   parser.add_argument("-ymin", dest="ymin",help="ymin")
   parser.add_argument("-ymax", dest="ymax",help="ymax")
   parser.add_argument("-ptmin", dest="ptmin",help="ymin")
   parser.add_argument("-ptmax", dest="ptmax",help="ymax")

   parser.add_argument("-mult", dest="mult",help="mult")
   parser.add_argument("-local", dest="local",help="local script generated", action="store_true", default=False)

   parser.add_argument("-daq", dest="daqPath",help=" daq path")
   parser.add_argument("-tag", dest="tagPath",help=" tag path")
   parser.add_argument("-outPath", dest="outPath",help="output path",default='/global/projecta/projectdirs/starprod/embedding')

   parser.add_argument("-fSetRange", dest="fSetRange",help="range of fSET, '-' separated")
   parser.add_argument("-fSetCPUHours", dest="fSetCPUHours",help="estimated CPU hours for one fSET",default='1000')

   parser.add_argument("-wallHour", dest="wallHour",help="max running time for one task (in hours)",default='35')
   parser.add_argument("-nThreads", dest="nThreads",help="max number of threads on one node",default='50')

   parser.add_argument("-nevents", dest="nevents",help="num. eve per r4s task",default='1000')

#----------------------------------------------------------
def  supplementArgs(argD):
   # prep trig list
   trigL=argD['trigIdL']
   out=""
   for trigId in trigL:
      #print(trigId)
      out=out+"    triggers.push_back(%s);\n"%trigId

   #print('out=',out)
   argD['triggers_push_back']=out

   now = datetime.datetime.now()
   argD['scriptProdDate']=now.strftime("%B %d %Y %I:%M%p")

#----------------------------------------------------------
def makeTaskScript(argD,inpF):
   outF=inpF.replace('templ','csh')
   outF=os.path.basename(outF)
   print('replacing content of template=',inpF, outF)
   
   t = open(inpF, 'r')
   tempstr = t.read()
   t.close()

   for key in argD:
      if key=='local': continue
      if key=='trigIdL': continue
      print('replace:',key, ' \t->',argD[key])
      tempstr=tempstr.replace('<'+key+'>',argD[key])
   

   #print('tempstr=',tempstr)

   # Write out the new config file
   fout = open(outF, 'w')
   fout.write(tempstr)
   fout.close()

   print('Done1 exec:  cat ',outF)

#----------------------------------------------------------
def makeFarmerScript(argD,inpF):
   outF=argD['taskList']
   outF=outF.replace('list','slr')
   outF=outF.replace('starTask','starFarmer')
   print('replacing content of template=',inpF, outF)
   
   t = open(inpF, 'r')
   tempstr = t.read()
   t.close()

   for key in argD:
      if key=='local': continue
      if key=='trigIdL': continue
      print('replace:',key, ' \t->',argD[key])
      tempstr=tempstr.replace('<'+key+'>',argD[key])
   
   #print('tempstr=',tempstr)

   # Write out the new config file
   fout = open(outF, 'w')
   fout.write(tempstr)
   fout.close()

   print('Done2 exec:  cat ',outF)

#----------------------------------------------------------
def   makeTaskList(argD,taskLF):
   from random import shuffle
   aa=argD['fSetRange'].split('-')
   print('produce fsets:',aa)
   fs1=int(aa[0])
   fs2=int(aa[1])

   assert fs1<=fs2
   outF=taskLF+'_'+argD['embedRequestID']+"_"+str(fs1)+"_"+str(fs2)+'.list'

   argD['taskList']=outF
   
   mypath=argD['daqPath']
   print('get daq files from',mypath)
   from os import listdir
   from os.path import isfile, join
   from os.path import getsize
   onlyfilessize = [(f,getsize(join(mypath, f))) for f in listdir(mypath) if isfile(join(mypath, f))]
   #print(len(onlyfilessize))
   assert len(onlyfilessize) >0
   onlyfilessizesort = sorted(onlyfilessize, key=lambda daqname: daqname[1], reverse=True)
   onlyfiles = [f[0] for f in onlyfilessizesort]
   print('example daq',onlyfiles[0])

   # build list of daq file names
   coreL=[]
   for x in onlyfiles:
      if not 'daq' in x[-4:]: continue 
      coreL.append(x[:-4])

   print('base daq list len',len(coreL))
   print('example daq',coreL[0])

   print('output:',outF)
   nFtot=0
   fout = open(outF, 'w')
   nr=0
   for x in coreL:
      nevents=int(argD['nevents'])
      for fset in range(fs1,fs2+1):
         print('---make daq=',x,' fset=',fset,' nevents=',nevents)
         text='shifter  /bin/tcsh  ${WRK_DIR}/r4sTask_embed.csh '+x+' '+str(fset)+' '+str(nevents)+'  >&   ${WRK_DIR}/logs/'+x+'_fset'+str(fset)+'.taskLog\n'
         fout.write(text)
         nFtot=nFtot+1
      nr=nr+1
      #if nr>5: break

   fout.close()
   print('total len of task list is',nFtot)   
   whr=int(argD['wallHour'])
   nthreads=int(argD['nThreads'])
   ntaskhour=float(argD['fSetCPUHours'])/(nFtot/(fs2-fs1+1))
   npass=int(whr*0.8/ntaskhour)
   
   nnode=int(nFtot*1.0/nthreads/npass+0.9)+1  #1 node is used to monitor tasks at Cori
   argD['nodeNumber']=str(nnode)
   nskew=int((nnode-1)*300) #allow a DB connection time of 300 seconds for one node
   argD['skewNumber']=str(nskew)

#----------------------------------------------------------
if __name__ == "__main__":
   setup_parser()
   args=parser.parse_args()
   print('M:args',args)

   argD=vars(args)

   supplementArgs(argD)

   inpF='StRoot/macros/embedding/cori/r4sTask_embed.templ'
   makeTaskScript(argD,inpF)

   taskLF='starTask'
   makeTaskList(argD,taskLF)

   inpfarmerF='StRoot/macros/embedding/cori/starFarmer.templ'
   makeFarmerScript(argD,inpfarmerF)

 
