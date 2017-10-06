#!/usr/bin/env python
''' needs those modules on Cori

module load root
module load python/3.5-anaconda

'''

import sys
import os
import subprocess
import json

print ('check python ver=',sys.version)
(va,vb,vc,vd,ve)=sys.version_info
#print (va,vb)
if va<3:
    print ('proceed... ver2')
else:
    print ('proceed... ver3')
assert(va==3)

# units tests ---------
logPath='/global/cscratch1/sd/balewski/starFarm-4606020/'
listPath='./'
listName='starTask_600.list'
taskF='starQA1'
isAppend=1  # set to 0 to start frm scratch
print (logPath)

def readTaskList(dataF ):
    #expFilt='16092063_raw_3000006'
    expFilt='16092031_raw_5500006'
    expFilt='adc_'
    print ('readTaskList exp=%s inp:%s'%(expFilt,dataF))
    taskDB={}
    k=0
    with open(dataF, "r") as ins:
        
        for line in ins:
            #print (line)
            k=k+1
            if expFilt not in line:
                continue
            textL=line.split()

            core=textL[4]
            
            taskCore=core
            #print("taskCore=",taskCore)
            taskDB[taskCore]=k

            if len(taskDB) >993000:
                break
    return taskDB


taskDB=readTaskList(listPath+listName)
print('total taskDB len=',len(taskDB))

for core in taskDB:
    print(core,taskDB[core])
    #break



################################
def hhmmss2sec(st):
    x1 = st.split(':')
    #print (st, len(x1 ))
    t=0.
    k=0
    if len(x1)==3:
        t+=float(x1[2])
        t+=float(x1[1])*60.
        t+=float(x1[0])*3600.
    if len(x1)==2:
        t+=float(x1[1])
        t+=float(x1[0])*60.
    return t


################################
def readOneLogFile( logF ):  
   print ('readOneLogFile inp:',logF, 'exist=',os.path.isfile(logF))

   if not os.path.isfile(logF):
       return -10,-1,-1, 'none'
   statInfo = os.stat(logF) # in Bytes
   #print ('statInfo=',statInfo)  
   sizekB= float(statInfo.st_size)/1024.
   #print (sizekB)

   # - - - - - -  get task start date  - - - - - - 
   task = subprocess.Popen("head -n1 "+logF, shell=True, stdout=subprocess.PIPE)
   data = task.stdout.read()
   assert task.wait() == 0
   #print('data1=',data)
   ss=data.decode('utf-8')[:-1]
   ssL=ss.split(",")   
   #print("ssL=",ssL)
   xx=ssL[5]
   startDate=xx[11:]
   #print('xx=',startDate)
   

   # - - - -   get completion flag - - - -

   task = subprocess.Popen("tail -n4 "+logF+" |grep 'This is the end of ROOT -- Goodbye'", shell=True, stdout=subprocess.PIPE)
   data = task.stdout.read()
   if task.wait():
       return -20,-1,-1,startDate
   
   # - - - -  get num muDst - - - 
   task = subprocess.Popen("tail -n500 "+logF+" |grep NumberOfEvents | grep MuDst", shell=True, stdout=subprocess.PIPE)   
   data = task.stdout.read()
   assert task.wait()==0
   ss=data.decode('utf-8')[:-1]
   ssL=ss.split()   
   #print( type(ss),'data3='+ss+"=")
   nMuDst=int(ssL[5])
   #print("ssL=",ssL,nMuDst)

   
   #  - - - - - -  get CPU processing time  - - - - - -     
   task = subprocess.Popen("tail -n4 "+logF+" |grep pf\+", shell=True, stdout=subprocess.PIPE)
   data = task.stdout.read()
   #print('ww=',task.wait())
   if task.wait():
       return -30,-1,nMuDst,startDate
   
   #print('data1=',data)
   ss=data.decode('utf-8')[:-1]
   ssL=ss.split("\t")   
   #print( type(ss),'data3='+ss+"=")
   #print("ssL=",ssL)
   if len(ssL)==2:
       xxL=ssL[0].split()
       #print('xxL=',xxL)
       realSec=hhmmss2sec(xxL[2])
       cpuEfi=float(xxL[3][:-1])/100.
       return realSec,cpuEfi,nMuDst,startDate
   return -40,-1,nMuDst,startDate

xxx=readOneLogFile( logPath+core+".log")
print ('timing=',xxx)

def readDataFile(dataF ):
   #print ('readDataFile inp:',dataF)
   statInfo = os.stat(dataF) # in Bytes
   #print ('statInfo=',statInfo)  
   logSizeGB= float(statInfo.st_size)/1024./1024./1024.
   #print (logSizeGB)
   return logSizeGB


#dataSizeGb=readDataFile(  dataPath+"daq."+taskCore.replace(prodTag,daqTag)+".data")
#print ('dataSizeGb=%.3f'%dataSizeGb)



################################
def readRecoFile( core, path ):    
    recoF=path+"reco."+core+".root"
    #print ('readRecoFile inp:',recoF)
    statInfo = os.stat(recoF) # in Bytes
    #print ('statInfo=',statInfo)  
    sizeGB= float(statInfo.st_size)/1024./1024./1024.
    #print (sizeGB)
    if sizeGB <0.01:
        xx={}
        xx["core"]=core
        xx["recoSizeGb"]=sizeGB
        return xx

    cmdX='root -b -q qaOneTask.C\\(\\"%s\\",\\"%s\\"\\) | grep core'%(core,path)
    #print("cmdX1=",cmdX)
    task = subprocess.Popen(cmdX, shell=True, stdout=subprocess.PIPE)
    data = task.stdout.read()
    assert task.wait() == 0
    #print('data2=',data)
    ss=data.decode('utf-8')[:-1]
    #print( type(ss),'data3='+ss+"=")

    xx=json.loads(ss)
    xx["recoSizeGb"]="%.4f"%sizeGB

    return xx

#recoPath='aabb'
#recoJSN=readRecoFile( taskCore, recoPath  )
#print ('recoJSN=',recoJSN)


#====================
#  M A I N 
#===================

print("-------")
print("-------")
print("-------")
print(" MAIN , query tasks from list=%s  size =%d"%(taskF,len(taskDB)))
outJF=taskF+".json"

doneTaskD={}

if isAppend and os.path.isfile(outJF):
    print("append to ",outJF)
    nr=0
    nmu=0
    with open(outJF) as inpFile:
        for line in inpFile:
            #print (line)
            d=json.loads(line)
            nr=nr+1
            if d["nMuDst"] >=0:
                doneTaskD[d["core"]]=d
        inpFile.close
    print("found %d rec, nMu=%d"%(nr,len(doneTaskD)))

# produce now output, reuse records from completed muDst
outScanFD=open(outJF,"w")
nr=0
ns=0
nc=0
for core in taskDB:
    nr+=1
    if core in doneTaskD:
        json.dump(doneTaskD[core],outScanFD ) 
        outScanFD.write('\n')
        nc=nc+1
        #print("copied ",doneTaskD[core])
        continue
    #print(nr,core,taskDB[core])
    recoOutD={}
    recoOutD['core']=core
    (realSec,cpuEfi,nMuDst,startDate)=readOneLogFile(  logPath+core+".log")
    if startDate=='none':
        continue
    ns=ns+1
    #print ('ns=',ns,'realSec=',realSec, 'cpuEfi=',cpuEfi, 'cpuSec=',realSec*cpuEfi)
    #dataSizeGb=readDataFile( taskDB[core] )
    #print ('dataSizeGb=%.3f'%dataSizeGb)
    #recoOutD=readRecoFile( core, recoPath  )
    #recoOutD['dataSizeGb']=dataSizeGb
    #
    recoOutD['realSec']=realSec
    recoOutD['cpuEfi']=cpuEfi
    recoOutD['startDate']=startDate
    recoOutD['nMuDst']=nMuDst

    print (ns,nc,nr,"of",len(taskDB),'recoOutD=',recoOutD)
    json.dump(recoOutD,outScanFD ) 
    outScanFD.write('\n')
    if nr>=100999:
      break	

