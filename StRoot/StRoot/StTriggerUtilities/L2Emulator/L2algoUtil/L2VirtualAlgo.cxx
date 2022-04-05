#include <string.h>
#include <stdio.h>

#ifdef  IS_REAL_L2  //in l2-ana  environment
  #include "../L2algoUtil/L2Histo.h"
  #include "trgStructures.h"
 #else
  #include "StDaqLib/TRG/trgStructures.h"
  #include "StTriggerUtilities/L2Emulator/L2algoUtil/L2Histo.h"
#endif

#include "L2VirtualAlgo.h"
//=============================================
L2VirtualAlgo::L2VirtualAlgo(const char* name, L2EmcDb* db, char* outDir, int resOff) :  mDb(db), mResultOffset(resOff) {
  strncpy(mName, name,sizeof(mName));  
  strncpy(mOutDir,outDir,sizeof(mOutDir));
  setOflTrigID(0);
  mhT=new   L2Histo(901,"L2 time used per input event;  x: time (CPU kTics); y: events ",400);
} 

//=============================================
void 
L2VirtualAlgo::finishCommonHistos() {
  
  int iMax=-3, iFWHM=-4;
  mhT->findMax( &iMax, &iFWHM);
  printf("L2:%s  CPU/eve MPV %d kTicks,  FWHM=%d, seen eve=%d\n",mName,iMax, iFWHM,mEventsInRun);

  if (mLogFile==0)  return; // failed open log file, skip
  fprintf(mLogFile,"L2:%s  CPU/eve MPV %d kTicks,  FWHM=%d, seen eve=%d\n",mName,iMax, iFWHM,mEventsInRun);
  mhT->print(0,mLogFile); 
  // mhT->printCSV(mLogFile);
   
  if (mHistFile) mhT->write(mHistFile);
}

//=============================================

L2VirtualAlgo::~L2VirtualAlgo(){};

//=============================================
int 
L2VirtualAlgo::readParams(const char *fileN, int mxPar, int *iPar, float *fPar) {
  /* return:
    <0 : error in input
    >=0 : # of valid params : int+float
  */

  memset(iPar,0,mxPar*sizeof(int));
  memset(fPar,0,mxPar*sizeof(int));
  FILE *fd=fopen(fileN,"r");
  if(fd==0) { printf("   L2VirtualAlgo::readParams failed to open =%s=\n",fileN); return -2222;}

  int nVal=0; // sum of read in ints & floats
  int nInt=0, nFloat=0; // # of read in values
  
  const int mx=1000;
  char buf[mx];
  int mode=0; // 1=int, 2=float
  
  for(;;) { 
    char * ret=fgets(buf,mx,fd);
    //printf("xx1=%p\n",ret);
    if(ret==0) break;
    if(buf[0]==0) continue;
    if(buf[0]=='#') continue;
    if(buf[0]=='\n') continue;

    if (mode==0 && strstr(buf,"INTS")) { mode=1; continue; }
    if (mode==1 && strstr(buf,"FLOATS")) { mode=2; continue; }
    // printf("AA %d =%s= %p %p \n",mode,buf,strstr(buf,"INTS"),strstr("FLOATS",buf));
    if(mode==1) { // ints[]
      if(nInt>=mxPar) {nVal=-501; break;} // too many int-params
      int ret1=sscanf(buf,"%i",iPar+nInt); 
      if(ret1!=1)  {nVal=-100*mode -nInt; break;} // wrong input file for this int-par
      nInt++;    nVal++;
    }
    else  if(mode==2) { // floats[]
      if(nFloat>=mxPar) {nVal=-502; break;} // too many float-params
      int ret1=sscanf(buf,"%f",fPar+nFloat); 
      if(ret1!=1) {nVal= -100*mode -nFloat; break;} // wrong input file for this float-par
      nFloat++;    nVal++;
    }

  }

  fclose(fd);
  printf("    L2VirtualAlgo::readParams %d from '%s'\n",nVal,fileN); 
  return nVal;
}
