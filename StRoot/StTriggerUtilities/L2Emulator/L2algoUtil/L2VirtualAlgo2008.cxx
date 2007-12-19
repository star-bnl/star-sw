#include <string.h>
#include <stdio.h>

#ifdef  IS_REAL_L2  //in l2-ana  environment
  #include "trgStructures.h"
  #include "../L2algoUtil/L2Histo.h"
  #include "../L2algoUtil/L2EmcDb.h"
 #else
  #include "StDaqLib/TRG/trgStructures.h"
  #include "StTriggerUtilities/L2Emulator/L2algoUtil/L2Histo.h"
  #include "StTriggerUtilities/L2Emulator/L2algoUtil/L2EmcDb.h"
#endif

#include "L2VirtualAlgo2008.h"
//=============================================
L2VirtualAlgo2008::L2VirtualAlgo2008(const char* name, L2EmcDb* db, char* outDir) :  mDb(db) {
  mxHA=0;// initialy no user defined histos
  mName1=name;
  mOutDir1=outDir;

  // map L2event variables for _read_
  globEve_btow_hitSize=globL2event2008.get_btow_hitSize();
  globEve_btow_hit=globL2event2008.get_btow_hits();

  setOflTrigID(0); // relevant only for offline analysis
  mhN =new   L2Histo(900,"total events 0=anyInput, 10=anyAccept; x=cases",19);
  mhTc=new   L2Histo(901,"L2 COMPUTE time per input event;  x: COMPUTE time (CPU kTics); y: events ",400);
  mhTd=new   L2Histo(902,"L2 DECISION time per input event;  x: DECISION time (CPU kTics); y: events ",40);
  mhTcd=new  L2Histo(903,"L2 COMP+DECI time per input event;  x: COMP+DECIS time (CPU kTics); y: events ",400);

  int mxRunDration=200;
  mhRc= new   L2Histo(905,"rate of COMPUTE; x: time in this run (seconds); y: rate (Hz)", mxRunDration);
  mhRd= new   L2Histo(906,"rate of DECISION; x: time in this run (seconds); y: rate (Hz)", mxRunDration);
  mhRa= new   L2Histo(907,"rate of ACCEPT; x: time in this run (seconds); y: rate (Hz)", mxRunDration);
  printf("L2-%s instantiated, logPath='%s'\n",getName(),mOutDir1.c_str());
  
} 

/*========================================
  ======================================== */
int 
L2VirtualAlgo2008::initRun( int runNo, int *rc_ints, float *rc_floats) {
  mEventID=-1; // clear local event identificator
  if(mDb->getRun()!=runNo) return -700; // L2EmcDb not initialized properly
  
  if(mRunNumber==runNo) {  
    if (mLogFile) fprintf(mLogFile,"L2-%s::initRun(%d)=ghost already initilized, Abort run\n",getName(), runNo);
    return -701;
  }

  unsigned int high,low;
  rdtsc_macro(low,high);  // needs also high to get tim in seconds
  mRunStartTicks=high;  mRunStartTicks <<= 32; mRunStartTicks  +=  low;

  mRunNumber  =runNo;  // serves as a flag this run is initialized
  
  char Fname[1000];
  sprintf(Fname,"%s/run%d.l2%s.log",mOutDir1.c_str(),mRunNumber,getName());
  printf("L2-%s::initRun('%s') ...\n",getName(),Fname);
  
  mLogFile = fopen(Fname,"w");
  if( mLogFile==0) printf(" L2-%s() UNABLE to open run summary log file, continue anyhow\n",getName());

  int kBad=initRunUser( runNo, rc_ints, rc_floats); 

  if (mLogFile) {
    fprintf(mLogFile,"L2-%s initRun() params checked for consistency, Error flag=0x%04x\n",getName(),kBad);
  }
  
  if(kBad)  { 
    if (mLogFile) {
      fprintf(mLogFile,"L2-%s initRun()  ABORT due to internal logic\n",getName());
      fclose(mLogFile);
  }
    mRunNumber=-55;
  }
  if (mLogFile) {
    fprintf(mLogFile,"L2-%s initRun succesfull\n",getName());
  }

  // printf("L2initRaunVirtual2008-%s kBad=%d\n",getName(),kBad);

  return kBad;
}

/*========================================
  ======================================== */
void 
L2VirtualAlgo2008::finishRun() {  /* called once at the end of the run */

  if(mRunNumber<0) return; // already finished
  if(mLogFile)fprintf(mLogFile,"L2-%s: finishRun(%d) called after %d seconds\n",getName(),mRunNumber ,mSecondsInRun);
  

  // save run summary histos
  char Fname[1000];
  sprintf(Fname,"%s/run%d.l2%s.hist.bin",mOutDir1.c_str(),mRunNumber,getName());
  printf("\nL2:%s::finishRun('%s') , save histo ...\n",getName(),Fname);
  mHistFile = fopen(Fname,"w");
  
  
  if( mHistFile==0) {
    printf(" L2-%s: finishRun() UNABLE to open run summary log file, continue anyhow\n",getName());
    if (mLogFile)
      fprintf(mLogFile,"L2-%d histos NOT saved, I/O error\n",getName());
  } else { // save histos  

    finishRunUser(); 
    int nh=finishCommonHistos();

    if (mLogFile)
      fprintf(mLogFile,"L2-%s: %d histos saved to '%s'\n",getName(),nh,Fname);
  }
  
  mRunNumber=-2; // clear run #

  /* close the output file if it is open */
  if (mLogFile) {
    fclose(mLogFile);
    mLogFile=0;
  }

  if ( mHistFile) {
    fclose(mHistFile);
    mHistFile=0;
  }
  
}


//=============================================
int  
L2VirtualAlgo2008::finishCommonHistos() {
  int j;
  int nh=0;
  for(j=0;j<mxHA;j++) {
    if(hA[j]==0) continue;
    hA[j]->write(mHistFile);
    nh++;
  }
  
  const int nHt=3;
  L2Histo *hT[nHt]={mhTc,mhTd,mhTcd};
  char *text[nHt]={"Compute  ","Decision ","Comp+Deci"};
  int ih;
  for(ih=0;ih<nHt;ih++) {
    int iMax=-3, iFWHM=-4;
    hT[ih]->findMax( &iMax, &iFWHM);
    printf("L2-%s  %s CPU/eve MPV %d kTicks,  FWHM=%d, seen eve=%d\n",getName(),text[ih],iMax, iFWHM,mEventsInRun);
    
    if (mLogFile){
      fprintf(mLogFile,"L2:%s  %s CPU/eve MPV %d kTicks,  FWHM=%d, seen eve=%d\n",getName(),text[ih],iMax, iFWHM,mEventsInRun);
      // hT[ih] ->print(0,mLogFile);   // mhT->printCSV(mLogFile);
    }
    if (mHistFile) {hT[ih]->write(mHistFile); nh++;}
  }
  
  if (mHistFile) {
    mhN->write(mHistFile); nh++;
    mhRc->write(mHistFile);nh++;
    mhRd->write(mHistFile);nh++;
    mhRa->write(mHistFile);nh++;
  }
  return nh;
}

//=============================================

L2VirtualAlgo2008::~L2VirtualAlgo2008(){};

//=============================================
int 
L2VirtualAlgo2008::readParams(const char *fileN, int mxPar, int *iPar, float *fPar) {
  /* return:
    <0 : error in input
    >=0 : # of valid params : int+float
  */

  memset(iPar,0,mxPar*sizeof(int));
  memset(fPar,0,mxPar*sizeof(int));
  FILE *fd=fopen(fileN,"r");
  if(fd==0) { printf("   L2VirtualAlgo2008::readParams failed to open =%s=\n",fileN); return -2222;}

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
  printf("    L2VirtualAlgo2008::readParams %d from '%s'\n",nVal,fileN); 
  return nVal;
}

 
//=============================================
void
L2VirtualAlgo2008::compute(int flag, int inpL2EveId){
  /* STRICT TIME BUDGET  START ....*/
  if(mEventID==inpL2EveId) return; // never process the same event twice
  computeStart(flag, inpL2EveId);
  computeUser(flag, inpL2EveId);
  computeStop();
}

//=============================================
void
L2VirtualAlgo2008::computeStart(int flag, int inpL2EveId){
  /* STRICT TIME BUDGET  START ....*/
  unsigned int high,low;
  rdtsc_macro(low,high);  // needs also high to get tim in seconds

  mComputeTimeDiff=0;
  mComputeTimeStart=low;
  unsigned long long ticks = high;
  ticks  <<= 32;
  ticks  +=  low;

  // the line below costs ~100 ticks, wherase use of time(0) costs ~10,000 ticks
  mSecondsInRun=(ticks- mRunStartTicks)/par_cpuTicksPerSecond;  
 
  mhRc->fill(mSecondsInRun);

  mEventID=inpL2EveId;
  mAccept=true; // by default accept every event
  mEventsInRun++;

  mhN->fill(0);
}

//=============================================
void
L2VirtualAlgo2008::computeStop(){

  // HARDCODED DELAY
  // for(int i=0;i<5*100;i++) { float x=i*i; x=x;}// to add 5kTicks delay, tmp

  rdtscl_macro(mComputeTimeStop);
  mComputeTimeDiff=mComputeTimeStop-mComputeTimeStart;
  int  kTick=mComputeTimeDiff/1000;
  //printf("jj delT/kTick=%f t1=%d t2=%d \n",mComputeTimeDiff/1000.,mComputeTimeStart,mComputeTimeStop);
  mhTc->fill(kTick);
}


//=============================================
bool
L2VirtualAlgo2008::decision(int flag, int inpL2EveId){
  /* STRICT TIME BUDGET  START ....*/
  /*
    Chris doesn't want us to write  out anything
    during event processing ...
  */

  if(mEventID!=inpL2EveId) return true; // accept every event which was not 'computed'
  rdtscl_macro(mDecisionTimeStart);
  mDecisionTimeDiff=0;

  mhN->fill(1);
  mhRd->fill(mSecondsInRun);

  // HARDCODED DELAY
  // tmporary, for testing of histos, it costs 3 kTicks
  for(int i=0;i<3*100;i++) { float x=i*i; x=x;}// to add 3kTicks delay, tmp

  mAccept=decisionUser(flag, inpL2EveId);
  if(mAccept) { 
    mhN->fill(10);
    mhRa->fill(mSecondsInRun);
  }
  rdtscl_macro(mDecisionTimeStop);
  mDecisionTimeDiff=mDecisionTimeStop-mDecisionTimeStart;
  int  kTick=mDecisionTimeDiff/1000;
 
  // printf("deci-%s compT=%d;  deciT=%d kTick=%d\n", getName(),mComputeTimeDiff,mDecisionTimeDiff,kTick);
  mhTd->fill(kTick);
  kTick=(mDecisionTimeDiff+mComputeTimeDiff)/1000;
  mhTcd->fill(kTick);
  return mAccept;
}
