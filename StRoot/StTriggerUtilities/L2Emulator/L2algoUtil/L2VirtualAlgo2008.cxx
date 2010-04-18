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

//#define ADD_HARDCODED_DELAY // take it off for real on-line

#include "L2VirtualAlgo2008.h"
//=============================================
L2VirtualAlgo2008::L2VirtualAlgo2008(const char* name, L2EmcDb* db, char* outDir) :  mDb(db) {
  mxHA=0;// initialy no user defined histos
  mName1=name;
  mOutDir1=outDir;

  // map L2event variables for _read_
  mEveStream_btow=globL2eventStream2008.get_btow();
  mEveStream_etow=globL2eventStream2008.get_etow();

  setOflTrigID(0); // relevant only for offline analysis
  mhN =new   L2Histo(900,"total events 0=anyInput, 10=anyAccept; x=cases",19);
  mhTc=new   L2Histo(901,"L2 COMPUTE time per input event;  x: COMPUTE time (CPU kTics); y: events ",180);
  mhTd=new   L2Histo(902,"L2 DECISION time per input event;  x: DECISION time (CPU kTics); y: events ",36);
  mhTcd=new  L2Histo(903,"L2 COMP+DECI time per input event;  x: COMP+DECIS time (CPU kTics); y: events ",180);

  int mxRunDration=200;
  mhRc= new   L2Histo(905,"rate of COMPUTE; x: time in this run (seconds); y: rate (Hz)", mxRunDration);
  mhRd= new   L2Histo(906,"rate of DECISION; x: time in this run (seconds); y: rate (Hz)", mxRunDration);
  mhRa= new   L2Histo(907,"rate of ACCEPT; x: time in this run (seconds); y: rate (Hz)", mxRunDration);
  printf("L2-%s instantiated, logPath='%s'\n",getName(),mOutDir1.c_str());
  
  // consistency checks, should never fail
  assert(L2eventStream2008::mxToken == L2eventStream2008::tokenMask+1);
} 

/*========================================
  ======================================== */
int 
L2VirtualAlgo2008::initRun( int runNo, int *rc_ints, float *rc_floats) {
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

#ifdef ADD_HARDCODED_DELAY
    fprintf(mLogFile,"WARN: HARDCODED_DELAY in compute() & decision() is ON\n");
#endif 

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
      fprintf(mLogFile,"L2-%s histos NOT saved, I/O error\n",getName());
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
  const char *text[nHt]={"Compute  ","Decision ","Deci+Comp"};
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
L2VirtualAlgo2008::compute(int token){
  /* STRICT TIME BUDGET  START ....*/
  computeStart();
  mhN->fill(1);
  token&=L2eventStream2008::tokenMask; // only protect against bad token, Gerard's trick
  
#ifdef ADD_HARDCODED_DELAY
  /* for testing of histos,  
     adds 3kTicks delay,  - to see sth in the spectra 
     even if algo is very fast */
  for(int i=0;i<3*100;i++) { float x=i*i; x=x;}
#endif 

  computeUser( token );
  computeStop( token);
  
}

//=============================================
void
L2VirtualAlgo2008::computeStart(){

  /* STRICT TIME BUDGET  START ....*/
  unsigned int high,low;
  rdtsc_macro(low,high);  // needs also high to get tim in seconds

  mComputeTimeStart=low;
  unsigned long long ticks = high;
  ticks  <<= 32;
  ticks  +=  low;

  // the line below costs ~100 ticks, wherase use of time(0) costs ~10,000 ticks
  mSecondsInRun=(ticks- mRunStartTicks)/par_cpuTicksPerSecond;  
 
  mhRc->fill(mSecondsInRun);

  mAccept=true; // by default accept every event
  mEventsInRun++;

  mhN->fill(0);
}

//=============================================
void
L2VirtualAlgo2008::computeStop(int token){

  rdtscl_macro(mComputeTimeStop);
  unsigned long xxx=mComputeTimeStop-mComputeTimeStart;
  mComputeTimeDiff[token]=xxx;
  int  kTick=xxx/1000;
  //printf("jj delT/kTick=%f t1=%d t2=%d \n",mComputeTimeDiff/1000.,mComputeTimeStart,mComputeTimeStop);
  mhTc->fill(kTick);
}


//=============================================
bool
L2VirtualAlgo2008::decision(int token, void **myL2Result){
  /* STRICT TIME BUDGET  START ....*/
  /*
    Chris doesn't want us to write  out anything
    during event processing ...
  */

  rdtscl_macro(mDecisionTimeStart);
  token&=L2eventStream2008::tokenMask; // only protect against bad token, Gerard's trick
  mDecisionTimeDiff=0;

  mhRd->fill(mSecondsInRun);

#ifdef ADD_HARDCODED_DELAY
  /* for testing of histos,  
     adds 3kTicks delay,  - to see sth in the spectra 
     even if algo is very fast */
  for(int i=0;i<3*100;i++) { float x=i*i; x=x;}
#endif 

  mhN->fill(2);
  mAccept=decisionUser(token, myL2Result);
  //printf("compuDDDD tkn=%d  dec=%d myRes=%p\n",token,mAccept, *myL2Result);

  if(mAccept) { 
    mhN->fill(10);
    mhRa->fill(mSecondsInRun);
  }
  rdtscl_macro(mDecisionTimeStop);
  mDecisionTimeDiff=mDecisionTimeStop-mDecisionTimeStart;
  int  kTick=mDecisionTimeDiff/1000;
 
  // printf("deci-%s compT=%d;  deciT=%d kTick=%d\n", getName(),mComputeTimeDiff,mDecisionTimeDiff,kTick);
  mhTd->fill(kTick);
  kTick=(mDecisionTimeDiff+mComputeTimeDiff[token])/1000;
  mhTcd->fill(kTick);
  return mAccept;
}



/* ========================================
  ======================================== */
void 
L2VirtualAlgo2008::printCalibratedData(int token){ //
  // now print always BARREL - fix it later
  int i;
  const int hitSize=mEveStream_btow[token].get_hitSize();
  printf("printCalibratedData-%s: ---BTOW ADC list--- size=%d\n",getName(),hitSize);
   const HitTower1 *hit=mEveStream_btow[token].get_hits();
  for(i=0;i< hitSize;i++,hit++) {
    int adc=hit->adc;
    int rdo=hit->rdo;
    float et=hit->et;
    float ene=hit->ene;
    printf("  btow: i=%2d rdo=%4d  adc=%d  et=%.3f  ene=%.3f\n",i,rdo,adc,et,ene);
  }
}


/******************************************************
  $Log: L2VirtualAlgo2008.cxx,v $
  Revision 1.7  2010/04/18 06:05:32  pibero
  Address compiler warnings.

  Revision 1.6  2008/01/30 21:56:40  balewski
  E+B high-enery-filter L2-algo fuly functional

  Revision 1.5  2008/01/30 00:47:15  balewski
  Added L2-Etow-calib

  Revision 1.4  2008/01/18 23:29:12  balewski
  now L2result is exported

  Revision 1.3  2008/01/17 23:15:51  balewski
  bug in token-addressed memory fixed

  Revision 1.2  2008/01/16 23:32:33  balewski
  toward token dependent compute()


 
*/
