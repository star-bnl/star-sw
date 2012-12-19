#include <string.h>
#include <stdio.h>
#include <cstdlib>

#include "fakeRtsLog.h"

#ifdef  IS_REAL_L2  //in l2-ana  environment
  #include "../L2algoUtil/L2Histo.h"
  #include "../L2algoUtil/L2EmcDb2012.h"
#else  //full path needed for cvs'd code
  #include "StTriggerUtilities/L2Emulator/L2algoUtil/L2Histo.h"
  #include "StTriggerUtilities/L2Emulator/L2algoUtil/L2EmcDb2012.h"
#endif

//#define ADD_HARDCODED_DELAY // take it off for real on-line

#include "L2VirtualAlgo2012.h"
//=============================================
L2VirtualAlgo2012::L2VirtualAlgo2012(const char* name, const char *uid, L2EmcDb2012* db, char* outDir, bool needbarrel, bool needendcap, int resOff) :  mDb(db) {
  algoIsOkay=true; //whether the algorithm is in a functional state.  innocent until proven guilty.
  mxHA=0;// initially no user defined histos
  mName1=name;
  mName1 = mName1 + "." + uid;
  mOutDir1=outDir;
  mNeeds_barrel=needbarrel;
  mNeeds_endcap=needendcap;
  mRunNumber=-1;
  mResultOffset=resOff;

  // map L2event variables for _read_
  mEveStream_btow=globL2eventStream2012.get_btow();
  mEveStream_etow=globL2eventStream2012.get_etow();

  setOflTrigID(0); // relevant only for offline analysis
  mhN =new   L2Histo(900,"total events. 0=anyInput 10=anyAccept 11=normalAccept 12=rndAccept",19);
  mhTc=new   L2Histo(901,"L2 COMPUTE time per input event;  x: COMPUTE time (CPU kTics); y: events ",180);
  mhTd=new   L2Histo(902,"L2 DECISION time per input event;  x: DECISION time (CPU kTics); y: events ",36);
  mhTcd=new  L2Histo(903,"L2 COMP+DECI time per input event;  x: COMP+DECIS time (CPU kTics); y: events ",180);

  int mxRunDration=2000;
  mhRc= new   L2Histo(905,"rate of COMPUTE; x: time in this run (seconds); y: rate (Hz)", mxRunDration);
  mhRd= new   L2Histo(906,"rate of DECISION; x: time in this run (seconds); y: rate (Hz)", mxRunDration);
  mhRa= new   L2Histo(907,"rate of ACCEPT; x: time in this run (seconds); y: rate (Hz)", mxRunDration);
  
  // consistency checks, should never fail

  if (!(L2eventStream2012::mxToken == L2eventStream2012::tokenMask+1))
    {
      char err[200];
      sprintf(err,"Algo %s has failed consistency check '(L2eventStream2012::mxToken == L2eventStream2012::tokenMask+1)'",name);
      criticalError(err);
    }
} 

/*========================================
  ======================================== */
int 
L2VirtualAlgo2012::initRun( int runNo, int *rc_ints, float *rc_floats) {
  if(!algoIsOkay) 
    {
      char err[100];
      sprintf(err,"%s is flagged as broken.  Aborting init.",getName());
      criticalError(err);
      return -999;
    }

  useDsmMask=false;


  if(mDb->getRun()!=runNo) return -700; // L2EmcDb not initialized properly
  
  if(mRunNumber==runNo) {  
    if (mLogFile) fprintf(mLogFile,"#L2-%s::initRun(%d)=ghost already initilized, Abort run\n",getName(), runNo);
    return -701;
  }

  //clear VirtualAlgo histograms:
  mhN->reset();
  mhTc->reset();
  mhTd->reset();
  mhTcd->reset();
  mhRc->reset();
  mhRd->reset();
  mhRa->reset();


  unsigned int high,low;
  rdtsc_macro(low,high);  // needs also high to get tim in seconds
  mRunStartTicks=high;  mRunStartTicks <<= 32; mRunStartTicks  +=  low;

  mRunNumber  =runNo;  // serves as a flag this run is initialized
  mEventsInRun=0;

  char Fname[1000];
  sprintf(Fname,"%s/run%d.l2%s.log",mOutDir1.c_str(),mRunNumber,getName());
  
  mLogFile = fopen(Fname,"w");
  if( mLogFile==0) {
    LOG(ERR," L2-%s() UNABLE to open run summary log file, continue anyhow (%s)",getName(),Fname);
  }

  //set default for par_RndAcceptPrescale, just in case initRunUser() doesn't define it:
  par_RndAcceptPrescale=0;//no random accepts.
  mRndAcceptCounter=0;

  int kBad=initRunUser( runNo, rc_ints, rc_floats); 

  //setup the random accept stuff:

  mRandomAccept=false; //initialize the mRandomAccept to false,
  //this covers the case where the prescale=0.  Note this is NOT a boolean of whether
  //we are randomly accepting events, but just the marker for whether to accept the
  //most recent event given.

  //par_RndAcceptPrescale is set in initRunUser(), but check range:
  if (par_RndAcceptPrescale<0) par_RndAcceptPrescale=0;
  if (par_RndAcceptPrescale>0)
      mRndAcceptCounter=rand()%par_RndAcceptPrescale;


  if (mLogFile) {
    fprintf(mLogFile,"#L2-%s initRun() params checked for consistency, Error flag=0x%04x\n",getName(),kBad);
  }
  
  if(kBad<0)  { 
    if (mLogFile) {
      fprintf(mLogFile,"#L2-%s initRun()  ABORT due to internal logic\n",getName());
      fclose(mLogFile);
    }
    mRunNumber=-55;
    return kBad;
  }
  
  if (mLogFile) {
    fprintf(mLogFile,"#L2-%s random accept counter started at %d\n",getName(),mRndAcceptCounter);
    fprintf(mLogFile,"#L2-%s initRun successful\n",getName());

#ifdef ADD_HARDCODED_DELAY
    fprintf(mLogFile,"#WARN: HARDCODED_DELAY in compute() & decision() is ON\n");
#endif 

  }

  return kBad;
}

/*========================================
  ======================================== */
void 
L2VirtualAlgo2012::finishRun() {  /* called once at the end of the run */

  if(mRunNumber<0) return; // already finished
  if(mLogFile)fprintf(mLogFile,"#L2-%s: finishRun(%d) called after %d seconds\n",getName(),mRunNumber ,mSecondsInRun);
  

  // save run summary histos
  char Fname[1000];
  sprintf(Fname,"%s/run%d.l2%s.hist.bin",mOutDir1.c_str(),mRunNumber,getName());
  mHistFile = fopen(Fname,"w");
  
  
  if( mHistFile==0) {
    LOG(ERR," L2-%s: finishRun() UNABLE to open run summary log file, continue anyhow (fn=%s)\n",getName(),Fname);
    if (mLogFile)
      fprintf(mLogFile,"#L2-%s histos NOT saved, I/O error\n",getName());
  } else { // save histos  
    finishRunUser(); 
    if (mLogFile)
      mhN->printCSV(mLogFile);
    int nh=finishCommonHistos();
    if (mLogFile)
      fprintf(mLogFile,"#L2-%s: %d histos saved to '%s'\n",getName(),nh,Fname);
  }
  
  if (mLogFile && useDsmMask)
    {
      fprintf(mLogFile,"#L2-%s: %d DSM masks are used.\n",getName(),nmasks);
      for (int i=0;i<nmasks;i++)
	  fprintf(mLogFile,"#  Mask %d: 0x%04x 0x%04x 0x%04x 0x%04x 0x%04x 0x%04x 0x%04x 0x%04x\n",
		  i,DsmMask[i][0],DsmMask[i][1],DsmMask[i][2],DsmMask[i][3],DsmMask[i][4],
		  DsmMask[i][5],DsmMask[i][6],DsmMask[i][7]);
    }
  mRunNumber=-2; // clear run #
  for (int i=0; i<mxHA;i++) if(hA[i])hA[i]->reset();
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
L2VirtualAlgo2012::finishCommonHistos() {
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
    
    if (mLogFile){
      fprintf(mLogFile,"#L2:%s  %s CPU/eve MPV %d kTicks,  FWHM=%d, seen eve=%d\n",getName(),text[ih],iMax, iFWHM,mEventsInRun);
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

L2VirtualAlgo2012::~L2VirtualAlgo2012(){};

//=============================================
int 
L2VirtualAlgo2012::readParams(const char *fileN, int mxPar, int *iPar, float *fPar) {
  /* return:
    <0 : error in input
    >=0 : # of valid params : int+float
  */

  memset(iPar,0,mxPar*sizeof(int));
  memset(fPar,0,mxPar*sizeof(int));
  FILE *fd=fopen(fileN,"r");
  if(fd==0) { LOG(ERR,"L2VirtualAlgo2012::readParams failed to open =%s=\n",fileN); return -2222;}

  int nVal=0; // sum of read in ints & floats
  int nInt=0, nFloat=0; // # of read in values
  
  const int mx=1000;
  char buf[mx];
  int mode=0; // 1=int, 2=float
  
  for(;;) { 
    char * ret=fgets(buf,mx,fd);
    if(ret==0) break;
    if(buf[0]==0) continue;
    if(buf[0]=='#') continue;
    if(buf[0]=='\n') continue;

    if (mode==0 && strstr(buf,"INTS")) { mode=1; continue; }
    if (mode==1 && strstr(buf,"FLOATS")) { mode=2; continue; }
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
  LOG(DBG,"L2VirtualAlgo2012::readParams %d from '%s'\n",nVal,fileN); 
  return nVal;
}

 
//=============================================
void
L2VirtualAlgo2012::compute(int token){
  /* STRICT TIME BUDGET  START ....*/
  computeStart();
  mhN->fill(1);
  token&=L2eventStream2012::tokenMask; // only protect against bad token, Gerard's trick
  
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
L2VirtualAlgo2012::computeStart(){

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
L2VirtualAlgo2012::computeStop(int token){

  rdtscl_macro(mComputeTimeStop);
  unsigned long xxx=mComputeTimeStop-mComputeTimeStart;
  mComputeTimeDiff[token]=xxx;
  int  kTick=xxx/1000;
  mhTc->fill(kTick);
}


//=============================================
bool
L2VirtualAlgo2012::decision(int token, bool barrel_is_in, bool endcap_is_in, int *myL2Result){
  /* STRICT TIME BUDGET  START ....*/
  /*
    Chris doesn't want us to write  out anything
    during event processing ...
  */

  LOG(DBG, "Decision...");

  rdtscl_macro(mDecisionTimeStart); //start timer
  token&=L2eventStream2012::tokenMask; // only protect against bad token, Gerard's trick
  mDecisionTimeDiff=0;

  mhRd->fill(mSecondsInRun);

#ifdef ADD_HARDCODED_DELAY
  /* for testing of histos,  
     adds 3kTicks delay,  - to see sth in the spectra 
     even if algo is very fast */
  for(int i=0;i<3*100;i++) { float x=i*i; x=x;}
#endif 

  mhN->fill(2);

  if (par_RndAcceptPrescale>0)
    {
      mRndAcceptCounter=(mRndAcceptCounter+1)%par_RndAcceptPrescale;
      mRandomAccept=(mRndAcceptCounter==0);
    }

  //if a component is needed but not included, do not call decision(), set accept to 0.

  LOG(DBG, "bin %d needsb %d ee %d ne %d",
      barrel_is_in, mNeeds_barrel, endcap_is_in, mNeeds_endcap);

  if ( (!endcap_is_in && mNeeds_endcap) || (!barrel_is_in && mNeeds_barrel) ) 
    {
      mAccept=0;
    }
  else
    {
      mAccept=decisionUser(token, myL2Result+mResultOffset);
    }

  if(mAccept || mRandomAccept) { 
    mhN->fill(10); //Breakdown of how many events got to each stage.  10=any accept
    mhRa->fill(mSecondsInRun);
  }
  if(mRandomAccept) 
    mhN->fill(12); //same.  12=random accept
  if(mAccept)
    mhN->fill(11); //same.  11=regular accept


  rdtscl_macro(mDecisionTimeStop); //stop timer
  mDecisionTimeDiff=mDecisionTimeStop-mDecisionTimeStart;
  int  kTick=mDecisionTimeDiff/1000;
 
  mhTd->fill(kTick);
  kTick=(mDecisionTimeDiff+mComputeTimeDiff[token])/1000;
  mhTcd->fill(kTick);
  return (mAccept || mRandomAccept);
}



/* ========================================
  ======================================== */
void 
L2VirtualAlgo2012::printCalibratedData(int token){ //
  // now print always BARREL - fix it later
  int i;
  const int hitSize=mEveStream_btow[token].get_hitSize();
  LOG(NOTE,"printCalibratedData-%s: ---BTOW ADC list--- size=%d\n",getName(),hitSize);
   const HitTower1 *hit=mEveStream_btow[token].get_hits();
  for(i=0;i< hitSize;i++,hit++) {
    int adc=hit->adc;
    int rdo=hit->rdo;
    float et=hit->et;
    float ene=hit->ene;
    LOG(NOTE,"  btow: i=%2d rdo=%4d  adc=%d  et=%.3f  ene=%.3f\n",i,rdo,adc,et,ene);
  }
}


/* ========================================
  ======================================== */
void 
L2VirtualAlgo2012::criticalError(const char* message){
  algoIsOkay=false;
#ifdef IS_REAL_L2
  LOG(CRIT,"%s",message,0,0,0,0);
#else
  LOG(CRIT,"CRITICAL MESSAGE: %s\n",message);
  //asert(1==2);
#endif
  return;
}


//three functions needed for the temporary hack for the old TCU:
bool  L2VirtualAlgo2012::checkDsmMask(unsigned short *lastDSM)
{
  if (!useDsmMask)  return 1;  //short circuit if we're not using a mask (ie new TCU is being used).
  if (nmasks==0) return 1; //if we don't have any masks set up for some reason, return a 'yes'

  bool isGood;
  for (int i=0;i<nmasks;i++)
    {
      isGood=true;
      for (int j=0;j<8 && isGood;j++)
	{
	  if((swap_bytes(lastDSM[j]) & DsmMask[i][j]) != DsmMask[i][j]) isGood=false;
	}
      if (isGood) return 1;
    }
  return 0; //we had masks and none of them matched, hence return 'no'.
}

int L2VirtualAlgo2012::readDsmMask(const char *fileN)
{
  for (int i=0;i<kMaximumNumberOfDsmMasks;i++)
    for (int j=0;j<8;j++)
      DsmMask[i][j]=0;

  FILE *fd=fopen(fileN,"r");
  if(fd==0) { 
    LOG(ERR,"   L2VirtualAlgo2012::readDsmMask failed to open =%s=.  Assuming no mask.\n",fileN); 
    return 0; //0=no error occurred.
    useDsmMask=false;
  }

  int n=0; // # of read in values
  nmasks=0; //number of masks being used
  
  const int mx=1000;
  char buf[mx];
  
  for(;;) { 
    char * ret=fgets(buf,mx,fd);
    if(ret==0) break;
    if(buf[0]==0) continue;
    if(buf[0]=='#') continue;
    if(buf[0]=='\n') continue;
    if(nmasks>=kMaximumNumberOfDsmMasks) {LOG(ERR,"   L2VirtualAlgo2012::readDsmMask:  Too many masks %s.\n",fileN); return -3333;}
    int ret1=sscanf(buf,"%hu",&(DsmMask[nmasks][n%8])); 
    if(ret1!=1)  {LOG(ERR,"   L2VirtualAlgo2012::readDsmMask: Problem reading %s.\n",fileN); return -4444;} // wrong input file for this int-par
    n++;
    if (n%8==0) nmasks++;
  }
  fclose(fd);
  if (n%8!=0){LOG(ERR,"   L2VirtualAlgo2012::readDsmMask:  Wrong number of arguments in %s (n=%d).\n",fileN,n); return -3333;}
  useDsmMask=true;
  return 0;  //0=no error occurred
}

unsigned short L2VirtualAlgo2012::swap_bytes(unsigned short in)
{
  unsigned short out=0;
  unsigned char *a, *b;

  a=(unsigned char*)(&in);
  b=(unsigned char*)(&out);

  b[1]=a[0];
  b[0]=a[1];
  return out;
}
//ewnd of the three old TCU functions



/******************************************************
  $Log: L2VirtualAlgo2012.cxx,v $
  Revision 1.6  2012/12/19 17:25:18  jml
  logging

  Revision 1.5  2012/03/21 18:18:03  jml
  got rid of printfs from 2012 files

  Revision 1.4  2011/10/19 15:58:06  jml
  more compile offline

  Revision 1.3  2011/10/19 15:39:42  jml
  2012

  Revision 1.2  2011/10/19 14:34:23  jml
  added fakeRtsLog.h to turn log statements into printfs

  Revision 1.1  2011/10/18 15:11:41  jml
  adding 2012 algorithms

  Revision 1.6  2011/04/02 20:35:51  pibero
  Initialize mRunNumber to -1

  Revision 1.5  2010/04/18 06:05:32  pibero
  Address compiler warnings.

  Revision 1.4  2010/04/17 05:01:27  pibero
  Updates for Run 9 jet tree production

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
