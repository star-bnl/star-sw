#include <stdio.h>


#include <stdio.h>
#include "L2gammaAlgo.h"
#include "L2gammaResult2006.h"

#include "bemcConstants.h"
#include "eemcConstants.h"

void L2gammaAlgo::setLogFile( const char *fname){ mLogFile=fopen(fname,"w"); }
void L2gammaAlgo::setHistFile( const char *fname  ){ mHistFile=fopen(fname,"w"); }

//#define DEBUG
//#define DEBUG_PATCH_THRESHOLDS
// ----------------------------------------------------------------------------
L2gammaAlgo::L2gammaAlgo(const char* name, L2EmcDb* db, char* outDir, int resOff) 
  :  L2VirtualAlgo( name,  db,  outDir, resOff)
{

  int geom,  thresh; // temporary fix,JB
#if 1
  if(strstr(name,"etow_gamma")) {
    geom= L2gammaAlgo::kEEmcAlgo; thresh = L2gammaAlgo::kThresh1;
  } else if(strstr(name,"etow_ht2")) {
   geom= L2gammaAlgo::kEEmcAlgo; thresh = L2gammaAlgo::kThresh2;
 } else if(strstr(name,"btow_gamma")) {
   geom= L2gammaAlgo::kBEmcAlgo; thresh = L2gammaAlgo::kThresh1;
 } else if(strstr(name,"btow_ht2")) {
   geom= L2gammaAlgo::kBEmcAlgo; thresh = L2gammaAlgo::kThresh2;
 } else {
   printf("undefined config of L2gamma-algo ->%s<-, abort L2main\n",name);
   assert(1==2);
 }
#endif

  
  printf("L2gammaEmCall2006 instantiated geom=%d thresh=%d logpath=%s\n",geom,thresh,mOutDir);
  int I_par[]={
    1, // 0=bemc, 1=eemc
    0, // prescale
    0, // free
    0, // free
    0  // free
  };
  float F_par[]={
    2.50, // high tower threshold
    3.50, // 3x3 patch threshold
    0.,   // free
    0.,   // free
    0.    // free
  };  
  mThresholdLevel=thresh;
  init(geom,I_par,F_par);
  mL2input=0;
  mPrescale=0;

}


// ----------------------------------------------------------------------------
//
// perform initialization at L2 main program startup not
// done in the constructor
//
void L2gammaAlgo::init(int geom, int I_par[5], float F_par[5] )
{

  printf("+ init geom=%d\n",geom);

  mLogFile=0;
  mHistFile=0;
  mUseOfflineGains = I_par[0];
  mUseBbc=false;
  setTowerThreshold( F_par[0] );
  setPatchThreshold( F_par[1] );

  for ( int i=0;i<5;i++ )
    {
      mDefaultI_par[i]=I_par[i];
      mDefaultF_par[i]=F_par[i];
    }
  
  if ( I_par[1] ) mPrescale=I_par[1];

  /* 
   * Select endcap or barrel based on the geom flag.
   */
  mEEmc=geom;mBEmc=!geom;

  mNumEtas = (mEEmc)? kEEmcNumEtas : kBEmcNumEtas; /* number of etabins */
  mNumPhis = (mEEmc)? kEEmcNumPhis : kBEmcNumPhis; /* number of phi bins */
  mNumSubs = (mEEmc)? kEEmcNumSubs : kBEmcNumSubs; /* number of subsectors */
  mNumSecs = (mEEmc)? kEEmcNumSecs : kBEmcNumSecs; /* number of sectors */
  
  mNumTower = (mEEmc)? kEEmcNumTower : kBEmcNumTower;
  mNumClust = mNumTower;
  mNumRdo   = mNumTower;
  mEtaBins  = (mEEmc)? kEEmcEtaBins : kBEmcEtaBins;

  mMaxADC = (mEEmc)? kEEmcMaxADC : kBEmcMaxADC;
  mMaxET  = (mEEmc)? kEEmcMaxET  : kBEmcMaxET;
  mIdealGainT = (mEEmc)? kEEmcIdealGainT : kBEmcIdealGainT;

  const char *names[]={"bemc","eemc"};

  printf("L2gammaAlgo v0.92 (prociutto)                   \n");
  printf("registering new threshold for %4s \n",names[geom]);

#ifdef DEBUGIT
  printf("                                                         \n");

  printf("============================================================================\n");
  //  Jason, I am commenting this for the last time.
  // there is no benefit of rinting this on the L2 console.
  // If this printout of any value write it to a file saved on /data/... disk as all other outputs. Then at least you can retrieve it after the run.
  //Or activate this only in the debug mode - you can have GUI param for debug - if you want

  printf("algorithm residing at %p\n",this);
  printf("l2algo used as input at %p\n",mL2input);
  printf("\n");
  printf("\n");
  printf("mNumEtas  = %-2d\n",mNumEtas);
  printf("mNumPhis  = %-4d\n",mNumPhis);
  printf("mNumSubs  = %-4d\n",mNumSubs);
  printf("mNumSecs  = %-4d\n",mNumSecs);
  printf("mNumTower = %-4d\n",mNumTower);
  printf("mNumClust = %-4d\n",mNumClust);
  printf("mNumRdo   = %-4d\n",mNumRdo);
  printf("mEtaBins  = {");
  for ( int i=0;i<mNumEtas+1;i++ ) { 
    printf("%4.2f, ",mEtaBins[i]);
    if ( !((i+1)%4) ) printf("\n               ");
  }
  printf("X}\n");
  printf("mMaxADC   = %4d\n",mMaxADC);
  printf("mMaxET    = %4.1f\n",mMaxET);
  printf("mIdealGainT = %5.3f\n",mIdealGainT);
  printf("\n");
  printf("============================================================================\n");
  
#endif

  // book "histograms" for QA (clears from previous run)
  jbook();

}



// ----------------------------------------------------------------------------
//
// runtime initialization
//
// at this stage, time is not overly critical.  so we will take 
// the time to do most of the hard work.  we will do two things:
//
// 1) establish a high tower threshold for all 720 towers in
//    terms of adc.  This will be the high tower threshold (in
//    adc) plus pedestal.  Thus we will not need to do a ped
//    subtraction in the event loop.
//
// 2) We implement a 3x3 tower patch around each tower.  For 
//    each rdo channel we determine the number of (physically)
//    adjacent towers and 
//
// 3) An optional parameter (use offline gains) will correct
//    the ADC thresholds for gain variations measured offline.
//    When applied to patch sum thresholds, we will also need
//    to apply correction to the towers in the patch sums.
//
int L2gammaAlgo::initRun( int run )
{
  return initRun(run,mDefaultI_par,mDefaultF_par);
}
int L2gammaAlgo::initRun( char *myname, int run, int I_par[5], float F_par[5] )
{
  return initRun( run, I_par, F_par );
}
int L2gammaAlgo::initRun( int run, int I_par[5], float F_par[5] )
{


  printf("%s ::initRun() for run=%d\n",mName,run);
  
  if ( mDb->initRun(run) ) return -7; // this must be in, if your algos are the only one in the game, _you_ must initialize DB for the new run,JB

  mRunNumber = run;

  // recreate histograms (clears)
  //$$$  jbook();
  jclear();

  mUseOfflineGains = I_par[0];
  if ( I_par[1] ) mPrescale = I_par[1];
  mUseBbc=false;
  setTowerThreshold( F_par[0] );
  setPatchThreshold( F_par[1] );


  /***********************************************************************
   * test validity of input parameters
   */
  {
    
    if ( F_par[1] < 0. ) {
      printf("L2gammaAlgo::ERROR -- patch threshold < 0\n");
      return 1;
    }
    if ( F_par[1] > 2.0 * F_par[0] ) {
      printf("L2gammaAlgo::ERROR -- patch threshold > 2x tower threshold\n");
      return 1;
    }
    
    if ( mL2input )
      {
	if ( F_par[0] < mL2input->getPatchThreshold() ) {
	  printf("L2gammaAlgo::WARNING -- l2input to this algorithm has a patch threshold < tower threshold\n");
	  //return 1;
	}
      }

    if ( I_par[0] < 0 || I_par[0] > 1 ) 
      {
	printf("L2gammaAlgo::ERROR -- I_par[0] should be 0 (use ideal gains) or 1 (use online gains)\n");
      }
    
  }/* valid */


  // close histogram and log files and reopen w/ new paths
  if ( mLogFile && mLogFile != stdout ) fclose(mLogFile);
  if ( mHistFile ) fclose(mHistFile);

  //const char *names[]={"bemc","eemc"};

  char clog[128];
  sprintf(clog,"%s/run%d.l2%s.out",mOutDir,run,mName);
  //printf("%s=\n",clog);
  //setHistFile(chis);
  //  setLogFile(clog);
  mLogFile=fopen(clog,"w");
  // assert(mLogFile);
  if(mLogFile==0)  printf("%s ::initRun() for run=%d UNABLE to open log-file=%s=, it is not fatal, continue initialization\n",mName,run,clog);

  if(mLogFile) {
    fprintf(mLogFile,"\n\n====================================================================================================================================\n");
    fprintf(mLogFile,"L2gammaAlgo start of run %d  summary, compiled: %s , %s\n",run,__DATE__,__TIME__);
    fprintf(mLogFile,"calorimeter:              %s\n",mName);
    fprintf(mLogFile,"run:                      %d\n",run);
    fprintf(mLogFile,"use offline gains   I[0]: %d\n",mUseOfflineGains);
    fprintf(mLogFile,"prescaled accept    I[1]: %d\n",mPrescale);
    fprintf(mLogFile,"threshold level:          %d\n",mThresholdLevel);
    fprintf(mLogFile,"tower threshold     F[0]: %-5.2f [GeV]\n",mTowerThreshold);
    fprintf(mLogFile,"patch threshold     F[1]: %-5.2f [GeV]\n",mPatchThreshold);
    fprintf(mLogFile,"patch size:               %s\n","3x3"); /* may change in later revisions */
    fprintf(mLogFile,"logfile:                  %s\n",clog);
    //  fprintf(mLogFile,"histograms:               %s\n",chis);
    fprintf(mLogFile,"database at:              %p\n",mDb);
    fprintf(mLogFile,"l2 input at:              %p\n",mL2input);
    
    fprintf(mLogFile,"\nDetector Geometry\n");
    fprintf(mLogFile,"mEtaBins  = {");
    for ( int i=0;i<mNumEtas+1;i++ ) { 
      fprintf(mLogFile,"%4.2f, ",mEtaBins[i]);
      if ( !((i+1)%4) ) fprintf(mLogFile,"\n               ");
    }
    fprintf(mLogFile,"X}\n");
    fprintf(mLogFile,"mMaxADC   = %4d\n",mMaxADC);
    fprintf(mLogFile,"mMaxET    = %4.1f\n",mMaxET);
    fprintf(mLogFile,"mIdealGainT = %5.3f\n",mIdealGainT);
    fprintf(mLogFile,"\n");
    
    fprintf(mLogFile,"\n");
  }
  if ( !mDb ) return 100;

   

  /// Reset event counters
  mNumberInput    = 0;
  mNumberAcceptHT = 0;
  mNumberAcceptTP = 0;

  mNumberLive=0;

  /// read in database for this run
  if(mLogFile)  fprintf(mLogFile,"initialize EMC ascii database\n\n");
  //$$$ assume it's already initialized by a call in main??? gEmcCDb_init();
  if(mLogFile)  fprintf(mLogFile,"\n");

  ///
  /// determine ideal gains for each of the eta bins
  ///
  //float emc_ideal_gains[mNumEtas];
  float *emc_ideal_gains = new float[mNumEtas];
  if(mLogFile)  fprintf(mLogFile,"configure ideal gains for %d eta bins\n",mNumEtas);
  for ( int i=0;i<mNumEtas;i++ )
    {
      float eta_mean = mEtaBins[ i ] + mEtaBins[ i+1 ];
      eta_mean /= 2.0;
      emc_ideal_gains[i] = mMaxADC / mMaxET / cosh(eta_mean);
     if(mLogFile)   fprintf(mLogFile,"+ etabin=%d eta=%5.2f bemc ideal gain=%5.2f\n", i+1, eta_mean, emc_ideal_gains[i]);
      
    }

  if ( mNumEtas != 12 && mNumEtas != 40 ) {
    if(mLogFile)  fprintf(mLogFile,"L2gammaEmCal::FATAL ERROR -- expect 12 or 40 etabins, got %d\n",mNumEtas);
    if(mLogFile)  fprintf(mLogFile,"  + likely cause -- invalid runtime parameter\n");
    delete emc_ideal_gains;
    return 10; /* EMC not properly configured */
  }

  /// clear high tower and patch thresholds
  for ( int index=0;index<mNumTower;index++ )
    {
      mTowerAdcThreshold[index]=0;
      mPatchAdcThreshold[index]=mPatchThreshold;
      mTowerPed[index]=0.;
      mPatchPed[index]=0.;
      mTowerFrequency[index]=0;
      mPatchFrequency[index]=0;
      mTowerGain[index]=-1.0;
    }

  if(mLogFile)  fprintf(mLogFile,"\nInitializing tower ADC threshods\n");

  /// loop over all db entries and calculate ADC thresholds
  for ( int index=0; index<EmcDbIndexMax; index++ ) 
    {

      // get the item from the database
      //$$$struct EmcCDbItem *x = &gEmcCDbByIndex[index];
      const L2EmcDb::EmcCDbItem *x = mDb->getByIndex(index);
      if ( x==0 ) continue;

#if 1
      // make sure db item matches the calorimeter we're using
      if ( mBEmc && !mDb->isBTOW(x) )
	continue;
      if ( mEEmc && !mDb->isETOW(x) )
	continue;
#endif


      /* temporary kludge */
      //$$$  if ( x->gain < 0 ) x->fail |= 0x0080;
     
      // local numbering scheme counts from 0
      int sec = x->sec - 1;
      int sub = 8192;
      if ( mEEmc ) sub = x->sub - 'A';
      if ( mBEmc ) sub = x->sub - 'a';
      int eta = x->eta - 1;
      int phi = phibin(sec,sub); //mNumSubs*sec + sub;
      int tow = tower(phi,eta); //mNumEtas*phi + eta;
      int rdo = x->rdo;

      //fprintf(mLogFile,"sec=%d sub=%d eta=%d tow=%d\n",sec,sub,eta,tow);

      /* temporary kludge */
      //if ( eta < 20 ) x->fail |= 0x0080;


      // equal energy gain
      //float gain = x->gain;    
      float ped  = x->ped;
      float gain = x->gain;
      float ideal = emc_ideal_gains[eta];

      

      // check fail bit and artificially raise HT threshold 
      ushort stat = x->stat;   
      ushort fail = x->fail;   
      if ( gain < 0.0 ) {
	if(mLogFile) 	fprintf(mLogFile,"L2gammaEmCal::WARN %s gain=%5.2f will be masked\n",x->name,x->gain);
	fail = 0xffff;
      }

      mTowerStat[rdo]=stat; mPatchStat[rdo]=stat;
      mTowerFail[rdo]=fail; mPatchFail[rdo]=fail;
      mTowerGain[rdo]=gain;
      mTowerGainIdeal[rdo]=ideal;
      

      if ( !fail ) 
	mTowerAdcCorrection[rdo]=(mUseOfflineGains)?ideal/gain:1.0;        /** I really need to think about this! **/
      else
	mTowerAdcCorrection[rdo]=0.; /* tower is masked out */
            
      // calculate high tower threshold for this
      // rdo channel in ADC
      if ( !fail ) {
	mTowerAdcThreshold[rdo] = (ushort)(ped + (mIdealGainT * mTowerThreshold)/mTowerAdcCorrection[rdo] + 0.5 );
	mTowerPed[rdo]=ped;
      }
      else {
	mTowerAdcThreshold[rdo] = 4095;
	mTowerPed[rdo]=4095;
      }
      
                                                          /* upgrade: verify that tower threshold is above mMinAdc, issue warning and log */

      mRdo2tower[ rdo ] = tow;    // returns tower for given rdo channel
      mTower2rdo[ tow ] = rdo;    // returns rdo channel for given tower

      if ( !fail ) mNumberLive++;
 
    }





  // fprintf(mLogFile,"L2gammaAlgo::initRun(%d) initialize 3x3 patch thresholds for ET=%5.2f\n",mRunNumber,mPatchThreshold);

  if(mLogFile)  fprintf(mLogFile,"Initializing 3x3 tower cluster thresholds\n");

  /// second loop over all db entries, this time we calculate
  /// ADC thresholds on the 3x3 tower patch
  for ( int index=0; index<EmcDbIndexMax; index++ ) 
    {

      // get the item from the database
      //$$$   struct EmcCDbItem *x = &gEmcCDbByIndex[index];
      const L2EmcDb::EmcCDbItem *x = mDb->getByIndex(index);
      if ( x==0 ) continue;

      // make sure db item matches the calorimeter we're using
      if ( mBEmc && !mDb->isBTOW(x) )
	continue;
      if ( mEEmc && !mDb->isETOW(x) )
	continue;
     
      // local numbering scheme counts from 0
      int sec = x->sec - 1;
      int sub = 8192;
      if ( mEEmc ) sub = x->sub - 'A';
      if ( mBEmc ) sub = x->sub - 'a';
      int eta = x->eta - 1;
      int phi = phibin(sec,sub);
      //int tow = tower(phi,eta);
      int rdo = x->rdo;

      mPatchStat[rdo] |= x->stat;
      mPatchFail[rdo] |= x->fail;

      // masked towers will be ignored in the patch thresholds...
      // our butts are covered in the event that a crate is masked
      // out, by the requirement of a high tower in coincidence with
      // a 3x3 tower patch sum.
      if ( x->fail ) continue;
      if ( mTowerFail[rdo] ) continue;

      // equal energy gain
      //float gain = x->gain;    
      float ped  = x->ped; 


      /// loop over neighboring towers, **including the current
      /// tower**, and add ped/gain to the patch threshold.  the
      /// result should be sum_i ped_i/g_i for each rdo channel
      /// (plus the ET threshold already initialized).
      int nn=0;      
      for ( int ieta=eta-1;ieta<=eta+1;ieta++ )
	for ( int iphi=phi-1;iphi<=phi+1;iphi++ )
	  {

	    if ( ieta < 0 || ieta > mNumEtas-1 ) continue; // out of bounds
	    int jeta=ieta;
	    int jphi=(iphi+mNumPhis)%mNumPhis;
	    int jtow=tower(jphi,jeta);
	    int jrdo=mTower2rdo[jtow];

	    mPatchAdcThreshold[jrdo] += ped/mIdealGainT * 
	      mTowerAdcCorrection[rdo];

	    if ( jtow == 12 ) {
	      if(mLogFile)  fprintf(mLogFile,"+ name=%s ped=%5.2f gfact=%5.2f thresh=%5.2f\n",x->name,x->ped,mTowerAdcCorrection[rdo],mPatchAdcThreshold[jrdo] );
	    }

	    // add ped for this channel to all patches 
	    // which contain it
	    mPatchPed[jrdo]+=ped;

	    /// assign this rdo to the physical tower patch
	    mRdoPatch[rdo][nn++]=jrdo;
	    

	  }
      mNumPatch[rdo]=nn;

    }

  //fprintf(mLogFile,"done w/ initialization, writing to log file\n");


  //
  // Write to logfile (stdout for now) the thresholds generated
  //

  for ( int index=0;index<EmcDbIndexMax;index++ )
    {


      // get the item from the database
      //.      struct EmcCDbItem *x = &gEmcCDbByIndex[index];
      const L2EmcDb::EmcCDbItem *x = mDb->getByIndex(index);
      if ( x==0 ) continue;

#if 1
      // make sure db item matches the calorimeter we're using
      if ( mBEmc && !mDb->isBTOW(x) )
	continue;
      if ( mEEmc && !mDb->isETOW(x) )
	continue;
#endif
 
      float gain=x->gain;
      int   rdo=x->rdo;
      if ( gain==0. ) gain=-1.;

      int tow=mRdo2tower[rdo];
      int eta=tow%mNumEtas;


      /*
	Floating point exception occurs in my output???? 
      */

#if 0
      fprintf(mLogFile,"tower=%s tow=%d rdo=%d mask=%2x gain=%5.2f ideal=%5.2f ped=%5.2f thr=%d %5.2f GeV patch=%5.2f + %5.2f GeV\n",
	     x->name,
	     mRdo2tower[rdo],
	     rdo,
	     x->fail,
	     x->gain,
	     emc_ideal_gains[eta],
	     x->ped,
	     mTowerAdcThreshold[rdo],
	     (float)((mTowerAdcThreshold[rdo]-x->ped)*mMaxET/mMaxADC),
	     mPatchAdcThreshold[rdo]-mPatchThreshold,
	     mPatchThreshold);
#endif

#if 1
   if(mLogFile)     fprintf(mLogFile,"tower=%s tow=%d rdo=%d ideal=%5.2f thr=%d patch=%5.2f GeV\n",
	     x->name,
	     mRdo2tower[rdo],
	     rdo,
	     emc_ideal_gains[eta],
	     mTowerAdcThreshold[rdo],
	     mPatchAdcThreshold[rdo]);
#endif



    }

#ifdef DEBUG_PATCH_THRESHOLDS
  for ( int rdo=0;rdo<mNumRdo;rdo++ )
    printPatchConfig(rdo);
#endif

 if(mLogFile) {
   fprintf(mLogFile,"\n");
   fprintf(mLogFile,"total number of towers:        %d\n",mNumTower);
   fprintf(mLogFile,"number of unmasked towers:     %d\n",mNumberLive);
   fprintf(mLogFile,"\n====================================================================================================================================\n\n");
 }
 
  /*
   * cleanup allocated memory
   */
  delete emc_ideal_gains;

  return 0;

}/* ::initRun */



// -------------------------------------------------------------------------------------------- eval --
//
// evaluate trigger conditions
//
// most of the work has already been done for us.  at the start of
// the run, we took the user-supplied ET thresholds on the high 
// tower and associated cluster (patch) and determined two thresholds:
//
// o mTowerAdcThreshold[rdo] -- the ADC value for which the tower will
//   exceed the specified ET threshold (i.e. ped*4096/60+E_T).
//
// o mPatchAdcThreshold[rdo] -- actually a floating point ET threshold
//   to be calculated ONLY when we have a tower above the specified
//   ET threshold.
//
// therefore, the only thing we need to do w/in eval is to loop over
// all rdo's and find a high tower, then test the 3x3 tower cluster
// against its threshold.
//

bool  L2gammaAlgo::doEvent( int L0trigger, int inputEventID, TrgDataType* trgData,
			    int bemcIn, unsigned short *bemcData,
			    int eemcIn, unsigned short *eemcData )
{
  rdtscl_macro(mEveTimeStart);
  mAccept=false;
  if ( mEEmc ) {
    mAccept=doEvent( inputEventID, trgData, eemcIn, eemcData );
  }
  else {
    mAccept=doEvent( inputEventID, trgData, bemcIn, bemcData );
  } 

  rdtscl_macro(mEveTimeStop);
  mEveTimeDiff=mEveTimeStop-mEveTimeStart;
  int  kTick=mEveTimeDiff/1000;
  //   printf("gg=%f t1=%d t2=%d \n",mEveTimeDiff/1000.,mEveTimeStart,mEveTimeStop);
  mhT->fill(kTick);
  
  return mAccept;
}


bool L2gammaAlgo::doEvent( int inpEveId, TrgDataType* trgData, 
			   int  emcIn, unsigned short *emcData )
{
  //I think you will need to use inpEveId for second copy, if not - ignore it,JB
  clear();
  // we got nothin'
  if ( !emcIn ) return false;

  mNumberInput++;

  mHistos[0].fill(0); 

  //
  // first section of the code evaluates the trigger decision
  //

  // track execution time of the algorithm (in cpu ticks)
  unsigned long eval_time_start;
  unsigned long eval_time_stop;
  rdtscl_macro(eval_time_start);

  bool eht  = false;
  bool trig = false;

  float  sum=0.;                                   /* tower patch sum, 3x3 cluster around high tower */
  ushort htrdo=8192;                               /* rdo of high tower */
  ushort tprdo=8192;                               /* rdo of high cluster */

  nRdosHT=0;
  nRdosTP=0;

  // ------------------------------------------------------------------------------------ preprocess --
#ifdef OPT_PREPROCESS

  // If another "lower threshold" is being run as input to this algorithm,
  // then we process the results of that algorithm and jump to the QA
  // section.

  if ( mL2input ) {
    for ( ushort i=0;i<mL2input->getNumberOfHighTowers();i++ )
      {
	ushort rdo=mL2input->mListOfRdosHT[i];
#ifdef DEBUG
	mHistos[3].fill( emcData[rdo] );
#endif
	if ( emcData[rdo] < mTowerAdcThreshold[rdo]) continue;	
	mADCofHT[ nRdosHT ]      = emcData[rdo];
	mListOfRdosHT[ nRdosHT++ ] = rdo; /* store rdo of high tower */	
	mHistos[4].fill( emcData[rdo] );

	eht=true; 
	htrdo=rdo;
	sum = 0.;
	for ( int i=0;i<mNumPatch[rdo];i++ ) {
	  int prdo=mRdoPatch[rdo][i];
	  if ( mTowerFail[prdo] ) continue; /* skip masked towers */
	  sum += emcData[prdo] / mIdealGainT * mTowerAdcCorrection[rdo];
	}
	ushort sumx2=(ushort)(2.0*sum); 
#ifdef DEBUG
	mHistos[7].fill( sumx2 );
#endif

	if ( sum > mPatchAdcThreshold[rdo] ) {
	  trig=true;  
	  tprdo=rdo;
	  mETofTP[ nRdosTP ] = sum-mPatchAdcThreshold[rdo]+mPatchThreshold;
	  mListOfRdosTP[ nRdosTP++ ] = rdo; /* store rdo of cluster */
	  mHistos[5].fill( emcData[rdo] );	  
	  mHistos[8].fill( sumx2 );
	}
      }
    goto QA;
  }
#endif

  // ------------------------------------------------------------------------- standalone processing --

  // Here we loop over all RDO channels and determine whether thresholds have
  // been met.  We store results for higher thresholds/other algorithms to
  // process at a later point.

  for ( ushort rdo=0; rdo<mNumRdo; rdo++ ) 
    {

      // check if tower is above ADC=ET*gain+ped threshold
      // these two lines consume about 1/2 of the time of the 
      // algorithm for any given event.
#ifdef DEBUG
      mHistos[3].fill( emcData[rdo] );
#endif
      if ( emcData[rdo] < mTowerAdcThreshold[rdo]) continue;

      // past here, we have a high tower trigger.  when we
      // have such a trigger, the following code takes about
      // the same time as the preceding two lines.
      
      mADCofHT[ nRdosHT ]      = emcData[rdo];
      mListOfRdosHT[ nRdosHT++ ] = rdo;                   /* store rdo of high tower for 2nd stage */
      mHistos[4].fill( emcData[rdo] );
       
      eht=true; 
      htrdo=rdo;

      
      // Sum the 3x3 patch of towers around the high tower
      sum = 0.;
      for ( int i=0;i<mNumPatch[rdo];i++ ) {
	int prdo=mRdoPatch[rdo][i];
	if ( mTowerFail[prdo] ) continue; /* skip masked towers */
	sum += emcData[prdo] / mIdealGainT * mTowerAdcCorrection[rdo];
      }
      ushort sumx2=(ushort)(2.0*sum);
#ifdef DEBUG
      mHistos[7].fill( sumx2 );
#endif

      // we have found a high tower in coincidence 
      // with a 3x3 patch of towers above specified
      // thresholds
      if ( sum > mPatchAdcThreshold[rdo] ) {
	trig=true;  
	tprdo=rdo;
	mETofTP[ nRdosTP ] = sum-mPatchAdcThreshold[rdo]+mPatchThreshold;
	mListOfRdosTP[ nRdosTP++ ] = rdo; /* store rdo of cluster */
	mHistos[5].fill( emcData[rdo] );
	mHistos[8].fill( sumx2 );
      }

    }
  
  // ------------------------------------------------------------------------ process QA histograms --

  // Fill QA histograms, set bits in the L2 result, and return the result

#ifdef OPT_PREPROCESS
 QA:
#endif 

  bool prescaleAccept = false;
  if ( mPrescale>0 ) 
    prescaleAccept = !((mNumberInput) % mPrescale);

  mHistos[1].fill(nRdosHT); // Number of high towers > threshold
  mHistos[2].fill(nRdosTP); // Number of clusters > threshold


  if ( eht ) { 
    mNumberAcceptHT++;
    mTowerFrequency[mRdo2tower[htrdo]]++;
    mHistos[0].fill(1);
  }

  if ( trig ) {
    mNumberAcceptTP++;
    mPatchFrequency[mRdo2tower[tprdo]]++;
    mHistos[0].fill(2);
  }
  rdtscl_macro(eval_time_stop);

  /* algorithm selects fraction of events to do QA on */
  unsigned long qa_time_start;
  unsigned long qa_time_stop;
  rdtscl_macro(qa_time_start);
     
  // ------------------------------------------------------------------------- save trigger decision --
  

 L2gammaResult mResult;
  // initialize "result"
  mResult.result_version    = LEVEL2_GAMMA_RESULT_VERSION;
  mResult.threshold         = (mThresholdLevel==kThresh1)? (0x1|0x2) : (0x4|0x8);
  // reset monitor portion of L2gammaResult
  mResult.elapsed=0x00;
  // clear L2 trigger result
  mResult.trigger = 0x00;

  float max=0.;
  ushort maxrdo=8192;

  // set high tower bit
  if ( eht ) 
    mResult.trigger |= 0x1; 


  // set cluster bit and 
  for ( ushort i=0;i<nRdosTP;i++ )
    {
      if ( mETofTP[i] > max ) {
	max=mETofTP[i];
	maxrdo=mListOfRdosTP[i];
      }
    }
  if ( maxrdo < mNumRdo ) 
    {
      mResult.trigger |= 0x2;
      ushort tow=mRdo2tower[maxrdo];
      mResult.phibin=tow/mNumEtas;
      mResult.etabin=tow%mNumEtas;
      max-=mPatchAdcThreshold[maxrdo];
      max+=mPatchThreshold;
      if ( max < 127.5 )
	mResult.ptclusterx2 = ((ushort)(2.0*max));
      else
	mResult.ptclusterx2 = 255;
      float pttow=emcData[maxrdo]-mTowerPed[maxrdo];
      pttow*=mTowerAdcCorrection[maxrdo]/mIdealGainT;
      if ( pttow < 127.5 )
	mResult.pttowerx2 = (ushort)(2.0*pttow);
      else
	mResult.pttowerx2 = 255;
      if ( mEEmc ) mResult.phibin |= 0x8;
   }


  // set prescale bit
  if ( prescaleAccept )
    mResult.trigger |= 0x4;

  // set trigger bit
  if ( trig || prescaleAccept )
    mResult.trigger |= 0x8;
   
  rdtscl_macro(qa_time_stop);
  int dt=(int)(qa_time_stop-eval_time_start)/1000;

  mEvalTime += dt; 
  //mResult.mon.elapsed = 0;

  /**
   ** Get pointer to block of bytes where L2gammaResult will reside
   **/
  unsigned int *pResult 
//  = &trgData->TrgSum.L2Result[L2RESULTS_OFFSET_PIG +2*mEEmc+mThresholdLevel];
    = &trgData->TrgSum.L2Result[mResultOffset+2*mEEmc+mThresholdLevel];

  /**
   ** And write to L2result
   **/
  memcpy(pResult,&mResult,sizeof(L2gammaResult));
  //#define __L2GAMMA_PRINT_RESULT__
#ifdef __L2GAMMA_PRINT_RESULT__
  print_L2gammaResult(mResult);
#endif

  /**
   ** Test trig and prescale and fill QA histograms when they are satisfied
   **/
 

  if ( trig || prescaleAccept )
    {
      mHistos[0].fill(3);
      for ( ushort i=0;i<nRdosHT;i++ ) {
	int tower=(int)mRdo2tower[mListOfRdosHT[i]];
	mHistos[6].fill( (mADCofHT[i] - (ushort)mTowerPed[ mListOfRdosHT[i] ])/5,tower );
	mHistos[13].fill(tower);
      }

      for ( ushort i=0;i<nRdosTP;i++ ) {
	int tower=(int)mRdo2tower[mListOfRdosTP[i]];
	mHistos[9].fill( (int)(mETofTP[i]*2),tower );
	mHistos[14].fill( tower );
      }

      mHistos[18].fill( dt );

    }

  mHistos[15].fill(dt);
  if ( eht ) mHistos[16].fill(dt);
  if ( trig ) mHistos[17].fill(dt);

  return ( trig || prescaleAccept );
  
}

// ----------------------------------------------------------------------------
//

void L2gammaAlgo::clear()
{

  nRdosHT=0;
  nRdosTP=0;

}

// ----------------------------------------------------------------------------

void L2gammaAlgo::finishRun()
{

  int run=mRunNumber;

 
  char chis[128];
  printf("%s/run%d.l2%s.hist.bin\n",mOutDir,run,mName);
  sprintf(chis,"%s/run%d.l2%s.hist.bin",mOutDir,run,mName);
  printf("L2gamma:saving '%s'\n",chis);
  //  mLogFile=fopen(clog,"a");
  //  mLogFile=stdout;

  mHistFile=fopen(chis,"w");
  if(mHistFile==0)  printf("%s ::finishRun() for run=%d UNABLE to open histo-file=%s=, continue initialization\n",mName,run,chis); 
  //assert( mHistFile);

  //
  // trap some potential divide by zeros but still do output
  //
  if ( !mNumberInput ) mNumberInput=-1; /* short/nonexistent run */
  if ( !mNumberLive  ) mNumberLive=-1;  /* db is ft^ */


  //  if ( !mLogFile ) 
  //    mLogFile = stdout;
 if(mLogFile) {
   fprintf(mLogFile,"\n\n===================================================================================================================================\n");
   fprintf(mLogFile,"L2gammaAlgo end of run %d summary\n",run);
   fprintf(mLogFile,"run:            %d\n",run);
   fprintf(mLogFile,"tower threshold: %5.2f [GeV]\n",mTowerThreshold);
   fprintf(mLogFile,"patch threshold: %5.2f [GeV]\n",mPatchThreshold);
   fprintf(mLogFile,"patch size:      3x3\n"); /* may change in later revisions */
   fprintf(mLogFile,"eval time:      %d [kTicks]\n",mEvalTime);
   fprintf(mLogFile,"avg time/event: %d [kTicks]\n",mEvalTime/mNumberInput);
   fprintf(mLogFile,"# input:        %d\n",mNumberInput);
   fprintf(mLogFile,"# ht accept:    %d\n",mNumberAcceptHT);
   fprintf(mLogFile,"# ht+tp accept: %d\n",mNumberAcceptTP);
   fprintf(mLogFile,"\n");
   
   //
   // first section, loop over the tower and patch frequencies
   // to look for hot towers
   //
   float avgt=0,avgp=0;
   int maxtf=0,maxpf=0;
   
   for ( int tow=0;tow<mNumTower;tow++ )
     {       
       avgt+=mTowerFrequency[tow];
       avgp+=mPatchFrequency[tow];      
       if ( mTowerFrequency[tow]>maxtf ) maxtf=mTowerFrequency[tow];
       if ( mPatchFrequency[tow]>maxpf ) maxpf=mTowerFrequency[tow];
     }
   avgt=( ((float)avgt)/(float)mNumberLive );
   avgp=( ((float)avgp)/(float)mNumberLive );
   
   //  fprintf(mLogFile,"sumt=%5.2f sump=%5.2f nlive=%d\n",avgt,avgp,mNumberLive);
   fprintf(mLogFile,"max # ht accept in one tower:          %d\n",maxtf);
   fprintf(mLogFile,"max # ht+tp accept in one tower:       %d\n",maxpf);
   fprintf(mLogFile,"avg # ht accept:                       %6f\n",avgt);
   fprintf(mLogFile,"avg # ht+tp accept:                    %6f\n",avgp);
   fprintf(mLogFile,"expected # ht accept in one tower:     %6f\n",((float)mNumberAcceptHT)/mNumberLive);
   fprintf(mLogFile,"expected # ht+tp accept in one tower:  %6f\n",((float)mNumberAcceptTP)/mNumberLive);
   fprintf(mLogFile,"\n");
   
   
  //#define DEBUG_PATCH_THRESHOLDS

#ifndef DEBUG_PATCH_THRESHOLDS
   fprintf(mLogFile,"Summary of hot towers (# ht accept > 3*average or # ht+tp accept > 3*average):\n\n");

   for ( int tow=0;tow<mNumTower;tow++ )
     {
       int phi=tow/mNumEtas;
       int eta=tow%mNumEtas;
       int sec=phi/mNumSubs;
       int sub=phi%mNumSubs;
       int myrdo=mTower2rdo[tow];
       
       
       bool hott = (mTowerFrequency[tow] > (3.0*avgt));
       bool hotp = 0 && (mPatchFrequency[tow] > (3.0*avgp));
       
       /*           ^^^^^^^^^^^^^ need a statistically robust sample or we will get noise! */
       
       if ( hott||hotp ) fprintf(mLogFile,"\n");
       
       if ( mBEmc ) 
	 {
	   if ( hott )
	     fprintf(mLogFile,"L2gammaAlgo::WARNING -- possible hot tower %02dt%c%02d freq=%d avg=%5.2f\n",sec+1,sub+'a',eta+1,mTowerFrequency[tow],avgt);
	   if ( hotp )
	     fprintf(mLogFile,"L2gammaAlgo::WARNING -- possible hot patch %02dt%c%02d freq=%d avg=%5.2f\n",sec+1,sub+'a',eta+1,mPatchFrequency[tow],avgp);
	 }
       else 
	 {
	   if ( hott )
	     fprintf(mLogFile,"L2gammaAlgo::WARNING -- possible hot tower %02dT%c%02d freq=%d avg=%5.2f\n",sec+1,sub+'A',eta+1,mTowerFrequency[tow],avgt);
	   if ( hotp )
	     fprintf(mLogFile,"L2gammaAlgo::WARNING -- possible hot patch %02dT%c%02d freq=%d avg=%5.2f\n",sec+1,sub+'A',eta+1,mPatchFrequency[tow],avgp);
	 }
       
       if ( hott || hotp )
	 { 
	   
	   printPatchConfig( myrdo );
	   
	 } 
       
     }
 }
#endif
   
 if ( mLogFile ) {
   fprintf(mLogFile,"\n====================================================================================================================================\n\n");
   finishCommonHistos();

 }
 
  finish(); /* output histogram */

}

void L2gammaAlgo::finish()
{
  // Write histograms at end of run
  if ( mHistFile ) {
    for ( unsigned int i=0;i<sizeof(mHistos)/sizeof(L2Histo);i++)
      {
	mHistos[i].write(mHistFile);
      }
    fflush(mHistFile);
  }
}

// ----------------------------------------------------------------------------

void L2gammaAlgo::jclear()
{
  for ( int i=0;i<19;i++ ) mHistos[i].reset();
}
void L2gammaAlgo::jbook()
{
  // Book L2 histograms for QA

  // on first call book histograms
  if ( 1 ) {

    /* Event counters */
    
    mHistos[0]=L2Histo(100, "Counter.  0=N_{input} 1=N_{ht} 2=N_{clust} 3=N_{accept}", 10);
    mHistos[1]=L2Histo(101, "N high towers > threshold", 10);
    mHistos[2]=L2Histo(102, "N clusters > threshold", 10);
    
    /* ADC spectra */
    
    mHistos[3]=L2Histo(103, "Raw ADC of ht (if DEBUG)", 1024);
    mHistos[4]=L2Histo(104, "Raw ADC of ht, ht > threshold", 1024);
    mHistos[5]=L2Histo(105, "Raw ADC of ht, cluster > threshold", 1024);
    mHistos[6]=L2Histo(106, "HT event accepted; X: (ADC-ped)/5;  Y: tower index", 128,mNumTower);

    /* Cluster spectra */
    
    mHistos[7]=L2Histo(107, "2*ET sum of cluster, ht > threshold", 32);
    mHistos[8]=L2Histo(108, "2*ET sum of cluster, cluster > threshold", 32);
    mHistos[9]=L2Histo(109, "2*ET sum of cluster - ped/gain, event accepted", 32,mNumTower);
    
    /* BBC difference (not implemented) */
    
    mHistos[10]=L2Histo(110, "Raw BBC TAC difference+256 (not implemented)", 1);
    mHistos[11]=L2Histo(111, "Raw BBC TAC difference+256, ht > threshold (not implemented)", 1);
    mHistos[12]=L2Histo(112, "Raw BBC TAC difference+256, cluster > threshold (not implemented)" , 1);

    /* Tower and cluster trigger frequencies */
    
    mHistos[13]=L2Histo(113, "High tower > threshold frequency, X: ieta+mNumEta*iphi", mNumTower);
    mHistos[14]=L2Histo(114, "Cluster > threshold frequency, X: ieta+mNumEta*iphi", mNumTower);
    
    /* Event timing */
    
    mHistos[15]=L2Histo(115, "kTicks / input event", 128);
    mHistos[16]=L2Histo(116, "kTicks / ht event", 128);
    mHistos[17]=L2Histo(117, "kTicks / cluster event", 128);
    mHistos[18]=L2Histo(118, "kTicks / trigger", 128);

  }

  
}

// ----------------------------------------------------------------------------
#ifndef DEBUG_PATCH_THRESHOLDS
void L2gammaAlgo::printPatchConfig( int rdo )
{

  // get the tower id
  int tow = mRdo2tower[rdo];
  int phi = tow/mNumEtas;
  int eta = tow%mNumEtas;

  int sec = phi/mNumSubs;
  int sub = phi%mNumSubs;

  /*
   
  The goal here is to output several quantities in an ascii 
  format which matches the physical layout of the towers in
  order to help us debug problems online

  as least as far as the endcap is concerned (I can switch the
  order for the barrel if need be.  Specifically, we want the
  following output:

  tower: 06TC05
  gain:  22.64
  ped:   20.90
  thr:   212 [2.8 GeV]

  pedestals           gains, stat, fail, high tower frequency, etc...

  18.7 | -3.0 | 22.5
  -----+-----+-----
  22.9 | 20.9 | 15.9
  -----+------+-----
  11.1 | 16.7 | 32.0

  */

  if ( mEEmc ) 
    fprintf(mLogFile,"etow:  %02dT%c%02d\n",sec+1,sub+'A',eta+1);
  if ( mBEmc )
    fprintf(mLogFile,"btow:  %02dt%c%02d\n",sec+1,sub+'a',eta+1);    

  fprintf(mLogFile,"index: %4d\n",tow);
  fprintf(mLogFile,"gain:  %6.3f [ideal=%6.3f]\n",mTowerGain[rdo],mTowerGainIdeal[rdo]);
  fprintf(mLogFile,"ped:   %6.3f\n",mTowerPed[rdo]);
  fprintf(mLogFile,"thr:   %3d [%4.2f GeV]\n",mTowerAdcThreshold[rdo],mTowerAdcThreshold[rdo]/mIdealGainT);
  fprintf(mLogFile,"\n");

  //      14.3 | 14.5 |  8.8      22.6 | 22.9 | 21.6      80   | 80   | 80         0   |  0   |  0           1 |    0 |    0
  fprintf(mLogFile,"loaded peds             gain factors            status bits             mask bits                frequency > thr\n");

  for ( int i_eta=eta+1;i_eta>=eta-1;i_eta-- ) {

    int phi_l = (phi+mNumPhis-1)%mNumPhis;
    int phi_r = (phi+mNumPhis+1)%mNumPhis;
    int phi_c = phi;

    //int phis[]={phi_l,phi,phi_r};
    float peds[]={0,0,0};
    float gains[]={0,0,0};
    ushort stats[]={0xff,0xff,0xff};
    ushort fails[]={0xff,0xff,0xff};
    int freqs[]={0,0,0};

    // get numbers from db
    if ( i_eta < mNumEtas && i_eta >= 0 ) {

      int tow_l = tower(phi_l,i_eta);
      int tow_r = tower(phi_r,i_eta);
      int tow_c = tower(phi_c,i_eta);

      int rdo_l = mTower2rdo[tow_l];
      int rdo_r = mTower2rdo[tow_r];
      int rdo_c = mTower2rdo[tow_c];

      peds[0]=mTowerPed[rdo_l];peds[1]=mTowerPed[rdo_c];peds[2]=mTowerPed[rdo_r];
      gains[0]=mTowerAdcCorrection[rdo_l];gains[1]=mTowerAdcCorrection[rdo_c];gains[2]=mTowerAdcCorrection[rdo_r];
      stats[0]=mTowerStat[rdo_l];stats[1]=mTowerStat[rdo_c];stats[2]=mTowerStat[rdo_r];
      fails[0]=mTowerFail[rdo_l];fails[1]=mTowerFail[rdo_c];fails[2]=mTowerFail[rdo_r];
      freqs[0]=mTowerFrequency[tow_l];freqs[1]=mTowerFrequency[tow_c];freqs[2]=mTowerFrequency[tow_r];


    }

    fprintf(mLogFile, "%4.1f | %4.1f | %4.1f\t", peds[0],peds[1],peds[2] );
    fprintf(mLogFile, "%4.2f | %4.2f | %4.2f\t", gains[0],gains[1],gains[2] );
    fprintf(mLogFile, "%2x   | %2x   | %2x  \t", stats[0],stats[1],stats[2] );
    fprintf(mLogFile, "%2x   | %2x   | %2x  \t", fails[0],fails[1],fails[2] );
    fprintf(mLogFile, "%4.0f | %4.0f | %4.0f\t", (float)freqs[0],(float)freqs[1],(float)freqs[2] );

    fprintf(mLogFile,"\n");

    if ( i_eta > eta-1 ) {
      fprintf(mLogFile, "-----+------+-----\t");
      fprintf(mLogFile, "-----+------+-----\t");
      fprintf(mLogFile, "-----+------+-----\t");
      fprintf(mLogFile, "-----+------+-----\t");
      fprintf(mLogFile, "-----+------+-----\t");
      fprintf(mLogFile, "\n" );
    }

  }

}
#endif
#ifdef DEBUG_PATCH_THRESHOLDS
void L2gammaAlgo::printPatchConfig( int rdo )
{

  // get the tower id
  int tow = mRdo2tower[rdo];
  int phi = tow/mNumEtas;
  int eta = tow%mNumEtas;

  int sec = phi/mNumSubs;
  int sub = phi%mNumSubs;

  /*
   
  The goal here is to output several quantities in an ascii 
  format which matches the physical layout of the towers in
  order to help us debug problems online

  as least as far as the endcap is concerned (I can switch the
  order for the barrel if need be.  Specifically, we want the
  following output:

  tower: 06TC05
  gain:  22.64
  ped:   20.90
  thr:   212 [2.8 GeV]

  pedestals           gains, stat, fail, high tower frequency, etc...

  18.7 | -3.0 | 22.5
  -----+------+-----
  22.9 | 20.9 | 15.9
  -----+------+-----
  11.1 | 16.7 | 32.0

  */

  if ( mEEmc ) 
    fprintf(mLogFile,"etow:  %02dT%c%02d\n",sec+1,sub+'A',eta+1);
  if ( mBEmc )
    fprintf(mLogFile,"btow:  %02dt%c%02d\n",sec+1,sub+'a',eta+1);    

  fprintf(mLogFile,"index: %4d\n",tow);
  fprintf(mLogFile,"gain:  %6.3f [ideal=%6.3f]\n",mTowerGain[rdo],mTowerGainIdeal[rdo]);
  fprintf(mLogFile,"ped:   %6.3f\n",mTowerPed[rdo]);
  fprintf(mLogFile,"thr:   %3d [%4.2f GeV]\n",mTowerAdcThreshold[rdo],mTowerAdcThreshold[rdo]/mIdealGainT);
  fprintf(mLogFile,"patch: %6.3f\n",mPatchAdcThreshold[rdo]);
  fprintf(mLogFile,"\n");

  //      14.3 | 14.5 |  8.8      22.6 | 22.9 | 21.6      80   | 80   | 80         0   |  0   |  0           1 |    0 |    0
  fprintf(mLogFile,"loaded peds             gain factors            status bits             mask bits                frequency > thr\n");

  for ( int i_eta=eta+1;i_eta>=eta-1;i_eta-- ) {

    int phi_l = (phi+mNumPhis-1)%mNumPhis;
    int phi_r = (phi+mNumPhis+1)%mNumPhis;
    int phi_c = phi;

    int phis[]={phi_l,phi,phi_r};
    float peds[]={0,0,0};
    float gains[]={0,0,0};
    ushort stats[]={0xff,0xff,0xff};
    ushort fails[]={0xff,0xff,0xff};
    int freqs[]={0,0,0};

    float corr[]={0.,0.,0.};

    // get numbers from db
    if ( i_eta < mNumEtas && i_eta >= 0 ) {

      int tow_l = tower(phi_l,i_eta);
      int tow_r = tower(phi_r,i_eta);
      int tow_c = tower(phi_c,i_eta);

      int rdo_l = mTower2rdo[tow_l];
      int rdo_r = mTower2rdo[tow_r];
      int rdo_c = mTower2rdo[tow_c];

      peds[0]=mTowerPed[rdo_l];peds[1]=mTowerPed[rdo_c];peds[2]=mTowerPed[rdo_r];
      //gains[0]=mTowerAdcCorrection[rdo_l];gains[1]=mTowerAdcCorrection[rdo_c];gains[2]=mTowerAdcCorrection[rdo_r];
      gains[0]=(float)mIdealGainT;
      gains[1]=(float)mIdealGainT;
      gains[2]=(float)mIdealGainT;
      //$$$      gains[0]=mTowerGainIdeal[rdo_l]/m
      stats[0]=mTowerStat[rdo_l];stats[1]=mTowerStat[rdo_c];stats[2]=mTowerStat[rdo_r];
      fails[0]=mTowerFail[rdo_l];fails[1]=mTowerFail[rdo_c];fails[2]=mTowerFail[rdo_r];
      freqs[0]=mTowerFrequency[tow_l];freqs[1]=mTowerFrequency[tow_c];freqs[2]=mTowerFrequency[tow_r];
      
      corr[0]=mTowerAdcCorrection[rdo_l];
      corr[1]=mTowerAdcCorrection[rdo_c];
      corr[2]=mTowerAdcCorrection[rdo_r];


    }

    fprintf(mLogFile, "%4.1f | %4.1f | %4.1f\t", peds[0],peds[1],peds[2] );
    fprintf(mLogFile, "%4.0f | %4.0f | %4.0f\t", gains[0],gains[1],gains[2] );
    fprintf(mLogFile, "%2x   | %2x   | %2x  \t", stats[0],stats[1],stats[2] );
    fprintf(mLogFile, "%2x   | %2x   | %2x  \t", fails[0],fails[1],fails[2] );
    fprintf(mLogFile, "%4.2f | %4.2f | %4.2f\t", 
	    corr[0]*peds[0]/gains[0],
	    corr[1]*peds[1]/gains[1],
	    corr[2]*peds[2]/gains[2]);

    fprintf(mLogFile,"\n");

    if ( i_eta > eta-1 ) {
      fprintf(mLogFile, "-----+------+-----\t");
      fprintf(mLogFile, "-----+------+-----\t");
      fprintf(mLogFile, "-----+------+-----\t");
      fprintf(mLogFile, "-----+------+-----\t");
      fprintf(mLogFile, "-----+------+-----\t");
      fprintf(mLogFile, "\n" );
    }
    else
      fprintf(mLogFile, "\n\n" );

  }

}

#endif
