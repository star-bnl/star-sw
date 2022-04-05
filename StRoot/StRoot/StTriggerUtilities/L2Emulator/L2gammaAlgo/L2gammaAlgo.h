#ifndef __L2gammaAlgo_h__
#define __L2gammaAlgo_h__

#ifdef IS_REAL_L2  //in l2-ana  environment
  #include "L2VirtualAlgo.h"
  #include "L2Histo.h"
  #include "L2EmcDb.h"

#else
  #include "StTriggerUtilities/L2Emulator/L2algoUtil/L2VirtualAlgo.h"
  #include "StTriggerUtilities/L2Emulator/L2algoUtil/L2EmcDb.h"
  #include "StTriggerUtilities/L2Emulator/L2algoUtil/L2Histo.h"

#endif



/* CHANGE THIS WITH EVERY COMPILATION ON L2 MACHINE DURING RUN */
#define LEVEL2_GAMMA_ALGORITHM_VERSION 0x1 

//#define ushort unsigned short
typedef unsigned short ushort;

#define MAX_TOWERS 4800


#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <assert.h> /* tmp */
#include <time.h>
#include <string>



///////////////////////////////////////////////////////////////////////////////

/*!
 * \class L2gammaAlgo
 * \brief A level-2 "cluster" based trigger
 * \author Jason C. Webb, IUCF
 * \date 3/28/2006
 *
 * This class implements an L2 gamma, pi0 and electron trigger for both 
 * the barrel and endcap calorimeters.  The trigger is essentially a 
 * "cluster finder".  It locates a high tower above a specified threshold,
 * and forms a 3x3 cluster of towers centered on the high tower.  If the
 * cluster exceeds a second threshold, the trigger condition is satisfied.
 * An optional "prescaled accept" will accept some prescaled rate of input
 * events.
 *
 * $Id: L2gammaAlgo.h,v 1.4 2007/11/14 03:58:11 balewski Exp $
 *
 */


class L2gammaAlgo;

class L2gammaAlgo : public L2VirtualAlgo
{

 public:
  enum { kBEmcAlgo=0, kEEmcAlgo };
  enum { kThresh1=0, kThresh2 };

 private:
 protected:

  /// The following block of variables are initialized
  /// in the constructor and select between the two 
  /// calorimeter geometries
  ushort  mEEmc;
  ushort  mBEmc;
  ushort  mThresholdLevel;
  ushort  mNumEtas;
  ushort  mNumPhis;
  ushort  mNumSubs;
  ushort  mNumSecs;
  ushort  mNumTower;
  ushort  mNumClust;
  ushort  mNumRdo;
  const float *mEtaBins; /* points to preexisting array, no "new" allocation */
  ushort  mMaxADC;
  float   mMaxET;
  float   mIdealGainT;
  int     mHistogramBase;
  ushort  mHistogramPres;
  ushort  mIdThreshold; // 0=low threshold, 1=high threshold

 public:


  /*
   * The following are made publicly available for the possible
   * use of other algorithms.
   */

  ushort getNumberOfHighTowers(){ return nRdosHT; }
  ushort getRdoOfHighTower(ushort i){ return mListOfRdosHT[i]; }
  ushort rdo2tower(ushort rdo){ return mRdo2tower[rdo]; }
  ushort tower2rdo(ushort tow){ return mTower2rdo[tow]; }
  
  ushort mListOfRdosHT[ MAX_TOWERS ]; /* list of rdos where HT exceeded specified threshold */
  ushort mListOfRdosTP[ MAX_TOWERS ]; /* list of rdos where 3x3 "cluster" exceeded specified threshold */
  ushort nRdosHT;                /* number of high towers */
  ushort nRdosTP;                /* number of clusters (patches) */
  float  mETofTP[ MAX_TOWERS ];       /* ET of clusters (patches) */
  ushort mADCofHT[ MAX_TOWERS ];      /* raw ADC of high tower */

  /*
   * Algorithm parameters, may be set in constructor 
   * using I_par[5] and F_par[5]  
   */

  /// set the L2 high tower threshold.  F_par[0]
  void setTowerThreshold( float pt );
  /// set the L2 trigger patch threshold.  F_par[1]
  void setPatchThreshold( float pt );

  float getTowerThreshold(){ return mTowerThreshold; }
  float getPatchThreshold(){ return mPatchThreshold; }

  /// if set, instructs the algo to correct for offline gains
  void setUseOfflineGains();
  
  
  /// class constructor
  L2gammaAlgo(const char* name, L2EmcDb* db, char* outDir, int resOff);
  /// class destructor
  ~L2gammaAlgo(){ /* nada */ };

  /// One time initialization
  void init(int run, int I_par[5], float F_par[5]);

  /// initialize the data structures for the next run
  int  initRun( int run );  
  /// initialize the data structures for the next run
  int  initRun( int run, int I_par[5], float F_par[5] );

  int  initRun( char *myname, int run, int I_par[5], float F_par[5] );

  /// process one event through L2 and evaluate.  returns true to 
  /// accept an event, false to reject.  fatal algorithm
  /// errors will always return true to tick off the 
  /// shift crew and get them to take action (we hope).
  /// \param inpEveId event id 
  /// \param trgData  the trigger data block 
  /// \param emcIn    specifies whether the calorimeter is in the run or not
  /// \param emcData  is the L2 data block (raw ADC values) to be processed
  bool  doEvent(int inpEveId, TrgDataType* trgData, int emcIn, unsigned short *emcData ); 


  /// implementation of the "virtual" method in L2VirtualAlgo.  this is a wrapper for
  /// doEvent( int, TrgDataType*, int, ushort ) above.
  bool  doEvent( int L0trigger, int inuptEventID, TrgDataType* trgData,
		 int bemcIn, unsigned short *bemcData,
		 int eemcIn, unsigned short *eemcData );



  /*****
  //
  // There aren't supposed to be any "calorimeter specific" methods in the header file. 
  // These got slipped in while I wasn't looking.  I suspect l2main already uses them.
  // These disapeear at the end of the run. 
  //
  bool  doEventBtow( int inpEveId, TrgDataType* trgData, int bemcIn, unsigned short *bemcData ) 
        { return doEvent(inpEveId,trgData, bemcIn,bemcData);};// it allows to add soe security - if you want, JB
  bool  doEventEtow(int inpEveId, TrgDataType* trgData, int eemcIn, unsigned short *eemcData ) 
        { return doEvent(inpEveId,trgData, eemcIn,eemcData);};// it allows to add soe security - if you want, JB

  *****/


  /// clear data structures for ...
  void clear();

  /// cleanup for runs.  output of warnings (eg hot channels) etc...
  void finishRun();

  /// final output of statistics, histograms, etc...
  void finish();


  /// Set logfile for summary output (overrides constructor option)
  void setLogFile( const char *fname = "./bsqueal.log" );//{ mLogFile=fopen(fname,"w"); }

  /// Set filename for histogram output (overrides constructor option)
  void setHistFile( const char *fname = "./bsqueal.dat" );//{ mHistFile=fopen(fname,"w"); }

#ifdef OPT_PREPROCESS
  /// If OPT_PREPROCESS is selected at compile time, sets a pointer to
  /// an instance of the algorithm running with a lower threshold which
  /// will provide a smaller list of "seeds" which this algo may restrict
  /// its search to.
  void setL2input( L2gammaAlgo *in ){ mL2input=in; }
#endif

 private:
 protected:  


  /// pointer to algorithm evaluating a lower threshold
  /// which we simply want to evaluate a higher threshold
  L2gammaAlgo *mL2input;

  /// Run number for the current run
  int mRunNumber;

  /// High tower threshold
  float mTowerThreshold;
  /// Patch threshold
  float mPatchThreshold;
  /// Option to use offline gains 
  int mUseOfflineGains;
  /// Option to correct for vertex effects (not in use yet)
  int mUseBbc;

  int mDefaultI_par[5];
  float mDefaultF_par[5];

  /// event counters
  int mNumberInput;
  int mNumberAcceptHT;
  int mNumberAcceptTP;
  int mPrescale;

  int mTowerFrequency[MAX_TOWERS]; 
  int mPatchFrequency[MAX_TOWERS];

  /// High tower threshold expressed in terms of ADC
  /// for each of the 720 rdo channels
  ushort mTowerAdcThreshold[MAX_TOWERS];

  /// Thresholds on the 3x3 patch of towers centered
  /// on the rdo channel.  The threshold is calculated
  /// as T= E_T + sum_i ( ped_i/g_i ) and will be applied
  /// to the sum ADC_i/g_i > T
  float  mPatchAdcThreshold[MAX_TOWERS];

  /*
   ******************************************************************
   Thresholds on the high tower and associated 3x3 tower cluster
   (generically refered to as a "patch" throughout this code) will be
   initialzed at the start of the run and stored in the following
   lookup tables.  The LUTs have sufficient space to store the 4800
   values needed by the barrel.  The endcap's 720 towers will fit
   just fine within that space.
   ******************************************************************
   */


  /// bitwise "or" of _all_ stat and fail bits of towers
  /// w/in the 3x3 tower patch
  ushort mPatchStat[MAX_TOWERS];
  ushort mPatchFail[MAX_TOWERS];

  ushort mTowerStat[MAX_TOWERS];
  ushort mTowerFail[MAX_TOWERS];

  /// Map to go from Rdo to Tower and Tower to Rdo
  ushort mRdo2tower[ MAX_TOWERS ];
  ushort mTower2rdo[ MAX_TOWERS ];

  /// Number and list of up to 9 neighboring towers
  ushort mNumPatch[ MAX_TOWERS ];
  ushort mRdoPatch[ MAX_TOWERS ][ 9 ];

  float mTowerPed[ MAX_TOWERS ];
  float mPatchPed[ MAX_TOWERS ];
  
  float mTowerGain[ MAX_TOWERS ];
  float mTowerGainIdeal[ MAX_TOWERS ];
  float mTowerAdcCorrection[ MAX_TOWERS ];



  /// prints patch configuration (peds, stat, fail, gains, etc...)
  /// for easier(?) debugging of patch problems online
  void printPatchConfig( int rdo );


  /// Counters for error monitoring

  int mNumberLive;                   /* number of unmasked towers in db */
  int mEvalTime;                     /* total time spent in eval in kTicks */
  
  /// L2 histograms
  L2Histo mHistos[19];
  void jbook(); 
  void jclear();

  /// Log file
  // FILE *mLogFile;
  // FILE *mHistFile;

  ushort phibin( ushort sec, ushort sub ) { return mNumSubs * sec + sub; }
  ushort tower( ushort phi, ushort eta ) { return mNumEtas * phi + eta; }



};

inline void L2gammaAlgo::setTowerThreshold(float pt)
{
  mTowerThreshold=pt;
}
inline void L2gammaAlgo::setPatchThreshold(float pt)
{
  mPatchThreshold=pt;
}
inline void L2gammaAlgo::setUseOfflineGains()
{
  mUseOfflineGains=true;
}

// $Log: L2gammaAlgo.h,v $
// Revision 1.4  2007/11/14 03:58:11  balewski
// cleanup of common timing measurement
//
// Revision 1.3  2007/11/08 04:02:29  balewski
// run on l2ana as well
//
// Revision 1.2  2007/11/02 03:03:44  balewski
// modified L2VirtualAlgo
//
// Revision 1.1  2007/10/25 15:30:50  balewski
// added L2gamma, full
//
// Revision 1.1  2006/03/29 13:08:15  balewski
// ver 16c
//



#endif
