#ifndef L2adc2energy_H
#define L2adc2energy_H
/*********************************************************************
 * $Id: L2adc2energyAlgo.h,v 1.1 2007/11/19 22:18:25 balewski Exp $
 * \author Jan Balewski, IUCF, 2006 
 *********************************************************************
 * Descripion:
 * Reco of mono- & di-jets in L2 using BTOW+ETOW
 *********************************************************************
 */


class L2Histo;
#ifdef  IS_REAL_L2  //in l2-ana  environmen
  #include "L2VirtualAlgo.h"
#else
  #include "StTriggerUtilities/L2Emulator/L2algoUtil/L2VirtualAlgo.h"
#endif

class L2adc2energyAlgo : public  L2VirtualAlgo {
 public:
  /* usefull dimensions */
#define MaxBtowRdo (L2EmcDb::BTOW_MAXFEE*L2EmcDb::BTOW_DATSIZE)
#define MaxEtowRdo (L2EmcDb::ETOW_MAXFEE*L2EmcDb::ETOW_DATSIZE)

  enum {cl2jetMaxEtaBins=15, cl2jetMaxPhiBins=30};

  /* enum below defines L2 jet 0.6x0.6 size
     WARN: do NOT change it w/o understaning of
     ScanEta/phi-algos algo,JB */
  enum {cl2jet_par_mxPhiBin=3, cl2jet_par_mxEtaBin=3};
 private:

  //................. params set in constructor
  float  par_maxADC;
  float  par_maxEt;
  unsigned short par_adcMask,par_pedOff; 
  int    par_hotTwEtThr; // only monitoring

  //..................... params set in initRun
  int   par_useBtowEast;
  int   par_useBtowWest;
  int   par_useEndcap;
  int   par_minPhiBinDiff;
  int   par_cutTag;
  float par_energyScale;
  float par_diJetThrHigh;
  float par_diJetThrLow;
  float par_oneJetThr;
  float par_rndAccProb;
  int   par_rndAccThr;
  int   par_dbg;
  int   *raw_ints; 
  float *raw_floats;

  //.............run-long variables
  enum { mxHA=128, mxJ=2};
  L2Histo *hA[mxHA]; // my private HBOOK@L2
  long   run_startUnix;
  int    run_number;

  // event counters
  int run_nEventOneJet, run_nEventDiJet, run_nEventRnd;

  /*  fast DB lookup tables */
  unsigned short db_btowThr[MaxBtowRdo];
  unsigned short db_btowPedS[MaxBtowRdo]; // ped offset, similar to DSM
  unsigned short db_btowGainCorr[MaxBtowRdo];
  unsigned short db_btowL2PhiBin[MaxBtowRdo];
  unsigned short db_btowL2PatchBin[MaxBtowRdo];

  // similar tables for ETOW ....
  unsigned short db_etowThr[MaxEtowRdo];
  unsigned short db_etowPedS[MaxEtowRdo];// ped offset, similar to DSM
  unsigned short db_etowGainCorr[MaxEtowRdo];
  unsigned short db_etowL2PhiBin[MaxEtowRdo];
  unsigned short db_etowL2PatchBin[MaxEtowRdo];


  //............... event-long variables
  int  eve_ID; 
  int  eve_patchEne[cl2jetMaxEtaBins*cl2jetMaxPhiBins];
  int  eve_phiEne[cl2jetMaxPhiBins+cl2jet_par_mxPhiBin-1];

  // utility methods
  void createHisto();
  void clearEvent();
  int  projectAdc( ushort *rawAdc, int nRdo,
		  ushort *thr, ushort *ped, ushort *gainCorr,
		  ushort *phiBin, ushort *patchBin,
		  L2Histo *hHot	 );

 
  void dumpPatchEneA();
  void finishRunHisto();
  bool paramsChanged( int *rc_ints, float *rc_floats);

 public:
  L2adc2energyAlgo(const char* name, L2EmcDb* db, char* outDir, int resOff);
  // ~L2adc2energyAlgo(){}; // memory leak NOT taken care of
  int   initRun( int runNo, int *rc_ints, float *rc_floats);
  bool  doEvent(int L0trg, int inpEveId, TrgDataType* trgData,  // for every event
	      int bemcIn, ushort *bemcData,
	      int eemcIn, ushort *eemcData);
  void  finishRun();// at the end of each run
};

#endif 

/**********************************************************************
  $Log: L2adc2energyAlgo.h,v $
  Revision 1.1  2007/11/19 22:18:25  balewski
  most L2algos provide triggerID's


*/

