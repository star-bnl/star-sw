#ifndef L2JETALGO2006_H
#define L2JETALGO2006_H
/*********************************************************************
 * $Id: L2jetAlgo2006.h,v 1.1 2007/11/19 22:18:28 balewski Exp $
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

class L2jetAlgo2006 : public  L2VirtualAlgo {
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

  class L2Jet {
  public:
    /* Note, some  variables below are NOT in physical units,
       this is tricky, be carefull
    */
    int   iphiBin;  //  phi-bin location of _left_edge_ 
    float fphiBin;  //  phi-bin location of _centroid_ 
    int   ietaBin;  //  eta-bin location of _left_edge_
    float fetaBin;  //  eta-bin location of _centroid_ 
    int   iene;     //  int4 transverse energy 
    float phiRad;   //  phi in radians, energy weighted
    float eneGeV;   //  energy in GeV

    L2Jet(){}; // compact jet holder
    void clear(){
      iphiBin=ietaBin=iene=0;
      fphiBin=fetaBin=phiRad=eneGeV=0.;
    }
  };

  //............... event-long variables
  TrgDataType*  eve_TrigData;
  int  eve_ID; 
  int  eve_patchEne[cl2jetMaxEtaBins*cl2jetMaxPhiBins];
  int  eve_phiEne[cl2jetMaxPhiBins+cl2jet_par_mxPhiBin-1];
  L2Jet * eve_Jet[mxJ];

  // utility methods
  void createHisto();
  void clearEvent();
  int  projectAdc( ushort *rawAdc, int nRdo,
		  ushort *thr, ushort *ped, ushort *gainCorr,
		  ushort *phiBin, ushort *patchBin,
		  L2Histo *hHot	 );
  int  scanPhi();
  void scanEta(int iJ);//int iphi0, int  *etaEneA, float *fetaBinMax, int *eneMax ,int *ietaBinLeft);
  void weightedPhi(int iJ);
  void dumpPatchEneA();
  void finishRunHisto();
  bool paramsChanged( int *rc_ints, float *rc_floats);

 public:
  L2jetAlgo2006(const char* name, L2EmcDb* db, char* outDir, int resOff);
  // ~L2jetAlgo2006(){}; // memory leak NOT taken care of
  int   initRun( int runNo, int *rc_ints, float *rc_floats);
  bool  doEvent(int L0trg, int inpEveId, TrgDataType* trgData,  // for every event
	      int bemcIn, ushort *bemcData,
	      int eemcIn, ushort *eemcData);
  void  finishRun();// at the end of each run
};

#endif 

/**********************************************************************
  $Log: L2jetAlgo2006.h,v $
  Revision 1.1  2007/11/19 22:18:28  balewski
  most L2algos provide triggerID's

  Revision 1.6  2007/11/14 03:58:14  balewski
  cleanup of common timing measurement

  Revision 1.5  2007/11/13 23:06:07  balewski
  toward more unified L2-algos

  Revision 1.4  2007/11/08 04:02:31  balewski
  run on l2ana as well

  Revision 1.3  2007/11/02 03:03:47  balewski
  modified L2VirtualAlgo

  Revision 1.2  2007/10/25 02:07:03  balewski
  added L2upsilon & binary event dump

  Revision 1.1  2007/10/11 00:33:20  balewski
  L2algo added

  Revision 1.7  2006/03/28 19:46:49  balewski
  ver16b, in l2new

  Revision 1.6  2006/03/11 17:08:33  balewski
  now CVS comments should work

*/

