#ifndef L2JETALGO2D_H
#define L2JETALGO2D_H
/***********************************************************
 * $Id: L2jetAlgo2012.h,v 1.3 2011/10/19 16:12:11 jml Exp $
 * \author Jan Balewski, IUCF, 2008 
 ***********************************************************
 * Descripion:
 * Reco of mono- & di-jets in L2 using BTOW+ETOW
 ***********************************************************
 * version updated 2/17/2008 W.W. Jacobs
 * 2008 to 2009 change by B.S. Page 11/12/2008
*/

#ifdef  IS_REAL_L2  //in l2-ana  environmen
  #include "L2VirtualAlgo2012.h"
#else
  #include "L2algoUtil/L2VirtualAlgo2012.h"
  #include "L2algoUtil/L2EmcGeom2012.h"
#endif

class L2Histo;
class L2Jet;


class L2jetAlgo2012 : public  L2VirtualAlgo2012 {
 public:
  enum {MaxBtowRdo=(BtowGeom::mxEtaBin) * (BtowGeom::mxPhiBin)}; // shortcut
  enum {MaxEtowRdo=(EtowGeom::mxEtaBin) * (EtowGeom::mxPhiBin)}; // shortcut
  enum {cl2jetMaxEtaBins=15, cl2jetMaxPhiBins=30};

  /* enum below defines L2 jet 0.6x0.6 size
     WARN: do NOT change it w/o understaning of
     ScanEta/phi-algos algo,JB */
  //enum {cl2jet_par_mxPhiBin=3, cl2jet_par_mxEtaBin=3}; //For .6x.6 JP
  enum {cl2jet_par_mxPhiBin=5, cl2jet_par_mxEtaBin=5}; //For 1x1 JP BP
 private:

  //..................... params set in initRun
  int   par_useBtowEast;
  int   par_useBtowWest;
  int   par_useEndcap;
  int   par_minPhiBinDiff;
  
  float par_diJetThrHigh;
  float par_diJetThrLow;
  float par_diJetThr_2; // added for xtra E_T cuts -- wwj 2/10
  float par_diJetThr_3; // added for xtra E_T cuts -- wwj 2/10
  float par_diJetThr_4; // added for xtra E_T cuts -- wwj 2/10
  float par_diJetThr_5; // added for xtra E_T cuts -- wwj 2/10
  float par_oneJetThr;
  float par_diJetEtaLow; // added for xtra Eta cuts -- wwj 2/10
  float par_diJetEtaHigh;  // added for xtra Eta cuts -- wwj 2/10
  int   par_dbg;
  int   *raw_ints; 
  float *raw_floats;

  float par_hotTwEtThr;

  //.............run-long variables
  enum { mxHA=256, mxJ=2}; // change to 256 (need ~ 134) -- wwj 2/10
  L2Histo *hA[mxHA]; // my private HBOOK@L2
  long   run_startUnix;
  int    run_number;
  char namechar; //single character name to differentiate low and high jet.  printed to 'dumm' in L2jetResults.

  // event counters
  int run_nEventOneJet, run_nEventDiJet, run_nEventRnd;

  /*  fast DB lookup tables */
  unsigned short db_btowL2PhiBin[MaxBtowRdo];
  unsigned short db_btowL2PatchBin[MaxBtowRdo];

  // similar tables for ETOW ....
  unsigned short db_etowL2PhiBin[MaxEtowRdo];
  unsigned short db_etowL2PatchBin[MaxEtowRdo];

  class L2Jet {
  public:
    /* Note, some  variables below are NOT in physical units,
       this is tricky, be careful
    */
    int   iphiBin;  //  phi-bin location of _left_edge_ 
    float fphiBin;  //  phi-bin location of _centroid_ 
    int   ietaBin;  //  eta-bin location of _left_edge_
    float fetaBin;  //  eta-bin location of _centroid_ 
    float   iene;     //  int4 transverse energy 
    float phiRad;   //  phi in radians, energy weighted
    float eneGeV;   //  energy in GeV
    float rmsEtaBin;   // put in to carry rms calc out of functions
    float rmsPhiBin; // rms of jet in units of phi bins (15 bins=3 eta units)  

    L2Jet(){}; // compact jet holder
    void clear(){
      iphiBin=ietaBin=0;
      fphiBin=fetaBin=phiRad=eneGeV=iene=0.;
    }
  };

  //............... event-long variables
  int  eve_ID; 
  float  eve_patchEne[cl2jetMaxEtaBins*cl2jetMaxPhiBins];//450 was int
  float  eve_phiEne[cl2jetMaxPhiBins+cl2jet_par_mxPhiBin-1];//was int
  L2Jet * eve_Jet[mxJ];

  // utility methods
  void createHisto();
  void clearEvent();
  int  projectAdc(const HitTower1 *hit, const int hitSize,
		  ushort *phiBin, ushort *patchBin,
		  L2Histo *hHot	 );
  float  scanPhi();
  void scanEta(int iJ);//int iphi0, int  *etaEneA, float *fetaBinMax, int *eneMax ,int *ietaBinLeft);
  float true2Dscan();
  void weightedEtaPhi(int iK);
  void weightedPhi(int iJ);
  void dumpPatchEneA();
  void finishRunHisto();
  bool paramsChanged( int *rc_ints, float *rc_floats);
  void computeE(int token);
  //bool decisionUser(int token, int *myL2Result);//BP
  //void computeUser(int token);//BP these may have to be public?

 public:
  L2jetAlgo2012(const char* name, const char *uid, L2EmcDb2012* db, char* outDir, int resOff, bool writeHighResult=false);
  // ~L2jetAlgo2008(){}; // memory leak NOT taken care of
  int   initRunUser( int runNo, int *rc_ints, float *rc_floats);
  bool decisionUser(int token, int *myL2Result);//BP
  void computeUser(int token);//BP these may have to be public?
  void  finishRunUser();// at the end of each run
};

#endif 

/**********************************************************************
  $Log: L2jetAlgo2012.h,v $
  Revision 1.3  2011/10/19 16:12:11  jml
  more 2012 stuff

  Revision 1.2  2011/10/19 15:39:44  jml
  2012

  Revision 1.1  2011/10/18 15:11:43  jml
  adding 2012 algorithms

  Revision 1.1  2010/04/17 05:04:13  pibero
  Updates for Run 9 jet tree production

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

