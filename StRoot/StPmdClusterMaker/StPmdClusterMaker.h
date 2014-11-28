/*!
 * \class StPmdClusterMaker
 * \author
 */
/***********************************************************
 * $Id: StPmdClusterMaker.h,v 1.13 2014/08/06 11:43:33 jeromel Exp $
 *
 * Author:
 *
 ************************************************************
 *
 * Description: Base class for PMD cluster Maker
 *
 ************************************************************
 *
 * $Log: StPmdClusterMaker.h,v $
 * Revision 1.13  2014/08/06 11:43:33  jeromel
 * Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
 *
 * Revision 1.12  2010/04/15 06:52:13  rashmi
 * Clustering with option to turn calibration refineclustering on/off
 *
 * Revision 1.11  2007/08/31 10:54:39  rashmi
 * Included ReadCalibration to read PMD_MIP value from DB; Included inline SetAdcCutOff()
 *
 * Revision 1.10  2004/09/22 19:24:55  perev
 * Leak fixed + mess with i,j indexes
 *
 * Revision 1.8  2004/09/03 14:31:44  subhasis
 * OptHist introduced
 *
 * Revision 1.7  2004/06/24 13:48:33  subhasis
 * several changes in clustering code
 *
 * Revision 1.6  2004/04/09 23:01:48  jeromel
 * GetCVS() missing
 *
 * Revision 1.5  2004/03/23 05:18:51  subhasis
 * refclust changed to have correct sigma/ncell
 *
 * Revision 1.4  2003/09/10 19:47:26  perev
 * ansi corrs
 *
 * Revision 1.3  2003/05/14 10:49:12  subhasis
 * CPV clustering added
 *
 * Initial version:
 ************************************************************/
#ifndef STAR_StPmdClusterMaker
#define STAR_StPmdClusterMaker
#ifndef StMaker_H
#include "StMaker.h"
#endif
#include <TH2.h>
#include <TH1.h>
#include <TCanvas.h>
#include <TNtuple.h>
#include <TFile.h>
#include "tables/St_pmdSMChain_GNF_Table.h"

class StPmdCollection;
class StPmdDetector;
class StPmdClusterMaker: public StMaker{
  
 private:
  
  Bool_t mOptHist; 
  Bool_t mOptCalibrate;
  Bool_t mOptSimulate;
  Bool_t mOptRefineCluster;
  Double_t adccutoff;
  Float_t SM_chain_factor[24][48];
  Float_t PMD_MIP;
  TDataSet * mDb;  
 protected:
  
  // booking Pmd cluster histograms
  TH1F *mNclust;      //!  supermodule no for Pmd
  TH1F *mNclust1;      //!  supermodule no for Pmd
  TH1F *mNclust2;      //!  supermodule no for Pmd
  TH1F *mNclust3;      //!  supermodule no for Pmd
  
  TH1F *mSmPmdCluster;      //!  supermodule no for Pmd
  TH1F *mEdepPmdCluster;    //!  cluster edep in Pmd
  TH1F *mSigmaLPmdCluster;    //!  cluster SigmaL in Pmd
  TH1F *mSigmaSPmdCluster;    //!  cluster SigmaS in Pmd
  TH1F *mNcellPmdCluster;    //!  cluster edep in Pmd
  TH1F *mEtaPmdCluster;     //!  cluster eta in Pmd 
  TH1F *mPhiPmdCluster;     //!  cluster phi in Pmd 
  TH2F *mEtaPhiPmdCluster;  //!  eta vs. phi in Pmd
  TH2F *mPhi2ModPmd;        //!  phi vs.mod  in Pmd 
  TH2F *mHitVscluster;        //!  phi vs.mod  in Pmd 
  TH1F *mExtraclusterPmd;        //!  phi vs.mod  in Pmd 
  TH2F *mClusterEdepFracPmd;  //!  nclust vs. frac. edep in Pmd
  TH1F *mPmdCluster; //!number of Pmd clusters

  TH2F *mXYPmdCluster;
  
  TH1F *mSmCpvCluster;      //!  supermodule no for Cpv
  TH1F *mEdepCpvCluster;    //!  cluster edep in Cpv
  TH1F *mSigmaLCpvCluster;    //!  cluster SigmaL in Cpv
  TH1F *mSigmaSCpvCluster;    //!  cluster SigmaS in Cpv
  TH1F *mNcellCpvCluster;    //!  cluster edep in Cpv
  TH1F *mEtaCpvCluster;     //!  cluster eta in Cpv 
  TH1F *mPhiCpvCluster;     //!  cluster phi in Cpv 
  TH2F *mEtaPhiCpvCluster;  //!  eta vs. phi in Cpv
  TH2F *mPhi2ModCpv;        //!  phi vs.mod  in Cpv
  TH1F *mExtraclusterCpv;        //!  phi vs.mod  in Cpv  
  TH2F *mClusterEdepFracCpv;  //!  nclust vs. frac. edep in CpvPicoEventWrite(Bool_t flag=kFALSE);
  TH1F *mCpvCluster; //!number of Cpv clusters
  
  TH2F *mXYCpvCluster;
   public:
  //!constructor 
  StPmdClusterMaker(const char *name="pmdClust"); 
  ~StPmdClusterMaker();//!

  virtual Int_t Init();
  virtual Int_t  InitRun(Int_t runnr);          // Init for every run to read D\B

  virtual Int_t Make();
  virtual Int_t  Finish();

  //Int_t getChoice(Int_t num); 
  void  FillStEvent(StPmdDetector*, StPmdDetector*);

  void  bookHistograms(); //! booking histograms
  void  FillHistograms(StPmdDetector*, StPmdDetector*); //! filling histograms
  void  setPrint(Bool_t a) { mOptHist = a;}
  void  Browse(TBrowser* b); 
  void SetAdcCutOff(Double_t adccutoff);

  Bool_t ReadCalibrationsConst();
  void SetOptCalibrate(Bool_t a=kTRUE){mOptCalibrate = a;}  // Default is on; YES Calibrate
  void SetOptSimulate(Bool_t a=kFALSE){mOptSimulate = a;}    // Default is off; No Simulation
  void SetOptRefineCluster(Bool_t a=kTRUE){mOptRefineCluster = a;}    // Default is on; Yes Refine Clustering

  virtual const char *GetCVS() const {  ///< Returns version tag.
    static const char cvs[]="Tag $Name:  $ $Id: StPmdClusterMaker.h,v 1.13 2014/08/06 11:43:33 jeromel Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }
  
  ClassDef(StPmdClusterMaker,0) 
    };
    
    inline void StPmdClusterMaker::SetAdcCutOff(Double_t cutoff){
      adccutoff = cutoff;
    }


#endif

















