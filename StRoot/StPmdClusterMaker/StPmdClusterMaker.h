/*!
 * \class StPmdClusterMaker
 * \author
 */
/***********************************************************
 * $Id: StPmdClusterMaker.h,v 1.2 2003/05/12 12:12:18 subhasis Exp $
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
 * Revision 1.2  2003/05/12 12:12:18  subhasis
 * StEvent added
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


class StPmdCollection;
class StPmdDetector;
class StPmdClusterMaker: public StMaker{

   private:
  
   protected:
  
  // booking Pmd cluster histograms
  TH1F *mSmPmdCluster;      //!  supermodule no for Pmd
  TH1F *mEdepPmdCluster;    //!  cluster edep in Pmd
  TH1F *mSigmaPmdCluster;    //!  cluster Sigma in Pmd
  TH1F *mNcellPmdCluster;    //!  cluster edep in Pmd
  TH1F *mEtaPmdCluster;     //!  cluster eta in Pmd 
  TH1F *mPhiPmdCluster;     //!  cluster phi in Pmd 
  TH2F *mEtaPhiPmdCluster;  //!  eta vs. phi in Pmd
  TH2F *mPhi2ModPmd;        //!  phi vs.mod  in Pmd 
  TH2F *mHitVscluster;        //!  phi vs.mod  in Pmd 
  TH1F *mExtraclusterPmd;        //!  phi vs.mod  in Pmd 
  TH2F *mClusterEdepFracPmd;  //!  nclust vs. frac. edep in Pmd
  TH1F *mPmdCluster; //!number of Pmd clusters

  TH1F *mSmCpvCluster;      //!  supermodule no for Cpv
  TH1F *mEdepCpvCluster;    //!  cluster edep in Cpv
  TH1F *mNcellCpvCluster;    //!  cluster edep in Cpv
  TH1F *mEtaCpvCluster;     //!  cluster eta in Cpv 
  TH1F *mPhiCpvCluster;     //!  cluster phi in Cpv 
  TH2F *mEtaPhiCpvCluster;  //!  eta vs. phi in Cpv
  TH2F *mPhi2ModCpv;        //!  phi vs.mod  in Cpv
  TH1F *mExtraclusterCpv;        //!  phi vs.mod  in Cpv  
  TH2F *mClusterEdepFracCpv;  //!  nclust vs. frac. edep in Cpv
  TH1F *mCpvCluster; //!number of Cpv clusters
   public:
  //!constructor 
  StPmdClusterMaker(const char *name="PmdClust"); 
  ~StPmdClusterMaker();//!

  virtual Int_t Init();
  virtual Int_t Make();

  Int_t getChoice(Int_t num); 
  void  FillStEvent(StPmdDetector*, StPmdDetector*);

  void  bookHistograms(); //! booking histograms
  void  FillHistograms(StPmdDetector*, StPmdDetector*); //! filling histograms
  Float_t  AddEdep(StPmdDetector*, StPmdDetector*);
  void  Browse(TBrowser* b); 

  
  ClassDef(StPmdClusterMaker, 1) 
};

#endif

















