/*!
 * \class StPmdDiscriminatorMaker
 * \author
 */
/******************************************************************
 *
 * $Id: StPmdDiscriminatorMaker.h,v 1.1 2002/08/27 12:11:02 subhasis Exp $
 *
 * Author: Subhasis Chattopadhyay
 *         Gopika Sood
 ******************************************************************
 *
 * Description: This routine describes methods for photon/hadron
 * discrimination. 
 ******************************************************************
 *
 * $Log: StPmdDiscriminatorMaker.h,v $
 * Revision 1.1  2002/08/27 12:11:02  subhasis
 * First version
 *
 ******************************************************************/

#ifndef STAR_StPmdDiscriminatorMaker
#define STAR_StPmdDiscriminatorMaker
#ifndef StMaker_H
#include "StMaker.h"
#endif
#include <TH2.h>
#include <TH1.h>
#include <TCanvas.h>
class StPmdDetector;
class StPmdCluster;


class StPmdDiscriminatorMaker: public StMaker{

  private:
  Int_t m_photonlike;
	  Float_t mEdepThreshold;
	  Float_t mDeltaEta;
	  Float_t mDeltaPhi;
  protected:
  TH1F *mSmPmdCluster;    //! 1-D  
  TH1F *mEdepPmd;    //! 1-D EDep Display for PMD
  TH1F *mDeltaE;    //!  1-D delta eta distribution 
  TH1F *mDeltaP;    //!  1-D delts phi distribution
  TH2F *mEtaPhi;    //!  DeltaEta-DeltaPhi Distribution 
  TH2F *mEtaPhim;    //! Eta-Phi Disrtibution of Matched ones
  TH2F *mPmdClusterm; //! 2-D PmdClusters vs Matched Clusters
  TH2F *mCpvClusterm; //! 2-D CpvClusters vs Matched Clusters
  TH1F *mClusterPID;    //! 1-D plot for PID through Matching  
  TH1F *mClusterEdepPID;    //! 1-D plot for PID through E-cut
  TH1F *mAboveEdep;    //!  1-D plot for No. of clusters above e-cut
  
   public:
  StPmdDiscriminatorMaker(const char *name="StPmdDiscriminator"); 
  ~StPmdDiscriminatorMaker();

  virtual Int_t Init();  
  virtual Int_t Make();
  void getEdepThreshold(Float_t); //! Getting Energy value (3 MIP)
  void SetDeltaEta(Float_t);      //! Setting DeltaEta for Matching
  void SetDeltaPhi(Float_t);      //! Setting DeltaPhi for Matching
  void bookHistograms();          
  void fillHistograms(StPmdDetector*, StPmdDetector*);
  void Matching(StPmdDetector*, StPmdDetector*);
  void  Browse(TBrowser* b);

  ClassDef(StPmdDiscriminatorMaker, 1) 
};

#endif
inline void StPmdDiscriminatorMaker::getEdepThreshold(Float_t de){mEdepThreshold=de;}
inline void StPmdDiscriminatorMaker::SetDeltaEta(Float_t deta){mDeltaEta=deta;}
inline void StPmdDiscriminatorMaker::SetDeltaPhi(Float_t dphi){mDeltaPhi=dphi;}























