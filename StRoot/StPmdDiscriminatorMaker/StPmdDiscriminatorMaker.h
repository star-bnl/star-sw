/*!
 * \class StPmdDiscriminatorMaker
 * \author
 */
/******************************************************************
 *
 * $Id: StPmdDiscriminatorMaker.h,v 1.4 2004/10/30 00:08:06 subhasis Exp $
 *
 * Author: Subhasis Chattopadhyay
 *
 ******************************************************************
 *
 * Description: This routine describes methods for photon/hadron
 * discrimination. 
 ******************************************************************
 *
 * $Log: StPmdDiscriminatorMaker.h,v $
 * Revision 1.4  2004/10/30 00:08:06  subhasis
 * TranFlag added and set to 0 in ctor
 *
 * Revision 1.3  2003/09/10 19:47:27  perev
 * ansi corrs
 *
 * Revision 1.2  2003/05/29 13:12:51  subhasis
 * several changes to include NN
 *
 ******************************************************************/

#ifndef STAR_StPmdDiscriminatorMaker
#define STAR_StPmdDiscriminatorMaker
#ifndef StMaker_H
#include "StMaker.h"
#endif

//For CC5 compatibility
#include <vector>
#if !defined(ST_NO_NAMESPACES)
using std::vector;
#endif

#ifdef ST_NO_TEMPLATE_DEF_ARGS
// Syntax currently required by Solaris compiler
#define StVector(T) vector<T, allocator<T> >
#else
#define StVector(T) vector<T>
#endif
// *************************
#include <TH2.h>
#include <TH1.h>
#include <TCanvas.h>

class StPmdCollection;
class StPhmdCollection;
class StPmdDetector;
class StPmdCluster;
class StPhmdClusterCollection;
class StPhmdCluster;
class StNNCluster;

//typedef StVector(StPhmdCluster*) StPmdCl;
typedef StVector(StNNCluster*) StPmdCl;

class StPmdDiscriminatorMaker: public StMaker{

  private:
  Int_t m_photonlike;
	  Float_t mEdepThreshold;
	  Float_t mDeltaEta;
	  Float_t mDeltaPhi;
	  Int_t mApplyFlagNN;
	  Int_t mTrainFlag;
  protected:
  TH1F *mEdepPmd;    //! 1-D EDep Display for PMD
  TH1F *mEtaPmd;    //!  1-D Eta PMD
  TH1F *mPhiPmd;    //!  1-D Phi PMD
  TH1F *mEtadiff;    //!  1-D delta eta distribution 
  TH1F *mPhidiff;    //!  1-D delts phi distribution
  TH1F *mMCPID;    //!  1-D delts phi distribution
  TH2F *mEtaPhi;    //!  DeltaEta-DeltaPhi Distribution 
  TH1F *mClusterPID;    //! 1-D plot for PID through Matching  
  TH1F *mClusterEdepPID;    //! 1-D plot for PID through E-cut
  TH2F *mEdepVsPID; //! 2-D CpvClusters vs Matched Clusters
  TH1F *mcpvmatched; //! No of cpv clusters  Matched Clusters
  
   public:
  StPmdDiscriminatorMaker(const char *name="StPmdDiscriminator"); 
  ~StPmdDiscriminatorMaker();

  virtual Int_t Init();  
  virtual Int_t Make();
  virtual Int_t Finish();
  void getEdepThreshold(Float_t); //! Getting Energy value (3 MIP)
  void SetDeltaEta(Float_t);      //! Setting DeltaEta for Matching
  void SetDeltaPhi(Float_t);      //! Setting DeltaPhi for Matching
  void SetApplyFlag(Int_t);      //! Setting Traininbg or testing flag
  void SetTrainFlag(Int_t);      //! Setting Traininbg or testing flag
  void bookHistograms();          
  void fillHistograms(StPmdDetector*, StPmdDetector*);
  void Matching(StPmdDetector*, StPmdDetector*);
  Int_t PrepareInputforNN(StPmdDetector*,StPmdDetector*,StPhmdClusterCollection*,StPhmdClusterCollection*);
  //void Matching();
  void fillStEvent(StPmdCollection*,StPhmdCollection*);
  void  Browse(TBrowser* b);
//NN hist outs (made public to be accessed from StPmdNeunet
  TH1F *mNNoutput; //! No of cpv clusters  Matched Clusters

  ClassDef(StPmdDiscriminatorMaker,0) 
};

inline void StPmdDiscriminatorMaker::getEdepThreshold(Float_t de){mEdepThreshold=de;}
inline void StPmdDiscriminatorMaker::SetDeltaEta(Float_t deta){mDeltaEta=deta;}
inline void StPmdDiscriminatorMaker::SetDeltaPhi(Float_t dphi){mDeltaPhi=dphi;}
inline void StPmdDiscriminatorMaker::SetApplyFlag(Int_t flag){mApplyFlagNN=flag;}
inline void StPmdDiscriminatorMaker::SetTrainFlag(Int_t flag){mTrainFlag=flag;}
#endif























