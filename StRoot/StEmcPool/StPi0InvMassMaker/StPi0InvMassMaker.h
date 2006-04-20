/*
StPi0InvMassMaker.h

Author: Andre Mischke		a.mischke@phys.uu.nl   
*/

#ifndef STAR_StPi0InvMassMaker
#define STAR_StPi0InvMassMaker

#ifndef StMaker_H
#include "StMaker.h"
#include "TH1.h"
#include "TObjArray.h"
#include "TEventMixer.h"
#include "TMixer.h"

#endif

class TNtuple;
class TH2F;
#include "StThreeVectorF.hh"
class StChain;
class StPrimaryTrack;
class StPrimaryVertex;
class StEmcCollection;
class StEmcGeom;
class StEmcPoint;

#define badrunidmax 200
#define ntower 2400

class StPi0InvMassMaker : public StMaker 
{
 private:
 protected:
  Bool_t       mDoTracks; //!
  StChain*     mChain; //!      
  const char*  mFileName; //!

  float mPi; //!
  float mPi0Mass; //!
  
  bool debug; //!

  bool mb; //!
  bool ht1; //!
  bool ht2; //!

  Int_t badrunid[badrunidmax]; //!
  Int_t tower[ntower]; //!
  Float_t gain[ntower]; //!
  
  Int_t ievtot; //!
  Int_t ievaccep; //!
  Int_t inochainpt; //!
  Int_t inoevpt; //!
  Int_t ibadrun; //!
  Int_t iemc; //!
  Int_t inoprimvert; //!
  Int_t itrig; //!
  Int_t ievmix; //!
  Int_t ii; //!

  TObjArray   *photonlist;
  TObjArray   *pipluslist;
  
  TEventMixer  *fEventMixer;      //! bin in eventmixing
  TMixer       *fTMixer;          //! mixing class
  
  //Temp
  Int_t startphoton1;   //!
  Int_t startphoton2;   //!
  Int_t fnummixed;  //! number of mixed events 
  
  TObjArray  *mixedphoton1list; //! 
  TObjArray  *mixedphoton2list; //! 
 
  
  TH1F*        mEventSummary; //!
  TH1F*        mTriggerSummary; //!
  TH1F*        mTriggerSummaryCut1; //!
  TH1F*        mTriggerSummaryCut2; //!
  TH1F*        mTriggerSummaryZeroBemc; //!
  TH1F*        mSMDSummary; //! 

  TH1F*        mZvertex; //!
  TH1F*        mZvertexCut2; //!
  TH1F*        mPrimaryTracks; //!
  TH1F*        mPrimaryTracksCut1; //!
  TH2F*        mPrimaryTracks_BemcHits; //! 
  
  TH1F*        mBemcPoints; //!
  TH1F*        mBemcPointsCut1; //!
  TH2F*        mBemcPointsCut12; //!
  TH1F*        mBemcPointsCut2; //!
  TH1F*        mBemcPointsAfterCuts2Photons; //!  
  
  TH1F*        mPointCluster; //!
  //TNtuple*     mPointsNtuple; //!

  TNtuple*     mPiZeroNtuple; //!
  TNtuple*     mPiZeroMixNtuple; //!

  TH1F*        mInvMass[ntower]; //!
  TH2F*        mTowerIdhit2; //!
  TH1F*        mTowerIdhit; //!
  TH1F*        mTowerIdTw; //!

  TH1F*        mPhotonSpectra_MB; //!
  TH1F*        mPhotonSpectra_HT1; //!
  TH1F*        mPhotonSpectra_HT2; //!
  TH1F*        mPhotonSpectraSel_MB; //!
  TH1F*        mPhotonSpectraSel_HT1; //!
  TH1F*        mPhotonSpectraSel_HT2; //!
  TH1F*        mHiTowerEtSpectra_MB; //!  
  TH1F*        mHiTowerEtSpectra_HT1; //!
  TH1F*        mHiTowerEtSpectra_HT2; //!
  
  TH1F*        mTrackPtHistMinBias; //!
  TH1F*        mTrackPtHistHiTower; //!
  TH1F*        mPointPtHistMinBias; //!
  TH1F*        mPointPtHistHiTower; //!
      
  Float_t      getHiTowerEt(StEmcCollection*);
  Int_t        doTrackPtHist(Float_t energy, Float_t threshold, TObjArray *photonlist);
  
  Int_t        associateTracksWithEmcPoints(StMaker* anyMaker);

  void         readBadRunList();
  void         getTowerHitInfo();
  StThreeVectorF getPoint(StEmcPoint *p, Int_t&, Float_t&, Float_t&, Int_t&, Int_t&);
  void         printPointInfo(StEmcPoint *p);
  void         getPhotonSpectra(TObjArray *photonlist, Float_t, Int_t, Float_t, Int_t);

  Bool_t       getMass(StThreeVectorF, StThreeVectorF, Float_t, Float_t, Double_t&, StThreeVectorF&, Float_t&, Float_t&, Float_t&, Float_t&);
 
  void         getInvMass(Int_t, TObjArray *photonlist1, TObjArray *photonlist2, Float_t, Float_t);
  void         getInvMassTowerwise(Int_t, Int_t, Float_t, Float_t, Double_t);

 public: 

  StPi0InvMassMaker(const char *name="pi0AnaMaker", Bool_t mDoTracks=kTRUE, const char *mFileName="bla.root");
  virtual       ~StPi0InvMassMaker();  

  virtual Int_t Init();
  virtual Int_t Make();
  virtual Int_t Finish();
  void    saveHistograms();
  Bool_t  readPointList();
  
  ClassDef(StPi0InvMassMaker, 1)   //StAF chain virtual base class for Makers
};
#endif

