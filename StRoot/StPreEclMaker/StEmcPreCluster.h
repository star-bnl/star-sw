//
// $Id: StEmcPreCluster.h,v 1.1 2000/05/15 21:24:00 subhasis Exp $
//
// $Log: StEmcPreCluster.h,v $
// Revision 1.1  2000/05/15 21:24:00  subhasis
// initial version
//
// PreClusters Finder Maker for EMC
//
//
// Authors: Subhasis Chattopadhyay,
//          Aleksei Pavlinov , July 1999.
//          initial version from Akio Ogawa    
//    

#ifndef STAR_StEmcPreCluster
#define STAR_StEmcPreCluster

#include <math.h>
#include <iostream.h>
#include "TArrayI.h"
#include "TObject.h"
#include "St_emc_Maker/StEmcHitCollection.h"

class StEmcPreCluster : public TObject {
  friend class StBemcPreCluster;
  friend class StBsmdePreCluster;
  friend class StBsmdpPreCluster;
private:
  Float_t mEta;
  Float_t mPhi;
  Float_t mSigmaEta;
  Float_t mSigmaPhi;
  Float_t mEnergy;
  Int_t   mNhits;
  TArrayI mHitsID;  
public: 
  StEmcPreCluster(TArrayI*);
  ~StEmcPreCluster();
  Float_t Eta() const;
  Float_t Phi() const;
  Float_t SigmaEta() const;
  Float_t SigmaPhi() const;
  Float_t Energy() const;
  Int_t   Nhits() const;
  Int_t   ID(Int_t);
  TArrayI* HitsID();

  virtual void calcMeanAndRms(StEmcHitCollection*);
  virtual void print(ostream *os);

  ClassDef(StEmcPreCluster,1)// Base class for electromagnetic calorimeter cluster
};

ostream &operator<<(ostream&, StEmcPreCluster&); // Printing operator

inline            StEmcPreCluster::~StEmcPreCluster(){ /* Nobody */ }
inline   Float_t  StEmcPreCluster::Eta() const     {return mEta;} 
inline   Float_t  StEmcPreCluster::Phi() const    {return mPhi;}
inline   Float_t  StEmcPreCluster::SigmaEta() const{return mSigmaEta;}
inline   Float_t  StEmcPreCluster::SigmaPhi() const{return mSigmaPhi;}
inline   Float_t  StEmcPreCluster::Energy() const  {return mEnergy;}
inline   Int_t    StEmcPreCluster::Nhits() const {return mNhits;}

inline   Int_t    StEmcPreCluster::ID(Int_t i) {return mHitsID[i];}
inline   TArrayI *StEmcPreCluster::HitsID() {return &mHitsID;}

#endif





