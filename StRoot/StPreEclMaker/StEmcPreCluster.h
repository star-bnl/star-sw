//
// $Id: StEmcPreCluster.h,v 1.4 2000/08/24 22:11:34 suaide Exp $
//
// $Log: StEmcPreCluster.h,v $
// Revision 1.4  2000/08/24 22:11:34  suaide
// restored some files for background compatibility
//
// Revision 1.3  2000/08/24 19:45:37  suaide
//
//
// small modifications: some cout has been removed
//
// Revision 1.2  2000/08/24 11:26:48  suaide
//
//
//
// by A. A. P. Suaide - 2000/08/24 07:25:00
//
// Notes:
//
// 1. Full StEvent Compatible
// 2. Read hits from StEvent object
// 3. Write clusters in StEvent format and old format to keep background
//    compatibility
// 4. Do clustering in bemc, bprs, bsmde, bsmdp
// 5. Included method StPreEclMaker::SetClusterCollection
//
// Revision 1.1  2000/05/15 21:24:00  subhasis
// initial version
//
// PreClusters Finder Maker for EMC
//
//
// Authors: Alexandre A. P. Suaide (version 2.0)
//          Subhasis Chattopadhyay,
//          Aleksei Pavlinov , July 1999.
//          initial version from Akio Ogawa    
//    

#ifndef STAR_StEmcPreCluster
#define STAR_StEmcPreCluster

#include <math.h>
#include <iostream.h>
#include "TArrayI.h"
#include "TObject.h"
#include "StEvent/StEmcDetector.h"
class StEmcPreCluster : public TObject {

private:
  Int_t             mModule;
  Float_t           mEta;
  Float_t           mPhi;
  Float_t           mSigmaEta;
  Float_t           mSigmaPhi;
  Float_t           mEnergy;
  Int_t             mNhits;
  TArrayI           mHitsID;  
public: 
                    StEmcPreCluster(TArrayI*);
                    StEmcPreCluster(Int_t,TArrayI*,Int_t);
                    ~StEmcPreCluster();
  Float_t           Eta() const;
  Float_t           Phi() const;
  Float_t           SigmaEta() const;
  Float_t           SigmaPhi() const;
  Float_t           Energy() const;
  Int_t             Nhits() const;
  Int_t             Module() const;
  Int_t             ID(Int_t);
  TArrayI*          HitsID();

  virtual void      calcMeanAndRms(StEmcDetector*,Int_t);
  virtual void      print(ostream *os);

  ClassDef(StEmcPreCluster,2)// Base class for electromagnetic calorimeter cluster
};

ostream &operator<<(ostream&, StEmcPreCluster&); // Printing operator

inline              StEmcPreCluster::~StEmcPreCluster(){ /* Nobody */ }
inline   Float_t    StEmcPreCluster::Eta() const     {return mEta;} 
inline   Float_t    StEmcPreCluster::Phi() const    {return mPhi;}
inline   Float_t    StEmcPreCluster::SigmaEta() const{return mSigmaEta;}
inline   Float_t    StEmcPreCluster::SigmaPhi() const{return mSigmaPhi;}
inline   Float_t    StEmcPreCluster::Energy() const  {return mEnergy;}
inline   Int_t      StEmcPreCluster::Nhits() const {return mNhits;}
inline   Int_t      StEmcPreCluster::Module() const {return mModule;}
inline   Int_t      StEmcPreCluster::ID(Int_t i) {return mHitsID[i];}
inline   TArrayI    *StEmcPreCluster::HitsID() {return &mHitsID;}

#endif





