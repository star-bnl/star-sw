///
// $Id: StPi0Candidate.h,v 1.1 2000/05/15 21:18:33 subhasis Exp $
//
// $Log: StPi0Candidate.h,v $
// Revision 1.1  2000/05/15 21:18:33  subhasis
// initial version
//
//
// Authors: Subhasis Chattopadhyay , February 2000.
//    

#ifndef STAR_StPi0Candidate
#define STAR_StPi0Candidate

#include <math.h>
#include <iostream.h>
#include "TArrayI.h"
#include "TObject.h"
#include "St_emc_Maker/StEmcHitCollection.h"

class StPi0Candidate : public TObject {
private:
  Float_t mEta;
  Float_t mPhi;
  Float_t mSigmaEta;
  Float_t mSigmaPhi;
  Float_t mEnergy;
  Float_t mTrackMom;
  Float_t mDeltaEta;
  Float_t mDeltaPhi;
  Float_t mPointFlag;
public: 
  StPi0Candidate(Float_t*);
  virtual ~StPi0Candidate();
  Float_t Eta() const;
  Float_t Phi() const;
  Float_t SigmaEta() const;
  Float_t SigmaPhi() const;
  Float_t Energy() const;
  Float_t TrackMom() const;
  Float_t DeltaEta() const;
  Float_t DeltaPhi() const;
  Float_t PointFlag() const;

  ClassDef(StPi0Candidate,1)// Base class for electromagnetic calorimeter pi0Candidate
};

ostream &operator<<(ostream&, StPi0Candidate&); // Printing operator

inline            StPi0Candidate::~StPi0Candidate(){ /* Nobody */ }
inline   Float_t  StPi0Candidate::Eta() const     {return mEta;} 
inline   Float_t  StPi0Candidate::Phi() const    {return mPhi;}
inline   Float_t  StPi0Candidate::SigmaEta() const{return mSigmaEta;}
inline   Float_t  StPi0Candidate::SigmaPhi() const{return mSigmaPhi;}
inline   Float_t  StPi0Candidate::Energy() const  {return mEnergy;}
inline   Float_t  StPi0Candidate::TrackMom() const {return mTrackMom;}
inline   Float_t  StPi0Candidate::DeltaEta() const {return mDeltaEta;}
inline   Float_t  StPi0Candidate::DeltaPhi() const {return mDeltaPhi;}
inline   Float_t  StPi0Candidate::PointFlag() const {return mPointFlag;}

#endif





