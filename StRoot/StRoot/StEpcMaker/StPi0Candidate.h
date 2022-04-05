///
// $Id: StPi0Candidate.h,v 1.5 2012/06/11 14:39:43 fisyak Exp $
//
// $Log: StPi0Candidate.h,v $
// Revision 1.5  2012/06/11 14:39:43  fisyak
// std namespace
//
// Revision 1.4  2005/05/23 12:35:14  suaide
// New Point maker code
//
// Revision 1.3  2003/09/02 17:58:03  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.2  2001/03/13 16:23:04  subhasis
// StEmcGeom.h moved from St_emc_Maker
//
// Revision 1.1  2000/05/15 21:18:33  subhasis
// initial version
//
//
// Authors: Subhasis Chattopadhyay , February 2000.
//

#ifndef STAR_StPi0Candidate
#define STAR_StPi0Candidate

#include "TObject.h"

class StPi0Candidate : public TObject
{
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

std::ostream &operator<<(std::ostream&, StPi0Candidate&); // Printing operator

inline            StPi0Candidate::~StPi0Candidate()
{ /* Nobody */
}
inline   Float_t  StPi0Candidate::Eta() const
{
    return mEta;
}
inline   Float_t  StPi0Candidate::Phi() const
{
    return mPhi;
}
inline   Float_t  StPi0Candidate::SigmaEta() const
{
    return mSigmaEta;
}
inline   Float_t  StPi0Candidate::SigmaPhi() const
{
    return mSigmaPhi;
}
inline   Float_t  StPi0Candidate::Energy() const
{
    return mEnergy;
}
inline   Float_t  StPi0Candidate::TrackMom() const
{
    return mTrackMom;
}
inline   Float_t  StPi0Candidate::DeltaEta() const
{
    return mDeltaEta;
}
inline   Float_t  StPi0Candidate::DeltaPhi() const
{
    return mDeltaPhi;
}
inline   Float_t  StPi0Candidate::PointFlag() const
{
    return mPointFlag;
}

#endif





