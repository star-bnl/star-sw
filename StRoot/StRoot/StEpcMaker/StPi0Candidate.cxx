//
// $id$
//
// $Log: StPi0Candidate.cxx,v $
// Revision 1.2  2005/05/23 12:35:14  suaide
// New Point maker code
//
// Revision 1.1  2000/05/15 21:18:32  subhasis
// initial version
//
// Pi0Candidates for EMC
//
//
// Author: Subhasis Chattopadhyay , February 2000
//

//////////////////////////////////////////////////////////////////////////
//
// StPi0Candidate
//
//
//////////////////////////////////////////////////////////////////////////

#include "StPi0Candidate.h"
ClassImp(StPi0Candidate)
//_____________________________________________________________________________
StPi0Candidate::StPi0Candidate(Float_t *hits) : TObject()
{

    mEta = hits[0];
    mPhi = hits[1];
    mSigmaEta = hits[2];
    mSigmaPhi = hits[3];
    mEnergy = hits[4];
    mTrackMom =hits[5];
    mDeltaEta =hits[6];
    mDeltaPhi =hits[7];
    mPointFlag =hits[8];


}
