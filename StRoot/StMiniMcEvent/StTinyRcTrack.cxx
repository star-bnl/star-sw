/**
 * $Id $
 * \file  StTinyRcTrack.cxx
 * \brief Implementation of StTinyMcTrack (only a basic constructor and destructor are needed, plus ClassImp for CINT).
 * 
 *
 * \author Bum Choi
 * \date   March 2001
 *  $Log $
 */
#include "StTinyRcTrack.h"
#include "TString.h"
#include "Stiostream.h"
ClassImp(StTinyRcTrack);

StTinyRcTrack::StTinyRcTrack() {memset(&mIsValidGl, 0, &mIsValidPr-&mIsValidGl);}
void StTinyRcTrack::Print(Option_t *option) const {
  if (TString(option).Contains("desc",TString::kIgnoreCase)) 
    cout << Form("  V pT      pZ      eta     phi     Dca     DcaXY   DcaZ    Flag  Tpc Svt Ssd Ftpc All Ass Pos") << endl;
  else {
    if (mIsValidGl) 
      cout << Form("Gl%2i%8.3f%8.3f%8.3f%8.3f%8.3f%8.3f%8.3f%5i%4i%4i%4i%4i%4i%4i%4i%4i", 
		   (int) mIsValidGl, mPtGl,  mPzGl,  mEtaGl,  mPhiGl, mDcaGl, mDcaXYGl,mDcaZGl, 
		   mFlag,  mFitPts, mFitSvt, mFitSsd, mFitFtpc, mAllPts, mNAssocMc, mNPossible) << endl;
    if (mIsValidPr)
      cout << Form("Pr%2i%8.3f%8.3f%8.3f%8.3f%8.3f%8.3f%8.3f", 
		   (int) mIsValidPr, mPtPr,  mPzPr,  mEtaPr,  mPhiPr, mDcaPr, mDcaXYPr,mDcaZPr)      << endl;
  }
}
//________________________________________________________________________________
//
// $Log: StTinyRcTrack.cxx,v $
// Revision 1.3  2007/02/23 17:07:01  fisyak
// Add Ssd and DCA
//
// Revision 1.2  2002/06/06 18:58:29  calderon
// Added $Log: StTinyRcTrack.cxx,v $
// Added Revision 1.3  2007/02/23 17:07:01  fisyak
// Added Add Ssd and DCA
// Added
// Added mDedxPts data member, get and set methods, and updated ClassDef(StTinyRcTrack,1)
// to ClassDef(StTinyRcTrack,2) because of this change for schema evolution
//
// Revision 1.1  2002/05/30 01:20:58  calderon
// Classes for use in a general framework for extracting efficiencies
// from both embedding and full simulations
// (after GSTAR+TRS+StEvent+StMcEvent+StAssociationMaker)
// so that the information of the track matches gets stored persistently.
//
//
