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
//    cout <<Form("Gl%2i%8.3f%8.3f%8.3f%8.3f%8.3f%8.3f%8.3f%5i%4i%4i%4i%4i%4i%4i%4i%4i%4i%4i%8.3f", 
      cout <<Form("Gl%2i%8.3f%8.3f%8.3f%8.3f%8.3f%8.3f%8.3f%5i%4i%4i%4i%4i%4i%4i%4i%7i%7i%8.3f",
		   (int) mIsValidGl, mPtGl,  mPzGl,  mEtaGl,  mPhiGl, mDcaGl, mDcaXYGl,mDcaZGl, 
		   mFlag,  mFitPts, mFitSvt, mFitSsd, mFitFtpc, mAllPts, mNAssocMc, mNPossible, mEmcSoftIdHiTowerRc[0],mEmcTowerAdc[0], mEmcEnergyRcHit[0] ) << endl;
    if (mIsValidPr)
      cout << Form("Pr%2i%8.3f%8.3f%8.3f%8.3f%8.3f%8.3f%8.3f", 
		   (int) mIsValidPr, mPtPr,  mPzPr,  mEtaPr,  mPhiPr, mDcaPr, mDcaXYPr,mDcaZPr)      << endl;
  }
}
//________________________________________________________________________________
//
// $Log: StTinyRcTrack.cxx,v $
// Revision 1.5  2010/08/02 20:14:16  perev
// Format fix, thanks to Hiroshi
//
// Revision 1.4  2007/12/22 20:37:53  calderon
// Added EMC information to tracks.  MC info obtained from StMcTrack, Rec Info
// obtained from track extrapolation to BEMC of rec track.
//
// Revision 1.3  2007/02/23 17:07:01  fisyak
// Add Ssd and DCA
//
// Revision 1.2  2002/06/06 18:58:29  calderon
// Added $Log: StTinyRcTrack.cxx,v $
// Added Revision 1.5  2010/08/02 20:14:16  perev
// Added Format fix, thanks to Hiroshi
// Added
// Added Revision 1.4  2007/12/22 20:37:53  calderon
// Added Added EMC information to tracks.  MC info obtained from StMcTrack, Rec Info
// Added obtained from track extrapolation to BEMC of rec track.
// Added
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
