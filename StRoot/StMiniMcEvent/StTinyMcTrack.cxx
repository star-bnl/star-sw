/**
 * $Id $
 * \file  StTinyMcTrack.cxx
 * \brief Implementation of StTinyRcTrack (only a basic constructor and destructor are needed, plus ClassImp for CINT).
 * 
 *
 * \author Bum Choi
 * \date   March 2001
 *  
 */
#include "StTinyMcTrack.h"
#include "Stiostream.h"
#include "TString.h"
ClassImp(StTinyMcTrack);

StTinyMcTrack::StTinyMcTrack() {memset(&mIsValid, 0,((Char_t*) &mIsPrimary)-&mIsValid);}
//________________________________________________________________________________
void StTinyMcTrack::Print(Option_t *option) const {
  if (TString(option).Contains("desc",TString::kIgnoreCase)) {
    cout << Form("  V pT      pZ      eta     phi     HitMc Svt Ssd Ftpc GiD Char R   Key nGl nPr isPr") << endl;
  } else { 
    if (mIsValid) {
      Double_t R = mStopR;
      if (R < 0) R = 0;
      if (R > 999.99) R = 999.99;
      cout << Form("Mc%2i%8.3f%8.3f%8.3f%8.3f%5i%4i%4i%4i%4i%4i%4i%4i%4i%4i%4i%4i%4i%4i%8.3f%4i%8.3f%8.3f%4i%4i%4i%4i%2i", 
		   (int) mIsValid, mPtMc,  mPzMc,  mEtaMc,  mPhiMc,  
		   mNHitMc,  mNSvtHitMc,  mNSsdHitMc,  mNFtpcHitMc,
		   mNBemcHitMc, mNBprsHitMc, mNBsmdeHitMc, mNBsmdpHitMc,
		   mNEemcHitMc, mNEprsHitMc, mNEsmduHitMc, mNEsmdvHitMc,		  
		   mGeantId,  mChargeMc,  R,  mKey,  mParentKey, mEmcEnergyMcHit[0], mEmcEnergyMcSum, mEmcSoftIdHiTowerMc[0], mNAssocGl,  mNAssocPr,  (int) mIsPrimary) << endl;
    }
  }
}
//
// $Log: StTinyMcTrack.cxx,v $
// Revision 1.3  2007/12/22 20:37:53  calderon
// Added EMC information to tracks.  MC info obtained from StMcTrack, Rec Info
// obtained from track extrapolation to BEMC of rec track.
//
// Revision 1.2  2007/02/23 17:07:00  fisyak
// Add Ssd and DCA
//
// Revision 1.1  2002/05/30 01:20:58  calderon
// Classes for use in a general framework for extracting efficiencies
// from both embedding and full simulations
// (after GSTAR+TRS+StEvent+StMcEvent+StAssociationMaker)
// so that the information of the track matches gets stored persistently.
//
//
