////////////////////////////////////////////////////////////////////////////
//
// $Id: StFlowSelection.cxx,v 1.24 2004/12/09 23:43:38 posk Exp $
//
// Author: Art Poskanzer and Raimond Snellings, LBNL, Mar 2000
//          FTPC added by Markus Oldenburg, MPI, Dec 2000
//
// Description:  Class for making selections
//
////////////////////////////////////////////////////////////////////////////

#include <Stiostream.h>
#include <stdlib.h>
#include "StFlowSelection.h"
#include "StFlowEvent.h"
#include "StFlowTrack.h"
#define PR(x) cout << "##### FlowSelection: " << (#x) << " = " << (x) << endl;

ClassImp(StFlowSelection)

//-----------------------------------------------------------------------


StFlowSelection::StFlowSelection() : mSubevent(-1) {
  // To make selections
  mPidPart[0] = '\0';
  mPtPart[0]            = 0.;
  mPtPart[1]            = 0.;
  mPtBinsPart           = 0;
  mPPart[0]             = 0.;
  mPPart[1]             = 0.;
  mEtaPart[0]           = 0.;
  mEtaPart[1]           = 0.;
  mFitPtsPart[0]        = 0;
  mFitPtsPart[1]        = 0;
  mDedxPtsPart[0]       = 0;
  mDedxPtsPart[1]       = 0;
  mFitOverMaxPtsPart[0] = 0.;
  mFitOverMaxPtsPart[1] = 0.;
  mChiSqPart[0]         = 0.;
  mChiSqPart[1]         = 0.;
  mDcaGlobalPart[0]     = 0.;
  mDcaGlobalPart[1]     = 2.;
  mYPart[0]             = 0.;
  mYPart[1]             = 0.;
}

//-----------------------------------------------------------------------

StFlowSelection::~StFlowSelection() {
}

//-----------------------------------------------------------------------

Bool_t StFlowSelection::Select(StFlowEvent* pFlowEvent) {
  // Returns kTRUE if the event is selected

  return kTRUE;
}

//-----------------------------------------------------------------------

Bool_t StFlowSelection::Select(StFlowTrack* pFlowTrack) {
  // Selects particles for event plane determination
  // Returns kTRUE if the track is selected

  // Selected for event plane
  if (!pFlowTrack->Select(mHarmonic, mSelection, mSubevent)) return kFALSE;

  return kTRUE;
}

//-----------------------------------------------------------------------

Bool_t StFlowSelection::SelectPart(StFlowTrack* pFlowTrack) {
  // Selects particles for correlation with the event plane
  // Returns kTRUE if the track is selected

  // PID
  if (mPidPart[0] != '\0') {
    if (strstr(mPidPart, "h")!=0) {
      int charge = pFlowTrack->Charge();
      if (strcmp("h+", mPidPart)==0 && charge != 1)  return kFALSE;
      if (strcmp("h-", mPidPart)==0 && charge != -1) return kFALSE;
    } else {
      const Char_t* pid = pFlowTrack->Pid();
      if (strstr(pid, mPidPart)==0) return kFALSE;
    }
  }
  
  // Pt
  float pt = pFlowTrack->Pt();
  if (mPtPart[1] > mPtPart[0] && 
      (pt < mPtPart[0] || pt >= mPtPart[1])) return kFALSE;
  
  // P
  float totalp = pFlowTrack->P();
  if (mPPart[1] > mPPart[0] && 
      (totalp < mPPart[0] || totalp >= mPPart[1])) return kFALSE;
  
  // Eta
  float eta = pFlowTrack->Eta();
  if (mEtaPart[1] > mEtaPart[0] && 
      (eta < mEtaPart[0] || eta >= mEtaPart[1])) return kFALSE;

  // Fit Points
  int fitPts = pFlowTrack->FitPts();
  if (mFitPtsPart[1] > mFitPtsPart[0] && 
      (fitPts < mFitPtsPart[0] || fitPts >= mFitPtsPart[1])) return kFALSE;

  // Dedx Points
  int dedxPts = pFlowTrack->NdedxPts();
  if (mDedxPtsPart[1] > mDedxPtsPart[0] && 
      (dedxPts < mDedxPtsPart[0] || dedxPts >= mDedxPtsPart[1])) return kFALSE;


  // Fit Points over Max Points
  int maxPts = pFlowTrack->MaxPts();
  float fitOverMaxPts = (float)fitPts/(float)maxPts;
  if (mFitOverMaxPtsPart[1] > mFitOverMaxPtsPart[0] && 
      (fitOverMaxPts < mFitOverMaxPtsPart[0] || 
       fitOverMaxPts >= mFitOverMaxPtsPart[1])) return kFALSE;

  // Chi Squared
  float chiSq = pFlowTrack->Chi2();
  if (mChiSqPart[1] > mChiSqPart[0] && 
      (chiSq < mChiSqPart[0] || 
       chiSq >= mChiSqPart[1])) return kFALSE;

  // Dca Global
  float globdca = pFlowTrack->DcaGlobal();
  if (mDcaGlobalPart[1] > mDcaGlobalPart[0] && 
      (globdca < mDcaGlobalPart[0] || 
       globdca >= mDcaGlobalPart[1])) return kFALSE;

  // Rapidity
  float Y = pFlowTrack->Y();
  if (mYPart[1] > mYPart[0] && 
      (Y < mYPart[0] || Y >= mYPart[1])) return kFALSE;

  return kTRUE;
}

//-----------------------------------------------------------------------

void StFlowSelection::PrintList() const {
  
  cout << "#################################################################"
       << endl;
  cout << "# Selection List:" << endl;
  cout << "# Particles correlated with the event plane: " << mPidPart << endl;
  cout << "# Pt for particles correlated with the event plane: " << 
    mPtPart[0] << " to " << mPtPart[1] << " GeV/c" <<endl;
  cout << "# P for particles correlated with the event plane: " << 
    mPPart[0] << " to " << mPPart[1] << " GeV/c" <<endl;
  cout << "# Eta for particles correlated with the event plane: " << 
    mEtaPart[0] << " to " << mEtaPart[1] <<endl;
  cout << "# Y for particles correlated with the event plane: " << 
    mYPart[0] << " to " << mYPart[1] <<endl;
  cout << "# Fit Points for particles correlated with the event plane: " << 
    mFitPtsPart[0] << " to " << mFitPtsPart[1] <<endl;
  cout << "# Dedx Points for particles correlated with the event plane: " << 
    mDedxPtsPart[0] << " to " << mDedxPtsPart[1] <<endl;
  cout << "# Fit/Max Points for particles correlated with the event plane: " 
       << mFitOverMaxPtsPart[0] << " to " << mFitOverMaxPtsPart[1] <<endl;
  cout << "# Chi2 for particles correlated with the event plane: " << 
    mChiSqPart[0] << " to " << mChiSqPart[1] <<endl;
  cout << "# Global Dca for particles correlated with the event plane: " << 
    mDcaGlobalPart[0] << " to " << mDcaGlobalPart[1] <<endl;
  cout << "#################################################################"
       << endl;

}

////////////////////////////////////////////////////////////////////////////
//
// $Log: StFlowSelection.cxx,v $
// Revision 1.24  2004/12/09 23:43:38  posk
// Minor changes in code formatting.
//
// Revision 1.23  2004/12/07 17:01:13  posk
// Changed the default value of the dca selection for particles correlated with
// the event plane from 1 cm back to 2 cm.
//
// Revision 1.22  2004/08/18 00:19:21  oldi
// Several changes were necessary to comply with latest changes of MuDsts and StEvent:
//
// nHits, nFitPoints, nMaxPoints
// -----------------------------
// From now on
//  - the fit points used in StFlowMaker are the fit points within the TPC xor FTPC (vertex excluded).
//  - the max. possible points used in StFlowMAker are the max. possible points within the TPC xor FTPC (vertex excluded).
//  - the number of points (nHits; not used for analyses so far) are the total number of points on a track, i. e.
//    TPC + SVT + SSD + FTPCeast + FTPCwest [reading from HBT event gives a warning, but it seems like nobody uses it anyhow].
// - The fit/max plot (used to be (fit-1)/max) was updated accordingly.
// - The default cuts for fit points were changed (only for the FTPC, since TPC doesn't set default cuts).
// - All these changes are backward compatible, as long as you change your cuts for the fit points by 1 (the vertex used to
//   be included and is not included anymore). In other words, your results won't depend on old or new MuDst, StEvent,
//   PicoDsts as long as you use the new flow software (together with the latest MuDst and StEvent software version).
// - For backward compatibility reasons the number of fit points which is written out to the flowpicoevent.root file
//   includes the vertex. It is subtracted internally while reading back the pico files. This is completely hidden from the
//   user.
//
// zFirstPoint
// -----------
// The positions of the first point of tracks which have points in the TPC can lie outside of the TPC (the tracks can start in
// the SVT or SSD now). In this case, the first point of the track is obtained by extrapolating the track helix to the inner
// radius of the TPC.
//
// Revision 1.21  2004/02/03 22:36:37  posk
// Initialzed mPtBinsPart.
//
// Revision 1.20  2003/09/02 17:58:12  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.19  2003/05/15 06:08:41  aihong
// default PID is changed from none to NA, SetDedxPtsPart() added
//
// Revision 1.18  2002/06/12 22:36:44  posk
// FitOverMax points cut/selection is now done on (FitPts - 1)/MaxPts.
//
// Revision 1.17  2002/06/10 22:51:02  posk
// pt and eta weighting now default.
// DcaGlobalPart default now 0 to 1 cm.
// Event cut order changed.
//
// Revision 1.16  2002/01/31 01:04:52  posk
// *** empty log message ***
//
// Revision 1.15  2001/11/09 21:10:57  posk
// Switched from CERNLIB to TMath. Little q is now normalized.
//
// Revision 1.14  2001/05/22 20:17:58  posk
// Now can do pseudorapidity subevents.
//
// Revision 1.13  2000/12/12 20:22:06  posk
// Put log comments at end of files.
// Deleted persistent StFlowEvent (old micro DST).
//
// Revision 1.12  2000/12/08 17:03:39  oldi
// Phi weights for both FTPCs included.
//
// Revision 1.10  2000/09/16 22:20:32  snelling
// Added selection on P and global DCA and fixed rapidity calulation
//
// Revision 1.9  2000/09/15 22:51:34  posk
// Added pt weighting for event plane calcualtion.
//
// Revision 1.8  2000/09/15 01:20:02  snelling
// Added methods for P and Y and added selection on Y
//
// Revision 1.7  2000/09/13 00:32:27  snelling
// Added selections for particles correlated with reaction plane
//
// Revision 1.6  2000/08/31 18:58:26  posk
// For picoDST, added version number, runID, and multEta for centrality.
// Added centrality cut when reading picoDST.
// Added pt and eta selections for particles corr. wrt event plane.
//
// Revision 1.5  2000/08/12 20:22:21  posk
// Recalculate centrality in read from pico.
//
// Revision 1.4  2000/05/26 21:29:32  posk
// Protected Track data members from overflow.
//
// Revision 1.2  2000/03/28 23:21:04  posk
// Allow multiple instances of the AnalysisMaker.
//
// Revision 1.1  2000/03/15 23:28:53  posk
// Added StFlowSelection.
//
////////////////////////////////////////////////////////////////////////////
