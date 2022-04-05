/***************************************************************************
 *
 * $Id: StHbtTTreeTrack.cxx,v 1.3 2003/01/31 19:57:15 magestro Exp $
 *
 * Author: Frank Laue, BNL, laue@bnl.gov
 ***************************************************************************/

#include "StHbtTTreeTrack.h"
#include "StExceptions.hh"

#include "StHbtEvent.hh"
#include "StHbtTrack.hh"
// #include "StEvent/StEventTypes.h"
// #include "StEvent/StTpcDedxPidAlgorithm.h"

#include "StarClassLibrary/StElectron.hh"
#include "StarClassLibrary/StPionPlus.hh"
#include "StarClassLibrary/StKaonPlus.hh"
#include "StarClassLibrary/StProton.hh"

//   StGlobalTrack* gTrack = (StGlobalTrack*)track->node()->track(global);
//   StPrimaryTrack* pTrack = (StPrimaryTrack*)track->node()->track(primary);
//   StTrack* cTrack;
//   if (pTrack) cTrack=pTrack;
//   else cTrack=gTrack;
//   if (!gTrack) 
//     throw StExceptionNullPointer("StHbtTTreeTrack::StHbtTTreeTrack(...) - gTrack");
//   StTpcDedxPidAlgorithm* PidAlgorithm = new StTpcDedxPidAlgorithm();
  
//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
StHbtTTreeTrack::StHbtTTreeTrack(const StHbtEvent* event, const StHbtTrack* track) {
  StThreeVectorF vp = event->mPrimVertPos; //!
  // fill for global track values

    mHelixGlobalC = track->mHelixGlobal.curvature();
    mHelixGlobalDip = track->mHelixGlobal.dipAngle();
    mHelixGlobalPhase = track->mHelixGlobal.phase();
    mHelixGlobalX = track->mHelixGlobal.origin().x();
    mHelixGlobalY = track->mHelixGlobal.origin().y();
    mHelixGlobalZ = track->mHelixGlobal.origin().z();
    mHelixGlobalH = track->mHelixGlobal.h();

    mHelixC = track->mHelix.curvature();
    mHelixDip = track->mHelix.dipAngle();
    mHelixPhase = track->mHelix.phase();
    mHelixX = track->mHelix.origin().x();
    mHelixY = track->mHelix.origin().y();
    mHelixZ = track->mHelix.origin().z();
    mHelixH = track->mHelix.h();

    mMap[0] = track->mMap[0];
    mMap[1] = track->mMap[1];
    mNHits = track->mNHits;
    mNHitsPoss = track->mNHitsPoss;
    mNHitsDedx = track->mNHitsDedx;
    mChiSqXY = track->mChiSqXY;
    mChiSqZ = track->mChiSqXY;
    mdEdx      = track->mdEdx;
    mNSigmaElectron = track->mNSigmaElectron;
    mNSigmaPion     = track->mNSigmaPion;
    mNSigmaKaon     = track->mNSigmaKaon;
    mNSigmaProton   = track->mNSigmaProton;
    mPidProbElectron = (short) (1000.*track->mPidProbElectron+.5);
    mPidProbPion = (short) (1000.*track->mPidProbPion+.5);
    mPidProbKaon = (short) (1000.*track->mPidProbKaon+.5);
    mPidProbProton = (short) (1000.*track->mPidProbProton+.5);

    mTrackId = track->mTrackId;
    mTrackType = track->mTrackType;
};

ClassImp(StHbtTTreeTrack)


/***************************************************************************
 *
 * $Log: StHbtTTreeTrack.cxx,v $
 * Revision 1.3  2003/01/31 19:57:15  magestro
 * Cleared up simple compiler warnings on i386_linux24
 *
 * Revision 1.2  2001/09/05 20:41:42  laue
 * Updates of the hbtMuDstTree microDSTs
 *
 * Revision 1.1  2001/06/21 19:15:47  laue
 * Modified fiels:
 *   CTH.hh : new constructor added
 *   StHbtEvent, StHbtKink, StHbtTrack : constructors from the persistent
 *                                   (TTree) classes added
 *   StHbtLikeSignAnalysis : minor changes, for debugging
 *   StHbtTypes: split into different files
 * Added files: for the new TTree muDst's
 *   StExceptions.cxx StExceptions.hh StHbtEnumeration.hh
 *   StHbtHelix.hh StHbtHisto.hh StHbtString.hh StHbtTFile.hh
 *   StHbtTTreeEvent.cxx StHbtTTreeEvent.h StHbtTTreeKink.cxx
 *   StHbtTTreeKink.h StHbtTTreeTrack.cxx StHbtTTreeTrack.h
 *   StHbtTTreeV0.cxx StHbtTTreeV0.h StHbtVector.hh
 *
 *
 **************************************************************************/
