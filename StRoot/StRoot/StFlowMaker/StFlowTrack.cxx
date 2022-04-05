//////////////////////////////////////////////////////////////////////
//
// $Id: StFlowTrack.cxx,v 1.9 2000/12/12 20:22:06 posk Exp $
//
// Author: Raimond Snellings and Art Poskanzer
//          FTPC added by Markus Oldenburg, MPI, Dec 2000
//
//////////////////////////////////////////////////////////////////////
//
// Description: part of StFlowEvent 
//   FlowTrack is the main component of StFlowEvent
//
//////////////////////////////////////////////////////////////////////

#include "StFlowTrack.h"
#include "TMath.h"
#include "StEnumerations.h"

ClassImp(StFlowTrack)

Float_t StFlowTrack::maxInt  = 32.;

StFlowTrack::StFlowTrack() : mSelection(0) {
}


StFlowTrack::~StFlowTrack() {
}

//////////////////////////////////////////////////////////////////////
//
// $Log: StFlowTrack.cxx,v $
// Revision 1.9  2000/12/12 20:22:06  posk
// Put log comments at end of files.
// Deleted persistent StFlowEvent (old micro DST).
//
// Revision 1.8  2000/12/10 02:01:13  oldi
// A new member (StTrackTopologyMap mTopology) was added to StFlowPicoTrack.
// The evaluation of either a track originates from the FTPC or not is
// unambiguous now. The evaluation itself is easily extendible for other
// detectors (e.g. SVT+TPC). Old flowpicoevent.root files are treated as if
// they contain TPC tracks only (backward compatibility).
//
// Revision 1.6  2000/12/08 17:03:39  oldi
// Phi weights for both FTPCs included.
//
// Revision 1.5  2000/12/06 15:38:46  oldi
// Including FTPC.
//
// Revision 1.4  2000/09/05 16:11:39  snelling
// Added global DCA, electron and positron
//
// Revision 1.3  2000/06/01 18:26:41  posk
// Increased precision of Track integer data members.
//
// Revision 1.2  2000/05/26 21:29:34  posk
// Protected Track data members from overflow.
//
// Revision 1.1  2000/05/12 22:42:05  snelling
// Additions for persistency and minor fix
//
//////////////////////////////////////////////////////////////////////
