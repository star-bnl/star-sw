// $Id: tagFiller.cc,v 1.3 1999/03/30 20:37:49 wenaus Exp $
// $Log: tagFiller.cc,v $
// Revision 1.3  1999/03/30 20:37:49  wenaus
// Explicit StGlobalTrack include
//
// Revision 1.2  1999/03/30 15:33:43  wenaus
// eliminate obsolete branch methods
//
// Revision 1.1  1999/02/12 02:10:47  wenaus
// add tagFiller.cc
//
// Revision 1.2  1999/02/11 15:39:16  wenaus
// cleanup
//
//
///////////////////////////////////////////////////////////////////////////////
//
// tagFiller.cc
//
// Description: 
//  (Partial) example of filling and returning a tag database object
//
// Environment:
//  Software developed for the STAR Detector at Brookhaven National Laboratory
//
// Author List: 
//  Torre Wenaus, BNL  1/99
//
// History:
//
///////////////////////////////////////////////////////////////////////////////
 * Revision for new StEvent
#include "StEvent/StEvent.hh"
#include "StEvent/StGlobalTrack.hh"
#include "tables/HighPtTag.h"
#include "SystemOfUnits.h"
static const char rcsid[] = "$Id: tagFiller.cc,v 1.3 1999/03/30 20:37:49 wenaus Exp $";
#include "HighPtTag.h"
void tagFiller(StEvent& event, HighPtTag_st& hptTag)

  // get pt thresholds from somewhere!
  float ptThres[4];
  ptThres[0] = .5;
  ptThres[1] = 1.;
  ptThres[2] = 2.;
  ptThres[3] = 3.;
  // get field from somewhere!
  float bField = 0.5*tesla;
  // Loop over tracks
  StTrackCollection* tracks = event.trackCollection();
  StTrackIterator itr;
  StGlobalTrack *trk = 0;
  float pt;
  int ii;
  for (ii=0;ii<4;ii++) hptTag.Ntracks_gt_thres[ii]=0;
  for (itr = tracks->begin(); itr != tracks->end(); itr++) {
    trk = *itr;
    pt = trk->helix().momentum(bField).perp();

//    cout << "Track pt " << pt/GeV
//         << " p " << trk->helix().momentum(bField).magnitude()
//         << " dip " << trk->helix().dipAngle()
//         << " rad " << 1./trk->helix().curvature()*centimeter
//         << " phase " << trk->helix().phase()
//         << " x " << trk->helix().origin().x()/centimeter
//         << " y " << trk->helix().origin().y()/centimeter
//         << " z " << trk->helix().origin().z()/centimeter
//         << endl;
    ptThres[1] = 1.;
    for (ii=0; ii<4; ii++) {
      if (pt>ptThres[ii]) {
        hptTag.Ntracks_gt_thres[ii]++;
      }
	    }
  }
  cout << "Over thres: " <<
    hptTag.Ntracks_gt_thres[0] << " " <<
    hptTag.Ntracks_gt_thres[1] << " " <<
    hptTag.Ntracks_gt_thres[2] << " " <<
    hptTag.Ntracks_gt_thres[3] << " " << endl;
	hptTag.Ntracks_gt_thres[1] << " " <<
	hptTag.Ntracks_gt_thres[2] << " " <<
	hptTag.Ntracks_gt_thres[3] << " " << endl;
}
