/***************************************************************************
 *
 * $Id: tagFiller.cc,v 2.0 1999/11/04 16:10:14 ullrich Exp $
 *
 * Author: Torre Wenaus, BNL, 1/99
 *         Thomas Ullrich, Nov 1999
 ***************************************************************************
 *
 * Description:  (Partial) example of filling and returning a tag database
 *               object using StEvent.
 *               Use this as a template and customize it for your
 *               studies.
 *
 ***************************************************************************
 *
 * $Log: tagFiller.cc,v $
 * Revision 2.0  1999/11/04 16:10:14  ullrich
 * Revision for new StEvent
 *
 **************************************************************************/
#include "SystemOfUnits.h"
#include "StEventTypes.h"
#include "HighPtTag.h"

static const char rcsid[] = "$Id: tagFiller.cc,v 2.0 1999/11/04 16:10:14 ullrich Exp $";

void
tagFiller(StEvent& event, HighPtTag_st& hptTag)
{
    int i;
    
    // get pt thresholds from somewhere!
    float ptThres[4];
    ptThres[0] = .5;
    ptThres[1] = 1.;
    ptThres[2] = 2.;
    ptThres[3] = 3.;

    // reset counter
    for (i=0; i<4; i++) hptTag.Ntracks_gt_thres[i]=0;
    
    // Loop over 'event' vertices and their primary tracks
    double pt;
    if (event.primaryVertex()) {
	const StSPtrVecPrimaryTrack& tracks = event.primaryVertex()->daughters();
	for (unsigned int k=0; k<tracks.size(); k++) {
	    pt = tracks[k]->geometry()->momentum().perp();
	    for (i=0; i<4; i++) {
		if (pt>ptThres[i]) {
		    hptTag.Ntracks_gt_thres[i]++;
		}
	    }
	}
	    
    }
    
    cout << "Over thres: " <<
	hptTag.Ntracks_gt_thres[0] << " " <<
	hptTag.Ntracks_gt_thres[1] << " " <<
	hptTag.Ntracks_gt_thres[2] << " " <<
	hptTag.Ntracks_gt_thres[3] << " " << endl;
}
