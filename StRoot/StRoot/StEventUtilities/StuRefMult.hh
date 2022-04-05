/***************************************************************************
 *
 * $Id: StuRefMult.hh,v 1.8 2018/06/29 17:21:24 perev Exp $
 *
 * Author: Manuel Calderon de la Barca Sanchez Aug 2000
 ***************************************************************************
 *
 * Description:
 * Function to give a standard reference multiplicity for 
 * an event, uncorrected of course.  The agreed upon cuts are:
 * primary tracks only
 * flag > 0
 * charge < 0 or charge > 0 depending on the name,
 * fit points >= 10
 * abs(eta) < 0.5
 * dca < 3 cm
 ***************************************************************************
 *
 * $Log: StuRefMult.hh,v $
 * Revision 1.8  2018/06/29 17:21:24  perev
 * Irakli_Jun29
 *
 * Revision 1.7  2005/08/19 03:50:58  perev
 * Marco request
 *
 * Revision 1.6  2005/02/05 01:02:10  perev
 * test for zero momentum added
 *
 * Revision 1.5  2001/11/14 19:47:08  calderon
 * replace
 * StPrimaryVertex* -> const StPrimaryVertex*
 * everywhere for constistency
 *
 * Revision 1.4  2001/11/14 19:28:18  calderon
 * Made the functions take as argument a const StEvent&, as per Mike's request.
 * It is actually better to do this, as the function does not change the StEvent
 * object, and now the language reflects this.
 *
 * Revision 1.3  2000/09/05 18:53:22  calderon
 * Added the functions:
 * 1) uncorrectedNumberOfPositivePrimaries
 * 2) uncorrectedNumberOfPrimaries
 * The first applies the same cuts as before but looks at positive particles
 * and the second returns the sum of the 2.  (Not the fastest way, but anyway...)
 *
 * Revision 1.2  2000/08/28 21:25:28  calderon
 * changed the name of the function to
 * uncorrectedNumberOfNegativePrimaries as per Frank's request
 *
 * Revision 1.1  2000/08/24 17:53:57  calderon
 * Function to obtain a reference (uncorrected) multiplicity
 * from StEvent.  This counts track that satisfy the so called "Cut Set 1"
 * described in the header and also in the README file.
 *
 *
 **************************************************************************/
#ifndef StuRefMult_hh
#define StuRefMult_hh

#include "StEventTypes.h"

inline unsigned int
uncorrectedNumberOfNegativePrimaries(const StPrimaryVertex*& primVtx) {
    const StSPtrVecPrimaryTrack& tracks = primVtx->daughters();
    size_t countedTracks = 0;
    for (StSPtrVecPrimaryTrackConstIterator iter = tracks.begin(); iter != tracks.end(); iter++) {
      StTrack* track = (StTrack*) (*iter);
	// these first 3 checks are easy, save time
	if (track->flag()<0 || track->geometry()->charge()>0 || track->fitTraits().numberOfFitPoints()<10 ) continue; 
	// check eta, a bit more elaborate
	if (fabs(track->geometry()->momentum().mag())<1.e-10) 		continue;
	if (fabs(track->geometry()->momentum().pseudoRapidity())>0.5) 	continue;
	// finally, check dca, if a track satisfies gets inside the if, count it.
	if (track->geometry()->helix().distance(primVtx->position())<3) ++countedTracks;
    }
    return countedTracks;
}

inline unsigned int
uncorrectedNumberOfNegativePrimaries(const StEvent& evt, int vtx_id = 0)
{
    const StPrimaryVertex* primVtx = evt.primaryVertex(vtx_id);

    if (!primVtx) return 0;
    return uncorrectedNumberOfNegativePrimaries(primVtx);
}
    
inline unsigned int
uncorrectedNumberOfPositivePrimaries(const StPrimaryVertex*& primVtx) {
    const StSPtrVecPrimaryTrack& tracks = primVtx->daughters();
    size_t countedTracks = 0;
    for (StSPtrVecPrimaryTrackConstIterator iter = tracks.begin(); iter != tracks.end(); iter++) {
      StTrack* track = (StTrack* )(*iter);
	// these first 3 checks are easy, save time
	if (track->flag()<0 || track->geometry()->charge()<0 || track->fitTraits().numberOfFitPoints()<10 ) continue; 
	// check eta, a bit more elaborate
	if (fabs(track->geometry()->momentum().mag())<=1.e-10) continue;
	if (fabs(track->geometry()->momentum().pseudoRapidity())>0.5) continue;
	// finally, check dca, if a track satisfies gets inside the if, count it.
	if (track->geometry()->helix().distance(primVtx->position())<3) ++countedTracks;
    }
    return countedTracks;
}

inline unsigned int
uncorrectedNumberOfPositivePrimaries(const StEvent& evt, int vtx_id = 0)
{
    const StPrimaryVertex* primVtx = evt.primaryVertex(vtx_id);

    if (!primVtx) return 0;
    
    return uncorrectedNumberOfPositivePrimaries(primVtx);
}
    
inline unsigned int
uncorrectedNumberOfPrimaries(const StEvent& evt, int vtx_id = 0)
{
    const StPrimaryVertex* primVtx = evt.primaryVertex(vtx_id);
    
    if (!primVtx) return 0;
    
    return uncorrectedNumberOfPositivePrimaries(primVtx) + uncorrectedNumberOfNegativePrimaries(primVtx);
}
#endif
