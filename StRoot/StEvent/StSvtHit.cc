/***************************************************************************
 *
 * $Id: StSvtHit.cc,v 1.3 1999/03/23 21:51:14 ullrich Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSvtHit.cc,v $
 * Revision 1.3  1999/03/23 21:51:14  ullrich
 * Added table-based class specific constructor.
 *
 * Revision 1.2  1999/01/15 22:53:54  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#include "StEvent/StSvtHit.hh"
#include "StEvent/StGlobalTrack.hh"
#include "StEvent/StTrackCollection.hh"
#include "tables/dst_point.h"

static const char rcsid[] = "$Id: StSvtHit.cc,v 1.3 1999/03/23 21:51:14 ullrich Exp $";

StSvtHit::StSvtHit(const StThreeVector<float>& p,
	           const StThreeVector<float>& e,
	           float q, unsigned char c)
    : StHit(p, e, q, c)
{ /* noop */ }

StSvtHit::StSvtHit(dst_point_st* pt)
{
    //
    // Decode position. cf. pams/global/dst/dst_point_filler.F
    //                      pams/global/dst/dst_point_unpack.F
    //
    if (!pt) {                 // nothing to be done
	mCharge = 0;
	mTrackRefCount = 0;
	return;
    }

    //
    // Unpack charge:
    // The charge is decoded together with its error.
    // Currently only the charge is used but the corresponding
    // error can easily be added.
    //
    const unsigned long svtdq = pt->charge/(1L<<16);
    const unsigned long svtq  = pt->charge - svtdq*(1L<<16);
    mCharge = float(svtq)/(1<<21);

    //
    // Unpack position in xyz
    //
    const float maxRange   = 22;
    const float mapFactor  = 23800;   
    unsigned long svty11 = pt->position[0]/(1L<<20);
    unsigned long svtz   = pt->position[1]/(1L<<10);
    unsigned long svtx   = pt->position[0] - (1L<<20)*svty11;
    unsigned long svty10 = pt->position[1] - (1L<<10)*svtz;
    unsigned long svty   = svty11 + (1L<<10)*svty10;	
    mPosition.setX(float(svtx)/mapFactor - maxRange); 
    mPosition.setY(float(svty)/mapFactor - maxRange);
    mPosition.setZ(float(svtz)/mapFactor - maxRange);
    
    //
    // Unpack error on position in xyz
    //
    svty11 = pt->pos_err[0]/(1L<<20);
    svtz   = pt->pos_err[1]/(1L<<10);
    svtx   = pt->pos_err[0] - (1L<<20)*svty11;
    svty10 = pt->pos_err[1] - (1L<<10)*svtz;
    svty   = svty11 + (1L<<10)*svty10;
    mPositionError.setX(float(svtx)/(1L<<26)); 
    mPositionError.setY(float(svty)/(1L<<26));
    mPositionError.setZ(float(svtz)/(1L<<26));
}

StVecPtrGlobalTrack StSvtHit::relatedTracks(const StTrackCollection& c)
{
    StVecPtrGlobalTrack  result;
    StGlobalTrack        *track;
    StTrackConstIterator iter;
    
    for (iter = c.begin(); iter != c.end(); iter++) {
	track = *iter;
	const StVecPtrSvtHit &hits = track->svtHits();
	if (find(hits.begin(), hits.end(), this) != hits.end())
	    result.push_back(track);
    }
    return result;
}
