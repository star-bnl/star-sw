/***************************************************************************
 *
 * $Id: StTpcHit.cc,v 1.3 1999/03/23 21:51:09 ullrich Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTpcHit.cc,v $
 * Revision 1.3  1999/03/23 21:51:09  ullrich
 * Added table-based class specific constructor.
 *
 * Revision 1.2  1999/01/15 22:53:58  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#include "StEvent/StTpcHit.hh"
#include "StEvent/StGlobalTrack.hh"
#include "StEvent/StTrackCollection.hh"
#include "tables/dst_point.h"

static const char rcsid[] = "$Id: StTpcHit.cc,v 1.3 1999/03/23 21:51:09 ullrich Exp $";

StTpcHit::StTpcHit(const StThreeVector<float>& p,
	           const StThreeVector<float>& e,
	           float q, unsigned char c)  : StHit(p, e, q, c)
{ /* noop */ }

StTpcHit::StTpcHit(dst_point_st* pt)
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
    const unsigned long tpcdq = pt->charge/(1L<<16);
    const unsigned long tpcq  = pt->charge - tpcdq*(1L<<16);
    mCharge = float(tpcq)/(1<<16);

    //
    // Unpack position in xyz
    //
    const float maxRange   = 220;
    const float mapFactor  = 2380;   
    unsigned long tpcy11 = pt->position[0]/(1L<<20);
    unsigned long tpcz   = pt->position[1]/(1L<<10);
    unsigned long tpcx   = pt->position[0] - (1L<<20)*tpcy11;
    unsigned long tpcy10 = pt->position[1] - (1L<<10)*tpcz;
    unsigned long tpcy   = tpcy11 + (1L<<10)*tpcy10;	
    mPosition.setX(float(tpcx)/mapFactor - maxRange); 
    mPosition.setY(float(tpcy)/mapFactor - maxRange);
    mPosition.setZ(float(tpcz)/mapFactor - maxRange);
    
    //
    // Unpack error on position in xyz
    //
    tpcy11 = pt->pos_err[0]/(1L<<20);
    tpcz   = pt->pos_err[1]/(1L<<10);
    tpcx   = pt->pos_err[0] - (1L<<20)*tpcy11;
    tpcy10 = pt->pos_err[1] - (1L<<10)*tpcz;
    tpcy   = tpcy11 + (1L<<10)*tpcy10;
    mPositionError.setX(float(tpcx)/(1L<<17)); 
    mPositionError.setY(float(tpcy)/(1L<<17));
    mPositionError.setZ(float(tpcz)/(1L<<17));
}

StVecPtrGlobalTrack StTpcHit::relatedTracks(const StTrackCollection& c)
{
    StVecPtrGlobalTrack  result;
    StGlobalTrack        *track;
    StTrackConstIterator iter;
    
    for (iter = c.begin(); iter != c.end(); iter++) {
	track = *iter;
	const StVecPtrTpcHit &hits = track->tpcHits();
	if (find(hits.begin(), hits.end(), this) != hits.end())
	    result.push_back(track);
    }
    return result;
}
