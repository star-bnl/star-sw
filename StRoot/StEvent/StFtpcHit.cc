/***************************************************************************
 *
 * $Id: StFtpcHit.cc,v 1.3 1999/03/23 21:51:03 ullrich Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StFtpcHit.cc,v $
 * Revision 1.3  1999/03/23 21:51:03  ullrich
 * Added table-based class specific constructor.
 *
 * Revision 1.2  1999/01/15 22:53:42  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#include "StEvent/StFtpcHit.hh"
#include "StEvent/StGlobalTrack.hh"
#include "StEvent/StTrackCollection.hh"
#include "tables/dst_point.h"

static const char rcsid[] = "$Id: StFtpcHit.cc,v 1.3 1999/03/23 21:51:03 ullrich Exp $";

StFtpcHit::StFtpcHit(const StThreeVector<float>& p,
	             const StThreeVector<float>& e,
	             float q, unsigned char c) : StHit(p, e, q, c)
{ /* noop */ }

StFtpcHit::StFtpcHit(dst_point_st* pt)
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
    // The charge is decoded together with the max. ADC value.
    // Currently only the charge is used but the corresponding
    // ADC can easily be added.
    //
    const unsigned long maxadc = pt->charge/(1L<<16);
    const unsigned long ftpcq  = pt->charge - maxadc*(1L<<16);
    mCharge = float(ftpcq)/(1<<16);

    //
    // Unpack position in xyz
    //
    const float maxRange   = 270;
    const float mapFactor  = 2380;   
    unsigned long ftpcy11 = pt->position[0]/(1L<<20);
    unsigned long ftpcz   = pt->position[1]/(1L<<10);
    unsigned long ftpcx   = pt->position[0] - (1L<<20)*ftpcy11;
    unsigned long ftpcy10 = pt->position[1] - (1L<<10)*ftpcz;
    unsigned long ftpcy   = ftpcy11 + (1L<<10)*ftpcy10;	
    mPosition.setX(float(ftpcx)/mapFactor - maxRange); 
    mPosition.setY(float(ftpcy)/mapFactor - maxRange);
    mPosition.setZ(float(ftpcz)/mapFactor - maxRange);
    
    //
    // Unpack error on position in xyz
    //
    ftpcy11 = pt->pos_err[0]/(1L<<20);
    ftpcz   = pt->pos_err[1]/(1L<<10);
    ftpcx   = pt->pos_err[0] - (1L<<20)*ftpcy11;
    ftpcy10 = pt->pos_err[1] - (1L<<10)*ftpcz;
    ftpcy   = ftpcy11 + (1L<<10)*ftpcy10;
    mPositionError.setX(float(ftpcx)/(1L<<17)); 
    mPositionError.setY(float(ftpcy)/(1L<<17));
    mPositionError.setZ(float(ftpcz)/(1L<<17));
}

StVecPtrGlobalTrack StFtpcHit::relatedTracks(const StTrackCollection& c)
{
    StVecPtrGlobalTrack    result;
    StGlobalTrack          *track;
    StTrackConstIterator   iter;
    
    for (iter = c.begin(); iter != c.end(); iter++) {
	track = *iter;
	const StVecPtrFtpcHit &hits = track->ftpcHits();
	if (find(hits.begin(), hits.end(), this) != hits.end())
	    result.push_back(track);
    }
    return result;
}
