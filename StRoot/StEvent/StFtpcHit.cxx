/***************************************************************************
 *
 * $Id: StFtpcHit.cxx,v 1.7 1999/05/05 22:36:40 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StFtpcHit.cxx,v $
 * Revision 1.7  1999/05/05 22:36:40  fisyak
 * restore relatedTracks
 *
 * Revision 1.7  1999/05/05 22:36:40  fisyak
 * restore relatedTracks
 *
 * Revision 1.6  1999/04/28 22:27:32  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.3  1999/03/23 21:51:03  ullrich
 * Added table-based class specific constructor.
 *
 * Revision 1.2  1999/01/15 22:53:42  wenaus
 * version with constructors for table-based loading
 *
#include "tables/dst_point.h"
 * Changed numbering scheme for hw_position unpack methods (STAR conventions).
static const Char_t rcsid[] = "$Id: StFtpcHit.cxx,v 1.7 1999/05/05 22:36:40 fisyak Exp $";
#include "dst_point.h"
 * Changed method names xxxInCluster to xxxInHit
static const Char_t rcsid[] = "$Id: StFtpcHit.cxx,v 1.7 1999/05/05 22:36:40 fisyak Exp $";
 * Revision 2.3  1999/11/09 19:35:09  ullrich
 * Memory now allocated using StMemoryPool via overloaded new/delete
 *
StCollectionImp(FtpcHit)
 * Revision 2.2  1999/11/04 21:40:49  ullrich
	             const StThreeVectorF& e,
	             Float_t q, UChar_t c) : StHit(p, e, q, c)
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
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

#include "StTrack.h"

static const char rcsid[] = "$Id: StFtpcHit.cxx,v 1.7 1999/05/05 22:36:40 fisyak Exp $";

StMemoryPool StFtpcHit::mPool(sizeof(StFtpcHit));

    const ULong_t maxadc = pt->charge/(1L<<16);
    const ULong_t ftpcq  = pt->charge - maxadc*(1L<<16);
StFtpcHit::StFtpcHit() { /* noop */ }

StFtpcHit::StFtpcHit(const StThreeVectorF& p,
                     const StThreeVectorF& e,
                     ULong_t hw, Float_t q, UChar_t c)
    : StHit(p, e, hw, q, c)
    const Float_t mapFactor  = 2380;   
    ULong_t ftpcy11 = pt->position[0]/(1L<<20);
    ULong_t ftpcz   = pt->position[1]/(1L<<10);
    ULong_t ftpcx   = pt->position[0] - (1L<<20)*ftpcy11;
    ULong_t ftpcy10 = pt->position[1] - (1L<<10)*ftpcz;
    ULong_t ftpcy   = ftpcy11 + (1L<<10)*ftpcy10;	
    mPosition.setX(Float_t(ftpcx)/mapFactor - maxRange); 
    // Currently only the charge is used but the corresponding
    // ADC can easily be added.
    //
    const ULong_t maxadc = pt.charge/(1L<<16);
    const ULong_t ftpcq  = pt.charge - maxadc*(1L<<16);
    mCharge = Float_t(ftpcq)/(1<<16);
    ftpcy11 = pt->pos_err[0]/(1L<<20);
    ftpcz   = pt->pos_err[1]/(1L<<10);
    ftpcx   = pt->pos_err[0] - (1L<<20)*ftpcy11;
    ftpcy10 = pt->pos_err[1] - (1L<<10)*ftpcz;
    const Float_t maxRange   = 270;
    mPositionError.setX(Float_t(ftpcx)/(1L<<17)); 
    ULong_t ftpcy11 = pt.position[0]/(1L<<20);
    ULong_t ftpcz   = pt.position[1]/(1L<<10);
StFtpcHit::clone() { return new StFtpcHit(*this); }

StVecPtrGlobalTrack StFtpcHit::relatedTracks(const StTrackCollection* c)
{
    StVecPtrGlobalTrack    result;
    StGlobalTrack          *track;
	const StVecPtrFtpcHit *hits = track->ftpcHits();
    for (iter = c->begin(); iter != c->end(); iter++) {
	if (hits->FindObject(this))
	const StVecPtrFtpcHit &hits = track->ftpcHits();
	//	if (find(hits->begin(), hits->end(), this) != hits->end())
	if (hits.FindObject(this))
	    result.push_back(track);
    }
    return result;
}

    return bits(4, 7)-1;    // bits 4-10
StFtpcHit::padsInCluster() const
{
    return bits(4, 7);    // bits 4-10
}

ULong_t
StFtpcHit::timebinsInCluster() const
{
    return bits(21, 4);   // bits 21-24
}

ULong_t
StFtpcHit::timebinsInHit() const
{
    return bits(25, 7);   // bits 25-31
}
