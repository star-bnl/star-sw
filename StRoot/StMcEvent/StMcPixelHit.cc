/***************************************************************************
 *
 * $Id: StMcPixelHit.cc,v 2.2 2003/12/02 21:22:03 calderon Exp $
 * $Log: StMcPixelHit.cc,v $
 * Revision 2.2  2003/12/02 21:22:03  calderon
 * remove unnecessary #include "StMcTrack.hh"
 *
 * Revision 2.1  2003/08/20 18:50:21  calderon
 * Addition of Tof classes and Pixel classes.  Modified track, event, and
 * container code to reflect this.
 * Fix bug in StMcVertex and in clearing of some hit collections.
 *
 *
 **************************************************************************/
#include "StThreeVectorF.hh"

#include "StMcPixelHit.hh"
#include "tables/St_g2t_pix_hit_Table.h" 

static const char rcsid[] = "$Id: StMcPixelHit.cc,v 2.2 2003/12/02 21:22:03 calderon Exp $";

StMemoryPool StMcPixelHit::mPool(sizeof(StMcPixelHit));

StMcPixelHit::StMcPixelHit(const StThreeVectorF& x,const StThreeVectorF& p,
			 const float de, const float ds, const long key,
			 const long id,
			 StMcTrack* parent)  : StMcHit(x, p, de, ds, key, id, parent)
{ /* noop */ }

StMcPixelHit::StMcPixelHit(g2t_pix_hit_st* pt)
: StMcHit(StThreeVectorF(pt->x[0], pt->x[1], pt->x[2]),
	  StThreeVectorF(pt->p[0], pt->p[1], pt->p[2]),
	  pt->de,
	  pt->ds,
	  pt->id,
	  pt->volume_id,
	  0)
{/* noop */ }

StMcPixelHit::~StMcPixelHit() {/* noop */ }

ostream&  operator<<(ostream& os, const StMcPixelHit& h)
{
    os << "Position      : " << h.position() << endl; 
    os << "Local Momentum: " << h.localMomentum() << endl; 
    os << "Layer         : " << h.layer()    << endl;
    return os;
}

unsigned long
StMcPixelHit::layer() const
{
    //volume_id = 101 to 110 are the first PIXEL (1-10 in StEvent)
    //volume_id = 201 to 210 are the second PIXEL (11-20 in StEvent)
    //return (mVolumeId/100 - 1)*10 + mVolumeId%100;
  return mPosition.perp()>3.? 2 : 1; // cludge for the time being. I haven't paid attention to the volume id.
}
