/***************************************************************************
 *
 * $Id: StMcPixelHit.cc,v 2.3 2004/09/14 05:00:30 calderon Exp $
 * $Log: StMcPixelHit.cc,v $
 * Revision 2.3  2004/09/14 05:00:30  calderon
 * Added support for Ist, Ssd and changes to Pixel, from "El Kai".
 *
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

static const char rcsid[] = "$Id: StMcPixelHit.cc,v 2.3 2004/09/14 05:00:30 calderon Exp $";

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
  // 6 modules of 4 ladders each; 3 outer and 1 inner ladder per module
  // layer 1 : ladder 1 -  6
  // layer 2 : ladder 1 - 18
  unsigned long iLadder = (mVolumeId%1000000)/10000;
  unsigned long iLayer = 0;
  if (iLadder<4)
    {
      iLayer = 2;
    }
  else
    {
      iLayer = 1;
    }

  return iLayer;
}

unsigned long
StMcPixelHit::ladder() const
{
  // 6 modules of 4 ladders each; 3 outer and 1 inner ladder per module
  // layer 1 : ladder 1 -  6
  // layer 2 : ladder 1 - 18
 unsigned long iModule = mVolumeId/1000000;
  unsigned long iLadder = (mVolumeId%1000000)/10000;
  if (iLadder<4)
    {
      iLadder = (iModule-1)*3 + (iLadder);
    }
  else
    {
      iLadder = (iModule);
    }

  return iLadder;
}
