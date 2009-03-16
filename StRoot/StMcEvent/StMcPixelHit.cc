/***************************************************************************
 *
 * $Id: StMcPixelHit.cc,v 2.9 2009/02/06 15:56:46 fisyak Exp $
 * $Log: StMcPixelHit.cc,v $
 * Revision 2.9  2009/02/06 15:56:46  fisyak
 * Jonathan: decoding for upgr15 geometry
 *
 * Revision 2.8  2006/11/17 16:54:58  didenko
 * fixes from Willie for upgr05
 *
 * Revision 2.7  2005/11/22 21:44:52  fisyak
 * Add compress Print for McEvent, add Ssd collections
 *
 * Revision 2.6  2005/09/29 01:01:10  calderon
 * Fixed bugs in printing event and hit information.
 * Format operator<< for various classes.
 *
 * Revision 2.5  2005/09/28 21:30:15  fisyak
 * Persistent StMcEvent
 *
 * Revision 2.4  2005/01/27 23:40:47  calderon
 * Adding persistency to StMcEvent as a step for Virtual MonteCarlo.
 *
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

static const char rcsid[] = "$Id: StMcPixelHit.cc,v 2.9 2009/02/06 15:56:46 fisyak Exp $";

#ifdef POOL
StMemoryPool StMcPixelHit::mPool(sizeof(StMcPixelHit));
#endif
ClassImp(StMcPixelHit);

StMcPixelHit::StMcPixelHit() : StMcHit(StThreeVectorF(0, 0, 0),
				       StThreeVectorF(0, 0, 0),
				       0, 0, 0, 0, 0) {}
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
    os << "PixelHit" << endl;
    os << *((StMcHit *) &h);
    os << "Layer           : " << h.layer() << endl;
  return os;
}

unsigned long
StMcPixelHit::layer() const
{
  unsigned long iLayer=mVolumeId/1000000;
  return iLayer;
}

unsigned long
StMcPixelHit::ladder() const
{
  unsigned long iLadder = (mVolumeId%1000000)/10000;
  return iLadder;
}
//________________________________________________________________________________
void StMcPixelHit::Print(Option_t *option) const {
  cout << "PixelHit\t"; 
  StMcHit::Print();
  cout  << "\tLayer: " << layer();  
}
