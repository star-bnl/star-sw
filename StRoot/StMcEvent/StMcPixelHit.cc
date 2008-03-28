/***************************************************************************
 *
 * $Id: StMcPixelHit.cc,v 2.8 2006/11/17 16:54:58 didenko Exp $
 * $Log: StMcPixelHit.cc,v $
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

static const char rcsid[] = "$Id: StMcPixelHit.cc,v 2.8 2006/11/17 16:54:58 didenko Exp $";

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
  // 3 modules of 11 ladders each; 8 outer and 3 inner ladder per module
  // layer 1 : ladder 1 -  9
  // layer 2 : ladder 1 - 24
  unsigned long iLadder = (mVolumeId%1000000)/10000;
  unsigned long iLayer = 0;

  //inner ladders now declared first (numbered 1-3 in sector)
  if (iLadder<4)
    {
      iLayer = 1;
    }
  //otherwise, outer layer
  else
    {
      iLayer = 2;
    }

  return iLayer;
}

unsigned long
StMcPixelHit::ladder() const
{
  // 3 modules of 11 ladders each; 8 outer and 3 inner ladder per module
  // layer 1 : ladder 1 -  9
  // layer 2 : ladder 1 - 24
  unsigned long iModule = mVolumeId/1000000;
  unsigned long iLadder = (mVolumeId%1000000)/10000;
  //cout<<"volume id: "<<mVolumeId<<endl;
  //cout<<"iModule: "<<iModule<<endl;
  //cout<<"iLadder: "<<iLadder<<endl;
  if (iLadder>3) // outer: 3*(4-11) to 1-24 
    {
      iLadder=(iModule-1)*8+iLadder-3;
    }
  else
    {
      iLadder = (iModule-1)*3 + (iLadder);
    }
  //cout<<"final iLadder: "<<iLadder<<endl;
  return iLadder;
}
//________________________________________________________________________________
void StMcPixelHit::Print(Option_t *option) const {
  cout << "PixelHit\t"; 
  StMcHit::Print();
  cout  << "\tLayer: " << layer();  
}
