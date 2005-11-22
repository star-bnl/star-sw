/***************************************************************************
 *
 * $Id: StMcFgtHit.cc,v 2.4 2005/11/22 21:44:51 fisyak Exp $
 * $Log: StMcFgtHit.cc,v $
 * Revision 2.4  2005/11/22 21:44:51  fisyak
 * Add compress Print for McEvent, add Ssd collections
 *
 * Revision 2.3  2005/09/29 01:01:10  calderon
 * Fixed bugs in printing event and hit information.
 * Format operator<< for various classes.
 *
 * Revision 2.2  2005/07/19 20:07:34  calderon
 * Addition of default constructor, including base class StMcHit constructor.
 * Bracket calls to StMemoryPool inside #ifdef.
 *
 * Revision 2.1  2005/04/18 20:11:33  calderon
 * Addition of Fgt and Fst files.  Modified other files to accomodate changes.
 *
 *
 **************************************************************************/
#include "StThreeVectorF.hh"

#include "StMcFgtHit.hh"
#include "tables/St_g2t_fgt_hit_Table.h" 

static const char rcsid[] = "$Id: StMcFgtHit.cc,v 2.4 2005/11/22 21:44:51 fisyak Exp $";

ClassImp(StMcFgtHit)

#ifdef POOL
StMemoryPool StMcFgtHit::mPool(sizeof(StMcFgtHit));
#endif

StMcFgtHit::StMcFgtHit(const StThreeVectorF& x,const StThreeVectorF& p,
			 const float de, const float ds, const long key,
			 const long id,
			 StMcTrack* parent)  : StMcHit(x, p, de, ds, key, id, parent)
{ /* noop */ }

StMcFgtHit::StMcFgtHit(g2t_fgt_hit_st* pt)
: StMcHit(StThreeVectorF(pt->x[0], pt->x[1], pt->x[2]),
	  StThreeVectorF(pt->p[0], pt->p[1], pt->p[2]),
	  pt->de,
	  pt->ds,
	  pt->id,
	  pt->volume_id,
	  0)
{/* noop */ }

StMcFgtHit::~StMcFgtHit() {/* noop */ }

ostream&  operator<<(ostream& os, const StMcFgtHit& h)
{
    os << "StMcFgtHit" << endl;
    os << *((StMcHit *) &h);
    os << "Layer           : " << h.layer()    << endl;
    return os;
}

unsigned long
StMcFgtHit::layer() const
{    
  // this function is still a dummy
  //unsigned long iModule = mVolumeId/1000000;
  unsigned long iLayer = 1;
  
  return iLayer;
}

unsigned long
StMcFgtHit::ladder() const
{
  // this function is still a dummy
  //unsigned long iModule = mVolumeId/1000000;
  unsigned long iLadder = 1;
  
  return iLadder;
}
//________________________________________________________________________________
void StMcFgtHit::Print(Option_t *option) const {
  cout << "FgtHit\t"; 
  StMcHit::Print();
  cout  << "\tLayer: " << layer();  
}
