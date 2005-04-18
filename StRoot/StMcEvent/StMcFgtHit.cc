/***************************************************************************
 *
 * $Id: StMcFgtHit.cc,v 2.1 2005/04/18 20:11:33 calderon Exp $
 * $Log: StMcFgtHit.cc,v $
 * Revision 2.1  2005/04/18 20:11:33  calderon
 * Addition of Fgt and Fst files.  Modified other files to accomodate changes.
 *
 *
 **************************************************************************/
#include "StThreeVectorF.hh"

#include "StMcFgtHit.hh"
#include "tables/St_g2t_fgt_hit_Table.h" 

static const char rcsid[] = "$Id: StMcFgtHit.cc,v 2.1 2005/04/18 20:11:33 calderon Exp $";

ClassImp(StMcFgtHit)

StMemoryPool StMcFgtHit::mPool(sizeof(StMcFgtHit));

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
    os << "Position      : " << h.position() << endl; 
    os << "Local Momentum: " << h.localMomentum() << endl; 
    os << "Layer         : " << h.layer()    << endl;
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
