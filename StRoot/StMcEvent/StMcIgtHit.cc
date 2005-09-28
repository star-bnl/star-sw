/*!
 * \class  StMcIgtHit
 * \brief  Monte Carlo Hit class for the GEM Tracker.
 * The Igt hit class provides access to the layer,
 * wafer number (1-7, 1-10,1-13 for layers 1,2,3 respectively),  
 * and the layer side (1=inner, 2=outer).
 * It also gives access to the usual
 * StMcHit information.  The volume Id is used for the decoding of the
 * layer, wefer, side information.
 * \author Gerrit van Nieuwenhuizen, Manuel Calderon de la Barca Sanchez
 * \date   July 2005
 *
 ***************************************************************************
 *
 * $Id: StMcIgtHit.cc,v 2.2 2005/09/28 21:30:15 fisyak Exp $
 *
 ***************************************************************************
 * $Log: StMcIgtHit.cc,v $
 * Revision 2.2  2005/09/28 21:30:15  fisyak
 * Persistent StMcEvent
 *
 * Revision 2.1  2005/07/07 18:20:49  calderon
 * Added support for IGT detector.
 *
 *
 **************************************************************************/
#include "StThreeVectorF.hh"

#include "StMcIgtHit.hh"
#include "tables/St_g2t_igt_hit_Table.h" 

static const char rcsid[] = "$Id: StMcIgtHit.cc,v 2.2 2005/09/28 21:30:15 fisyak Exp $";

StMemoryPool StMcIgtHit::mPool(sizeof(StMcIgtHit));

ClassImp(StMcIgtHit);

StMcIgtHit::StMcIgtHit(const StThreeVectorF& x,const StThreeVectorF& p,
			 const float de, const float ds, const long key,
			 const long id,
			 StMcTrack* parent)  : StMcHit(x, p, de, ds, key, id, parent)
{ /* noop */ }

StMcIgtHit::StMcIgtHit(g2t_igt_hit_st* pt)
: StMcHit(StThreeVectorF(pt->x[0], pt->x[1], pt->x[2]),
	  StThreeVectorF(pt->p[0], pt->p[1], pt->p[2]),
	  pt->de,
	  pt->ds,
	  pt->id,
	  pt->volume_id,
	  0)
{/* noop */ }

StMcIgtHit::~StMcIgtHit() {/* noop */ }

ostream&  operator<<(ostream& os, const StMcIgtHit& h)
{
  os << "IgtHit\t" << *((StMcHit *) &h)
     << "\tLayer:" << h.layer();
  return os;
}

unsigned long
StMcIgtHit::layer() const
{    
  unsigned long iModule = mVolumeId/1000000;
  unsigned long iLayer = 0;
  if (0 < iModule && iModule <=304)
    {
      iLayer = 1;
    }
  else if (iModule <=928)
    {
      iLayer = 2;
    }
  else 
    {
      cout << "StMcIgtHit::layer() -E- volumeId not known!" << endl;
      iLayer = 10000; 
    }
  
  return iLayer;
}

unsigned long
StMcIgtHit::ladder() const
{
  unsigned long iModule = mVolumeId/1000000;
  unsigned long iLadder = 0;
  if (0 < iModule && iModule <=928)
    {
      //iLadder = iModule;
      iLadder = 1;
    }
  else 
    {
      cout << "StMcIgtHit::ladder() -E- volumeId not known!" << endl;
      iLadder = 1; 
    }
  
  return iLadder;
}
