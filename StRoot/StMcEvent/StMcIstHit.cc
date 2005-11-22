/***************************************************************************
 *
 * $Id: StMcIstHit.cc,v 2.6 2005/11/22 21:44:52 fisyak Exp $
 * $Log: StMcIstHit.cc,v $
 * Revision 2.6  2005/11/22 21:44:52  fisyak
 * Add compress Print for McEvent, add Ssd collections
 *
 * Revision 2.5  2005/09/29 01:01:10  calderon
 * Fixed bugs in printing event and hit information.
 * Format operator<< for various classes.
 *
 * Revision 2.4  2005/09/28 21:30:15  fisyak
 * Persistent StMcEvent
 *
 * Revision 2.3  2005/07/19 20:07:34  calderon
 * Addition of default constructor, including base class StMcHit constructor.
 * Bracket calls to StMemoryPool inside #ifdef.
 *
 * Revision 2.2  2005/05/11 20:54:28  calderon
 * Added persistency: ClassImp, ClassDef and inheritance from StObject.
 *
 * Revision 2.1  2004/09/14 05:00:29  calderon
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

#include "StMcIstHit.hh"
#include "tables/St_g2t_ist_hit_Table.h" 

static const char rcsid[] = "$Id: StMcIstHit.cc,v 2.6 2005/11/22 21:44:52 fisyak Exp $";
#ifdef POOL
StMemoryPool StMcIstHit::mPool(sizeof(StMcIstHit));
#endif
ClassImp(StMcIstHit);

StMcIstHit::StMcIstHit(const StThreeVectorF& x,const StThreeVectorF& p,
			 const float de, const float ds, const long key,
			 const long id,
			 StMcTrack* parent)  : StMcHit(x, p, de, ds, key, id, parent)
{ /* noop */ }

StMcIstHit::StMcIstHit(g2t_ist_hit_st* pt)
: StMcHit(StThreeVectorF(pt->x[0], pt->x[1], pt->x[2]),
	  StThreeVectorF(pt->p[0], pt->p[1], pt->p[2]),
	  pt->de,
	  pt->ds,
	  pt->id,
	  pt->volume_id,
	  0)
{/* noop */ }

StMcIstHit::~StMcIstHit() {/* noop */ }

ostream&  operator<<(ostream& os, const StMcIstHit& h)
{
    os << "IstHit" << endl;
    os << *((StMcHit *) &h);
    os << "Layer           : " << h.layer() << endl;
  return os;
}

unsigned long
StMcIstHit::layer() const
{    
  unsigned long iModule = mVolumeId/1000000;
  unsigned long iLayer = 0;
  if (0 < iModule && iModule <=11)
    {
      iLayer = 1;
    }
  else if (iModule <=30)
    {
      iLayer = 2;
    }
  else if (iModule <=57)
    {
      iLayer = 3;
    }
  else 
    {
      cout << "StMcIstHit::layer() -E- volumeId not known!" << endl;
      iLayer = 10000; 
    }
  
  return iLayer;
}

unsigned long
StMcIstHit::ladder() const
{
  unsigned long iModule = mVolumeId/1000000;
  unsigned long iLadder = 0;
  if (0 < iModule && iModule <=11)
    {
      iLadder = iModule;
    }
  else if (iModule <=30)
    {
      iLadder = iModule - 11;
    }
  else if (iModule <=57)
    {
      iLadder = iModule - 30;
    }
  else 
    {
      cout << "StMcIstHit::ladder() -E- volumeId not known!" << endl;
      iLadder = 10000; 
    }
  
  return iLadder;
}
//________________________________________________________________________________
void StMcIstHit::Print(Option_t *option) const {
  cout << "IstHit\t"; 
  StMcHit::Print();
  cout  << "\tLayer: " << layer();  
}
