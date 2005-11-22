/***************************************************************************
 *
 * $Id: StMcFstHit.cc,v 2.7 2005/11/22 21:44:51 fisyak Exp $
 * $Log: StMcFstHit.cc,v $
 * Revision 2.7  2005/11/22 21:44:51  fisyak
 * Add compress Print for McEvent, add Ssd collections
 *
 * Revision 2.6  2005/09/29 01:01:10  calderon
 * Fixed bugs in printing event and hit information.
 * Format operator<< for various classes.
 *
 * Revision 2.5  2005/09/28 21:30:14  fisyak
 * Persistent StMcEvent
 *
 * Revision 2.4  2005/07/19 20:07:34  calderon
 * Addition of default constructor, including base class StMcHit constructor.
 * Bracket calls to StMemoryPool inside #ifdef.
 *
 * Revision 2.3  2005/05/13 14:49:00  potekhin
 * Killed the debug cout, improved formatting, removed
 * unnecessary assignment operators and the previous
 * commented out dead code.
 *
 * Revision 2.2  2005/05/12 22:48:38  potekhin
 * Layer, plane and module accessors from Mirko
 *
 * Revision 2.1  2005/04/18 20:11:33  calderon
 * Addition of Fgt and Fst files.  Modified other files to accomodate changes.
 *
 *
 **************************************************************************/
#include "StThreeVectorF.hh"

#include "StMcFstHit.hh"
#include "tables/St_g2t_fst_hit_Table.h" 

static const char rcsid[] = "$Id: StMcFstHit.cc,v 2.7 2005/11/22 21:44:51 fisyak Exp $";

ClassImp(StMcFstHit)
    
#ifdef POOL
//------------------------------------------
StMemoryPool StMcFstHit::mPool(sizeof(StMcFstHit));
//------------------------------------------
#endif
StMcFstHit::StMcFstHit(const StThreeVectorF& x,const StThreeVectorF& p,
			 const float de, const float ds, const long key,
			 const long id,
			 StMcTrack* parent)  : StMcHit(x, p, de, ds, key, id, parent)
{ /* noop */ }
//------------------------------------------
StMcFstHit::StMcFstHit(g2t_fst_hit_st* pt)
  : StMcHit(StThreeVectorF(pt->x[0], pt->x[1], pt->x[2]),
	    StThreeVectorF(pt->p[0], pt->p[1], pt->p[2]),
	    pt->de,
	    pt->ds,
	    pt->id,
	    pt->volume_id,
	    0)
{/* noop */ }
//------------------------------------------
StMcFstHit::~StMcFstHit() {/* noop */ }
//------------------------------------------
ostream&  operator<<(ostream& os, const StMcFstHit& h) {
    os << "FstHit" << endl;
    os << *((StMcHit *) &h);
    os << "Layer           : " << h.layer() << endl;
  return os;
}

//------------------------------------------
unsigned long StMcFstHit::layer() const {    
  unsigned long iLayer =(mVolumeId/100)%10;
  return iLayer;
}
//------------------------------------------
unsigned long StMcFstHit::plane() const {    
  unsigned long iPlane = (mVolumeId/100)%10;
  return iPlane;
}
//------------------------------------------
unsigned long StMcFstHit::module() const {    
  unsigned long imodule = (mVolumeId/100)%10;

#if 0 
  if (0 < iModule && iModule <=11) { iLayer = 1; }
  else if (iModule <=30)           { iLayer = 2; }
  else if (iModule <=57)           { iLayer = 3; }
  else  {
    cout << "StMcFstHit::layer() -E- volumeId not known!" << endl;
    iLayer = 10000; 
  }
#endif 

  return imodule;
}
//------------------------------------------
unsigned long StMcFstHit::ladder() const {
  unsigned long iLadder = (mVolumeId/1000000)%10;

#if 0
  if (0 < iModule && iModule <=11) { iLadder = iModule; }
  else if (iModule <=30)           { iLadder = iModule - 11; }
  else if (iModule <=57)           { iLadder = iModule - 30; }
  else {
    cout << "StMcFstHit::ladder() -E- volumeId not known!" << endl;
    iLadder = 10000; 
  }
#endif  

  return iLadder;
}
//________________________________________________________________________________
void StMcFstHit::Print(Option_t *option) const {
  cout << "FstHit\t"; 
  StMcHit::Print();
  cout  << "\tLayer: " << layer();  
}
