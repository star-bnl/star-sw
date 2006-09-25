/* Initial version of HPD Mc Hit code
 **************************************************************************/
#include "StThreeVectorF.hh"

#include "StMcHpdHit.hh"
#include "tables/St_g2t_hpd_hit_Table.h" 

static const char rcsid[] = "";
#ifdef POOL
StMemoryPool StMcHpdHit::mPool(sizeof(StMcHpdHit));
#endif
ClassImp(StMcHpdHit);

StMcHpdHit::StMcHpdHit(const StThreeVectorF& x,const StThreeVectorF& p,
			 const float de, const float ds, const long key,
			 const long id,
			 StMcTrack* parent)  : StMcHit(x, p, de, ds, key, id, parent)
{ /* noop */ }

StMcHpdHit::StMcHpdHit(g2t_hpd_hit_st* pt)
: StMcHit(StThreeVectorF(pt->x[0], pt->x[1], pt->x[2]),
	  StThreeVectorF(pt->p[0], pt->p[1], pt->p[2]),
	  pt->de,
	  pt->ds,
	  pt->id,
	  pt->volume_id,
	  0)
{/* noop */ }

StMcHpdHit::~StMcHpdHit() {/* noop */ }

ostream&  operator<<(ostream& os, const StMcHpdHit& h)
{
    os << "HpdHit" << endl;
    os << *((StMcHit *) &h);
    os << "Layer           : " << h.layer() << endl;
  return os;
}

unsigned long
StMcHpdHit::layer() const
{    
  
  return 1;
}

unsigned long
StMcHpdHit::ladder() const
{cout<<"Volume Id  "<<mVolumeId<<endl;
  unsigned long iModule = mVolumeId/1000000;
  unsigned long iLadder = 0;
  if (0 < iModule && iModule <=48){
    iLadder = iModule;
  }
  else 
    {
      cout << "StMcHpdHit::ladder() -E- volumeId not known!" << endl;
      iLadder = 10000; 
    }
  
  return iLadder;
}
//________________________________________________________________________________
void StMcHpdHit::Print(Option_t *option) const {
  cout << "HpdHit\t"; 
  StMcHit::Print();
  cout  << "\tLayer: " << layer();  
}
