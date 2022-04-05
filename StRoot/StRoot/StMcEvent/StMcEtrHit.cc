/***************************************************************************
 *
 ***************************************************************************/
#include <assert.h>
#include "StMcEtrHit.hh"
#include "tables/St_g2t_etr_hit_Table.h" 

static const char rcsid[] = "$Id: StMcEtrHit.cc,v 2.2 2015/07/22 19:29:17 jwebb Exp $";

ClassImp(StMcEtrHit)

//_____________________________________________________________________________
ostream&  operator<<(ostream& os, const StMcEtrHit& h)
{
    os << "StMcEtrHit" << endl;
    os << *((StMcHit *) &h);
    os << "Layer           : " << h.layer()    << endl;
    os << "Sector          : " << h.sector()   << endl;
    return os;
}
//_____________________________________________________________________________
StMcEtrHit::StMcEtrHit(g2t_etr_hit_st* pt) : 
    StMcHit(StThreeVectorF(pt->x[0], pt->x[1], pt->x[2]),
	    StThreeVectorF(pt->p[0], pt->p[1], pt->p[2]), 
	    pt->de, pt->ds, pt->tof, pt->id, pt->volume_id, 0) {}

//_____________________________________________________________________________
int StMcEtrHit::layer() const
{
  int iLayer; // 3 disk in TRD, iLayer=0, 1, 2

  // volumeId encoded in UPGR16, section+100*layer+10000*sector
  int ilayer = (volumeId()/100)%100; 	// 3 disk in TRD, iLayer=0, 1, 2
  assert (ilayer <=2 || ilayer >=0);
  // NOTE:  Return ilayer (lower-case "l") here instead of the uninitialized
  // iLayer (upper "L").  May indicate a bug in the code.
  return iLayer=ilayer; 
}

 
//_____________________________________________________________________________
int StMcEtrHit::sector() const
{
  // volumeId encoded in UPGR16
  int iSector = volumeId()/10000;	// 12 sector in TRD layer, 0 - 11
  assert(iSector>=0 || iSector<=11);
  return iSector;
}
//________________________________________________________________________________
void StMcEtrHit::Print(Option_t *option) const {
  cout << "EtrHit\t"; 
  StMcHit::Print();
  cout  << "\tLayer: " << layer() 
        <<  "\tSector: " << sector() << endl;;  
}
