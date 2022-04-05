//
//                                                                           

#include <assert.h>
#include "StMcMtdHit.hh"
#include "StMcTrack.hh"
#include "StParticleDefinition.hh"
#include "tables/St_g2t_mtd_hit_Table.h"


ClassImp(StMcMtdHit);
//___________________________________________________________________________
StMcMtdHit::StMcMtdHit()
{
 mBackleg=0;
 mModule=0;
 mCell=0;
 mPathLength=0;
 mParentTrackId=0;
}
//___________________________________________________________________________
StMcMtdHit::~StMcMtdHit() {/* noop */ }
    
// //___________________________________________________________________________
// int StMcMtdHit::operator==(const StMcMtdHit& h) const
// {
// 	//we have some options here -- include 2nd line or no??
//   // Hits are from the same particle and the same tower(cell)
//     return h.mBackleg == mBackleg && h.mModule == mModule && h.mCell == mCell 
//            && h.mPathLength == mPathLength && h.mTime == mTime 
// 	   && h.mTof == mTof && h.mCharge == mCharge 
// 	   && h.mPosition == mPosition
// 	   && ( (h.mParentTrack == mParentTrack) || (h.mParentTrackId == mParentTrackId) );
// 
// }

//___________________________________________________________________________
int StMcMtdHit::operator!=(const StMcMtdHit& h) const
{
    return !(*this == h);  // use operator==()
}

// //___________________________________________________________________________
// void StMcMtdHit::operator+=(const StMcMtdHit& h)
// {
// 	//we have some options here
//     //if(*this == h) mdE += h.dE(); // use operator==()
//     //if(*this.sameCell(h)) mdE += h.dE(); 
//      mdE += h.dE();
//      mCharge += h.charge();
// }

//___________________________________________________________________________
bool StMcMtdHit::sameCell(const StMcMtdHit& h) const
{
  // Hits are from the same cell(tower)
  // For transition from MC hits to raw hits
    return h.mBackleg == mBackleg && h.mModule == mModule && h.mCell == mCell; 
}

    
ostream& operator<<(ostream& os, const StMcMtdHit & h)
{
  os << "MtdHit\t" 
     << " backleg: " << h.backleg()
     << " mod: " << h.module()
     << " cell: " << h.cell()
     << " dE: " << h.dE()
     << " pathL: " << h.pathLength()
     << " tof (simu): " << h.tof()
     << " position: " << h.position();
    if(h.parentTrack()) {
      StMcTrack* t=h.parentTrack();
      if(t->particleDefinition()){
        os << " | g2t key  : " << t->key()
	   << " Name: "<<(t->particleDefinition()->name()).c_str();
      }
    }
    else os <<" Parent track undefined ";
    return os;
}
//___________________________________________________________________________
StMcMtdHit::StMcMtdHit(g2t_mtd_hit_st* pt): 
    StMcHit(StThreeVectorF(pt->x[0], pt->x[1], pt->x[2]),
	    StThreeVectorF(pt->p[0], pt->p[1], pt->p[2]), 
	    pt->de, pt->ds, pt->tof, pt->id, pt->volume_id, 0)
{
  const static float kMtdPadWidth = 3.8 + 0.6;  	//! Pad Width: 38mm padwidth + 6mm innerspacing
  const static   int kNCell       = 12;     	//! 12 cells per box

  int volume_id = pt->volume_id;
  int ires      = volume_id/100;
  mPathLength   = pt->s_track;
  mModule       = ires%10;
  mBackleg      = ires/10;
  mCell         = Int_t((pt->x[1] + kMtdPadWidth * kNCell/2) / kMtdPadWidth) + 1;
  mParentTrackId = 0;
}        
    
