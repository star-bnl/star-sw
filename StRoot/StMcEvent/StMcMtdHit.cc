//
//                                                                           

#include <assert.h>
#include "StMcMtdHit.hh"
#include "StMcTrack.hh"
#include "StParticleDefinition.hh"
#include "tables/St_g2t_mtd_hit_Table.h"


ClassImp(StMcMtdHit);
StMcMtdHit::StMcMtdHit()
  : mBackleg(0),mModule(0),mCell(0),mPathLength(0),mTime(0),mTof(0),mCharge(0)
{/* noop */   }

StMcMtdHit::StMcMtdHit(int t,int m,int c,float de,float pl,float time,float tof,float q)
    :mBackleg(t),mModule(m),mCell(c),mPathLength(pl),mTime(time),mTof(tof),mCharge(q)
{
  mdE=de;
}

StMcMtdHit::StMcMtdHit(int t,int m,int c,float de,float pl,float time,float tof,float q,StThreeVectorF& x,StMcTrack* parent)
    :mBackleg(t),mModule(m),mCell(c),mPathLength(pl),mTime(time),mTof(tof),mCharge(q)
{ 
  mdE=de;
  mPosition = x;
  mParentTrack=parent;
}

StMcMtdHit::StMcMtdHit(g2t_mtd_hit_st* pt)
  : mBackleg(0),mModule(0),mCell(0),mPathLength(pt->ds),mTof(pt->tof),mCharge(0)

{
  mdE=pt->de;
  mPosition = StThreeVectorF(pt->x[0], pt->x[1], pt->x[2]);
//fg	    StThreeVectorF(pt->p[0], pt->p[1], pt->p[2]),
//fg	    pt->de,
//fg	    pt->ds,
//fg	    pt->id,
//fg	    pt->volume_id,
//fg	    0), mTof(pt->tof), mBackleg(0), mModule(0),mCell(0)
}



StMcMtdHit::~StMcMtdHit() {/* noop */ }
    
int StMcMtdHit::operator==(const StMcMtdHit& h) const
{
	//we have some options here -- include 2nd line or no??
  // Hits are from the same particle and the same tower(cell)
    return h.mBackleg == mBackleg && h.mModule == mModule && h.mCell == mCell 
           && h.mPathLength == mPathLength && h.mTime == mTime 
	   && h.mTof == mTof && h.mCharge == mCharge 
	   && h.mPosition == mPosition
	   && ( (h.mParentTrack == mParentTrack) || (h.mParentTrackId == mParentTrackId) );

}

int StMcMtdHit::operator!=(const StMcMtdHit& h) const
{
    return !(*this == h);  // use operator==()
}

void StMcMtdHit::operator+=(const StMcMtdHit& h)
{
	//we have some options here
    //if(*this == h) mdE += h.dE(); // use operator==()
    //if(*this.sameCell(h)) mdE += h.dE(); 
     mdE += h.dE();
     mCharge += h.charge();
}

bool StMcMtdHit::sameCell(const StMcMtdHit& h) const
{
  // Hits are from the same cell(tower)
  // For transition from MC hits to raw hits
    return h.mBackleg == mBackleg && h.mModule == mModule && h.mCell == mCell; 
}

void StMcMtdHit::setBackleg(int val)    { mBackleg = val; }

void StMcMtdHit::setModule(int val) { mModule = val; }

void StMcMtdHit::setCell(int val)    { mCell = val; }

void StMcMtdHit::setPathLength(float val)   { mPathLength = val; }

void StMcMtdHit::setTime(float val)   { mTime = val; }

void StMcMtdHit::setTof(float val)    { mTof = val; }

void StMcMtdHit::setCharge(float val)   { mCharge = val; }

void StMcMtdHit::setParentTrackId(int val) { mParentTrackId = val; }
    
    
ostream& operator<<(ostream& os, const StMcMtdHit & h)
{
  os << "MtdHit\t" 
     << " backleg: " << h.backleg()
     << " mod: " << h.module()
     << " cell: " << h.cell()
     << " dE: " << h.dE()
     << " pathL: " << h.pathLength()
     << " time: " << h.time()
     << " tof (simu): " << h.tof()
     << " charge: " << h.charge()
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
