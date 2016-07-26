//
//                                                                           

#include "StMcBTofHit.hh"
#include "StMcTrack.hh"
#include "StParticleDefinition.hh"


ClassImp(StMcBTofHit);
StMcBTofHit::StMcBTofHit()
  : mTray(0),mModule(0),mCell(0),mdE(0),mPathLength(0),mTime(0),mTof(0),mCharge(0),mPosition(0.,0.,0.),mParentTrack(0),mParentTrackId(0)
{ /* noop */   }

StMcBTofHit::StMcBTofHit(int t,int m,int c,float de,float pl,float time,float tof,float q)
  :mTray(t),mModule(m),mCell(c),mdE(de),mPathLength(pl),mTime(time),mTof(tof),mCharge(q),mPosition(0.,0.,0.),mParentTrack(0),mParentTrackId(0)
{ /* noop */ }

StMcBTofHit::StMcBTofHit(int t,int m,int c,float de,float pl,float time,float tof,float q,StThreeVectorF& x,StMcTrack* parent)
  :mTray(t),mModule(m),mCell(c),mdE(de),mPathLength(pl),mTime(time),mTof(tof),mCharge(q),mPosition(x),mParentTrack(parent),mParentTrackId(0)
{ /* noop */ }

StMcBTofHit::~StMcBTofHit() { /* noop */ }
    
int StMcBTofHit::operator==(const StMcBTofHit& h) const
{
	//we have some options here -- include 2nd line or no??
  // Hits are from the same particle and the same tower(cell)
    return h.mTray == mTray && h.mModule == mModule && h.mCell == mCell 
           && h.mPathLength == mPathLength && h.mTime == mTime 
	   && h.mTof == mTof && h.mCharge == mCharge 
	   && h.mPosition == mPosition
	   && ( (h.mParentTrack == mParentTrack) || (h.mParentTrackId == mParentTrackId) );

}

int StMcBTofHit::operator!=(const StMcBTofHit& h) const
{
    return !(*this == h);  // use operator==()
}

void StMcBTofHit::operator+=(const StMcBTofHit& h)
{
	//we have some options here
    //if(*this == h) mdE += h.dE(); // use operator==()
    //if(*this.sameCell(h)) mdE += h.dE(); 
     mdE += h.dE();
     mCharge += h.charge();
}

bool StMcBTofHit::sameCell(const StMcBTofHit& h) const
{
  // Hits are from the same cell(tower)
  // For transition from MC hits to raw hits
    return h.mTray == mTray && h.mModule == mModule && h.mCell == mCell; 
}

void StMcBTofHit::setTray(int val)    { mTray = val; }

void StMcBTofHit::setModule(int val) { mModule = val; }

void StMcBTofHit::setCell(int val)    { mCell = val; }

void StMcBTofHit::setdE(float val)   { mdE = val; }

void StMcBTofHit::setPathLength(float val)   { mPathLength = val; }

void StMcBTofHit::setTime(float val)   { mTime = val; }

void StMcBTofHit::setTof(float val)    { mTof = val; }

void StMcBTofHit::setCharge(float val)   { mCharge = val; }

void StMcBTofHit::setPosition(StThreeVectorF& val) { mPosition = val; }

void StMcBTofHit::setParentTrack(StMcTrack* val) { mParentTrack = val; }

void StMcBTofHit::setParentTrackId(int val) { mParentTrackId = val; }
    
    
ostream& operator<<(ostream& os, const StMcBTofHit & h)
{
  os << "BTofHit\t" 
     << " tray: " << h.tray()
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
