// $Id: StMcCalorimeterHit.cc,v 2.2 2000/06/06 02:58:40 calderon Exp $
//
// $Log: StMcCalorimeterHit.cc,v $
// Revision 2.2  2000/06/06 02:58:40  calderon
// Introduction of Calorimeter classes.  Modified several classes
// accordingly.
//
// Revision 2.1  2000/05/05 14:54:13  calderon
// Initial revision
//
//                                                                           

#include "StMcCalorimeterHit.hh"
#include "StMcTrack.hh"
#include "StParticleDefinition.hh"

// static const char rcsid[] = "$Id: StMcCalorimeterHit.cc,v 2.2 2000/06/06 02:58:40 calderon Exp $";

//StMemoryPool StMcCalorimeterHit::mPool(sizeof(StMcCalorimeterHit));

StMcCalorimeterHit::StMcCalorimeterHit():mModule(0),mEta(0),mSub(0),mdE(0),mParentTrack(0)
{ /* noop */   }

StMcCalorimeterHit::StMcCalorimeterHit(int m,int e,int s,float de)
    :mModule(m),mEta(e),mSub(s),mdE(de),mParentTrack(0)
{ /* noop */ }

StMcCalorimeterHit::StMcCalorimeterHit(int m,int e,int s,float de,StMcTrack* parent)
    :mModule(m),mEta(e),mSub(s),mdE(de),mParentTrack(parent)
{ /* noop */ }

StMcCalorimeterHit::~StMcCalorimeterHit() { /* noop */ }
    
int StMcCalorimeterHit::operator==(const StMcCalorimeterHit& h) const
{
  // Hits are from the same particle and the same tower(cell)
    return h.mModule == mModule && h.mEta == mEta && 
           h.mSub == mSub && h.mParentTrack == mParentTrack;
}

int StMcCalorimeterHit::operator!=(const StMcCalorimeterHit& h) const
{
    return !(*this == h);  // use operator==()
}

void StMcCalorimeterHit::operator+=(const StMcCalorimeterHit& h)
{
    if(*this == h) mdE += h.dE(); // use operator==()
}

bool StMcCalorimeterHit::sameCell(const StMcCalorimeterHit& h) const
{
  // Hits are from the same cell(tower)
  // For transition from MC hits to raw hits
    return h.mModule == mModule && h.mEta == mEta && h.mSub == mSub;
}

void StMcCalorimeterHit::setModule(int val) { mModule = val; }

void StMcCalorimeterHit::setEta(int val)    { mEta = val; }

void StMcCalorimeterHit::setSub(int val)    { mSub = val; }

void StMcCalorimeterHit::setdE(float val)   { mdE = val; }

void StMcCalorimeterHit::setParentTrack(StMcTrack* val) { mParentTrack = val; }
    
ostream& operator<<(ostream& os, const StMcCalorimeterHit & h)
{
    os << " m: " << h.module();
    os << " e: " << h.eta();
    os << " s: " << h.sub();
    os << " dE: " << h.dE();
    if(h.parentTrack()) {
      StMcTrack* t=h.parentTrack();
      if(t->particleDefinition()){
        os << " | g2t key  : " << t->key();
        os << " Name: "<<t->particleDefinition()->name().c_str();
      }
    }
    else os <<" Parent track undefined ";
    os <<endl;
    return os;
}
