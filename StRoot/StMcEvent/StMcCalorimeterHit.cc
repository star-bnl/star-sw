// $Id: StMcCalorimeterHit.cc,v 2.1 2000/05/05 14:54:13 calderon Exp $
//
// $Log: StMcCalorimeterHit.cc,v $
// Revision 2.1  2000/05/05 14:54:13  calderon
// Initial revision
//
//                                                                           

#include "StMcCalorimeterHit.hh"

// static const char rcsid[] = "$Id: StMcCalorimeterHit.cc,v 2.1 2000/05/05 14:54:13 calderon Exp $";

StMcCalorimeterHit::StMcCalorimeterHit():mModule(0),mEta(0),mSub(0),mdE(0),mParentTrack(0)
{ /* noop */   }

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

void StMcCalorimeterHit::operator+=(const float& de)
{
    mdE += de; // use operator==()
}

void StMcCalorimeterHit::setModule(int val) { mModule = val; }

void StMcCalorimeterHit::setEta(int val)    { mEta = val; }

void StMcCalorimeterHit::setSub(int val)    { mSub = val; }

void StMcCalorimeterHit::setdE(float val)   { mdE = val; }

void StMcCalorimeterHit::setParentTrack(StMcTrack* val) { mParentTrack = val; }
    
ostream& operator<<(ostream& os, const StMcCalorimeterHit & h)
{
    os << "Module: " << h.module();
    os << "Eta: " << h.eta();
    os << "Sub: " << h.sub();
    os << "dE: " << h.dE() << endl;
    return os;
}
