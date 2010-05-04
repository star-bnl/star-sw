/***************************************************************************
 *
 * $Id: StMcVertexC.cxx,v 2.1 2010/05/04 23:58:43 fine Exp $
 *
 **************************************************************************/
#include <algorithm>
#ifndef ST_NO_NAMESPACES
using std::find;
#endif

#include "StMcVertexC.h"
#include "StMcTrack.hh"

static const char rcsid[] = "$Id: StMcVertexC.cxx,v 2.1 2010/05/04 23:58:43 fine Exp $";

int StMcVertexC::operator==(const StMcVertexC& v) const
{
    return (geantProcess() == v.geantProcess() &&
            position()     == v.position()     &&
            tof()          == v.tof()          &&
            key()          == v.key()
          ) ;
}

#if 0

StMcVertexC::~StMcVertexC()
{
    mDaughters.clear();  //Not owner, so we don't have to delete.
}


int StMcVertexC::operator==(const StMcVertexC& v) const
{
    return (mGeantProcess == v.mGeantProcess &&
	    mPosition     == v.mPosition     &&
	    mTof          == v.mTof          &&
	    mKey          == v.mKey);
}

int StMcVertexC::operator!=(const StMcVertexC& v) const
{
    return !(v == *this);
}

ostream&  operator<<(ostream& os, const StMcVertexC& v)
{
    os << "Position      : " << Form("%8.3f%8.3f%8.3f",v.position().x(),v.position().y(),v.position().z()) << endl;
    os << "Geant Volume  : " << v.geantVolume() << endl;
    os << "Time of Flight: " << v.tof() << endl;
    os << "Geant Process : " << v.geantProcess() << endl;
    Int_t nDaughters = v.numberOfDaughters();
    os << "N. Daughters  : " << nDaughters;
    //for (int j = 0; j < nDaughters; j++) {
    //    os << "\t" << v.daughter(j)->key();
    //}
  
  return os;
}


void StMcVertexC::setParent(StMcTrack* val) {  mParent = val; }         

void StMcVertexC::addDaughter(StMcTrack* val) { mDaughters.push_back(val); }  

void StMcVertexC::setGeantVolume(const Char_t *val) { mGeantVolume = val; } 

void StMcVertexC::setTof(float val) { mTof = val; }

void StMcVertexC::setGeantProcess(int val) { mGeantProcess = val; }     

void StMcVertexC::removeDaughter(StMcTrack* trk) {
    StMcTrackIterator iter = find(mDaughters.begin(), mDaughters.end(), trk);
  if (iter != mDaughters.end()) mDaughters.erase(iter);
}
//________________________________________________________________________________
void StMcVertexC::Print(Option_t *option) const {
  cout << "StMcVertexC: pos:"  << Form("%8.3f%8.3f%8.3f",position().x(),position().y(),position().z())
       << " Volume: " << geantVolume()
       << " Time of Flight(ns): " << 1.e9*tof()
       << " Process: " << geantProcess();
  Int_t nDaughters = numberOfDaughters();
  cout << " N.Daughters: " << nDaughters;
  for (int j = 0; j < nDaughters; j++) {
    cout << "\t" << daughter(j)->key();
  }
  cout << endl;
}
#endif
