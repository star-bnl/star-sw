
// TpcHitVecUtilities.cxx
// M.L. Miller, Yale Software, 8/00

#include "TpcHitVecUtilities.h"
// StEvent
#include "StEventTypes.h"
#include "StThreeVectorD.hh"
// STL
#include <algorithm>
using std::sort;

//constructor-------------------------------------------
TpcHitVecUtilities::TpcHitVecUtilities() {}

TpcHitVecUtilities::~TpcHitVecUtilities() {};

//methods------------------------------------------------
void TpcHitVecUtilities::findHits()
{
    StPtrVecHit vec = m_StTrack->detectorInfo()->hits(kTpcId);
    StPtrVecHitIterator iter;
    StTpcHit* hit;
    
    for (iter = vec.begin(); iter != vec.end(); iter++) {
	if (*iter){
	    hit = dynamic_cast<StTpcHit*>(*iter);
	    if (hit) { m_tpcHitVec.push_back(hit); }
	}
    }
    return;
}

//Calculate meanDca of track to hits in the window [zmin, zmax/
int TpcHitVecUtilities::numberOfHitsInZTrack(double zmin, double zmax) {
  int n=0;
  for (vector<StTpcHit*>::iterator it = m_tpcHitVec.begin(); it != m_tpcHitVec.end(); it++) {
    if ( ((*it)->position().z()>=zmin) && ((*it)->position().z()<=zmax) ) {n++;}
    }
  return n;
}


//Accessors-----------------------------------------------------------------
void TpcHitVecUtilities::clear()
{
    m_tpcHitVec.clear();
    return;
}

void TpcHitVecUtilities::setTrack(StTrack* tck)
{
    m_StTrack = tck;
    return;
}

const vector<StTpcHit*>& TpcHitVecUtilities::tpcHitVec() const
{
    return m_tpcHitVec;
}

//STL utilities--------------------------------------------------------------
void TpcHitVecUtilities::printTpcHitVecPosition()
{
    cout<<"TpcHit Postion"<<endl;
    for (vector<StTpcHit*>::iterator it1 = m_tpcHitVec.begin(); it1 != m_tpcHitVec.end(); it1++) {
	cout <<(*it1)->position()<<endl;
    }
    return;
}

//Sort the TpcHitVec in ascending order w.r.t. z of hit
void TpcHitVecUtilities::sortTpcHitVecZ()
{
    sort(m_tpcHitVec.begin(), m_tpcHitVec.end(), zHitLessThan() );
    return;
}

bool zHitLessThan::operator() (const StTpcHit* hit1, const StTpcHit* hit2) const
{
    return ( hit1->position().z() < hit2->position().z() );
};
