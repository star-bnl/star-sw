// TpcHitVecUtilities.cxx
// M.L. Miller
// 6/00

#include "TpcHitVecUtilities.h"
#include "TreeEntryClasses.h"
#include "TNtuple.h"
// StEvent
#include "StEventTypes.h"
#include "StThreeVectorD.hh"
// STL
#include <algorithm>
using std::sort;
using std::find_if;

//constructor-------------------------------------------
TpcHitVecUtilities::TpcHitVecUtilities() {}

TpcHitVecUtilities::TpcHitVecUtilities(StTrack* track, int tnum)
{
    m_TrackNumber = tnum;
    m_StTrack = track;
    findHits();
}

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

void TpcHitVecUtilities::setTrackNumber(int num)
{
    m_TrackNumber = num;
    return;
}

const vector<StTpcHit*>& TpcHitVecUtilities::tpcHitVec() const
{
    return m_tpcHitVec;
}

//STL utilities--------------------------------------------------------------
bool TpcHitVecUtilities::findPadrow(unsigned int padrow)
{
    padrowEqual mpe;
    mpe.m_Padrow = padrow;
    bool found = false;
    vector<StTpcHit*>::iterator first = m_tpcHitVec.begin();
    vector<StTpcHit*>::iterator last = m_tpcHitVec.end();
    vector<StTpcHit*>::iterator where;
    where = find_if(first, last, mpe);
    if (where != m_tpcHitVec.end()) {found = true;}
    
    return found;
}

int TpcHitVecUtilities::findPadrowMaxDifference()
{
    int diff = 0;
    for (vector<StTpcHit*>::iterator it1 = m_tpcHitVec.begin(); it1 != m_tpcHitVec.end(); it1++) {
	vector<StTpcHit*>::iterator it2 = it1;
	if (it1 != m_tpcHitVec.end()) {
	    it2++;
	    if (it2 != m_tpcHitVec.end()){
		int val = padrowDifference((*it1), (*it2) );
		if (val > diff) {diff = val;} }  
	}
    }
    return diff-1;
}

//Print tpcHitVec to screen (debugging)
void TpcHitVecUtilities::printTpcHitVec()
{
    for (vector<StTpcHit*>::iterator it1 = m_tpcHitVec.begin(); it1 != m_tpcHitVec.end(); it1++) {
	cout <<(*it1)->sector()<<"\t"<<(*it1)->padrow()<<"\t"<<(*it1)->charge()<<"\t"<<endl;
    }
    return;
}
    

void TpcHitVecUtilities::printTpcHitVecCharge()
{
    for (vector<StTpcHit*>::iterator it1 = m_tpcHitVec.begin(); it1 != m_tpcHitVec.end(); it1++) {
	cout <<(*it1)->charge()<<endl;
    }
    return;
}

void TpcHitVecUtilities::printTpcHitVecPadrow()
{
    for (vector<StTpcHit*>::iterator it1 = m_tpcHitVec.begin(); it1 != m_tpcHitVec.end(); it1++) {
	cout <<(*it1)->padrow()<<endl;
    }
    return;
}

//Sort the TpcHitVec in ascending order w.r.t. padrow
void TpcHitVecUtilities::sortTpcHitVecPadRow()
{
    sort(m_tpcHitVec.begin(), m_tpcHitVec.end(), padrowLessThan());
    return;
}

//Sort the TpcHitVec in ascending order w.r.t. charge
void TpcHitVecUtilities::sortTpcHitVecCharge()
{
    sort(m_tpcHitVec.begin(), m_tpcHitVec.end(), chargeLessThan());
    return;
}

//Define Padrow Difference for TpcHitVec
int TpcHitVecUtilities::padrowDifference(StTpcHit* hit1, StTpcHit* hit2)
{
    return (hit2->padrow() - hit1->padrow());
}

//Define Padrow Equality (Unary).  Compare to a datamember
bool padrowEqual::operator() (const StTpcHit* hit) const
{
    return (hit->padrow() == m_Padrow);
}

//Define Charge Comparison.  Return true if a.charge() < b.charge()
bool chargeLessThan::operator() (const StTpcHit* hit1, const StTpcHit* hit2) const
{
    return ( hit1->charge() < hit2->charge() );
}

//Define Padrow Comparison.  Return true if a.padrow() < b.padrow()
bool padrowLessThan::operator() (const StTpcHit* hit1, const StTpcHit* hit2) const
{
    return ( hit1->padrow() < hit2->padrow() );
}

