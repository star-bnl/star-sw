//StiMapUtilities.cxx
//M.L. Miller (Yale Software)
//3/01

//STD
#include <iostream.h>
#include <math.h>

//SCL
#include "StThreeVectorD.hh"
#include "StEventTypes.h"
#include "StGetConfigValue.hh"

//Sti
#include "StiIOBroker.h"
#include "Sti/Base/Messenger.h"
#include "StiHit.h"
#include "StiMapUtilities.h"
#include "StiCompositeTreeNode.h"
#include "StiPlacement.h"
#include "StiDetector.h"
#include "StiTrackNode.h"

//------------------------------ Hit Map Utilities ----------------------------

//Equality defined by same refangle and position
bool HitMapKey::operator==(const HitMapKey& key2) const
{
    //cout <<"HitMapKey::operator==(const HitMapKey&)"<<endl;
    return (this->refangle==key2.refangle && this->position==key2.position);
}

//Return true if key2 < key1.  Order first by refangle, then by position.
bool MapKeyLessThan::operator() (const HitMapKey& key1, const HitMapKey& key2) const
{
    //cout <<"HitMapKey::operator<()"<<endl;
    bool val = false;
    if ( fabs(key1.refangle-key2.refangle) < reftolerance) {
	//Call these equal, check position
	
	if ( fabs(key1.position-key2.position) < postolerance) {
	    //Call these equal
	    val = false;
	}	
	else if (key1.position < key2.position) {val = true;}
	else {val = false;}
    }
    
    else if (key1.refangle < key2.refangle) {val = true;}
    else {val = false;}

    return val;
}

//Order StHits (NOT Sti!) by radius
bool StHitRadiusLessThan::operator() (const StHit* hit1, const StHit* hit2) const
{
    const StThreeVectorD& pos1 = hit1->position();
    const StThreeVectorD& pos2 = hit2->position();
    return (sqrt( pos1.x()*pos1.x() + pos1.y()*pos1.y() + pos1.z()*pos1.z() ) <
	    sqrt( pos2.x()*pos2.x() + pos2.y()*pos2.y() + pos2.z()*pos2.z() ) );
}

bool StiHitIsUsed::operator() (const StiHit* hit) const
{
    //return !hit->isUsed();
    return (hit->timesUsed()==0);
}

//Order StHits (NOT Sti!) by radius
bool StHitRadiusGreaterThan::operator() (const StHit* hit1, const StHit* hit2) const
{
    const StThreeVectorD& pos1 = hit1->position();
    const StThreeVectorD& pos2 = hit2->position();
    return (sqrt( pos1.x()*pos1.x() + pos1.y()*pos1.y() + pos1.z()*pos1.z() ) >
	    sqrt( pos2.x()*pos2.x() + pos2.y()*pos2.y() + pos2.z()*pos2.z() ) );
}
	
//Order by distance along position
bool StidHitLessThan::operator() (const StiHit* lhs, const StiHit* rhs) const
{
    return (lhs->y() < rhs->y()) ? true : false;
}

//Order by distance in z
bool StizHitLessThan::operator() (const StiHit* lhs, const StiHit* rhs) const
{
    return (lhs->z() < rhs->z()) ? true : false;
}

bool StiDetectorNodePositionLessThan::operator() (const StiCompositeTreeNode<StiDetector> * lhs,
						  const StiCompositeTreeNode<StiDetector> * rhs) const
{
    if (lhs->getData()==0 || rhs->getData()==0) {
	*(Messenger::instance(MessageType::kHitMessage)) <<"StiDetectorNodeLessThan::operator(). ERROR:\t";
	*(Messenger::instance(MessageType::kHitMessage)) <<"null data.  Return false"<<endl;
    }
    StiPlacement* lhsp = lhs->getData()->getPlacement();
    StiPlacement* rhsp = rhs->getData()->getPlacement();

    return lhsp->getCenterRadius()<rhsp->getCenterRadius();
}

bool SameStHit::operator()(const StiHit* rhs) const
{
    return (stHit == rhs->stHit() );
};

//----------------------- Detector Map Utilities ------------------------------------

bool NameMapKey::operator==(const NameMapKey& key2) const{
    //return( strcmp(name, key2.name) == 0 );
  return( name ==key2.name );
}

bool NameMapKey::operator<(const NameMapKey& key2) const{
    //return( strcmp(name, key2.name) < 0 );
  return( name < key2.name );
}

StTpcPadrowHitFilter::StTpcPadrowHitFilter()
    : mMinPadrow(0), mMaxPadrow(0)
{
}

void StTpcPadrowHitFilter::getNewState()
{
    StiIOBroker* broker = StiIOBroker::instance();
    mMinPadrow = broker->tphfMinPadrow();
    mMaxPadrow = broker->tphfMaxPadrow();
    //cout <<"StTpcPadrowHitFilter::getNewState()\t"
    //<<"minPadrow: "<<mMinPadrow<<"\tmaxPadrow: "<<mMaxPadrow<<endl;
}

bool StTpcPadrowHitFilter::operator()(const StTpcHit& hit) const
{
    return ( (hit.padrow()>=mMinPadrow) && (hit.padrow()<=mMaxPadrow) );
}

void SetHitUsed::operator()(StiTrackNode& node)
{
    StiHit* hit = node.getHit();
    if(hit!=0) {
	//hit->setUsed(true);
	hit->setTimesUsed(hit->timesUsed()+1);
    }
}

//----------------------- Streamers -------------------------------------------------
ostream& operator<<(ostream& os, const HitMapKey& a)
{
    return os <<a.refangle<<"\t"<<a.position;
}

ostream& operator<<(ostream& os, const NameMapKey& a){
  return os << a.name;
}
