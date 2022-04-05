//StiMapUtilities.cxx
//M.L. Miller (Yale Software)
//3/01

//STD
#include <Stiostream.h>
#include <math.h>

//SCL
#include "StThreeVectorD.hh"
#include "StEventTypes.h"
#include "StGetConfigValue.hh"

//Sti
#include "StiHit.h"
#include "StiMapUtilities.h"
#include "StiCompositeTreeNode.h"
#include "StiPlacement.h"
#include "StiDetector.h"
#include "StiTrackNode.h"

//------------------------------ Hit Map Utilities ----------------------------

//______________________________________________________________________________
//Equality defined by same refangle and position
bool HitMapKey::operator==(const HitMapKey& key2) const
{
    //cout <<"HitMapKey::operator==(const HitMapKey&)"<<endl;
    return (this->refangle==key2.refangle && this->position==key2.position);
}

//______________________________________________________________________________
//Return true if key2 < key1.  Order first by refangle, then by position.
bool MapKeyLessThan::operator() (const HitMapKey& key1, const HitMapKey& key2) const
{
    //cout <<"HitMapKey::operator()"<<endl;
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


//______________________________________________________________________________
bool StiDetectorNodePositionLessThan::operator() (const StiCompositeTreeNode<StiDetector> * lhs,
						  const StiCompositeTreeNode<StiDetector> * rhs) const {
  if (lhs->getData()==0 || rhs->getData()==0) 
    cout <<"StiDetectorNodeLessThan::operator(). ERROR:\t" <<"null data.  Return false"<<endl;
  
  StiPlacement* lhsp = lhs->getData()->getPlacement();
  StiPlacement* rhsp = rhs->getData()->getPlacement();
  
  return lhsp->getNormalRadius()<rhsp->getNormalRadius();
}

//----------------------- Detector Map Utilities ------------------------------------

//______________________________________________________________________________
bool NameMapKey::operator==(const NameMapKey& key2) const{
    //return( strcmp(name, key2.name) == 0 );
  return( name ==key2.name );
}

//______________________________________________________________________________
bool NameMapKey::operator<(const NameMapKey& key2) const{
    //return( strcmp(name, key2.name) < 0 );
  return( name < key2.name );
}

//______________________________________________________________________________
void SetHitUsed::operator()(StiTrackNode& node)
{
    StiHit* hit = node.getHit();
    if(!hit) 			return;
    if ( !node.isValid() || node.getChi2()>1e3)	{ node.setHit(0)    ;}
    else 					{hit->addTimesUsed();}
}

//______________________________________________________________________________
void SetHitUnused::operator()(StiTrackNode& node)
{
    StiHit* hit = node.getHit();
    if(!hit) 			return;
    if (hit->timesUsed())	hit->setTimesUsed(0);
}

//----------------------- Streamers -------------------------------------------------
//______________________________________________________________________________
ostream& operator<<(ostream& os, const HitMapKey& a)
{
    return os <<a.refangle<<"\t"<<a.position;
}

//______________________________________________________________________________
ostream& operator<<(ostream& os, const NameMapKey& a){
  return os << a.name;
}


