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
#include "StiDetectorNode.h"
#include "StiDetector.h"
#include "StiKalmanTrackNode.h"

//------------------------------ Hit Map Utilities ----------------------------

//Equality defined by same refangle and position
bool HitMapKey::operator==(const HitMapKey& key2) const
{
    //cout <<"HitMapKey::operator==(const HitMapKey&)"<<endl;
    return (this->refangle==key2.refangle && this->position==key2.position);
}

//Return true if key2 < key1.  Order first by refangle, then by position.
bool MapKeyLessThan::operator() (const HitMapKey& key1, const HitMapKey& key2) const {
  //cout <<"HitMapKey::operator()"<<endl;
  bool val = false;
  if ( fabs(key1.refangle-key2.refangle) < reftolerance) {
    //Call these equal, check position
    
    if ( 
	fabs(key1.position-key2.position) < postolerance) {
      //Call these equal
      val = false;
    }	
    else if (
	     key1.position < key2.position) {val = true;}
    else {val = false;}
  }
  
  else if (key1.refangle < key2.refangle) {val = true;}
  else {val = false;}
  
  return val;
}


bool StiDetectorNodePositionLessThan::operator() (const StiDetectorNode * lhs,
						  const StiDetectorNode * rhs) const {
  if (lhs->Data()==0 || rhs->Data()==0) 
    cout <<"StiDetectorNodeLessThan::operator(). ERROR:\t" <<"null data.  Return false"<<endl;
  
  return lhs->Data()->NormalRadius()<rhs->Data()->NormalRadius();
}

//----------------------- Detector Map Utilities ------------------------------------

bool NameMapKey::operator==(const NameMapKey& key2) const{
    //return( strcmp(name, key2.name) == 0 );
  return( name ==key2.name );
}

bool NameMapKey::operator<(const NameMapKey& key2) const{
    //return( strcmp(name, key2.name) < 0 );
  return( name < key2.name );
}

void SetHitUsed::operator()(StiKalmanTrackNode& node)
{
    StiHit* hit = node.Hit();
    if(!hit) 			return;
    if ( !node.IsValid() || node.Chi2()>1e3)	{node.SetHit(0);} 
    else 			{hit->setTimesUsed(hit->timesUsed()+1);}
}


//----------------------- Streamers -------------------------------------------------
ostream& operator<<(ostream& os, const HitMapKey& a)
{
    return os <<a.refangle<<"\t"<<a.position;
}

ostream& operator<<(ostream& os, const NameMapKey& a){
  return os << a.name;
}


