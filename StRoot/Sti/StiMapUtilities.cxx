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
#include "StiHit.h"
#include "StiMapUtilities.h"

//------------------------------ Hit Map Utilities ----------------------------

//Equality defined by same refangle and position
bool HitMapKey::operator==(const HitMapKey& key2) const
{
    cout <<"HitMapKey::operator==(const HitMapKey&)"<<endl;
    return (this->refangle==key2.refangle && this->position==key2.position);
}

//Return true if key2 < key1.  Order first by refangle, then by position.
bool MapKeyLessThan::operator() (const HitMapKey& key1, const HitMapKey& key2) const
{
    //cout <<"HitMapKey::operator<()"<<endl;
    bool val = false;
    if ( fabs(key1.refangle-key2.refangle) < reftolerance) { //Call these equal, check position

	if ( fabs(key1.position-key2.position) < postolerance) {//Call these equal
	    val = false;
	}
	
	else if (key1.position < key2.position) {val = true;}
	else {val = false;}
    }
    
    else if (key1.refangle < key2.refangle) {val = true;}
    else {val = false;}

    //Old
    /*
      if (key1.refangle > key2.refangle) {val =  false;}
      else if (key1.refangle < key2.refangle) {val = true;}
      else if (key1.refangle == key2.refangle) {
      if (key1.position > key2.position) {val = false;}
      else if (key1.position < key2.position) {val = true;}
      else if (key1.position == key2.position) {val = false;}
      }
    */

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
	
//Order StHits (NOT Sti!) by radius
bool StHitRadiusGreaterThan::operator() (const StHit* hit1, const StHit* hit2) const
{
    const StThreeVectorD& pos1 = hit1->position();
    const StThreeVectorD& pos2 = hit2->position();
    return (sqrt( pos1.x()*pos1.x() + pos1.y()*pos1.y() + pos1.z()*pos1.z() ) >
	    sqrt( pos2.x()*pos2.x() + pos2.y()*pos2.y() + pos2.z()*pos2.z() ) );
}
	
//Order by distance along position, then by z
bool StiHitLessThan::operator() (const StiHit* lhs, const StiHit* rhs) const
{
    bool val;
    if (lhs->y() < rhs->y()) val = true;
    if (lhs->y() > rhs->y()) val = false;
    if (lhs->y()==rhs->y()) {
	if (lhs->z()<rhs->z()) val = true;
	else val = false;
    }
    return val;
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

bool SameStHit::operator()(const StiHit* rhs) const
{
    return (stHit == rhs->stHit() );
};

//----------------------- Detector Map Utilities ------------------------------------

//Equality defined by position and refangle, not z!!!!
bool DetectorMapKey::operator==(const DetectorMapKey& key2) const
{
  return (this->position==key2.position && this->refangle==key2.refangle);
}

bool DetectorMapKey::operator<(const DetectorMapKey& key2) const
{
  bool val = false;
  if (refangle > key2.refangle) {val =  false;}
  else if (refangle < key2.refangle) {val = true;}
  else {
    if (position < key2.position) {val = false;}
    else if (position > key2.position) {val = true;}
    else {
      if (z > key2.z) {val = false;}
      else if (z < key2.z) {val = true;}
      else {val=false;}
    }
  }
  //cout <<"\tLessThan:\t"<<(*this)<<"\t"<<key2<<"\t"<<val<<endl;
  return val;
}

bool NameMapKey::operator==(const NameMapKey& key2) const{
    //return( strcmp(name, key2.name) == 0 );
  return( name ==key2.name );
}

bool NameMapKey::operator<(const NameMapKey& key2) const{
    //return( strcmp(name, key2.name) < 0 );
  return( name < key2.name );
}

bool StTpcPadrowHitFilter::operator()(const StTpcHit& hit) const
{
    return ( (hit.padrow()>=mMinPadrow) && (hit.padrow()<=mMaxPadrow) );
}

void StTpcPadrowHitFilter::build(const string& buildPath)
{
    cout <<"StTpcPadrowHitFilter::build()"<<endl;
    if (mBuilt==true) {
	cout <<"StTpcPadrowHitFilter::build(). ERROR:\tAlread built!  Abort."<<endl;
	return;
    }
    
    if (buildPath=="empty") {
	cout <<"StTpcPadrowHitFilter::build(). ERROR:\tbuildPath==empty.  Abort."<<endl;
	return;
    }
    StGetConfigValue(buildPath.c_str(), "mMinPadrow", mMinPadrow);
    StGetConfigValue(buildPath.c_str(), "mMaxPadrow", mMaxPadrow);
    
    if (mMinPadrow==999 || mMaxPadrow==999) {
	cout <<"StTpcPadrowHitFilter::build(). ERROR:\tmembers not initialized.  ABORT"<<endl;
	return;
    }
    cout <<"\tMinPadrow:\t"<<mMinPadrow<<"\tMaxPadrow:\t"<<mMaxPadrow<<endl;
    mBuilt=true;
}

//----------------------- Streamers -------------------------------------------------
ostream& operator<<(ostream& os, const HitMapKey& a)
{
    return os <<a.refangle<<"\t"<<a.position;
}

ostream& operator<<(ostream& os, const DetectorMapKey& a)
{
  return os <<a.refangle<<"\t"<<a.position<<"\t"<<a.z;
}

ostream& operator<<(ostream& os, const NameMapKey& a){
  return os << a.name;
}
