//StiMapUtilities.cxx
//M.L. Miller (Yale Software)
//3/01

#include <iostream>
#include "StiHit.h"
#include "StiMapUtilities.h"

//------------------------------ Hit Map Utilities ----------------------------

//Equality defined by same refangle and position
bool HitMapKey::operator==(const HitMapKey& key2) const
{
  return (this->refangle==key2.refangle && this->position==key2.position);
}

//Return true if key2 < key1.  Order first by refangle, then by position.
bool MapKeyLessThan::operator() (const HitMapKey& key1, const HitMapKey& key2) const
{
    bool val = false;
    if (key1.refangle > key2.refangle) {val =  false;}
    if (key1.refangle < key2.refangle) {val = true;}
    if (key1.refangle == key2.refangle) {
	if (key1.position > key2.position) {val = false;}
	if (key1.position < key2.position) {val = true;}
	if (key1.position == key2.position) {val = false;}
    }
    return val;
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

bool MaterialMapKey::operator==(const MaterialMapKey& key2) const{
  return( strcmp(name, key2.name) == 0 );
}

bool MaterialMapKey::operator<(const MaterialMapKey& key2) const{
  return( strcmp(name, key2.name) < 0 );
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

ostream& operator<<(ostream& os, const MaterialMapKey& a){
  return os << a.name;
}
