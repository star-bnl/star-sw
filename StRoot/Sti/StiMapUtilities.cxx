//StiMapUtilities.cxx
//M.L. Miller (Yale Software)
//3/01

#include <iostream>
#include "StiHit.h"
#include "StiMapUtilities.h"

//------------------------------ Hit Map Utilities ----------------------------

//Equality defined by same sector and padrow
bool HitMapKey::operator==(const HitMapKey& key2) const
{
  return (this->sector==key2.sector && this->padrow==key2.padrow);
}

//Return true if key2 < key1.  Order first by sector, then by padrow.
bool MapKeyLessThan::operator() (const HitMapKey& key1, const HitMapKey& key2) const
{
    bool val = false;
    if (key1.sector > key2.sector) {val =  false;}
    if (key1.sector < key2.sector) {val = true;}
    if (key1.sector == key2.sector) {
	if (key1.padrow > key2.padrow) {val = false;}
	if (key1.padrow < key2.padrow) {val = true;}
	if (key1.padrow == key2.padrow) {val = false;}
    }
    return val;
}

//Order by distance along padrow, then by z
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

//Order by distance along padrow
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

//Equality defined by padrow and sector
bool DetectorMapKey::operator==(const DetectorMapKey& key2) const
{
  return (this->padrow==key2.padrow && this->sector==key2.sector);
}

bool DetectorMapKey::operator<(const DetectorMapKey& key2) const
{
    bool val = false;
    if (sector > key2.sector) {val =  false;}
    else if (sector < key2.sector) {val = true;}
    else {
	if (padrow < key2.padrow) {val = false;}
	else if (padrow > key2.padrow) {val = true;}
	else {
	    if (z > key2.z) {val = false;}
	    else if (z < key2.z) {val = true;}
	    else {val=false;}
	}
    }
    return val;
}


//----------------------- Streamers -------------------------------------------------
ostream& operator<<(ostream& os, const HitMapKey& a)
{
    return os <<a.sector<<"\t"<<a.padrow;
}

ostream& operator<<(ostream& os, const DetectorMapKey& a)
{
    return os <<a.sector<<"\t"<<a.padrow<<"\t"<<a.z;
}
