//StiHitMapUtilities.cxx
//M.L. Miller, Yale Software, 7/00
#include <iostream>
#include "StTpcHit.h"

#include "StiHitMapUtilities.h"

//Equality defined by same sector and padrow
bool HitMapKey::operator==(const HitMapKey& key2) const
{
    if (this->sector==key2.sector && this->padrow==key2.padrow)
	{return true;}
    else {return false;}
}

ostream& operator<<(ostream& os, const HitMapKey& a)
{
  return os <<a.sector<<"\t"<<a.padrow;
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

//Order by distance along padrow
bool StidHitLessThan::operator() (const StHit* lhs, const StHit* rhs) const
{
    return (lhs->y < rhs->y) ? true : false;
}

//Order by distance along z
bool StizHitLessThan::operator() (const StHit* lhs, const StHit* rhs) const
{
    return (lhs->z < rhs->z) ? true : false;
}
