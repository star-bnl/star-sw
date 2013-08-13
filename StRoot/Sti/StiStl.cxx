// $Id: StiStl.cxx,v 2.1 2013/08/13 21:51:11 perev Exp $
//
//
// Class StiStl some stl containers for Sti objects
// ------------------
#include "StiStl.h"
#include "StiHit.h"
//_____________________________________________________________________________
StiHits &StiHits::operator+=(const StiHits &add)
{ insert(end(),add.begin(),add.end()); return *this;}

//_____________________________________________________________________________
void StiHits::unused()
{
  for (int i=0;i<(int)size();i++) {(*this)[i]->setTimesUsed(0);}
}
//_____________________________________________________________________________
StiPoints &StiPoints::operator+=(const float  add[3])
{ push_back(add[0]); push_back(add[1]); push_back(add[2]);return *this;}
//_____________________________________________________________________________
StiPoints &StiPoints::operator+=(const double  add[3])
{ push_back(add[0]); push_back(add[1]); push_back(add[2]);return *this;}
