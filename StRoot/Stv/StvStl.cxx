// $Id: StvStl.cxx,v 1.5 2015/10/07 02:52:21 perev Exp $
//
//
// Class StvStl some stl containers for Stv objects
// ------------------
#include <stdio.h>
#include "StvStl.h"
#include "StvHit.h"
//_____________________________________________________________________________
StvPoints &StvPoints::operator+=(const float  add[3])
{ push_back(add[0]); push_back(add[1]); push_back(add[2]);return *this;}
//_____________________________________________________________________________
StvPoints &StvPoints::operator+=(const double  add[3])
{ push_back(add[0]); push_back(add[1]); push_back(add[2]);return *this;}

//_____________________________________________________________________________
inline void StvHits::Print(const char *txt)
{
  if (txt && txt[0]) printf("StvHits(%s)\n",txt);
  for (int i=0;i<(int)size();i++) {
    const float *x = (*this)[i]->x();
    double r = sqrt(x[0]*x[0]+x[1]*x[1]);
    printf("%3d - Rxy=%g x=%g y=%g z=%g\n",i,r,x[0],x[1],x[2]);
  }
}
