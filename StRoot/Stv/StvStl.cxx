// $Id: StvStl.cxx,v 1.6 2015/12/12 01:58:25 perev Exp $
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
void StvHits::Print(const char *txt)
{
  if (txt && txt[0]) printf("StvHits(%s)\n",txt);
  for (int i=0;i<(int)size();i++) {
    auto *stvHit   = (*this)[i];
    const float *x = stvHit->x();
    double r = sqrt(x[0]*x[0]+x[1]*x[1]);
    int idTru = stvHit->idTru();


    printf("%3d - idTru = %d Rxy=%g x=%g y=%g z=%g\n",i,idTru,r,x[0],x[1],x[2]);
  }
}
