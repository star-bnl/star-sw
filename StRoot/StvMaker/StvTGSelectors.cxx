#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include "StvTGSelectors.h"

ClassImp(StvTpcSelector)

//______________________________________________________________________________
StvTpcSelector::StvTpcSelector(const char *name):StTGeoSele(name)
{
  mInOut=-99;
  TString ts(name);
  if (ts.Index("TpcInner",0,TString::kIgnoreCase)>-1) mInOut=0;
  if (ts.Index("TpcOuter",0,TString::kIgnoreCase)>-1) mInOut=1;
  assert(mInOut>=0);
}
//______________________________________________________________________________
int StvTpcSelector::Select(const char *path,int copyNumber, const float xyz[3]) const
{
enum {nbpads=73};
static const int isdets[nbpads] = {  1, 0, 2, 1, 0, 2, 1, 0, 2, 1,
			             0, 2, 1, 0, 2, 1, 0, 2, 1, 0,
			             2, 1, 0, 2, 1, 0, 2, 1, 0, 2,
			             1, 0, 2, 1, 0, 2, 1, 0, 2, 1,
			             0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
			             0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
			             0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
			             0, 0, 2};
  if (!strstr(path,"/TPA")) return 0;
  int tpad = copyNumber; if (tpad>nbpads) tpad-=nbpads;
  assert(tpad>0 && tpad <=nbpads);
  if (isdets[tpad-1]) return 0;
  int sec = (int)(atan2(xyz[1],xyz[0])/M_PI*6);
  double ang = M_PI/12+sec*(M_PI/6);
  double pseudoRad =  xyz[0]*cos(ang)+xyz[1]*sin(ang);
  return ((pseudoRad>123.) == (mInOut!=0));
}  
  

