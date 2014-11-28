// @(#)root/eg:$Id: filtAction.cxx,v 1.2 2012/07/19 21:33:14 jwebb Exp $
// Author: Victor Perev  17/03/2009

//______________________________________________________________________________
#include "stdlib.h"
#include "stdio.h"
#include <string>

void *gStarFiltAction = 0;
void *gStarFiltConfig = 0;

typedef int  (*StarFiltAction_t)(int, void*, void*);
typedef void (*StarFiltConfig_t)(std::string, const float);

extern "C" int   filt_act_(const int &kase,void *par1,void *par2);
extern "C" int   filt_cfg_(const char *key, float &value, int ks);

//______________________________________________________________________________
int filt_act_(const int &kase,void *par1,void *par2)
{
  enum { kSelect=0,kEGInit=1, kEGReje=2
                  ,kGTInit=3, kGTReje=4
		  ,kGEInit=5, kGEReje=6};
  if (kase==0 && !gStarFiltAction) {
    printf("AGXuser: ERROR, Filter library is not loaded\n");
    return 1;
  } 
  if (!gStarFiltAction) return 0;
  return ((*(StarFiltAction_t)gStarFiltAction))(kase,par1,par2);
}
//______________________________________________________________________________
int filt_cfg_(const char *KEY, float &VAL, int ks)
{
  std::string key = KEY;
  const float value     = VAL;
  if ( gStarFiltConfig )
    {
      ((*(StarFiltConfig_t)gStarFiltConfig))( key, value );
      return 1;
    }
  return 0;
}
//______________________________________________________________________________


