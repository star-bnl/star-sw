// @(#)root/eg:$Id: filtAction.cxx,v 1.1 2009/04/09 22:47:32 perev Exp $
// Author: Victor Perev  17/03/2009

//______________________________________________________________________________
#include "stdlib.h"
#include "stdio.h"

void *gStarFiltAction = 0;

typedef int (*StarFiltAction_t)(int, void*, void*);

extern "C" int filt_act_(const int &kase,void *par1,void *par2);
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
