#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "StMaker.h"
#include "StvConst.h"

const StvConst *StvConst::mgConst=0;

//______________________________________________________________________________
StvConst::StvConst() 
{
  assert(!mgConst);
  mgConst = this;
  StMaker *mk = StMaker::GetChain();
  St_StvKonst *tb = (St_StvKonst*)mk->GetDataBase("Calibrations/tracker/StvKonst");
  assert(tb);
  *((StvKonst_st*)this) = *(tb->GetTable());
}
