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
  int nCols = tb->GetNumberOfColumns();
  assert(nCols);
//	check table for zeros. All the fields either float or int
  int *row = (int*)tb->GetTable();
  for (int i=0;i<nCols;i++) {
    assert(row[i] && "field not filled");
  }
  *((StvKonst_st*)this) = *(tb->GetTable());
}
