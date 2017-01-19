#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "StMaker.h"
#include "StvConst.h"

const StvConst *StvConst::mgConst=0;

//______________________________________________________________________________
StvConst::StvConst() 
{
  mFw = 0;
  assert(!mgConst);
  mgConst = this;
  StMaker *mk = StMaker::GetChain();
  St_StvKonst *tb = (St_StvKonst*)mk->GetDataBase("Calibrations/tracker/StvKonst");
  assert(tb);
  int nCols = tb->GetNumberOfColumns();
  assert(nCols);
  tb->Print();

//	check table for zeros. All the fields either float or int
  int *row = (int*)tb->GetTable();
  for (int i=0;i<nCols;i++) {
    assert(row[i] && "field not filled");
  }
  *((StvKonst_st*)this) = *(tb->GetTable());
}
//______________________________________________________________________________
const StvKonst_st *StvConst::At(int idx) const
{
  if (!mFw) {
    mFw = new StvKonst_st;
    *mFw = *this;
    memcpy(&mFw->mMinSeedHits,&mFw->mMinSeedHitsFw,
          (char*)&mMinSeedHitsFw-(char*)&mMinSeedHits);
    assert(mFw->mZMax==mFw->mZMaxFw);
  }
  switch(idx) {
    case 0: return this;
    case 1: return mFw;
    default: assert(0 && "Wring index in StvConst::At(indedx)");
  }
}
	  
