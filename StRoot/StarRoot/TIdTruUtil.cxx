/***************************************************************************
 *
 * $Id: TIdTruUtil.cxx,v 1.1 2016/02/16 16:58:28 perev Exp $
 *
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 **************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "TIdTruUtil.h"
ClassImp(TIdTruUtil)
//______________________________________________________________________________
TIdTruUtil::TIdTruUtil(const char *name):TNamed(name,"")
{
   Clear();
}
//______________________________________________________________________________
TIdTruUtil::~TIdTruUtil()
{
}

//______________________________________________________________________________
void TIdTruUtil::Clear(const char*)
{
mEvalted = 0;
mIdTru   = 0;
mQua     = 0;
mSize    = 0;
mDetWt.clear();
}
//______________________________________________________________________________
void TIdTruUtil::Eval()
{
  mEvalted = 1;  
  double totWt=0;
  std::multimap <double,int> myWtDet;
  for (auto it = mDetWt.begin(); it!= mDetWt.end(); ++it) {
    int idTru = (*it).first;
    double wt = (*it).second;
    totWt+=wt;
    myWtDet.insert( std::pair<double,int>(-wt,idTru));
  }
  mQua   = fabs((*myWtDet.begin()).first/(totWt+1e-11));
  mIdTru = (*myWtDet.begin()).second;
}
//______________________________________________________________________________
void TIdTruUtil::Add(int idTru,int qa)
{
   mSize++;
   mDetWt[idTru]+=qa;
}

//______________________________________________________________________________
int TIdTruUtil::GetIdTru()
{
  if (!mEvalted) Eval();
  return mIdTru;
}

//______________________________________________________________________________
double TIdTruUtil::GetQua()
{
  if (!mEvalted) Eval();
  return mQua;
}














