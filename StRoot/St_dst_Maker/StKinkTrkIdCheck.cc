#include "StKinkTrkIdCheck.hh"

StKinkTrkIdCheck::StKinkTrkIdCheck()
{
 mCommonIdp = 0; 
 mCommonIdd = 0;
 mPosInKinkVtx = 0;
}

StKinkTrkIdCheck::~StKinkTrkIdCheck() {
}

void StKinkTrkIdCheck::setCommonIdp(Int_t val)
{
mCommonIdp = val;
}

void StKinkTrkIdCheck::setCommonIdd(Int_t val)
{
mCommonIdd = val;
}

void StKinkTrkIdCheck::setPosInKinkVtx(Int_t val)
{
mPosInKinkVtx = val;
}
