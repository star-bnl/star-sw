// $Id: StKinkTrkIdCheck.cc,v 1.2 1999/12/16 21:52:27 wdeng Exp $
// $Log: StKinkTrkIdCheck.cc,v $
// Revision 1.2  1999/12/16 21:52:27  wdeng
// Added CVS Id strings
//

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
