//###########################################################
// EMC Micro Event
// Author: Alexandre A. P. Suaide
// initial version 08/2001
//
// See README for details
//########################################################### 
#include "StEmcMicroHit.h"

ClassImp(StEmcMicroHit);

StEmcMicroHit::StEmcMicroHit()
{
}
StEmcMicroHit::~StEmcMicroHit()
{
}
StEmcMicroHit::StEmcMicroHit(StEmcMicroHit * hit)
{
    mModule=(UChar_t)hit->getModule();
    mEta=(UChar_t)hit->getEta();
    mSub=(UChar_t)hit->getSub();
    mAdc=(Short_t)hit->getAdc();
    mEnergy=hit->getEnergy();
}


