/**********************************************************************
 *
 * $Id: StEStructTEventList.cxx,v 1.1 2010/09/02 21:55:43 prindle Exp $
 *
 * Author: Duncan Prindle
 *
 **********************************************************************
 *
 * Description:  Add an append method to TEventList
 *
 ***********************************************************************/

#include "StEStructTEventList.h" 

ClassImp(StEStructTEventList);

StEStructTEventList::StEStructTEventList() {
}
StEStructTEventList::~StEStructTEventList() {
}

void StEStructTEventList::Append(Long64_t entry)
{
    // Append element entry to the list.
    if (!fList) {
        fList = new Long64_t[fSize];
        fList[0] = entry;
        fN = 1;
        return;
    }
    if (fN >= fSize) {
        Int_t newsize = TMath::Max(2*fSize,fN+fDelta);
        Resize(newsize-fSize);
    }
    fList[fN] = entry;
    ++fN;
}
