//St4pMaker.cxx
//M.L. Miller (MIT Software)
//9/04

//std
#include <iostream>
#include <cmath>
using namespace std;

//local
#include "StJetMaker/StFourPMakers/St4pMaker.h"
#include "StJetMaker/StTrackMatchers/StEmcTrackMatcher.h"

ClassImp(St4pMaker)
    
    St4pMaker::St4pMaker(const char* name, StMuDstMaker* dst, StEmcTrackMatcher* m) :  StMaker(name), mMuDstMaker(dst), mEmcTrackMatcher(m)
{
}

St4pMaker::~St4pMaker()
{
    //no action here...
}

void St4pMaker::Clear(const Option_t* opt)
{
    //clear and destroy...
    for (FourList::iterator it=mFourList.begin(); it!=mFourList.end(); ++it) {
	delete (*it);
	(*it)=0;
    }
    mFourList.clear();
    return StMaker::Clear();
}

Int_t St4pMaker::Make()
{
    return StMaker::Make();
}
