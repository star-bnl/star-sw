//StBET4pMaker.cxx
//M.L. Miller (MIT Software)
//9/04

//std
#include <iostream>
#include <algorithm>
using namespace std;
#include <string>
#include <iostream>
#include <cmath>
#include <sys/times.h>

//STAR
#include "TFile.h"
#include "StChain.h"
#include "SystemOfUnits.h"

//StMuDstMaker
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuEmcCollection.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEmcUtil.h"

//StEmc
#include "StEmcClusterCollection.h"
#include "StEmcPoint.h"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEmcUtil/others/emcDetectorName.h"
#include "StEmcADCtoEMaker/StBemcData.h"

//StJetMaker
#include "StJetMaker/StMuTrackFourVec.h"
#include "StJetMaker/StFourPMakers/StBET4pMaker.h"

ClassImp(StBET4pMaker)
    
    StBET4pMaker::StBET4pMaker(const char* name, StMuDstMaker* dst, StEmcTrackMatcher* match) : St4pMaker(name, dst, match)
{
}

StBET4pMaker::~StBET4pMaker()
{
}

Int_t StBET4pMaker::Make()
{
    //clear the track and hit map
    mTracks.clear();
    mHits.clear();

    //get the tracks:
    StMuDst* mudst = mMuDstMaker->muDst();
    assert(mudst);
    StMuEvent* event = mudst->event();
    assert(event);

    return St4pMaker::Make();
}
