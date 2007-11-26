/**********************************************************************
 *
 * $Id: StEStructTherminator.cxx,v 1.1 2007/11/26 20:08:24 prindle Exp $
 *
 * Author: Chunhui Han
 *
 **********************************************************************
 *
 * Description:  EStructEventReader wrapper for Therminator event generator
 *
 **********************************************************************/
#include "StEStructTherminator.h"

#include "StEStructPool/AnalysisMaker/StEStructEventCuts.h"
#include "StEStructPool/AnalysisMaker/StEStructTrackCuts.h"
#include "StEStructPool/EventMaker/StEStructEvent.h"
#include "StEStructPool/EventMaker/StEStructTrack.h"
#include "StEStructPool/EventMaker/StEStructCentrality.h"

StEStructTherminator::StEStructTherminator() {
    mTherminator        = 0;
    mECuts              = 0;
    mTCuts              = 0;
    mInChain            = false;
    mEventsToDo         = 100;
    mEventCount         = 0;
    mAmDone             = false;
};

StEStructTherminator::StEStructTherminator(Therminator *therminator,
                                           StEStructEventCuts* ecuts,
                                           StEStructTrackCuts* tcuts,
                                           int  eventsToDo) {
    mTherminator        = therminator;
    mECuts              = ecuts;
    mTCuts              = tcuts;
    mInChain            = false;
    mEventsToDo         = eventsToDo;
    mEventCount         = 0;
    mAmDone             = false;
};

bool StEStructTherminator::hasGenerator() { return (mTherminator) ? true : false; }
bool StEStructTherminator::hasEventCuts() { return (mECuts) ? true : false ; }
bool StEStructTherminator::hasTrackCuts() { return (mTCuts) ? true : false ; }

//-------------------------------------------------------------------------
StEStructEvent* StEStructTherminator::next() {
    if(!mTherminator || mEventCount== mEventsToDo) {
        mAmDone=true;
        return (StEStructEvent*)NULL;
    }
    StEStructEvent* retVal=NULL;

    if (!mInChain ) {
          mTherminator->GenerateEvent();
          mEventCount++;
    }

    retVal = new StEStructEvent();

    retVal->SetVx(0);
    retVal->SetVy(0);
    retVal->SetVz(0);
    retVal->SetBField(0.5);

    int   nTracks = countGoodTracks();
    // Therminator has no concept of impact parameter. Always use number of
    // tracks as measure of centrality (although within the model that doesn't
    // really completely make sense.)
    float centMeasure = nTracks;
    retVal->SetCentrality(centMeasure);
    if (!mECuts->goodCentrality(centMeasure)) {
        delete retVal;
        retVal=NULL;
        mECuts->fillHistogram(mECuts->centralityName(),centMeasure,false);
    } else {
        fillTracks(retVal);
        retVal->FillChargeCollections();
        mECuts->fillHistogram(mECuts->centralityName(),centMeasure,true);
    }
    return retVal;
}

//--------------------------------------------------------------------------
void StEStructTherminator::fillTracks(StEStructEvent* estructEvent) {
    int numParticles=mTherminator->GetNParticles();

    StEStructTrack* eTrack= new StEStructTrack();
    for (int i=0;i<numParticles;i++) {
        if (!isTrackGood(i)) {
            mTCuts->fillHistograms(false);
            continue;
        }
        mTCuts->fillHistograms(true);
        eTrack->SetInComplete();

        int pid = mTherminator->GetPdg(i);
        float p[3];
        float v[3];
        for(int k=0;k<3;k++){
            p[0] = mTherminator->GetPx(i);
            p[1] = mTherminator->GetPy(i);
            p[2] = mTherminator->GetPz(i);
            v[0] = mTherminator->GetVx(i);
            v[1] = mTherminator->GetVy(i);
            v[2] = mTherminator->GetVz(i);
        }

        float pt    = sqrt(p[0]*p[0] + p[1]*p[1]);
        float theta = acos(p[2]/ TMath::Sqrt(pt*pt + p[2]*p[2]) );
        float eta   = -1.0*log(tan(theta/2.0));
        float *gdca = globalDCA(p,v);
        float phi=atan2((double)p[1], (double)p[0]);

        eTrack->SetBx(gdca[0]);
        eTrack->SetBy(gdca[1]);
        eTrack->SetBz(gdca[2]);
        eTrack->SetBxPrimary(gdca[0]);
        eTrack->SetByPrimary(gdca[1]);
        eTrack->SetBzPrimary(gdca[2]);
        eTrack->SetBxGlobal(gdca[0]);
        eTrack->SetByGlobal(gdca[1]);
        eTrack->SetBzGlobal(gdca[2]);
        delete [] gdca;

        eTrack->SetPIDe(10);
        eTrack->SetPIDpi(10);
        eTrack->SetPIDk(10);
        eTrack->SetPIDp(10);
        eTrack->SetPIDd(10);
        if ((pid == 7) || (pid == 8)) {
            eTrack->SetPIDe(0);
        } else if ((pid == -211) || (pid == 211)) {
            eTrack->SetPIDpi(0);
        } else if ((pid == -321) || (pid == 321)) {
            eTrack->SetPIDk(0);
        } else if ((pid == -2212) || (pid == 2212)) {
            eTrack->SetPIDp(0);
        } else if ((pid == -2213) || (pid == 2213)) {
            // These numbers aren't right, but I don't care about dueterons right now.
            eTrack->SetPIDd(0);
        }

        eTrack->SetPx(p[0]);
        eTrack->SetPy(p[1]);
        eTrack->SetPz(p[2]);
        eTrack->SetEta(eta);
        eTrack->SetPhi(phi);
        eTrack->SetDedx(0);
        eTrack->SetChi2(1);
        eTrack->SetTopologyMapData(0, 0xffffff80);
        eTrack->SetTopologyMapData(1, 0x3fff);
        eTrack->SetTopologyMapTPCNHits(45);
        eTrack->SetNMaxPoints(45);
        eTrack->SetNFoundPoints(45);
        eTrack->SetNFitPoints(45);

        if(pid<0){
          eTrack->SetCharge(-1);
        } else {
          eTrack->SetCharge(1);
        }    

        estructEvent->AddTrack(eTrack);
    }
    delete eTrack;
    return;
}    

//-------------------------------------------------------------
// This method checks all track cuts.
// No histogramming or copying data around.
bool StEStructTherminator::isTrackGood(int i) {
    int pid = mTherminator->GetPdg(i);
    if (!measureable(pid)) {     // checks if pi,k,p or e
        return false;
    }
    float p[3];
    float v[3];
    p[0] = mTherminator->GetPx(i);
    p[1] = mTherminator->GetPy(i);
    p[2] = mTherminator->GetPz(i);
    v[0] = mTherminator->GetVx(i);
    v[1] = mTherminator->GetVy(i);
    v[2] = mTherminator->GetVz(i);

    int q;
    pid < 0 ? q=-1 : q=+1;
    float pt = sqrt(p[0]*p[0]+p[1]*p[1]);
    float theta = acos(p[2]/ TMath::Sqrt(pt * pt + p[2] * p[2]) );
    float eta   = -1.0*log(tan(theta/2.0));
    float phi = atan2((double)p[1], (double)p[0]);
    float* gdca  = globalDCA(p,v);
    float _r = pt/0.139;
    float yt = log(sqrt(1+_r*_r)+_r);

    bool useTrack = true;
    useTrack = (mTCuts->goodGlobalDCA(gdca[3]) && useTrack);
    useTrack = (mTCuts->goodCharge(q) && useTrack);
    useTrack = (mTCuts->goodPt(pt) && useTrack);
    useTrack = (mTCuts->goodEta(eta) && useTrack);
    useTrack = (mTCuts->goodPhi(phi) && useTrack);
    useTrack = (mTCuts->goodYt(yt) && useTrack);
    delete [] gdca;

    return useTrack;
}

//-------------------------------------------------------------------------
//-------------------------------------------------------------
// This method counts all good track.
// No histogramming or copying data around.
int StEStructTherminator::countGoodTracks() {
    mnumTracks = 0;
    int numParticles = mTherminator->GetNParticles();
    for (int i=0;i<numParticles;i++) {
        if (isTrackGood(i)) {
            mnumTracks++;
        }
    }
    return mnumTracks;
}
//--------------------------------------------------------------------------
void StEStructTherminator::setTherminatorReader(Therminator *therminator){
    if (mTherminator) delete mTherminator;
    mTherminator = therminator;
};
//--------------------------------------------------------------------------
void StEStructTherminator::setEventCuts(StEStructEventCuts* cuts){
    if (mECuts) delete mECuts;
    mECuts = cuts;
};

//---------------------------------------------------------------
void StEStructTherminator::setTrackCuts(StEStructTrackCuts* cuts){
    if (mTCuts) delete mTCuts;
    mTCuts = cuts;
}

/**********************************************************************
 *
 *********************************************************************/
