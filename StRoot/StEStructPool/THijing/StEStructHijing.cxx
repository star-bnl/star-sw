/**********************************************************************
 *
 * $Id: StEStructHijing.cxx,v 1.1 2005/09/14 17:28:11 msd Exp $
 *
 * Author: Chunhui Han
 *
 **********************************************************************
 *
 * Description:  EStructEventReader wrapper for (T)Hijing event generator
 *
 **********************************************************************/
#include "StEStructHijing.h"

#include "StEStructPool/AnalysisMaker/StEStructEventCuts.h"
#include "StEStructPool/AnalysisMaker/StEStructTrackCuts.h"
#include "StEStructPool/EventMaker/StEStructEvent.h"
#include "StEStructPool/EventMaker/StEStructTrack.h"

StEStructHijing::StEStructHijing() {
    mHijing       = 0;
    mECuts        = 0;
    mTCuts        = 0;
    mInChain      = false;
    mEventsToDo   = 100;
    mUseAllTracks = false;
    mCentBin      = 0;
    mEventCount   = 0;
    mAmDone       = false;
};

StEStructHijing::StEStructHijing(THijing* hijing,
                                 StEStructEventCuts* ecuts,
                                 StEStructTrackCuts* tcuts,
                                 bool inChain,
                                 bool useAllTracks,
                                 int  centBin,
                                 int  eventsToDo) {
    mHijing       = hijing;
    mECuts        = ecuts;
    mTCuts        = tcuts;
    mInChain      = inChain;
    mEventsToDo   = eventsToDo;
    mUseAllTracks = useAllTracks;
    mCentBin      = centBin;
    mEventCount   = 0;
    mAmDone       = false;
};

bool StEStructHijing::hasGenerator() { return (mHijing) ? true : false; }
bool StEStructHijing::hasEventCuts() { return (mECuts) ? true : false ; }
bool StEStructHijing::hasTrackCuts() { return (mTCuts) ? true : false ; }

//-------------------------------------------------------------------------
StEStructEvent* StEStructHijing::next() {
    if(!mHijing || mEventCount== mEventsToDo) {
        mAmDone=true;
        return (StEStructEvent*)NULL;
    }
    StEStructEvent* retVal=NULL;

    if (!mInChain ) {
          mHijing->GenerateEvent();
          mEventCount++;
    }

    retVal = new StEStructEvent();

    retVal->SetVx(0);
    retVal->SetVy(0);
    retVal->SetVz(0);
    retVal->SetBField(0.5);

    int nTracks = countGoodTracks();
    if (mUseAllTracks) {
        retVal->SetCentrality( (double) nTracks );
    } else {
        retVal->SetCentrality(mHijing->GetImpactParameter());
    }
    int jCent = retVal->Centrality();
    if (((mCentBin >= 0) && (jCent != mCentBin)) ||
        !mECuts->goodNumberOfTracks(mRefMult)) {
        delete retVal;
        retVal=NULL;
        mECuts->fillHistogram(mECuts->numTracksName(),(float)mRefMult,false);
    } else {
        fillTracks(retVal);
        retVal->SetOrigMult(mRefMult);
        retVal->SetCentMult(mRefMult);
        retVal->FillChargeCollections();
        mECuts->fillHistogram(mECuts->numTracksName(),(float)mRefMult,true);
    }
    return retVal;
}

//--------------------------------------------------------------------------
void StEStructHijing::fillTracks(StEStructEvent* estructEvent) {
    int numParticles=mHijing->GetNParticles();

    StEStructTrack* eTrack= new StEStructTrack();
    for (int i=0;i<numParticles;i++) {
        if (!isTrackGood(i)) {
            mTCuts->fillHistograms(false);
            continue;
        }
        mTCuts->fillHistograms(true);
        eTrack->SetInComplete();

        int pid = mHijing->GetPdg(i);
        float p[3];
        float v[3];
        for(int k=0;k<3;k++){
            p[0] = mHijing->GetPx(i);
            p[1] = mHijing->GetPy(i);
            p[2] = mHijing->GetPz(i);
            v[0] = mHijing->GetVx(i);
            v[1] = mHijing->GetVy(i);
            v[2] = mHijing->GetVz(i);
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

        eTrack->SetPIDe(0);
        eTrack->SetPIDpi(0);
        eTrack->SetPIDk(0);
        eTrack->SetPIDp(0);
        eTrack->SetPIDd(0);
        if ((pid == 7) || (pid == 8)) {
            eTrack->SetPIDe(1);
        } else if ((pid == -211) || (pid == 211)) {
            eTrack->SetPIDpi(1);
        } else if ((pid == -321) || (pid == 321)) {
            eTrack->SetPIDk(1);
        } else if ((pid == -2212) || (pid == 2212)) {
            eTrack->SetPIDp(1);
        } else if ((pid == -2213) || (pid == 2213)) {
            // These numbers aren't right, but I don't care about dueterons right now.
            eTrack->SetPIDd(1);
        }

        eTrack->SetPx(p[0]);
        eTrack->SetPy(p[1]);
        eTrack->SetPz(p[2]);
        eTrack->SetEta(eta);
        eTrack->SetPhi(phi);
        eTrack->SetDedx(0);
        eTrack->SetChi2(1);
        eTrack->SetTopologyMapData(0, 4294967168);
        eTrack->SetTopologyMapData(1, 16383);
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
bool StEStructHijing::isTrackGood(int i) {
    int pid = mHijing->GetPdg(i);
    if (!measureable(pid)) {     // checks if pi,k,p or e
        return false;
    }
    float p[3];
    float v[3];
    p[0] = mHijing->GetPx(i);
    p[1] = mHijing->GetPy(i);
    p[2] = mHijing->GetPz(i);
    v[0] = mHijing->GetVx(i);
    v[1] = mHijing->GetVy(i);
    v[2] = mHijing->GetVz(i);

    float pt = sqrt(p[0]*p[0]+p[1]*p[1]);
    if (pt < 0.15) {
        return false;
    }

    float theta = acos(p[2]/ TMath::Sqrt(pt * pt + p[2] * p[2]) );
    float eta   = -1.0*log(tan(theta/2.0));
    float phi = atan2((double)p[1], (double)p[0]);
    float* gdca  = globalDCA(p,v);
    float _r = pt/0.139;
    float yt = log(sqrt(1+_r*_r)+_r);

    bool useTrack = true;
    useTrack = (mTCuts->goodGlobalDCA(gdca[3]) && useTrack);
    useTrack = (mTCuts->goodEta(eta) && useTrack);
    useTrack = (mTCuts->goodPhi(phi) && useTrack);
    useTrack = (mTCuts->goodPt(pt) && useTrack);
    useTrack = (mTCuts->goodYt(yt) && useTrack);
    delete [] gdca;

    return useTrack;
}

//-------------------------------------------------------------------------
//-------------------------------------------------------------
// This method counts all good track.
// No histogramming or copying data around.
int StEStructHijing::countGoodTracks() {
    mRefMult = 0;
    int numParticles = mHijing->GetNParticles();
    for (int i=0;i<numParticles;i++) {
        if (isTrackGood(i)) {
            mRefMult++;
        }
    }
    return mRefMult;
}
//--------------------------------------------------------------------------
void StEStructHijing::setHijingReader(THijing* hijing){
    if (mHijing) delete mHijing;
    mHijing = hijing;
};
//--------------------------------------------------------------------------
void StEStructHijing::setEventCuts(StEStructEventCuts* cuts){
    if (mECuts) delete mECuts;
    mECuts = cuts;
};

//---------------------------------------------------------------
void StEStructHijing::setTrackCuts(StEStructTrackCuts* cuts){
    if (mTCuts) delete mTCuts;
    mTCuts = cuts;
}

/**********************************************************************
 *
 *********************************************************************/
