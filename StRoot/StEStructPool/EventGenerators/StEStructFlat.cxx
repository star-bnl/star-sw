/**********************************************************************
 *
 * $Id: StEStructFlat.cxx,v 1.3 2005/09/07 20:22:47 prindle Exp $
 *
 * Author: Jeff Porter 
 *
 **********************************************************************
 *
 * Description:  EStructEventReader which generates events Flat in eta,phi.
 *
 **********************************************************************/
#include "StEStructFlat.h"

#include "StEStructPool/AnalysisMaker/StEStructEventCuts.h"
#include "StEStructPool/AnalysisMaker/StEStructTrackCuts.h"
#include "StEStructPool/EventMaker/StEStructEvent.h"
#include "StEStructPool/EventMaker/StEStructTrack.h"

StEStructFlat::StEStructFlat() {
    mFlatEvent    = 0;
    mECuts        = 0;
    mTCuts        = 0;
    mInChain      = false;
    mEventsToDo   = 0;
    mUseAllTracks = false;
    mCentBin      = 0;
    mEventCount   = 0;
    mAmDone       = false;
}
StEStructFlat::StEStructFlat( StEStructEventCuts* ecuts,
                              StEStructTrackCuts* tcuts,
                              bool inChain,
                              bool useAllTracks,
                              int  centBin,
                              int  eventsToDo) {
    mFlatEvent    = 0;
    mECuts        = ecuts;
    mTCuts        = tcuts;
    mInChain      = inChain;
    mEventsToDo   = eventsToDo;
    mUseAllTracks = useAllTracks;
    mCentBin      = centBin;
    mEventCount   = 0;
    mAmDone       = false;
};

bool StEStructFlat::hasGenerator() { return true; };
bool StEStructFlat::hasEventCuts() { return (mECuts) ? true : false ; }
bool StEStructFlat::hasTrackCuts() { return (mTCuts) ? true : false ; }


//-------------------------------------------------------------------------
void StEStructFlat::setSeed(int iseed) {
    cout << " Calling srand48(" << iseed << ")" << endl;
    srand48(iseed);
}

//-------------------------------------------------------------------------
StEStructEvent* StEStructFlat::next() {
    if(mEventCount==mEventsToDo){
        mAmDone = true;
        return (StEStructEvent*) NULL;
    }

    if (!mInChain ) {
        mFlatEvent = new StEStructEvent();
        generateEvent();
        mEventCount++;
    }


    mFlatEvent->SetVx(0);
    mFlatEvent->SetVy(0);
    mFlatEvent->SetVz(0);
    mFlatEvent->SetBField(0.5);

    int nTracks = countGoodTracks();
    if (mUseAllTracks) {
        mFlatEvent->SetCentrality( (double) nTracks );
    } else {
        mFlatEvent->SetCentrality( (double) mRefMult );
    }
    int jCent = mFlatEvent->Centrality();
    if (((mCentBin >= 0) && (jCent != mCentBin)) ||
        !mECuts->goodNumberOfTracks(mRefMult)) {
        mECuts->fillHistogram(mECuts->numTracksName(),(float)mRefMult,false);
        return (StEStructEvent*) NULL;
    } else {
        mFlatEvent->SetNtrack(mRefMult);
        mFlatEvent->SetOrigMult(mRefMult);
        mFlatEvent->SetCentMult(mRefMult);
        mFlatEvent->FillChargeCollections();
        mECuts->fillHistogram(mECuts->numTracksName(),(float)mRefMult,true);
        return mFlatEvent;
    }
}

//--------------------------------------------------------------------------
void StEStructFlat::generateEvent() {
    fillTracks(mFlatEvent);
}   

//--------------------------------------------------------------------------
void StEStructFlat::fillTracks(StEStructEvent* estructEvent) {

    mRefMult = 0;
    StEStructTrack* eTrack = new StEStructTrack();
    int pid;

//    int    numCharge = int( -5*log(drand48()) );
    int    numCharge, numInEta = 850;
    double v2      = 0.2, pi = 3.1415926;
    double eta, quadEta = 0.2, etaMax = 2;
    double phi, phiSector, phiOff, sectorWidth;
    double sectorGap = 2, gapDepth = 0.2;
    int    numSector = 12, iSector;

    sectorWidth = 360 / numSector;
    phiOff = 360*drand48();

    float p[3], pt, pz, v[3];

    double etaMaxAmp = 1 + quadEta*etaMax*etaMax;
    numCharge = int( numInEta*etaMax * (3+quadEta*etaMax*etaMax) / (3+quadEta) );
    for(int i=0;i<numCharge;i++) {

        eTrack->SetInComplete();
        if (i < numCharge/2) {
            pid = 211;
        } else {
            pid = -211;
        }
        pt  = 0.1 - 0.5 * log(drand48());
        double etaAmp = 0, randAmp = 1;
        while (randAmp > etaAmp) {
            eta     = etaMax*(2*drand48() - 1);
            etaAmp  = 1 + quadEta*eta*eta;
            randAmp = drand48()*etaMaxAmp;
        }
        pz  = sqrt( pt*pt + 0.139*0.139) * (exp(eta)-exp(-eta)) / 2;

        
        double r = 1.1;
        while (r > 1-gapDepth) {
            // Put flow into phi.
            phi = 360*drand48();
            double h   = (1+v2)*drand48();
            while ( h > (1+v2*cos(2*phi*pi/180)) ) {
                phi = 360*drand48();
                h   = (1+v2)*drand48();
            }
            phi += phiOff;
            while (phi > 360) {
                phi -= 360;
            }
            iSector = int( phi / sectorWidth );
            phiSector = phi - sectorWidth*iSector;
            if (phiSector > sectorGap) {
                r = 0;
            } else {
                r = drand48();
            }
        }
        while (phi > 180) {
            phi -= 360;
        }

        p[0] = pt*cos(phi*pi/180);
        p[1] = pt*sin(phi*pi/180);
        p[2] = pz;
        v[0] = 0;
        v[1] = 0;
        v[2] = 0;
        if (!isTrackGood(v,p,eta)) {
            mTCuts->fillHistograms(false);
            continue;
        }
        mTCuts->fillHistograms(true);
        if (pt<0.15) continue;

        mRefMult++;

        float *gdca = globalDCA(p,v);
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

        eTrack->SetPx(p[0]);
        eTrack->SetPy(p[1]);
        eTrack->SetPz(p[2]);
        eTrack->SetEta(eta);
        eTrack->SetPhi(phi*pi/180);
        eTrack->SetDedx(0);
        eTrack->SetChi2(1);
        eTrack->SetTopologyMapData(0, 4294967168);
        eTrack->SetTopologyMapData(1, 16383);
        eTrack->SetTopologyMapTPCNHits(45);
        eTrack->SetNMaxPoints(45);
        eTrack->SetNFoundPoints(45);
        eTrack->SetNFitPoints(45);

        if (pid<0) {
            eTrack->SetCharge(-1);
        } else {
            eTrack->SetCharge(1);
        }
        estructEvent->AddTrack(eTrack);
    }

    estructEvent->SetCentrality( (double) mRefMult );
    delete eTrack;
    return;
}

//-------------------------------------------------------------
// This method checks all track cuts.
// No histogramming or copying data around.
bool StEStructFlat::isTrackGood(float *v, float *p, float eta) {
    float pt = sqrt(p[0]*p[0]+p[1]*p[1]);
    if (pt < 0.15) {
        return false;
    }

    float phi   = atan2((double)p[1], (double)p[0]);
    float *gdca = globalDCA(p,v);
    float _r = pt/0.139;
    float yt = log(sqrt(1+_r*_r)+_r);

    bool useTrack = true;
    useTrack = (mTCuts->goodGlobalDCA(gdca[3]) && useTrack);
    useTrack = (mTCuts->goodEta(eta) && useTrack);
    useTrack = (mTCuts->goodPhi(phi) && useTrack);
    useTrack = (mTCuts->goodPt(pt)   && useTrack);
    useTrack = (mTCuts->goodYt(yt)   && useTrack);
    delete [] gdca;

    return useTrack;
}
int StEStructFlat::countGoodTracks() {
    return mRefMult;
}
//--------------------------------------------------------------------------
void StEStructFlat::setEventCuts(StEStructEventCuts* cuts) {
    if (mECuts) delete mECuts;
    mECuts=cuts;
};

//---------------------------------------------------------------
void StEStructFlat::setTrackCuts(StEStructTrackCuts* cuts) {
    if (mTCuts) delete mTCuts;
    mTCuts=cuts;
}




/**********************************************************************
 *
 * $Log: StEStructFlat.cxx,v $
 * Revision 1.3  2005/09/07 20:22:47  prindle
 * Flat: Random changes to eta and phi distributions (which don't have to be flat).
 *
 * Revision 1.2  2003/11/25 22:45:14  prindle
 * Commiting changes so I can move code to rhic
 *
 * Revision 1.1  2003/11/21 23:48:00  prindle
 * Include my toy event generator in cvs
 *
 *
 *********************************************************************/
