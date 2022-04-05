/**********************************************************************
 *
 * $Id: StEStructFlat.cxx,v 1.7 2012/11/16 21:23:18 prindle Exp $
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
#include "StEStructPool/EventMaker/StEStructCentrality.h"

StEStructFlat::StEStructFlat() {
    mFlatEvent    = 0;
    mECuts        = 0;
    mTCuts        = 0;
    mInChain      = false;
    mEventsToDo   = 0;
    mCentBin      = 0;
    mEventCount   = 0;
    mAmDone       = false;
    mgRand2Good   = false;
}
StEStructFlat::StEStructFlat( StEStructEventCuts* ecuts,
                              StEStructTrackCuts* tcuts,
                              bool inChain,
                              int  centBin,
                              int  eventsToDo) {
    mFlatEvent    = 0;
    mECuts        = ecuts;
    mTCuts        = tcuts;
    mInChain      = inChain;
    mEventsToDo   = eventsToDo;
    mCentBin      = centBin;
    mEventCount   = 0;
    mAmDone       = false;
    mgRand2Good   = false;
};

bool StEStructFlat::hasGenerator() { return true; };

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

    float z = mFlatEvent->Vz();
    int nTracks = countGoodTracks();
    mFlatEvent->SetCentrality( (double) nTracks );
    StEStructCentrality* cent=StEStructCentrality::Instance();
    int jCent = cent->centrality( mFlatEvent->Centrality() );
    if (((mCentBin >= 0) && (jCent != mCentBin)) ||
        !mECuts->goodPrimaryVertexZ(z)           ||
        !mECuts->goodCentrality(mFlatEvent->Centrality())) {
        mECuts->fillHistogram(mECuts->centralityName(),(float)nTracks,false);
        mECuts->fillHistogram(mECuts->primaryVertexZName(),z,false);
        return (StEStructEvent*) NULL;
    } else {
        mFlatEvent->FillChargeCollections();
        mECuts->fillHistogram(mECuts->centralityName(),(float)nTracks,true);
        mECuts->fillHistogram(mECuts->primaryVertexZName(),z,true);
        return mFlatEvent;
    }
}

//--------------------------------------------------------------------------
void StEStructFlat::generateEvent() {
    mFlatEvent->SetVx(0);
    mFlatEvent->SetVy(0);
    mFlatEvent->SetVz(30*gRand48());
    mFlatEvent->SetBField(0.5);

    fillTracks(mFlatEvent);
}   

//--------------------------------------------------------------------------
void StEStructFlat::fillTracks(StEStructEvent* estructEvent) {

    mnumTracks = 0;
    StEStructTrack* eTrack = new StEStructTrack();
    int pid, sign;

//    int    numCharge = int( -5*log(drand48()) );
    int    numCharge, numInEta = 850;
    double v2      = 0.05, pi = 3.1415926;
    double eta, quadEta = 0.2, etaMax = 2;
    double phi, phiOff, sectorWidth;
    int    numSector = 12;

    sectorWidth = 360 / numSector;
    phiOff = 360*drand48();

    float p[3], pt, pz, v[3];

    double etaMaxAmp = 1 + quadEta*etaMax*etaMax;
    numCharge = int( numInEta*etaMax * (3+quadEta*etaMax*etaMax) / (3+quadEta) );
    for(int i=0;i<numCharge;i++) {

        eTrack->SetInComplete();
        if (i < numCharge/2) {
            sign = +1;
            pid = 211;
        } else {
            sign = -1;
            pid = -211;
        }
        double etaAmp = 0, randAmp = 1;
        while (randAmp > etaAmp) {
            eta     = etaMax*(2*drand48() - 1);
            etaAmp  = 1 + quadEta*eta*eta;
            randAmp = drand48()*etaMaxAmp;
        }

        // Put flow into phi.
        phi = 360*drand48();
        double h   = (1+v2)*drand48();
        while ( h > (1+v2*cos(2*phi*pi/180)) ) {
            phi = 360*drand48();
            h   = (1+v2)*drand48();
        }
        phi += phiOff;
        while (phi > 180) {
            phi -= 360;
        }

        pt  = 0.1 - 0.5 * log(drand48());
        pz  = sqrt( pt*pt + 0.139*0.139) * (exp(eta)-exp(-eta)) / 2;

        p[0] = pt*cos(phi*pi/180);
        p[1] = pt*sin(phi*pi/180);
        p[2] = pz;
        v[0] = 0;
        v[1] = 0;
        v[2] = estructEvent->Vz();

        // Check outer radius of track or radius of track at pad-plane,
        // which ever is bigger. Reject track if this is less than 137.
        if (maxRadius(eta,pt,v[2]) < 155.0) {
            continue;
        }

        if (!isTrackGood(v,p,eta)) {
            mTCuts->fillHistograms(false);
            continue;
        }
        mTCuts->fillHistograms(true);
        if (pt<0.15) continue;

        mnumTracks++;

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
        eTrack->SetTopologyMapData(0, 0xffffff80);
        eTrack->SetTopologyMapData(1, 0x3fff);
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

    estructEvent->SetCentrality( (double) mnumTracks );
    delete eTrack;
    return;
}


//-------------------------------------------------------------
// This method calculates maximum distance of track from beam axis.
// This point might be at the endplane or at twice the radius of curvature.
double StEStructFlat::maxRadius(double eta, double pt, double vz) {
    double pi = 3.14159265359;
    double lambda = pi/2 - 2*atan(exp(-eta));
    double s;
    if (lambda > 0) {
        s = (+200 - vz) / sin(lambda);
    } else {
        s = (-200 - vz) / sin(lambda);
    }
    double r = pt / 0.0015;
    double phi = s * cos(lambda) / r;
    if (phi < pi) {
        return r * sqrt(2 - 2*cos(phi));
    }
    return 2*r;
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
    return mnumTracks;
}
//--------------------------------------------------------------------------
double StEStructFlat::gRand48() {
    double x1, x2, w;
 
    if (mgRand2Good) {
        mgRand2Good = false;
        return mgRand2;
    }
    do {
        x1 = 2.0 * drand48() - 1.0;
        x2 = 2.0 * drand48() - 1.0;
        w = x1 * x1 + x2 * x2;
    } while ( w >= 1.0 );

    w = sqrt( (-2.0 * log( w ) ) / w );
    mgRand2 = x2 * w;
    mgRand2Good = true;
    return x1*w;
}



/**********************************************************************
 *
 * $Log: StEStructFlat.cxx,v $
 * Revision 1.7  2012/11/16 21:23:18  prindle
 * EventCuts and TrackCuts were moved to EventReader. Remove that code from
 * these readers.
 *
 * Revision 1.6  2006/04/06 01:03:30  prindle
 *
 *   Rationalization of centrality binning, as described in AnalysisMaker checkin.
 *
 * Revision 1.5  2006/02/22 22:05:35  prindle
 * Removed all references to multRef (?)
 *
 * Revision 1.4  2005/09/23 23:37:18  prindle
 *
 *   Starting to add vertex distribution and track acceptance dependance on
 * number of possible hits.
 *   Make Pythia interface look like Hijing interface so it now works within
 * my Fluctuation and Correlation framework.
 *
 * Revision 1.3  2005/09/07 20:22:47  prindle
 *
 *
 *     Flat: Random changes to eta and phi distributions (which don't have to be flat).
 *
 * Revision 1.2  2003/11/25 22:45:14  prindle
 * Commiting changes so I can move code to rhic
 *
 * Revision 1.1  2003/11/21 23:48:00  prindle
 * Include my toy event generator in cvs
 *
 *
 *********************************************************************/
