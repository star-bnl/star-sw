int sortCompare ( const void * elem1, const void * elem2 ) {
    int *e1 = (int *) elem1;
    int *e2 = (int *) elem2;
    return (e1[1] - e2[1]);
}
int rSortCompare ( const void * elem1, const void * elem2 ) {
    double *e1 = (double *) elem1;
    double *e2 = (double *) elem2;
    return (e1[2] - e2[2]);
}
/**********************************************************************
 *
 * $Id: StEStructHijing.cxx,v 1.10 2012/11/16 21:28:32 prindle Exp $
 *
 * Author: Chunhui Han
 *
 **********************************************************************
 *
 * Description:  EStructEventReader wrapper for (T)Hijing event generator
 *
 **********************************************************************/
#include <stdlib.h>
#include "TRandom3.h"

#include "StEStructHijing.h"

#include "StEStructPool/AnalysisMaker/StEStructEventCuts.h"
#include "StEStructPool/AnalysisMaker/StEStructTrackCuts.h"
#include "StEStructPool/EventMaker/StEStructEvent.h"
#include "StEStructPool/EventMaker/StEStructTrack.h"
#include "StEStructPool/EventMaker/StEStructCentrality.h"

StEStructHijing::StEStructHijing() {
    mHijing             = 0;
    mInChain            = false;
    mEventsToDo         = 100;
    museImpactParameter = true;
    mEventCount         = 0;
    mAmDone             = false;
    mTrackList          = 0;
    rTrackList          = 0;
};

StEStructHijing::StEStructHijing(THijing* hijing,
                                 StEStructEventCuts* ecuts,
                                 StEStructTrackCuts* tcuts,
                                 bool useImpactParameter,
                                 int  eventsToDo) {
    mHijing             = hijing;
    mInChain            = false;
    mEventsToDo         = eventsToDo;
    museImpactParameter = useImpactParameter;
    mEventCount         = 0;
    mAmDone             = false;
    mTrackList          = 0;
    rTrackList          = 0;
};

bool StEStructHijing::hasGenerator() { return (mHijing) ? true : false; }

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
//    int rej = filterTracks();
//    int rej = filterTrackArea();

    retVal = new StEStructEvent();

    retVal->SetVx(0);
    retVal->SetVy(0);
    retVal->SetVz(0);
    retVal->SetBField(0.5);

    int   nTracks = countGoodTracks();
    float centMeasure;
    mImpact = mHijing->GetImpactParameter();
    if (museImpactParameter) {
      centMeasure = mImpact;
    } else {
      centMeasure = nTracks;
    }
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

        eTrack->SetPIDe_dEdx(10);
        eTrack->SetPIDpi_dEdx(10);
        eTrack->SetPIDk_dEdx(10);
        eTrack->SetPIDp_dEdx(10);
        eTrack->SetPIDd_dEdx(10);
        eTrack->SetPIDe_ToF(10);
        eTrack->SetPIDpi_ToF(10);
        eTrack->SetPIDk_ToF(10);
        eTrack->SetPIDp_ToF(10);
        eTrack->SetPIDd_ToF(10);
        if ((pid == 7) || (pid == 8)) {
            eTrack->SetPIDe_dEdx(0);
            eTrack->SetPIDe_ToF(0);
        } else if ((pid == -211) || (pid == 211)) {
            eTrack->SetPIDpi_dEdx(0);
            eTrack->SetPIDpi_ToF(0);
        } else if ((pid == -321) || (pid == 321)) {
            eTrack->SetPIDk_dEdx(0);
            eTrack->SetPIDk_ToF(0);
        } else if ((pid == -2212) || (pid == 2212)) {
            eTrack->SetPIDp_dEdx(0);
            eTrack->SetPIDp_ToF(0);
        } else if ((pid == -2213) || (pid == 2213)) {
            // These numbers aren't right, but I don't care about dueterons right now.
            eTrack->SetPIDd_dEdx(0);
            eTrack->SetPIDd_ToF(0);
        }

        eTrack->SetPx(p[0]);
        eTrack->SetPy(p[1]);
        eTrack->SetPz(p[2]);
        eTrack->SetEta(eta);
        eTrack->SetPhi(phi);
        eTrack->SetBeta(0);
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
bool StEStructHijing::isTrackGood(int it) {
    int i = it;
    bool useTrack = true;
    if (mTrackList) {
        i = mTrackList[2*it];
        if (mTrackList[2*it+1] < 0) {   // reject because of two-track efficiency
            useTrack = false;
        }
    }
    if (rTrackList) {
        i = (int) rTrackList[4*it];
        if (rTrackList[4*it+3] < 0) {   // reject because of general track efficiency
            useTrack = false;
        }
    }
    int pid = mHijing->GetPdg(i);
    if (!measureable(pid)) {     // checks if pi,k,p or e
        useTrack = false;
    }
    float p[3];
    float v[3];
    p[0] = mHijing->GetPx(i);
    p[1] = mHijing->GetPy(i);
    p[2] = mHijing->GetPz(i);
    v[0] = mHijing->GetVx(i);
    v[1] = mHijing->GetVy(i);
    v[2] = mHijing->GetVz(i);

    int q;
    pid < 0 ? q=-1 : q=+1;
    float pt = sqrt(p[0]*p[0]+p[1]*p[1]);
    float theta = acos(p[2]/ TMath::Sqrt(pt * pt + p[2] * p[2]) );
    float eta   = -1.0*log(tan(theta/2.0));
    float phi = atan2((double)p[1], (double)p[0]);
    float* gdca  = globalDCA(p,v);
    float _r = pt/0.139;
    float yt = log(sqrt(1+_r*_r)+_r);
    int iFrag = mHijing->GetOrigin(i);

    useTrack = (mTCuts->goodGlobalDCA(gdca[3]) && useTrack);
    useTrack = (mTCuts->goodCharge(q) && useTrack);
    useTrack = (mTCuts->goodPt(pt) && useTrack);
    useTrack = (mTCuts->goodEta(eta) && useTrack);
    useTrack = (mTCuts->goodPhi(phi) && useTrack);
    useTrack = (mTCuts->goodYt(yt) && useTrack);
    if (useTrack) {
        useTrack = mTCuts->goodFragment(iFrag);
    }
    delete [] gdca;

    return useTrack;
}

//-------------------------------------------------------------------------
//-------------------------------------------------------------
// This method bins tracks in (\eta,\phi) bins and applies track
// efficiencies to bins with more than one track.
int StEStructHijing::filterTracks() {
    int nRejBadEta = 0;
    int nRejNext = 0;
    int nRejLast = 0;
    if (mTrackList) {
        delete [] mTrackList;
    }
    double pi = acos(-1);
    int numParticles = mHijing->GetNParticles();

    // Determine bin number in (\eta,\phi) for each track.
    mTrackList = new int[2*numParticles];
    for (int i=0;i<numParticles;i++) {
        double px = mHijing->GetPx(i);
        double py = mHijing->GetPy(i);
        double pz = mHijing->GetPz(i);
        double phi = atan2(py,px);
        if (phi < 0) {
            phi += 2*pi;
        }
        int iphi = 150 * phi / (2*pi);
        double theta = acos(pz/ TMath::Sqrt(px*px + py*py + pz*pz) );
        double eta   = -1.0*log(tan(theta/2.0));
        int ieta = 75 * (1 + eta) / 2;
        int ibin = -1;
        if (0 <= ieta  && ieta < 75) {
            // Only use tracks in -1 <= eta <= 1
            ibin = 150 * ieta + iphi;
        }
        mTrackList[2*i] = i;
        mTrackList[2*i+1] = ibin;
    }

    // Sort by bin number
    qsort(mTrackList, numParticles, 2*sizeof(int), sortCompare);

    // Randomly reject tracks from bins that have more than one.
    TRandom3 ran;
    for (int i=0;i<numParticles-1;i++) {
        if (mTrackList[2*i+1] < 0) {
            mTrackList[2*i] = -1;
            nRejBadEta++;
        } else if (mTrackList[2*i+1] == mTrackList[2*i+3]) {
            if (ran.Rndm() > 1.0) {
                mTrackList[2*i+1] *= -1;
                nRejNext++;
            }
        } else if (mTrackList[2*i+1] == mTrackList[2*i-1]) {
            if (ran.Rndm() > 1.0) {
                mTrackList[2*i+1] *= -1;
                nRejLast++;
            }
        }
    }
    return nRejNext + nRejLast;
}
//-------------------------------------------------------------------------
//-------------------------------------------------------------
// This method applies tracking efficiency depending on track density in
// a broader area than filterTracks (that is essentially a two-track resolution).
// Sort list in eta. Step through tracks both toward +phi and -phi until we
// exceed DeltaPhi. For each track see if we are within DeltaEta. Count total
// number, apply efficiency and flag if it fails.
int StEStructHijing::filterTrackArea() {
    int nRejEta = 0;
    int nRejEff = 0;
    if (rTrackList) {
        delete [] rTrackList;
    }
    double pi = acos(-1);
    int numParticles = mHijing->GetNParticles();

    // Determine bin number in (\eta,\phi) for each track.
    rTrackList = new double[4*numParticles];
    for (int i=0;i<numParticles;i++) {
        double px = mHijing->GetPx(i);
        double py = mHijing->GetPy(i);
        double pz = mHijing->GetPz(i);
        double phi = atan2(py,px);
        double theta = acos(pz/ TMath::Sqrt(px*px + py*py + pz*pz) );
        double eta   = -1.0*log(tan(theta/2.0));
        rTrackList[4*i] = i;
        rTrackList[4*i+1] = phi;
        rTrackList[4*i+2] = eta;
        rTrackList[4*i+3] = 1;
    }

    // Sort by bin number
    qsort(rTrackList, numParticles, 4*sizeof(double), rSortCompare);

    // Consider each track.
    //   Count number of other tracks in DeltaPhi, DeltaEta
    double DeltaEta = 2.0/15;
    double DeltaPhi = 2.0*pi/15;
    int j;
    TRandom3 ran;
    for (int i=0;i<numParticles;i++) {
        int iDense = 0;
        double eta = rTrackList[4*i+2];
        if (fabs(eta)-1 > DeltaEta) {
            nRejEta++;
            continue;
        }
        double phi = rTrackList[4*i+1];
        double dPhi;
        j = i + 1;
        while ((j < numParticles) && (rTrackList[4*j+2]-eta < DeltaEta)) {
            // Think this works. Is it faster?
            dPhi = fmod(fabs(phi-rTrackList[4*j+1])+pi,2*pi) - pi;;
            if (fabs(dPhi) < DeltaPhi) {
                iDense++;
            }
            j++;
        }
        j = i - 1;
        while ((j >= 0) && (rTrackList[4*j+2]-eta < DeltaEta)) {
            dPhi = fmod(fabs(phi-rTrackList[4*j+1])+pi,2*pi) - pi;;
            if (fabs(dPhi) < DeltaPhi) {
                iDense++;
            }
            j--;
        }
        // Local track density is now iDense / (DeltaEta*DeltaPhi)
        double eff = 0.9 - 0.0025*iDense/(DeltaEta*DeltaPhi);
        eff = 2.0;
        if (ran.Rndm() > eff) {
            rTrackList[4*i+3] = -1;
            nRejEff++;
        }
    }
    return nRejEff;
}
//--------------------------------------------------------------------------
//-------------------------------------------------------------------------
//-------------------------------------------------------------
// This method counts all good track.
// No histogramming or copying data around.
int StEStructHijing::countGoodTracks() {
    mnumTracks = 0;
    int numParticles = mHijing->GetNParticles();
    for (int i=0;i<numParticles;i++) {
        if (isTrackGood(i)) {
            mnumTracks++;
        }
    }
    return mnumTracks;
}
void StEStructHijing::setHijingReader(THijing* hijing){
    if (mHijing) delete mHijing;
    mHijing = hijing;
};

/**********************************************************************
 *
 *********************************************************************/
