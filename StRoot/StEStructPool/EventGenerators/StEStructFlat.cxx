/**********************************************************************
 *
 * $Id: StEStructFlat.cxx,v 1.2 2003/11/25 22:45:14 prindle Exp $
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

StEStructFlat::StEStructFlat(): meventCount(0), meventsToDo(0), mAmDone(false), mECuts(0), mTCuts(0){};

StEStructFlat::StEStructFlat(int nevents, StEStructEventCuts* ecuts, StEStructTrackCuts* tcuts): meventCount(0), mAmDone(false){

  
  meventsToDo=nevents;
  mECuts=ecuts;
  mTCuts=tcuts;

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

    if(meventCount==meventsToDo){
        mAmDone=true;
        return (StEStructEvent*)NULL;
    }
    return generateEvent();
}

//--------------------------------------------------------------------------
StEStructEvent* StEStructFlat::generateEvent() {

    StEStructEvent* retVal=NULL;

    retVal = new StEStructEvent();

    fillTracks(retVal);
    if (!mECuts->goodNumberOfTracks(mrefMult)) {
        delete retVal;
        retVal=NULL;
    } else {
        retVal->FillChargeCollections();
    }

    return retVal;
}   

//--------------------------------------------------------------------------
void StEStructFlat::fillTracks(StEStructEvent* estructEvent) {

    mrefMult=0;
    StEStructTrack* eTrack = new StEStructTrack();
    int pid;

    int    numCharge = int( -5*log(drand48()) );
    double v2 = 0.2;
    double phiOff    = 3.1415926*(2*drand48() - 1);

    for(int i=0;i<2*numCharge;i++) {

        eTrack->SetInComplete();
        if (i < numCharge) {
            pid = 211;
        } else {
            pid = -211;
        }
        double p[3];
        double v[3];
        double pt  = 0.1 - 0.5 * log(drand48());
        double eta = 6*drand48() - 3;
        double pz  = sqrt( pt*pt + 0.139*0.139) * (exp(eta)-exp(-eta)) / 2;
        // Put flow into phi.
        double phi = 3.1415926*(2*drand48() - 1);
        double h   = (1+v2)*drand48();
        while ( h > (1+v2*cos(2*phi)) ) {
            phi = 3.1415926*(2*drand48() - 1);
            h   = (1+v2)*drand48();
        }
        phi += phiOff;
        if (phi > 3.1415926) {
            phi -= 2*3.1415926;
        } else if (phi < -3.1415926) {
            phi += 2*3.1415926;
        }
        p[0] = pt*cos(phi);
        p[1] = pt*sin(phi);
        p[2] = pz;
        for (int k=0;k<3;k++) {
            v[k]=0;
        }

        bool useTrack = true;
        useTrack = (mTCuts->goodEta(eta) && useTrack);
        useTrack = (mTCuts->goodPhi(phi) && useTrack);

        if (pt<0.15) continue;

        mrefMult++;
        useTrack = (mTCuts->goodPt(pt) && useTrack);
        float _r=pt/0.139;
        float yt=log(sqrt(1+_r*_r)+_r);
        useTrack = (mTCuts->goodYt(yt) && useTrack);
        mTCuts->fillHistograms(useTrack);
        if (!useTrack) continue;

        eTrack->SetBx(0);
        eTrack->SetBy(0);
        eTrack->SetBz(0);
        eTrack->SetBxGlobal(0);
        eTrack->SetByGlobal(0);
        eTrack->SetBzGlobal(0);

        eTrack->SetPx(p[0]);
        eTrack->SetPy(p[1]);
        eTrack->SetPz(p[2]);

        eTrack->SetEta(eta);
        eTrack->SetPhi(phi);

        if (pid<0) {
            eTrack->SetCharge(-1);
        } else {
            eTrack->SetCharge(1);
        }
        estructEvent->AddTrack(eTrack);
    }

    delete eTrack;
    return;
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
 * Revision 1.2  2003/11/25 22:45:14  prindle
 * Commiting changes so I can move code to rhic
 *
 * Revision 1.1  2003/11/21 23:48:00  prindle
 * Include my toy event generator in cvs
 *
 *
 *********************************************************************/
