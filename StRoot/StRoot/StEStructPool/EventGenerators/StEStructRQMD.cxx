/**********************************************************************
 *
 * $Id: StEStructRQMD.cxx,v 1.4 2012/11/16 21:23:19 prindle Exp $
 *
 * Author: Jeff Porter 
 *
 **********************************************************************
 *
 * Description:  EStructEventReader which generates events Flat in eta,phi.
 *
 **********************************************************************/
#include "StEStructRQMD.h"

#include "StEStructPool/AnalysisMaker/StEStructEventCuts.h"
#include "StEStructPool/AnalysisMaker/StEStructTrackCuts.h"
#include "StEStructPool/EventMaker/StEStructEvent.h"
#include "StEStructPool/EventMaker/StEStructTrack.h"

StEStructRQMD::StEStructRQMD(): meventCount(0), meventsToDo(0), mAmDone(false) {
    mNFile    = 0;
    mIFile    = 0;
    mMaxFiles = 200;
    mFileDir  = "/aztera/estruct/aya/simulations/RQMD/AuAu200/with_rescattering/out/";
    mFile = NULL;
};

StEStructRQMD::StEStructRQMD(int nevents, StEStructEventCuts* ecuts, StEStructTrackCuts* tcuts): meventCount(0), mAmDone(false){

    mNFile    = 0;
    mIFile    = 0;
    mMaxFiles = 200;
    mFileDir  = "/aztera/estruct/aya/simulations/RQMD/AuAu200/with_rescattering/out/";
    mFile = NULL;
  
    meventsToDo=nevents;
    mECuts=ecuts;
    mTCuts=tcuts;
};

bool StEStructRQMD::hasGenerator() { return true; };


//-------------------------------------------------------------------------
StEStructEvent* StEStructRQMD::next() {

    if(meventCount==meventsToDo){
        mAmDone=true;
        return (StEStructEvent*)NULL;
    }
    return generateEvent();
}

//--------------------------------------------------------------------------
StEStructEvent* StEStructRQMD::generateEvent() {

    StEStructEvent* retVal=NULL;

    retVal = new StEStructEvent();

    fillTracks(retVal);
    retVal->SetCentrality( (float) mnumTracks );
    if (!mECuts->goodCentrality(retVal->Centrality())) {
        delete retVal;
        retVal=NULL;
    } else {
        retVal->FillChargeCollections();
    }

    return retVal;
}   

//--------------------------------------------------------------------------
void StEStructRQMD::fillTracks(StEStructEvent* estructEvent) {
    char buf[1024];
    int skip[] = {   2,  244,  368,  440,  520,  698,  826,  934, 1060,
                  1061, 1108, 1143, 1292, 1458, 1495, 1585, 1710, 1766,
                  1857, 2015, 2065, 2101, 2147, 2158, 2344, 2448};
    int nSkip = 26;

    const int kKMinus         = 12;
    const int kKPlus          = 11;
    const int kLambda         = 18;
    const int kLambdaBar      = 26;
    const int kProton         = 14;
    const int kPbar           = 15;
    const int kPhi            = 51;
    const int kPiMinus        =  9;
    const int kPiPlus         =  8;
    const int kPi0            =  7;
    const int kXi0            = 22;  
    const int kXiMinus        = 23;  
    const int kAntiXi0        = 30;  
    const int kAntiXiPlus     = 31;  
    const int kOmega          = 24;  
    const int kOmegaBar       = 32;  
    const int kSigmaPlus      = 19;
    const int kSigma0         = 20;
    const int kSigmaMinus     = 21;
    const int kAntiSigmaPlus  = 27;
    const int kAntiSigma0     = 28;
    const int kAntiSigmaMinus = 29;


    mnumTracks=0;
    StEStructTrack* eTrack = new StEStructTrack();

    char tDum[50];

    // On first call file will not have been open yet.
    // Also after reading event we check if we are at end of file. In that
    //  case we close the file.
    if (!mFile) {
        if (mIFile >= mMaxFiles) {
            mAmDone = true;
            return;
        }
        mNFile++;
        mIFile++;
        for (int i=0;i<nSkip;i++) {
            if (skip[i] == mNFile) {
                mNFile++;
                mIFile++;
            }
        }
        sprintf(buf, "%s%s%d", mFileDir, "file9_", mNFile);
        cout << "opening file " << buf << endl;
        mFile = new ifstream(buf);
        mLine = 0;
        if (mFile->is_open() == 0) {
            cout << "file not found, trying next one. "  << buf << endl;
            delete mFile;
            mFile = NULL;;
            return;
        }
        // Read and discard first line of file.
        *mFile >> tDum; mLine++;
        if (mFile->fail()) {
            cout << "Failed to read first line of file????, going on to next file" << endl;
            delete mFile;
            mFile = NULL;;
            return;
        }
    }

    int mNFreezeOutPart = 0;

    // read header and extract number of tracks in event
    *mFile >> tDum >> tDum >> tDum; mLine++;
    int tNucleonTarget = atoi(tDum);
    *mFile >> tDum >> tDum >> tDum; mLine++;
    *mFile >> tDum >> tDum >> tDum; mLine++;
    int tNucleonProj = atoi(tDum);
    *mFile >> tDum >> tDum >> tDum; mLine++;
    *mFile >> tDum >> tDum >> tDum;  mLine++;
    *mFile >> tDum >> tDum >> tDum; mLine++;
    *mFile >> tDum >> tDum >> tDum; mLine++;
    *mFile >> tDum >> tDum >> tDum; mLine++;
    *mFile >> mNFreezeOutPart >> tDum >> tDum >> tDum; mLine++;
    mNFreezeOutPart += (tNucleonTarget+tNucleonProj);
    *mFile >> tDum >> tDum >> tDum >> tDum >> tDum; mLine++;

    if ((mNFreezeOutPart < 394) || (15000 < mNFreezeOutPart)) {
        cout << "An unusual number of tracks in this event??  ";
        cout << mNFreezeOutPart << " tracks at line " << mLine << endl;
    }

    // read event
    int tPid1,tPid2,tCharge,tNClnt,tlastcl;
    float tt,tx,ty,tz,tE,tPx,tPy,tPz,tM,tDecay;
     
    while (mNFreezeOutPart) {
        eTrack->SetInComplete();
        int gPID = 0;
        *mFile >> tPid1 >> tPid2 >> tt >> tx >> ty >> tz 
                  >> tE >> tPx >> tPy >> tPz >> tM 
                  >> tDecay >> tNClnt >> tlastcl ; mLine++;

        float pt  = sqrt(pow(tPx,2)+pow(tPy,2));
        float eta = getPseudoRapidity(pt, tPz);
        float phi = atan2(tPy, tPx);
        mNFreezeOutPart--;
        
        tCharge = 0;
        if (tPid1 == 14 && tPid2 == -2) { // K-
            gPID = kKMinus;
            tCharge = -1;
        }
        if (tPid1 == 14 && tPid2 == 2) { // K+
            gPID = kKPlus;
            tCharge = +1;
        }
        if (tPid1 == 13 && tPid2 == 0) { // Lambda
            gPID = kLambda;
            tCharge = 0;
        }
        if (tPid1 == 99 && tPid2 == -57) { // Lambdabar
            gPID = kLambdaBar;
            tCharge = 0;
        }
        if (tPid1 == 2 && tPid2 == 0) { // proton
            gPID = kProton;
            tCharge = +1;
        }
        if (tPid1 == 15 && tPid2 == 1) { // SigmaPlus
            gPID = kSigmaPlus;
            tCharge = +1;
        }
        if (tPid1 == 15 && tPid2 == 0) { // Sigma0
            gPID = kSigma0;
            tCharge = 0;
        }
        if (tPid1 == 15 && tPid2 == -1) { // SigmaMinus
            gPID = kSigmaMinus;
            tCharge = -1;
        }
        if (tPid1 == 99 && tPid2 == -43) { // AntiSigmaPlus
            gPID = kAntiSigmaPlus;
            tCharge = -1;
        }
        if (tPid1 == 99 && tPid2 == -44) { // AntiSigma0
            gPID = kAntiSigma0;
            tCharge = 0;
        }
        if (tPid1 == 99 && tPid2 == -45) { // AntiSigmaMinus
            gPID = kAntiSigmaMinus;
            tCharge = +1;
        }
        if (tPid1 == 99 && tPid2 == -41) { // pbar
            gPID = kPbar;
            tCharge = -1;
        }
        if (tPid1 == 99 && tPid2 == 35) { // phi
            gPID = kPhi;
            tCharge = 0;
        }
        if (tPid1 == 7 && tPid2 == 0) { // pi-
            gPID = kPiMinus;
            tCharge = -1;
        }
        if (tPid1 == 9 && tPid2 == 0) { // pi+
            gPID = kPiPlus;
            tCharge = +1;
        }
        if (tPid1 == 8 && tPid2 == 0) { // pi0
            gPID = kPi0;
            tCharge = 0;
        }
        if (tPid1 == 99 && tPid2 == 46) { // Xi0
            gPID = kXi0;
            tCharge = 0;
        }
        if (tPid1 == 99 && tPid2 == 47) { // XiMinus
            gPID = kXiMinus;
            tCharge = -1;
        }
        if (tPid1 == 99 && tPid2 == -46) { // Anti-Xi0
            gPID = kAntiXi0;
            tCharge = 0;
        }
        if (tPid1 == 99 && tPid2 == -47) { // Anti-XiPlus
            gPID = kAntiXiPlus;
            tCharge = +1;
        }
        if (tPid1 == 99 && tPid2 == 70) { // Omega
            gPID = kOmega;
            tCharge = -1;
        }
        if (tPid1 == 99 && tPid2 == -70) { // Anti-Omega
            gPID = kOmegaBar;
            tCharge = +1;
        }
        if (0 == tCharge) {
            continue;
        }

        bool useTrack = true;
        useTrack = (mTCuts->goodEta(eta) && useTrack);
        useTrack = (mTCuts->goodPhi(phi) && useTrack);

        if (pt<0.15) continue;

        useTrack = (mTCuts->goodPt(pt) && useTrack);
        float _r=pt/tM;
        float yt=log(sqrt(1+_r*_r)+_r);
        useTrack = (mTCuts->goodYt(yt) && useTrack);
        mTCuts->fillHistograms(useTrack);
        if (!useTrack) continue;
        mnumTracks++;

        eTrack->SetBx(0);
        eTrack->SetBy(0);
        eTrack->SetBz(0);
        eTrack->SetBxGlobal(0);
        eTrack->SetByGlobal(0);
        eTrack->SetBzGlobal(0);

        eTrack->SetPx(tPx);
        eTrack->SetPy(tPy);
        eTrack->SetPz(tPz);

        eTrack->SetEta(eta);
        eTrack->SetPhi(phi);
        eTrack->SetCharge(tCharge);

        estructEvent->AddTrack(eTrack);
    }

    // Read first line of event header for next event.
    // If we just read last event this will cause eof.
    *mFile >> tDum; mLine++;
    if (mFile->eof()) {
        cout << "End of file. Closing it." << endl;
        delete mFile;
        mFile = NULL;
    }

    delete eTrack;
    return;
}
//--------------------------------------------------------------------------
float StEStructRQMD::getRapidity(float E, float pz) {
    float y = 0.5*log((E + pz)/(E - pz));
    return y;
}

//--------------------------------------------------------------------------
float StEStructRQMD::getPseudoRapidity(float pt, float pz) {
    float theta = fabs(atan(pt/pz));
    float eta = -log(tan(theta/2.));
    if (pz > 0) {
        return eta;
    } else {
        return -eta;
    }
}



/**********************************************************************
 *
 * $Log: StEStructRQMD.cxx,v $
 * Revision 1.4  2012/11/16 21:23:19  prindle
 * EventCuts and TrackCuts were moved to EventReader. Remove that code from
 * these readers.
 *
 * Revision 1.3  2006/04/06 01:03:35  prindle
 *
 *   Rationalization of centrality binning, as described in AnalysisMaker checkin.
 *
 * Revision 1.2  2006/02/22 22:05:42  prindle
 * Removed all references to multRef (?)
 *
 * Revision 1.1  2004/03/02 21:51:01  prindle
 *
 *   I forgot to cvs add my EventGenerator readers.
 *
 * Revision 1.2  2003/11/25 22:45:14  prindle
 * Commiting changes so I can move code to rhic
 *
 * Revision 1.1  2003/11/21 23:48:00  prindle
 * Include my toy event generator in cvs
 *
 *
 *********************************************************************/
