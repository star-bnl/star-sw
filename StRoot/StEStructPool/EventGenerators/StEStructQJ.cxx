/**********************************************************************
 *
 * $Id: StEStructQJ.cxx,v 1.1 2004/03/02 21:50:58 prindle Exp $
 *
 * Author: Jeff Porter 
 *
 **********************************************************************
 *
 * Description:  EStructEventReader which reads Quing Juns format
 *
 **********************************************************************/
#include "StEStructQJ.h"

#include "StEStructPool/AnalysisMaker/StEStructEventCuts.h"
#include "StEStructPool/AnalysisMaker/StEStructTrackCuts.h"
#include "StEStructPool/EventMaker/StEStructEvent.h"
#include "StEStructPool/EventMaker/StEStructTrack.h"

StEStructQJ::StEStructQJ(char *fileList): meventCount(0), meventsToDo(0), mAmDone(false), mECuts(0), mTCuts(0){
    fileListName = fileList;
    fList = NULL;
    inFile = NULL;
};

StEStructQJ::StEStructQJ(char *fileList, int nevents, StEStructEventCuts* ecuts, StEStructTrackCuts* tcuts): meventCount(0), mAmDone(false){

    fileListName = fileList;
    fList = NULL;
    inFile = NULL;

    meventsToDo=nevents;
    mECuts=ecuts;
    mTCuts=tcuts;

};

bool StEStructQJ::hasGenerator() { return true; };
bool StEStructQJ::hasEventCuts() { return (mECuts) ? true : false ; }
bool StEStructQJ::hasTrackCuts() { return (mTCuts) ? true : false ; }


//-------------------------------------------------------------------------
void StEStructQJ::setSeed(int iseed) {
    cout << " Calling srand48(" << iseed << ")" << endl;
    srand48(iseed);
}

//-------------------------------------------------------------------------
StEStructEvent* StEStructQJ::next() {

    if(meventCount==meventsToDo){
        mAmDone=true;
        return (StEStructEvent*)NULL;
    }
    return generateEvent();
}

//--------------------------------------------------------------------------
StEStructEvent* StEStructQJ::generateEvent() {

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
void StEStructQJ::fillTracks(StEStructEvent* estructEvent) {

    mrefMult=0;
    StEStructTrack* eTrack = new StEStructTrack();
    int pid, numTracks;
    double px, py, eta;

    if ((numTracks = getNumTracks()) < 0) {
        mAmDone=true;
        return;
    }
    int it = 0;
    for(int i=0;i<numTracks;i++) {
        it ++;
        eTrack->SetInComplete();
        *inFile >> px >> py >> eta >> pid;
        if (inFile->eof()) {
            delete estructEvent;
            estructEvent = NULL;
            return;
        }
            
        double pt  = sqrt( px*px + py*py );
        double pz  = sqrt( pt*pt + 0.139*0.139) * (exp(eta)-exp(-eta)) / 2;
        double phi = atan2( py, px );

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

        eTrack->SetPx(px);
        eTrack->SetPy(py);
        eTrack->SetPz(pz);
        eTrack->SetEta(eta);
        eTrack->SetPhi(phi);

        if (pid<0) {
            eTrack->SetCharge(-1);
        } else {
            eTrack->SetCharge(1);
        }
        estructEvent->AddTrack(eTrack);
    }
    estructEvent->SetCentrality(impact);

    delete eTrack;
    return;
}

//--------------------------------------------------------------------------
int StEStructQJ::getNumTracks() {
   int nTot, nAcc, i;
   char buffer[1024];

   if (!fList) {
cout << "Opening file list " << fileListName << endl;
       fList = new std::ifstream(fileListName);
   }
   if (!inFile) {
       *fList >> buffer;
       if (fList->eof()) {
cout << "Got to end of fileList. Returning -1. (Should end program)" << endl;
           delete fList;
           return -1;
       }
cout << "About to open file " << buffer << endl;
       inFile = new std::ifstream(buffer);
   }
   *inFile >> nTot >> nAcc >> impact >> i >> i >> i >> i >> i >> i >> i;
   if (!inFile->eof()) {
       return nAcc;
   } else {
cout << "Read to end of file. Closing it and returning 0." << endl;
cout << "(Should allow us to go on to next file.)" << endl;
       delete inFile;
       inFile = NULL;
       return 0;
   }
}

//--------------------------------------------------------------------------
void StEStructQJ::setEventCuts(StEStructEventCuts* cuts) {

  if (mECuts) delete mECuts;
  mECuts=cuts;

};

//---------------------------------------------------------------
void StEStructQJ::setTrackCuts(StEStructTrackCuts* cuts) {
  if (mTCuts) delete mTCuts;
  mTCuts=cuts;
}




/**********************************************************************
 *
 * $Log: StEStructQJ.cxx,v $
 * Revision 1.1  2004/03/02 21:50:58  prindle
 * I forgot to cvs add my EventGenerator readers.
 *
 * Revision 1.2  2003/11/25 22:45:14  prindle
 * Commiting changes so I can move code to rhic
 *
 * Revision 1.1  2003/11/21 23:48:00  prindle
 * Include my toy event generator in cvs
 *
 *
 *********************************************************************/
