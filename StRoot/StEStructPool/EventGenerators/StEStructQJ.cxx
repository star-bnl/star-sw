/**********************************************************************
 *
 * $Id: StEStructQJ.cxx,v 1.4 2012/11/16 21:23:19 prindle Exp $
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

StEStructQJ::StEStructQJ(char *fileList): meventCount(0), meventsToDo(0), mAmDone(false) {
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
    if (!mECuts->goodCentrality(retVal->Centrality())) {
        delete retVal;
        retVal=NULL;
    } else {
        retVal->FillChargeCollections();
    }

    return retVal;
}   

//--------------------------------------------------------------------------
void StEStructQJ::fillTracks(StEStructEvent* estructEvent) {

    mnumTracks=0;
    StEStructTrack* eTrack = new StEStructTrack();
    int pid, totTracks;
    double px, py, eta;

    if ((totTracks = getNumTracks()) < 0) {
        mAmDone=true;
        return;
    }
    int it = 0;
    for(int i=0;i<totTracks;i++) {
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

        mnumTracks++;
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



/**********************************************************************
 *
 * $Log: StEStructQJ.cxx,v $
 * Revision 1.4  2012/11/16 21:23:19  prindle
 * EventCuts and TrackCuts were moved to EventReader. Remove that code from
 * these readers.
 *
 * Revision 1.3  2006/04/06 01:03:33  prindle
 *
 *   Rationalization of centrality binning, as described in AnalysisMaker checkin.
 *
 * Revision 1.2  2006/02/22 22:05:39  prindle
 * Removed all references to multRef (?)
 *
 * Revision 1.1  2004/03/02 21:50:58  prindle
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
