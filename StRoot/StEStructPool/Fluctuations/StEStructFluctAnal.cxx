
#include "StEStructFluctAnal.h"

#include "TH1F.h"
#include "TH2F.h"
#include "TFile.h"

#include "StEStructPool/EventMaker/StEStructEvent.h"
#include "StEStructPool/EventMaker/StEStructTrack.h"
#include "StTimer.hh"

#include <stdlib.h>


ClassImp(StEStructFluctAnal)

//--------------------------------------------------------------------------
StEStructFluctAnal::StEStructFluctAnal(int mode, int invokePairCuts,
            int etaSumMode, int phiSumMode): manalysisMode(mode), mskipPairCuts(false), mdoPairCutHistograms(false) {
    doingPairCuts  = invokePairCuts;
    etaSummingMode = etaSumMode;
    phiSummingMode = phiSumMode;
    mEtaMin = -1.1;
    mEtaMax =  1.1;
    mFluct   = 0;
    mPtFluct = 0;
    mnCentEvents = 0;
    mptnplus     = 0;
    mptnminus    = 0;
    mptpplus     = 0;
    mptpminus    = 0;

    mnCents = 0;
    mnPts = 0;
    mnPtCents = 0;
    init();
}

//--------------------------------------------------------------------------
StEStructFluctAnal::~StEStructFluctAnal() {
    cleanUp();
}


void StEStructFluctAnal::init() {
    mCurrentEvent=NULL;

    if (manalysisMode & 1) {
        mskipPairCuts = true;
    } else if (manalysisMode & 2) {
        mdoPairCutHistograms = true;
    }
    if (manalysisMode & 4) {
        mUseAllEtaTracks = true;
    } else {
        mUseAllEtaTracks = false;
    }


    ms  = new multStruct();
    initHistograms();
}
void StEStructFluctAnal::initCentralityObjects() {
    if (mFluct) {
        for (int i=0;i<mnCents;i++) {
            delete mFluct[i];  mFluct[i] = 0;
        }
        delete [] mFluct;
    }
    if (mPtFluct) {
        for (int i=0;i<mnPts;i++) {
            for (int j=0;j<mnPtCents*mnPts;j++) {
                delete mPtFluct[i];  mPtFluct[i] = 0;
            }
            delete [] mPtFluct[i];  mPtFluct[i] = 0;
        }
    delete [] mPtFluct;  mPtFluct = 0;
    }
    if (mnCentEvents) {
        delete [] mnCentEvents;  mnCentEvents = 0;
    }
    if (mptnplus) {
        delete [] mptnplus;  mptnplus = 0;
    }
    if (mptnminus) {
        delete [] mptnminus;  mptnminus = 0;
    }
    if (mptpplus) {
        delete [] mptpplus;  mptpplus = 0;
    }
    if (mptpminus) {
        delete [] mptpminus;  mptpminus = 0;
    }

    mnCents = mCentralities->numCentralities() - 1;
    mnTotEvents = 0;
    mnCentEvents = new int[mnCents];
    for (int i=0;i<mnCents;i++) {
        mnCentEvents[i] = 0;
    }

    mFluct = new StEStructFluct*[mnCents]; 
    char key[1024];
    for (int i=0;i<mnCents;i++) {
        sprintf(key,"%i",i);
        mFluct[i] = new StEStructFluct(key,mTotBins,mEtaMin,mEtaMax);
    }
    mnPts     = mCentralities->numPts() - 1;
    mnPtCents = mCentralities->numPtCentralities() - 1;
    mPtFluct = new StEStructFluct*[mnPtCents*mnPts]; 
    for (int i=0;i<mnPtCents;i++) {
        for (int j=0;j<mnPts;j++) {
            sprintf(key,"%i_%i",i,j);
            int index = j + i*mnPtCents;
            mPtFluct[index] = new StEStructFluct(key,mTotBins,mEtaMin,mEtaMax);
        }
    }

    mptnplus  = new double[mnPts];
    mptnminus = new double[mnPts];
    mptpplus  = new double[mnPts];
    mptpminus = new double[mnPts];
}

void StEStructFluctAnal::cleanUp() {
    delete ms;
    deleteHistograms();
}
// Parse cuts file for limits on eta.
void StEStructFluctAnal::setEtaLimits( const char* cutFileName ) {
    ifstream from(cutFileName);

    if(!from){ 
        cout<<" Cut file Not Found while looking for eta limits in StEStructFluctAnal::setEtaLimits"<<endl; 
    }

    char line[256], lineRead[256];
    char* puteol;
    char** val = new char*[100];
    int ival;

    bool done = false;
    while(!done) {
        if(from.eof()){
            cout<<" Did not find eta limits in StEStructFluctAnal::setEtaLimits, file "<< cutFileName <<endl; 
            done=true;
        } else {
            from.getline(lineRead,256);
            strcpy(line,lineRead);
            if ( (line[0]=='#') ) {
                continue;
            }
            if ( !strstr(line,"Eta") ) {
                continue;
            }
            if ((puteol=strstr(line,"#"))) {
                *puteol='\0';
            }
            ival=0;
            val[ival]=line;
            char* fcomma;
            while ((fcomma=strstr(val[ival],","))) {
                *fcomma='\0';
                fcomma++;
                ival++;
                val[ival]=fcomma;
            }
            if (ival==2) {
                done=true;
                break;
            }
        }
    }
    if (ival != 2) {
        cout << " Did not find a line containing Eta and two numbers in file " << cutFileName <<endl; 
    } else {
        mEtaMin = strtof(val[1],0);
        mEtaMax = strtof(val[2],0);
    }
    from.close();
    delete [] val;
}
// Parse cuts file for limits on Pt.
// Adjust minimum Pt of first Pt bin and maximum Pt of last Pt bin.
// Not really obvious I want to do this. Also why not adjust multiplicity
// limits in the same way?
void StEStructFluctAnal::adjustPtLimits( const char* cutFileName ) {
    ifstream from(cutFileName);

    if(!from){ 
        cout<<" Cut file Not Found while looking for eta limits in StEStructFluctAnal::adjustPtLimits"<<endl; 
    }

    char line[256], lineRead[256];
    char* puteol;
    char** val = new char*[100];
    int ival;

    bool done = false;
    while(!done) {
        if(from.eof()){
            cout<<" Did not find Pt limits in StEStructFluctAnal::adjustPtLimits, file "<< cutFileName <<endl; 
            done=true;
        } else {
            from.getline(lineRead,256);
            strcpy(line,lineRead);
            if ( (line[0]=='#') ) {
                continue;
            }
            if ( !strstr(line,"Pt") ) {
                continue;
            }
            if ((puteol=strstr(line,"#"))) {
                *puteol='\0';
            }
            ival=0;
            val[ival]=line;
            char* fcomma;
            while ((fcomma=strstr(val[ival],","))) {
                *fcomma='\0';
                fcomma++;
                ival++;
                val[ival]=fcomma;
            }
            if (ival==2) {
                done=true;
                break;
            }
        }
    }
    if (ival != 2) {
        cout << " Did not find a line containing Pt and two numbers in file " << cutFileName <<endl; 
    } else {
        double PtMin = strtof(val[1],0);
        double PtMax = strtof(val[2],0);
        if (PtMax <= PtMin) {
            cout << " Pt cuts must be wrong. PtMin = " << PtMin << ", PtMax = " << PtMax << endl;
        }
        double ptMax0 = mCentralities->ptLimit(1);
        if (PtMin > ptMax0) {
            cout << " New lower Pt cut, " << PtMin << " larger than max Pt cut of first Pt bin, " << ptMax0 << endl;
        }
        mCentralities->setPtLimit(0,PtMin);
        int nPts = mCentralities->numPts();
        double ptMinN = mCentralities->ptLimit(nPts-2);
        if (PtMax < ptMinN) {
            cout << " New upper Pt cut, " << PtMax << " smaller than min Pt cut of last Pt bin, " << ptMinN << endl;
        }
        mCentralities->setPtLimit(nPts,PtMax);
    }
    from.close();
    delete [] val;
}

//
//-------  Analysis Function ------//
//
//--------------------------------------------------------------------------
bool StEStructFluctAnal::doEvent(StEStructEvent* event) {
    if(!event) {
        return false;
    }

    if(2>event->Ntrack()) {
        delete event;
        return true;
    }

    moveEvents();
    mCurrentEvent=event;
    if (doingPairCuts) {
        if(!doPairCuts()) {
            return false;
        }
    }
    makeMultStruct();
    return true;
}

//--------------------------------------------------------------------------
void StEStructFluctAnal::moveEvents() {

    if (!mCurrentEvent) {
        return;
    }
    delete mCurrentEvent;  mCurrentEvent = 0;
//    if(mMixingEvent) delete mMixingEvent;
//    mMixingEvent=mCurrentEvent;

}


//--------------------------------------------------------------------------
bool StEStructFluctAnal::doPairCuts() {

  if(!mCurrentEvent) return false; // logic problem!

  pairCuts(mCurrentEvent,mCurrentEvent,0);
  pairCuts(mCurrentEvent,mCurrentEvent,1);
  pairCuts(mCurrentEvent,mCurrentEvent,2);

  return true;
}

//--------------------------------------------------------------------------
void StEStructFluctAnal::pairCuts(StEStructEvent* e1, StEStructEvent* e2, int j){

  if(j>=3) return;
  StEStructTrackCollection* tc1 = 0;
  StEStructTrackCollection* tc2 = 0;

  switch(j) {
    case 0: {
        tc1=e1->TrackCollectionP();
        tc2=e2->TrackCollectionP();
        mPair.setPairType(0);
        break;
    }
    case 1: {
        tc1=e1->TrackCollectionP();
        tc2=e2->TrackCollectionM();
        mPair.setPairType(1);
        break;
    }
    case 2: {
        tc1=e1->TrackCollectionM();
        tc2=e2->TrackCollectionM();
        mPair.setPairType(0);
        break;
    }
    default: {
        return;
    }
  }


  StEStructTrackIterator Iter1;
  StEStructTrackIterator Iter2;
  StEStructTrack* t;

  for(Iter1=tc1->begin(); Iter1!=tc1->end();++Iter1){

    mPair.SetTrack1(*Iter1);

    if(j==0 || j==2) { 
       Iter2=Iter1+1; 
    } else { 
       Iter2=tc2->begin(); 
    }

    for(; Iter2!=tc2->end(); ++Iter2){

      mPair.SetTrack2(*Iter2);
      if( mPair.cutPair(mdoPairCutHistograms) ){

          // When a pair of tracks fails cuts mark both of them somehow.
          t = *Iter1;
          t->SetChi2(-1.0);
          t = *Iter2;
          t->SetChi2(-1.0);

      };// pair cut
    };// iter2 loop
  };// iter 1 loop

}
// Modified 8/18/2004 djp
// Assume Chi2, Eta cuts done in Track cuts.
void StEStructFluctAnal::makeMultStruct() {
    StEStructTrackCollection* tc;
    StEStructTrackIterator Iter;
    StEStructTrack* t;

    int jEta, jPhi, jPt, jCent, jPtCent, totMult = 0;

    ms->NewEvent(mCurrentEvent->Vx(), mCurrentEvent->Vy(), mCurrentEvent->Vz() );
    if (mUseAllEtaTracks) {
        mCurrentEvent->SetCentrality( (double) mCurrentEvent->Ntrack() );
    }
    jCent   = mCurrentEvent->Centrality();
    jPtCent = mCurrentEvent->PtCentrality();

    tc = mCurrentEvent->TrackCollectionP();
    for(Iter=tc->begin(); Iter!=tc->end(); ++Iter){
        t = *Iter;
        jEta = int(NETABINS*(t->Eta()-mEtaMin)/(mEtaMax-mEtaMin));
        if ((jEta < 0) || (NETABINS <= jEta)) {
            continue;
        }
        jPhi = int(NPHIBINS*(1.0 + t->Phi()/3.141592654)/2.0);
        if ((jPhi < 0) || (NPHIBINS <= jPhi)) {
            continue;
        }
        jPt = mCentralities->ptIndex( t->Pt() );
        if (jPt < 0) {
            continue;
        }

        ms->AddTrack( jPhi, jEta, jPt, +1, t->Pt() );
        totMult++;

        StTrackTopologyMap *map = new StTrackTopologyMap(t->TopologyMapData(0),t->TopologyMapData(1));
        int iF=1, iL=45;
        for (int ifirst=0;ifirst<46;ifirst++) {
            if (map->hasHitInRow(kTpcId,ifirst)) {
                iF = ifirst;
                break;
            }
        }
        for (int ilast=45;ilast>0;ilast--) {
            if (map->hasHitInRow(kTpcId,ilast)) {
                iL = ilast;
                break;
            }
        }
        if ((-1 < jCent) && (jCent < mnCents)) {
            mFluct[jCent]->fillEtaZ( mCurrentEvent->Vz(), t->Eta(), t->NMaxPoints(),
                                     t->NFoundPoints(), t->NFitPoints(), iF, iL );
        }
        int index = jPt + jPtCent*mnPtCents;
        if ((-1 < index) && (index < mnPtCents*mnPts)) {
            mPtFluct[index]->fillEtaZ( mCurrentEvent->Vz(), t->Eta(), t->NMaxPoints(),
                                       t->NFoundPoints(), t->NFitPoints(), iF, iL );
        }
        delete map;
    }
    tc = mCurrentEvent->TrackCollectionM();
    for(Iter=tc->begin(); Iter!=tc->end(); ++Iter){
        t = *Iter;
        jEta = int(NETABINS*(t->Eta()-mEtaMin)/(mEtaMax-mEtaMin));
        if ((jEta < 0) || (NETABINS <= jEta)) {
            continue;
        }
        jPhi = int(NPHIBINS*(1.0 + t->Phi()/3.141592654)/2.0);
        if ((jPhi < 0) || (NPHIBINS <= jPhi)) {
            continue;
        }
        jPt = mCentralities->ptIndex( t->Pt() );
        if (jPt < 0) {
            continue;
        }

        ms->AddTrack( jPhi, jEta, jPt, -1, t->Pt() );
        totMult++;

        StTrackTopologyMap *map = new StTrackTopologyMap(t->TopologyMapData(0),t->TopologyMapData(1));
        int iF=1, iL=45;
        for (int ifirst=0;ifirst<46;ifirst++) {
            if (map->hasHitInRow(kTpcId,ifirst)) {
                iF = ifirst;
                break;
            }
        }
        for (int ilast=45;ilast>0;ilast--) {
            if (map->hasHitInRow(kTpcId,ilast)) {
                iL = ilast;
                break;
            }
        }
        if ((-1 < jCent) && (jCent < mnCents)) {
            mFluct[jCent]->fillEtaZ( mCurrentEvent->Vz(), t->Eta(), t->NMaxPoints(),
                                     t->NFoundPoints(), t->NFitPoints(), iF, iL );
        }
        int index = jPt + jPtCent*mnPtCents;
        if ((-1 < index) && (index < mnPtCents*mnPts)) {
            mPtFluct[index]->fillEtaZ( mCurrentEvent->Vz(), t->Eta(), t->NMaxPoints(),
                                       t->NFoundPoints(), t->NFitPoints(), iF, iL );
        }
        delete map;
    }

    AddEvent(ms);
    hRefMultiplicity->Fill(mCurrentEvent->CentMult());
    hMultiplicity->Fill(totMult);
}
void StEStructFluctAnal::AddEvent(multStruct *ms) {
    double delEta, delPhi;
    int jCent, jPtCent;

    mnTotEvents++;

    if ((jCent = mCurrentEvent->Centrality()) < 0) {
        return;
    }
    mnCentEvents[jCent]++;
    jPtCent = mCurrentEvent->PtCentrality();

    // First two loops define the scale.
    //    To try getting better performance I move all variable declarations
    //    outside these loops.
    int iPhi, jPhi, kPhi, lPhi, dPhi, iEta, jEta, kEta, lEta, dEta;
    int iBin, jPt;
    double NPlus, NMinus, PtPlus, PtMinus, Pt2Plus, Pt2Minus;
    double ptNPlus, ptNMinus, ptPtPlus, ptPtMinus, ptPt2Plus, ptPt2Minus;
    for (jPhi=0;jPhi<NPHIBINS;jPhi++) {
        dPhi = jPhi + 1;
        for (jEta=0;jEta<NETABINS;jEta++) {
            dEta = jEta + 1;

            // Second two loops are over all bins at this scale
            // Details of this are chosen by the summingMode.
            iBin = offset[jPhi][jEta];
            iPhi = 1;
            while ( (kPhi=getPhiStart(iPhi,dPhi)) >= 0) {
                iEta = 1;
                while ( (kEta=getEtaStart(iEta,dEta)) >= 0) {
                    iBin++;

                    // Next loop is over Pt binning.
                    // We keep bins that are summed over all pt in
                    // addition to bins for particular pt ranges.
                    NPlus    = 0, NMinus   = 0;
                    PtPlus   = 0, PtMinus  = 0;
                    Pt2Plus  = 0, Pt2Minus = 0;
                    for (jPt=0;jPt<mnPts;jPt++) {
                        // Final two loops sum the small bins.
                        ptNPlus    = 0, ptNMinus   = 0;
                        ptPtPlus   = 0, ptPtMinus  = 0;
                        ptPt2Plus  = 0, ptPt2Minus = 0;
                        for (lPhi=kPhi;lPhi<kPhi+dPhi;lPhi++) {
                            for (lEta=kEta;lEta<kEta+dEta;lEta++) {
                                ptNPlus    += ms->mTrackBinPlus[jPt][lPhi][lEta][0];
                                ptNMinus   += ms->mTrackBinMinus[jPt][lPhi][lEta][0];
                                ptPtPlus   += ms->mTrackBinPlus[jPt][lPhi][lEta][1];
                                ptPtMinus  += ms->mTrackBinMinus[jPt][lPhi][lEta][1];
                                ptPt2Plus  += ms->mTrackBinPlus[jPt][lPhi][lEta][2];
                                ptPt2Minus += ms->mTrackBinMinus[jPt][lPhi][lEta][2];
                            }
                        }
                        int index = jPt + jPtCent*mnPtCents;
                        if ((-1 < index) && (index < mnPtCents*mnPts)) {
                            mPtFluct[index]->AddToBin( iBin,
                                                       ptNPlus,   ptNMinus,
                                                       ptPtPlus,  ptPtMinus,
                                                       ptPt2Plus, ptPt2Minus );
                        }
                        NPlus    += ptNPlus;
                        NMinus   += ptNMinus;
                        PtPlus   += ptPtPlus;
                        PtMinus  += ptPtMinus;
                        Pt2Plus  += ptPt2Plus;
                        Pt2Minus += ptPt2Minus;
                    }

                    mFluct[jCent]->AddToBin( iBin,
                                             NPlus,   NMinus,
                                             PtPlus,  PtMinus,
                                             Pt2Plus, Pt2Minus );
                    iEta++;
                }
                iPhi++;
            }
        }
    }

    // Increment occupancy histograms.
    delEta = (mEtaMax-mEtaMin) / NETABINS;
    delPhi = 2.0*3.1415926 / NPHIBINS;
    double nplus = 0, nminus = 0, pplus = 0, pminus = 0;

    for (int jPt=0;jPt<mnPts;jPt++) {
        mptnplus[jPt]  = 0;
        mptnminus[jPt] = 0;
        mptpplus[jPt]  = 0;
        mptpminus[jPt] = 0;
    }
    for (int jPhi=0;jPhi<NPHIBINS;jPhi++) {
        double dp = -3.1415926 + delPhi*(jPhi+0.5);
        for (int jEta=0;jEta<NETABINS;jEta++) {
            double de = mEtaMin + delEta*(jEta+0.5);
            double nPlus = 0, nMinus = 0, pPlus = 0, pMinus = 0;
            for (int jPt=0;jPt<mnPts;jPt++) {
                double np, nm, pp, pm;
                np = ms->GetNPlus(jPhi,jEta,jPt);
                nm = ms->GetNMinus(jPhi,jEta,jPt);
                pp = ms->GetPtPlus(jPhi,jEta,jPt);
                pm = ms->GetPtMinus(jPhi,jEta,jPt);
                if (jPtCent >= 0) {
                    int index = jPt + jPtCent*mnPtCents;
                    mPtFluct[index]->fillOccupancies( dp, de, np, nm, pp, pm );

                    mptnplus[jPt]  += np;
                    mptnminus[jPt] += nm;
                    mptpplus[jPt]  += pp;
                    mptpminus[jPt] += pm;
                }
                nPlus  += np;
                nMinus += nm;
                pPlus  += pp;
                pMinus += pm;
            }
            mFluct[jCent]->fillOccupancies( dp, de, nPlus, nMinus, pPlus, pMinus );

            nplus  += nPlus;
            nminus += nMinus;
            pplus  += pPlus;
            pminus += pMinus;
        }
    }
    if (jPtCent >= 0) {
        for (int jPt=0;jPt<mnPts;jPt++) {
            double np, nm, pp, pm;
            np = mptnplus[jPt];
            nm = mptnminus[jPt];
            pp = mptpplus[jPt];
            pm = mptpminus[jPt];
            int index = jPt + jPtCent*mnPtCents;
            mPtFluct[index]->fillMults( np, nm, pp, pm );
        }
    }
    mFluct[jCent]->fillMults( nplus, nminus, pplus, pminus );
}
// iEta runs from 1 up to the number of etaBins that fit in,
// according to the binning mode.
// Return starting bin (first bin is called 0.)
// When iEta is too big return -1.
int StEStructFluctAnal::getEtaStart( int iEta, int dEta ) {
    if (dEta > NETABINS) {
        return -1;
    }
    if (1 == etaSummingMode) {
        int nEta = NETABINS / dEta;
        int oEta = NETABINS % dEta;
        if ( iEta*dEta <= NETABINS ) {
            return (iEta-1)*dEta;
        }
        if (0 == oEta) {
            return -1;
        }
        if (oEta+(iEta-nEta)*dEta <= NETABINS) {
            return oEta+(iEta-nEta-1)*dEta;
        }
        return -1;
    } else if (2 == etaSummingMode) {
        if (iEta+dEta <= NETABINS) {
            return iEta - 1;
        } else {
            return -1;
        }
    } else if (3 == etaSummingMode) {
        if (iEta > 1) {
            return -1;
        } else {
            return 0;
        }
    } else if (4 == etaSummingMode) {
        if (iEta > 1) {
            return -1;
        } else {
            return NETABINS-dEta-1;
        }
    } else if (5 == etaSummingMode) {
        if (iEta > 1) {
            return -1;
        } else {
            return (NETABINS-dEta) / 2;
        }
    }
    return -1;
}
int StEStructFluctAnal::getPhiStart( int iPhi, int dPhi ) {
    if (dPhi > NPHIBINS) {
        return -1;
    }
    if (1 == phiSummingMode) {
        int nPhi = NPHIBINS / dPhi;
        int oPhi = NPHIBINS % dPhi;
        if ( iPhi*dPhi <= NPHIBINS ) {
            return (iPhi-1)*dPhi;
        }
        if (0 == oPhi) {
            return -1;
        }
        if (oPhi+(iPhi-nPhi)*dPhi <= NPHIBINS) {
            return oPhi+(iPhi-nPhi-1)*dPhi;
        }
        return -1;
    } else if (2 == phiSummingMode) {
        if (iPhi+dPhi <= NPHIBINS) {
            return iPhi - 1;
        } else {
            return -1;
        }
    } else if (3 == phiSummingMode) {
        if (iPhi > 1) {
            return -1;
        } else {
            return 0;
        }
    } else if (4 == phiSummingMode) {
        if (iPhi > 1) {
            return -1;
        } else {
            return NPHIBINS-dPhi - 1;
        }
    } else if (5 == phiSummingMode) {
        if (iPhi > 1) {
            return -1;
        } else {
            return (NPHIBINS-dPhi) / 2;
        }
    }
    return -1;
}
int StEStructFluctAnal::getNumEtaBins( int dEta ) {
    if (1 == etaSummingMode) {
        int nEta = NETABINS / dEta;
        int oEta = NETABINS % dEta;
        if ( 0 == oEta ) {
            return nEta;
        } else {
            return 2 * nEta;
        }
    } else if (2 == etaSummingMode) {
        return NETABINS + 1 - dEta;
    } else if (3 == etaSummingMode) {
        return 1;
    } else if (4 == etaSummingMode) {
        return 1;
    } else if (5 == etaSummingMode) {
        return 1;
    }
    return 0;
}
int StEStructFluctAnal::getNumPhiBins( int dPhi ) {
    if (1 == phiSummingMode) {
        int nPhi = NPHIBINS / dPhi;
        int oPhi = NPHIBINS % dPhi;
        if ( 0 == oPhi ) {
            return nPhi;
        } else {
            return 2 * nPhi;
        }
    } else if (2 == phiSummingMode) {
        return NPHIBINS + 1 - dPhi;
    } else if (3 == phiSummingMode) {
        return 1;
    } else if (4 == phiSummingMode) {
        return 1;
    } else if (5 == phiSummingMode) {
        return 1;
    }
    return 0;
}

void StEStructFluctAnal::writeHistograms(TFile* tf){

    tf->cd();

    TH1F *hEtaLimits = new TH1F("EtaLimits","EtaLimits",2,1,2);
    hEtaLimits->SetBinContent(1,mEtaMin);
    hEtaLimits->SetBinContent(2,mEtaMax);
    hEtaLimits->Write();
    hRefMultiplicity->Write();
    hMultiplicity->Write();

    for (int i=0;i<mnCents;i++) {
        mFluct[i]->writeHistograms();
    }
    for (int i=0;i<mnPtCents;i++) {
        for (int j=0;j<mnPts;j++) {
            int index = j + i*mnPtCents;
            mPtFluct[index]->writeHistograms();
        }
    }

    // Here I write out the statistics needed to combine all jobs
    // and calculate \sigma^2/\bar n and the errors.
    // Write this out to a seperate file.

    hnBins->Write();
    hoffset->Write();
    hfUnique->Write();


    cout << "For this analysis we used doingPairCuts = " << doingPairCuts << endl;
    cout << "(0 means don't invoke pair cuts, 1 means invoke pair cuts.)" << endl;
    cout << endl;
    cout << "For this analysis we have used etaSummingMode = " << etaSummingMode << endl;
    cout << "For this analysis we have used phiSummingMode = " << phiSummingMode << endl;
    cout << "(1 = start at bin 0. Fit as many non-overlapping bins as possible" <<endl;
    cout << "   If it doesn't end evenly start at end and work back.)" << endl;
    cout << "(2 = start at bin 0, shift over one bin until we hit the end.)" << endl;
    cout << "(3 = use one bin at beginning.)" << endl;
    cout << "(4 = use one bin at end.)" << endl;
    cout << "(5 = use one bin near center.)" << endl;
    cout << endl;

    cout << "Looped over " << mnTotEvents << " total events" << endl;
    cout << "  For each centrality have ";
    for (int jCent=0;jCent<mnCents;jCent++) {
        cout << mnCentEvents[jCent] << "  ";
    }
    cout << endl;
}
void StEStructFluctAnal::writeQAHists(TFile* qatf) {
    qatf->cd();

    for (int i=0;i<mnCents;i++) {
        mFluct[i]->writeQAHistograms();
    }
    for (int i=0;i<mnPtCents;i++) {
        for (int j=0;j<mnPts;j++) {
            int index = j + i*mnPtCents;
            mPtFluct[index]->writeQAHistograms();
        }
    }

}

//--------------------------------------------------------------------------
void StEStructFluctAnal::setOutputFileName(const char* outFileName) {
}
void StEStructFluctAnal::initHistograms(){

    hRefMultiplicity = new TH1F("RefMultiplicity","RefMultiplicity",1500,1,1500);
    hMultiplicity = new TH1F("Multiplicity","Multiplicity",1500,1,1500);

    // Now we allocate arrays to store information.
    // Need to keep track of all bins at each scale.

    int off = 0;
    for (int jPhi=0;jPhi<NPHIBINS;jPhi++) {
        int dPhi = jPhi + 1;
        for (int jEta=0;jEta<NETABINS;jEta++) {
            int dEta = jEta + 1;
            int iBin = 0;
            double area = 0;
            int iPhi = 1, kPhi;
            while ( (kPhi=getPhiStart(iPhi,dPhi)) >= 0) {
                int iEta = 1, kEta;
                while ( (kEta=getEtaStart(iEta,dEta)) >= 0) {
                    area += double(dEta*dPhi);
                    iBin++;
                    iEta++;
                }
                iPhi++;
            }
            nBins[jPhi][jEta] = iBin;
            if (area < NPHIBINS*NETABINS) {
                fUnique[jPhi][jEta] = 1;
            } else {
                fUnique[jPhi][jEta] = double(NPHIBINS*NETABINS) / area;
            }
            offset[jPhi][jEta] = off;
            off += iBin;
        }
    }
    mTotBins = off;
cout << "Creating histograms for bins, uniqueness etc. " << endl;
    hnBins   = new TH2F("nBins",  "nBins",  NPHIBINS,0.5,NPHIBINS+0.5,NETABINS,0.5,NETABINS+0.5);
    hoffset = new TH2F("offset","offset",NPHIBINS,0.5,NPHIBINS+0.5,NETABINS,0.5,NETABINS+0.5);
    hfUnique = new TH2F("fUnique","fUnique",NPHIBINS,0.5,NPHIBINS+0.5,NETABINS,0.5,NETABINS+0.5);
    for (int iPhi=1;iPhi<=NPHIBINS;iPhi++) {
        int jPhi = iPhi - 1;
        for (int iEta=1;iEta<=NETABINS;iEta++) {
            int jEta = iEta - 1;
            hnBins->SetBinContent(iPhi,iEta,nBins[jPhi][jEta]);
            hoffset->SetBinContent(iPhi,iEta,offset[jPhi][jEta]);
            hfUnique->SetBinContent(iPhi,iEta,fUnique[jPhi][jEta]);
        }
    }
}

void StEStructFluctAnal::deleteHistograms() {
    delete hnBins;
    delete hoffset;
    delete hfUnique;
}



