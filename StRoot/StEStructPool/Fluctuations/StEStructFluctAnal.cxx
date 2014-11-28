
#include "StEStructFluctAnal.h"

#include "TH1F.h"
#include "TH2F.h"
#include "TFile.h"

#include "StEStructPool/EventMaker/StEStructEvent.h"
#include "StEStructPool/EventMaker/StEStructTrack.h"
#include "StEStructPool/AnalysisMaker/StEStructQAHists.h"
#include "StTimer.hh"

#include <stdlib.h>


ClassImp(StEStructFluctAnal)

//--------------------------------------------------------------------------
StEStructFluctAnal::StEStructFluctAnal(int mode, int etaSumMode, int phiSumMode): manalysisMode(mode)  {
    mEtaSumMode      = etaSumMode;
    mPhiSumMode      = phiSumMode;
    mCurrentEvent = 0;
    mCentralities = 0;

    mEtaMin = -1.0;
    mEtaMax =  1.0;
    mPtMin  =  0.0;
    mPtMax  = 99.9;

    mnTotEvents  = 0;
    mnCentEvents = 0;
    mnTotBins    = 0;
    mnCents      = 0;
    mnPts        = 0;
    mnPtCents    = 0;


    mFluct    = 0;
    mPtFluct  = 0;
    mptnplus  = 0;
    mptnminus = 0;
    mptpplus  = 0;
    mptpminus = 0;

    mAmDone = false;
    mQAHists=NULL;
    mlocalQAHists = false;
    mPairCuts = new StEStructPairCuts; 

    initHistograms();
}

//--------------------------------------------------------------------------
StEStructFluctAnal::~StEStructFluctAnal() {
    delete ms;  ms = 0;
    deleteHistograms();
    deleteCentralityObjects();
}

void StEStructFluctAnal::initStructures(StEStructCuts *tcut){
  //(const char* cutFileName) {
// Want to expand range of eta cuts beyond range used in analysis so that we
// can add a vertex dependent offset.
//    setEtaLimits(cutFileName);
//    setPtLimits(cutFileName);
  setEtaLimits(tcut);
  setPtLimits(tcut);

    mCentralities = StEStructCentrality::Instance(); // dynamic_cast<StEStructCentrality*>(cent);
    if (!mCentralities) {
        printf("Invalid centrality object passed into StEStructFluctAnal::setCutFile!!!!!\n");
    }
    createCentralityObjects();
    ms = new multStruct(mCentralities->numPts());
    //    mPairCuts->setCutFile(cutFileName);
    //    mPairCuts->loadCuts();
};
void StEStructFluctAnal::createCentralityObjects() {
    deleteCentralityObjects();

    mnCents = mCentralities->numCentralities() - 1;
    mnCentEvents = new int[mnCents];
    for (int jC=0;jC<mnCents;jC++) {
        mnCentEvents[jC] = 0;
    }

    mFluct = new StEStructFluct*[mnCents]; 
    char key[1024];
    for (int jC=0;jC<mnCents;jC++) {
        sprintf(key,"%i",jC);
        mFluct[jC] = new StEStructFluct(key,mnTotBins,mEtaMin,mEtaMax,mPtMin,mPtMax);
    }
    mnPts     = mCentralities->numPts() - 1;
    mnPtCents = mCentralities->numPtCentralities() - 1;
    mPtFluct = new StEStructFluct*[mnPtCents*mnPts]; 
    for (int jC=0;jC<mnPtCents;jC++) {
        for (int jP=0;jP<mnPts;jP++) {
            sprintf(key,"%i_%i",jC,jP);
            int index = jP + jC*mnPts;
            mPtFluct[index] = new StEStructFluct(key,mnTotBins,mEtaMin,mEtaMax,mPtMin,mPtMax);
        }
    }

    mptnplus  = new double[mnPts];
    mptnminus = new double[mnPts];
    mptpplus  = new double[mnPts];
    mptpminus = new double[mnPts];
}
void StEStructFluctAnal::deleteCentralityObjects() {
    if (mFluct) {
        for (int i=0;i<mnCents;i++) {
            delete mFluct[i];  mFluct[i] = 0;
        }
        delete [] mFluct; mFluct = 0;
    }
    if (mPtFluct) {
        for (int i=0;i<mnPts*mnPtCents;i++) {
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
}

void StEStructFluctAnal::setEtaLimits( StEStructCuts* tcut){

  if(!tcut){
    cout<<"Error Track Cuts Not found to get eta range "<<endl;
    assert(tcut);
  } 

  mEtaMin=tcut->minVal("Eta");
  mEtaMax=tcut->maxVal("Eta");
    
};

void StEStructFluctAnal::setPtLimits( StEStructCuts* tcut){

  if(!tcut){
    cout<<"Error Track Cuts Not found to get eta range "<<endl;
    assert(tcut);
  } 

  mPtMin=tcut->minVal("Pt");
  mPtMax=tcut->maxVal("Pt");
    
}

//
//-------  Analysis Function ------//
//
//--------------------------------------------------------------------------
bool StEStructFluctAnal::doEvent(StEStructEvent* event) {
    if (!event) {
        return false;
    }

    if (1 > event->Ntrack()) {
        delete event;  event = 0;
        return true;
    }

    if (mCurrentEvent) {
        delete mCurrentEvent;
    }
    mCurrentEvent=event;
    fillMultStruct();
    return true;
}

//--------------------------------------------------------------------------

// Modified 8/18/2004 djp
// Assume Chi2 cut done in Track cuts.
void StEStructFluctAnal::fillMultStruct() {
    StEStructTrackCollection* tc;
    StEStructTrackIterator Iter;
    StEStructTrack* t;

    int   jEta, jPhi, jPt, jCent, jPtCent, totMult = 0;
    float totPt = 0;

    ms->NewEvent(mCurrentEvent->Vx(), mCurrentEvent->Vy(), mCurrentEvent->Vz() );
    jCent = mCentralities->centrality( mCurrentEvent->Centrality() );
    if ((jCent < 0) || (mnCents <= jCent)) {
        return;
    }
    jPtCent = mCentralities->ptCentrality( mCurrentEvent->Centrality() );
    double etaOff = etaOffset( mCurrentEvent->Vz() );

    tc = mCurrentEvent->TrackCollectionP();
    for(Iter=tc->begin(); Iter!=tc->end(); ++Iter){
        t = *Iter;
        if ( (t->Eta() > mEtaMax+etaOff) || (mEtaMin+etaOff > t->Eta()) ) {
            continue;
        }
        jEta = int(NETABINS*(t->Eta()-(mEtaMin+etaOff))/(mEtaMax-mEtaMin));
        if ((jEta < 0) || (NETABINS <= jEta)) {
            continue;
        }
        jPhi = int(NPHIBINS*(1.0 + t->Phi()/3.141592654)/2.0);
        if ((jPhi < 0) || (NPHIBINS <= jPhi)) {
            continue;
        }
        jPt = mCentralities->ptIndex( t->Pt() );

        ms->AddTrack( jPhi, jEta, jPt, +1, t->Pt() );
        totMult++;
        totPt += t->Pt();


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
        mFluct[jCent]->fillEtaZ( mCurrentEvent->Vz(), t->Eta(), t->NMaxPoints(),
                                 t->NFoundPoints(), t->NFitPoints(), iF, iL );
        mFluct[jCent]->fillPtHist( t->Pt(), +1 );
        mFluct[jCent]->fillPhiHist( t->Phi(), +1 );
        mFluct[jCent]->fillEtaHist( t->Eta(), +1 );
        int index = jPt + jPtCent*mnPts;
        if ((-1 < index) && (index < mnPtCents*mnPts)) {
            mPtFluct[index]->fillEtaZ( mCurrentEvent->Vz(), t->Eta(), t->NMaxPoints(),
                                       t->NFoundPoints(), t->NFitPoints(), iF, iL );
            mPtFluct[index]->fillPtHist( t->Pt(), +1 );
            mPtFluct[index]->fillPtHist( t->Phi(), +1 );
            mPtFluct[index]->fillPtHist( t->Eta(), +1 );
        }
        delete map;  map = 0;
    }
    tc = mCurrentEvent->TrackCollectionM();
    for(Iter=tc->begin(); Iter!=tc->end(); ++Iter){
        t = *Iter;
        if ( (t->Eta() > mEtaMax+etaOff) || (mEtaMin+etaOff > t->Eta()) ) {
            continue;
        }
        jEta = int(NETABINS*(t->Eta()-(mEtaMin+etaOff))/(mEtaMax-mEtaMin));
        if ((jEta < 0) || (NETABINS <= jEta)) {
            continue;
        }
        jPhi = int(NPHIBINS*(1.0 + t->Phi()/3.141592654)/2.0);
        if ((jPhi < 0) || (NPHIBINS <= jPhi)) {
            continue;
        }
        jPt = mCentralities->ptIndex( t->Pt() );

        ms->AddTrack( jPhi, jEta, jPt, -1, t->Pt() );
        totMult++;
        totPt += t->Pt();

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
        mFluct[jCent]->fillEtaZ( mCurrentEvent->Vz(), t->Eta(), t->NMaxPoints(),
                                 t->NFoundPoints(), t->NFitPoints(), iF, iL );
        mFluct[jCent]->fillPtHist( t->Pt(), -1 );
        mFluct[jCent]->fillPhiHist( t->Phi(), -1 );
        mFluct[jCent]->fillEtaHist( t->Eta(), -1 );
        int index = jPt + jPtCent*mnPts;
        if ((-1 < index) && (index < mnPtCents*mnPts)) {
            mPtFluct[index]->fillEtaZ( mCurrentEvent->Vz(), t->Eta(), t->NMaxPoints(),
                                       t->NFoundPoints(), t->NFitPoints(), iF, iL );
            mPtFluct[index]->fillPtHist( t->Pt(), -1 );
            mPtFluct[index]->fillPtHist( t->Phi(), -1 );
            mPtFluct[index]->fillPtHist( t->Eta(), -1 );
        }
        delete map;
    }

    AddEvent();
    hMultiplicity->Fill(totMult);
    hMultiplicityBinned->Fill(pow((double)totMult,0.25));
    hPt->Fill(totPt);
    hPtBinned->Fill(pow((double)totPt,0.25));
}
void StEStructFluctAnal::AddEvent() {
    double delEta, delPhi;
    int jCent, jPtCent;

    mnTotEvents++;

    jCent = mCentralities->centrality( mCurrentEvent->Centrality() );
    if ((jCent < 0) || (mnCents <= jCent)) {
        return;
    }
    mnCentEvents[jCent]++;
    jPtCent = mCentralities->ptCentrality( mCurrentEvent->Centrality() );

    // First two loops define the scale.
    //    To try getting better performance I move all variable declarations
    //    outside these loops.
    int iPhi, jPhi, kPhi, lPhi, dPhi, iEta, jEta, kEta, lEta, dEta;
    int jBin, jPt;
    double NPlus, NMinus, PtPlus, PtMinus, Pt2Plus, Pt2Minus;
    double ptNPlus, ptNMinus, ptPtPlus, ptPtMinus, ptPt2Plus, ptPt2Minus;
    for (jPhi=0;jPhi<NPHIBINS;jPhi++) {
        dPhi = jPhi + 1;
        for (jEta=0;jEta<NETABINS;jEta++) {
            dEta = jEta + 1;

            // Second two loops are over all bins at this scale
            // Details of this are chosen by the summingMode.
            jBin = offset[jPhi][jEta];
            iPhi = 1;
            while ( (kPhi=getPhiStart(iPhi,dPhi)) >= 0) {
                iEta = 1;
                while ( (kEta=getEtaStart(iEta,dEta)) >= 0) {

                    // Next loop is over Pt binning.
                    // We keep bins that are summed over all pt in
                    // addition to bins for particular pt ranges.
                    //
                    // In case our pt binning doesn't cover entire
                    // pt cut range we accumulate `extra' in bin with jPt=mnPts.
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
                        int index = jPt + jPtCent*mnPts;
                        if ((-1 < index) && (index < mnPtCents*mnPts)) {
                            mPtFluct[index]->AddToBin( jBin,
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

                    mFluct[jCent]->AddToBin( jBin,
                                             NPlus,   NMinus,
                                             PtPlus,  PtMinus,
                                             Pt2Plus, Pt2Minus );
                    jBin++;
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
            for (int jPt=0;jPt<=mnPts;jPt++) {
                double np, nm, pp, pm;
                np = ms->GetNPlus(jPhi,jEta,jPt);
                nm = ms->GetNMinus(jPhi,jEta,jPt);
                pp = ms->GetPtPlus(jPhi,jEta,jPt);
                pm = ms->GetPtMinus(jPhi,jEta,jPt);
                if (jPt<mnPts) {
                    if (jPtCent >= 0) {
                        int index = jPt + jPtCent*mnPts;
                        mPtFluct[index]->fillOccupancies( dp, de, np, nm, pp, pm );

                        mptnplus[jPt]  += np;
                        mptnminus[jPt] += nm;
                        mptpplus[jPt]  += pp;
                        mptpminus[jPt] += pm;
                    }
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
            int index = jPt + jPtCent*mnPts;
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
    if (1 == mEtaSumMode) {
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
    } else if (2 == mEtaSumMode) {
        if (iEta+dEta <= NETABINS) {
            return iEta - 1;
        } else {
            return -1;
        }
    } else if (3 == mEtaSumMode) {
        if (iEta > 1) {
            return -1;
        } else {
            return 0;
        }
    } else if (4 == mEtaSumMode) {
        if (iEta > 1) {
            return -1;
        } else {
            return NETABINS-dEta-1;
        }
    } else if (5 == mEtaSumMode) {
        if (iEta > 1) {
            return -1;
        } else {
            return (NETABINS-dEta) / 2;
        }
    } else if (6 == mEtaSumMode) {
        int nEta = NETABINS / dEta;
        if (iEta > nEta) {
            return -1;
        }
        int oEta = (NETABINS % dEta) / 2;
        return oEta + (iEta-1)*dEta;
    }
    return -1;
}
int StEStructFluctAnal::getPhiStart( int iPhi, int dPhi ) {
    if (dPhi > NPHIBINS) {
        return -1;
    }
    if (1 == mPhiSumMode) {
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
    } else if (2 == mPhiSumMode) {
        if (iPhi+dPhi <= NPHIBINS) {
            return iPhi - 1;
        } else {
            return -1;
        }
    } else if (3 == mPhiSumMode) {
        if (iPhi > 1) {
            return -1;
        } else {
            return 0;
        }
    } else if (4 == mPhiSumMode) {
        if (iPhi > 1) {
            return -1;
        } else {
            return NPHIBINS-dPhi - 1;
        }
    } else if (5 == mPhiSumMode) {
        if (iPhi > 1) {
            return -1;
        } else {
            return (NPHIBINS-dPhi) / 2;
        }
    } else if (6 == mPhiSumMode) {
        int nPhi = NPHIBINS / dPhi;
        if (iPhi > nPhi) {
            return -1;
        }
        int oPhi = (NPHIBINS % dPhi) / 2;
        return oPhi + (iPhi-1)*dPhi;
    }
    return -1;
}
int StEStructFluctAnal::getNumEtaBins( int dEta ) {
    if (1 == mEtaSumMode) {
        int nEta = NETABINS / dEta;
        int oEta = NETABINS % dEta;
        if ( 0 == oEta ) {
            return nEta;
        } else {
            return 2 * nEta;
        }
    } else if (2 == mEtaSumMode) {
        return NETABINS + 1 - dEta;
    } else if (3 == mEtaSumMode) {
        return 1;
    } else if (4 == mEtaSumMode) {
        return 1;
    } else if (5 == mEtaSumMode) {
        return 1;
    } else if (5 == mEtaSumMode) {
        int nEta = NETABINS / dEta;
        return nEta;
    }
    return 0;
}
int StEStructFluctAnal::getNumPhiBins( int dPhi ) {
    if (1 == mPhiSumMode) {
        int nPhi = NPHIBINS / dPhi;
        int oPhi = NPHIBINS % dPhi;
        if ( 0 == oPhi ) {
            return nPhi;
        } else {
            return 2 * nPhi;
        }
    } else if (2 == mPhiSumMode) {
        return NPHIBINS + 1 - dPhi;
    } else if (3 == mPhiSumMode) {
        return 1;
    } else if (4 == mPhiSumMode) {
        return 1;
    } else if (5 == mPhiSumMode) {
        return 1;
    } else if (5 == mPhiSumMode) {
        int nPhi = NPHIBINS / dPhi;
        return nPhi;
    }
    return 0;
}

void StEStructFluctAnal::writeHistograms(){

    TH1F *hEtaLimits = new TH1F("EtaLimits","EtaLimits",2,1,2);
    hEtaLimits->SetBinContent(1,mEtaMin);
    hEtaLimits->SetBinContent(2,mEtaMax);
    hEtaLimits->Write();
    delete hEtaLimits;  hEtaLimits = 0;
    hMultiplicity->Write();
    hMultiplicityBinned->Write();
    hPt->Write();
    hPtBinned->Write();

    for (int i=0;i<mnCents;i++) {
        mFluct[i]->writeHistograms();
    }
    for (int i=0;i<mnPtCents;i++) {
        for (int j=0;j<mnPts;j++) {
            int index = j + i*mnPts;
            mPtFluct[index]->writeHistograms();
        }
    }

    // Here I write out the statistics needed to combine all jobs
    // and calculate \sigma^2/\bar n and the errors.
    // Write this out to a seperate file.

    hnBins->Write();
    hoffset->Write();
    hfUnique->Write();


    cout << "For this analysis we have used mEtaSumMode = " << mEtaSumMode << endl;
    cout << "For this analysis we have used mPhiSumMode = " << mPhiSumMode << endl;
    cout << "(1 = start at bin 0. Fit as many non-overlapping bins as possible" <<endl;
    cout << "   If it doesn't end evenly start at end and work back.)" << endl;
    cout << "(2 = start at bin 0, shift over one bin until we hit the end.)" << endl;
    cout << "(3 = use one bin at beginning.)" << endl;
    cout << "(4 = use one bin at end.)" << endl;
    cout << "(5 = use one bin near center.)" << endl;
    cout << "(6 = fit as many non-overlapping bins in as possible.";
    cout << "     Coverage as centered as possible.)" << endl;
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

    // next line is currently never satisfied... must build structure
    // so that local mQAHists are created per analysis object as in 
    // correlations.  .... leave this here as a reminder only and for 
    // symmetry in correlations code reflected in doEStruct...

    if(mlocalQAHists) mQAHists->writeTrackHistograms(qatf);

    for (int i=0;i<mnCents;i++) {
        mFluct[i]->writeQAHistograms();
    }
    for (int i=0;i<mnPtCents;i++) {
        for (int j=0;j<mnPts;j++) {
            int index = j + i*mnPts;
            mPtFluct[index]->writeQAHistograms();
        }
    }

}

//--------------------------------------------------------------------------
void StEStructFluctAnal::initHistograms(){

    hMultiplicity = new TH1F("Multiplicity","Multiplicity",2000,1,2000);
    hMultiplicityBinned = new TH1F("MultiplicityBinned","MultiplicityBinned",120,1.0,7.0);
    hPt = new TH1F("Pt","Pt",2000,1,2000);
    hPtBinned = new TH1F("PtBinned","PtBinned",120,0.0,6.0);

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
    mnTotBins = off;
cout << "Creating histograms for bins, uniqueness etc. " << endl;
    hnBins   = new TH2F("nBins",  "nBins",  NPHIBINS,0.5,NPHIBINS+0.5,NETABINS,0.5,NETABINS+0.5);
    hoffset = new TH2F("offset","offset",NPHIBINS,0.5,NPHIBINS+0.5,NETABINS,0.5,NETABINS+0.5);
    hfUnique = new TH2F("fUnique","fUnique",NPHIBINS,0.5,NPHIBINS+0.5,NETABINS,0.5,NETABINS+0.5);
    for (int jPhi=0;jPhi<NPHIBINS;jPhi++) {
        for (int jEta=0;jEta<NETABINS;jEta++) {
            hnBins->SetBinContent(jPhi+1,jEta+1,nBins[jPhi][jEta]);
            hoffset->SetBinContent(jPhi+1,jEta+1,offset[jPhi][jEta]);
            hfUnique->SetBinContent(jPhi+1,jEta+1,fUnique[jPhi][jEta]);
        }
    }
}

void StEStructFluctAnal::deleteHistograms() {
    if (hMultiplicity) {
        delete hMultiplicity;     hMultiplicity = 0;
    }
    if (hMultiplicityBinned) {
        delete hMultiplicityBinned;     hMultiplicityBinned = 0;
    }
    if (hPt) {
        delete hPt;     hPt = 0;
    }
    if (hPtBinned) {
        delete hPtBinned;     hPtBinned = 0;
    }
    if (hnBins) {
        delete hnBins;            hnBins = 0;
    }
    if (hoffset) {
        delete hoffset;           hoffset = 0;
    }
    if (hfUnique) {
        delete hfUnique;          hfUnique = 0;
    }
}
float StEStructFluctAnal::etaOffset( float vz ) {
    return 0;
//    double eta1 = -log(tan(atan(160.0/(200+vz))/2));
//    double eta2 = -log(tan(atan(160.0/(200-vz))/2));
//    return (eta2-eta1)/2;
}
