
#include "StEStructFluctuations.h"

#include "TH1F.h"
#include "TH2F.h"
#include "TFile.h"

#include "StEStructPool/EventMaker/StEStructEvent.h"
#include "StEStructPool/EventMaker/StEStructTrack.h"
#include "StTimer.hh"


ClassImp(StEStructFluctuations)

//--------------------------------------------------------------------------
StEStructFluctuations::StEStructFluctuations(int mode, int invokePairCuts, int sumMode): manalysisMode(mode), mskipPairCuts(false), mdoPairCutHistograms(false) {
    doingPairCuts = invokePairCuts;
    summingMode   = sumMode;
    init();
}

//--------------------------------------------------------------------------
StEStructFluctuations::StEStructFluctuations(const char* cutFileName, int mode, int invokePairCuts,
                                             int sumMode): manalysisMode(mode), mskipPairCuts(false), mdoPairCutHistograms(false), mPair(cutFileName) {
    doingPairCuts = invokePairCuts;
    summingMode   = sumMode;
    init();
}

//--------------------------------------------------------------------------
StEStructFluctuations::~StEStructFluctuations() {
    cleanUp();
}


void StEStructFluctuations::init() {

  if(manalysisMode & 1) {
     mskipPairCuts=true;
  } else if(manalysisMode & 2){
    mdoPairCutHistograms=true;
  }

  ms  = new multStruct();
  initArraysAndHistograms();
}


void StEStructFluctuations::cleanUp() {
    delete ms;
    deleteArraysAndHistograms();
}

//
//-------  Analysis Function ------//
//
//--------------------------------------------------------------------------
bool StEStructFluctuations::doEvent(StEStructEvent* event) {
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
void StEStructFluctuations::moveEvents(){

    if (!mCurrentEvent) {
        return;
    }
    delete mCurrentEvent;
    mCurrentEvent = NULL;
//    if(mMixingEvent) delete mMixingEvent;
//    mMixingEvent=mCurrentEvent;

}


//--------------------------------------------------------------------------
bool StEStructFluctuations::doPairCuts() {

  if(!mCurrentEvent) return false; // logic problem!

  pairCuts(mCurrentEvent,mCurrentEvent,0);
  pairCuts(mCurrentEvent,mCurrentEvent,1);
  pairCuts(mCurrentEvent,mCurrentEvent,2);

  return true;
}

//--------------------------------------------------------------------------
void StEStructFluctuations::pairCuts(StEStructEvent* e1, StEStructEvent* e2, int j){

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

  if(mtimer)mtimer->start();

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

  if(mtimer)mtimer->stop();
}
void StEStructFluctuations::makeMultStruct() {
    StEStructTrackCollection* tc;
    StEStructTrackIterator Iter;
    StEStructTrack* t;

    int iEta, iPhi, iPt;

    ms->NewEvent(mCurrentEvent->Vx(), mCurrentEvent->Vy(), mCurrentEvent->Vz() );
    int nGoodTracks = 0;

    tc = mCurrentEvent->TrackCollectionP();
    for(Iter=tc->begin(); Iter!=tc->end(); ++Iter){
        t = *Iter;
        if (t->Chi2() < 0) {
            continue;
        }
        if ((t->Eta() < ETAMIN) || (ETAMAX < t->Eta())) {
            continue;
        }
        iEta = int(NETABINS*(t->Eta()-ETAMIN)/(ETAMAX-ETAMIN));
        if ((iEta < 0) || (NETABINS <= iEta)) {
            continue;
        }
        iPhi = int(NPHIBINS*(1.0 + t->Phi()/3.141592654)/2.0);
        if ((iPhi < 0) || (NPHIBINS <= iPhi)) {
            continue;
        }
        iPt = getPtBin( t->Pt() );
        if ((iPt < 0) || (NPTBINS <= iPt)) {
            continue;
        }

        ms->AddTrack( iPhi, iEta, iPt, +1, t->Pt() );
        nGoodTracks++;
    }
    tc = mCurrentEvent->TrackCollectionM();
    for(Iter=tc->begin(); Iter!=tc->end(); ++Iter){
        t = *Iter;
        if (t->Chi2() < 0) {
            continue;
        }
        if ((t->Eta() < ETAMIN) || (ETAMAX < t->Eta())) {
            continue;
        }
        iEta = int(NETABINS*(t->Eta()-ETAMIN)/(ETAMAX-ETAMIN));
        if ((iEta < 0) || (NETABINS <= iEta)) {
            continue;
        }
        iPhi = int(NPHIBINS*(1.0 + t->Phi()/3.141592654)/2.0);
        if ((iPhi < 0) || (NPHIBINS <= iPhi)) {
            continue;
        }
        iPt = getPtBin( t->Pt() );
        if ((iPt < 0) || (NPTBINS <= iPt)) {
            continue;
        }

        ms->AddTrack( iPhi, iEta, iPt, -1, t->Pt() );
        nGoodTracks++;
    }

    AddEvent(ms);
}
void StEStructFluctuations::constantMultStruct(int nt, float val) {
    ms->NewEvent( 0, 0, 0 );
    for (int iPhi=0;iPhi<NPHIBINS;iPhi++) {
        for (int iEta=0;iEta<NETABINS;iEta++) {
            for (int it=0;it<nt;it++) {
                ms->AddTrack( iPhi, iEta, 0, +1, val );
            }
        }
    }
    AddEvent(ms);
}
void StEStructFluctuations::randomMultStruct(double p, float val) {
    int ir = 0;
    ms->NewEvent( 0, 0, 0 );
    for (int iPhi=0;iPhi<NPHIBINS;iPhi++) {
        for (int iEta=0;iEta<NETABINS;iEta++) {
//            ir = ranlib::ignpoi(p);
            while (ir >= 1) {
                ms->AddTrack( iPhi, iEta, 0, +1, val );
                ir--;
            }
//            ir = ranlib::ignpoi(p);
            while (ir >= 1) {
                ms->AddTrack( iPhi, iEta, 0, -1, val );
                ir--;
            }
        }
    }
    AddEvent(ms);
}
void StEStructFluctuations::AddEvent(multStruct *ms) {
    double delEta, delPhi;
    int jCent, jPtCent;

    nTotEvents++;

    if ((jCent = getCentBin(ms->mRefMult)) < 0) {
        return;
    }
    nCentEvents[jCent]++;
    jPtCent = getPtCentBin(jCent);

    // First two loops define the scale.
    //    To try getting better performance I move all variable declarations
    //    outside these loops.
    int iPhi, jPhi, kPhi, lPhi, dPhi, iEta, jEta, kEta, lEta, dEta;
    int jBin, jPt;
    double binNPlus = 0, binNMinus = 0;
    double binPPlus = 0, binPMinus = 0;
    double ptBinNPlus = 0, ptBinNMinus = 0;
    double ptBinPPlus = 0, ptBinPMinus = 0;
    for (jPhi=0;jPhi<NPHIBINS;jPhi++) {
        dPhi = jPhi + 1;
        for (jEta=0;jEta<NETABINS;jEta++) {
            dEta = jEta + 1;

            // Second two loops are over all bins at this scale
            // Details of this are chosen by the summingMode.
            jBin = 0;
            iPhi = 1;
            while ( (kPhi=getPhiStart(iPhi,dPhi)) >= 0) {
                iEta = 1;
                while ( (kEta=getEtaStart(iEta,dEta)) >= 0) {

                    // Next loop is over Pt binning.
                    // We keep bins that are summed over all pt in
                    // addition to bins for particular pt ranges.
                    binNPlus  = 0;
                    binNMinus = 0;
                    binPPlus  = 0;
                    binPMinus = 0;
                    for (jPt=0;jPt<NPTBINS;jPt++) {
                        // Final two loops sum the small bins.
                        ptBinNPlus  = 0;
                        ptBinNMinus = 0;
                        ptBinPPlus  = 0;
                        ptBinPMinus = 0;
                        for (lPhi=kPhi;lPhi<kPhi+dPhi;lPhi++) {
                            for (lEta=kEta;lEta<kEta+dEta;lEta++) {
                                ptBinNPlus  += ms->mTrackBinPlus[jPt][lPhi][lEta][0];
                                ptBinNMinus += ms->mTrackBinMinus[jPt][lPhi][lEta][0];
                                ptBinPPlus  += ms->mTrackBinPlus[jPt][lPhi][lEta][1];
                                ptBinPMinus += ms->mTrackBinMinus[jPt][lPhi][lEta][1];
                            }
                        }
                        AddToPtBin( jPhi, jEta, jPtCent, jPt, jBin,
                                    ptBinNPlus, ptBinNMinus,
                                    ptBinPPlus, ptBinPMinus );
                        binNPlus  += ptBinNPlus;
                        binNMinus += ptBinNMinus;
                        binPPlus  += ptBinPPlus;
                        binPMinus += ptBinPMinus;
                    }

                    AddToBin( jPhi, jEta, jCent, jBin,
                              binNPlus, binNMinus,
                              binPPlus, binPMinus );
                    jBin++;
                    iEta++;
                }
                iPhi++;
            }
        }
    }
    // Fill histograms of pt^2 for all tracks.
    int iCent = 1 + jCent;
    int iPtCent = 1 + jPtCent;
    double PtSqPlus = 0, PtSqMinus = 0;
    for (int jPt=0;jPt<NPTBINS;jPt++) {
        double ptPtSqPlus  = ms->GetPtSqPlus(jPt);
        double ptPtSqMinus = ms->GetPtSqMinus(jPt);
        if (iPtCent > 0) {
            hptPtSumSq[jPt][0]->Fill(iPtCent,ptPtSqPlus+ptPtSqMinus);
            hptPtSumSq[jPt][1]->Fill(iPtCent,ptPtSqPlus);
            hptPtSumSq[jPt][2]->Fill(iPtCent,ptPtSqMinus);
        }
        PtSqPlus  += ptPtSqPlus;
        PtSqMinus += ptPtSqMinus;
    }
    hPtSumSq[0]->Fill(iCent,PtSqPlus+PtSqMinus);
    hPtSumSq[1]->Fill(iCent,PtSqPlus);
    hPtSumSq[2]->Fill(iCent,PtSqMinus);

    // Increment occupancy histograms.
    delEta = (ETAMAX-ETAMIN) / NETABINS;
    delPhi = 2.0*3.1415926 / NPHIBINS;
    for (int jPhi=0;jPhi<NPHIBINS;jPhi++) {
        double dp = -3.1415926 + delPhi*(jPhi+0.5);
        for (int jEta=0;jEta<NETABINS;jEta++) {
            double de = ETAMIN + delEta*(jEta+0.5);
            double nsum, ndiff, psum, pdiff;
            double nplus = 0, nminus = 0, pplus = 0, pminus = 0;
            for (int jPt=0;jPt<NPTBINS;jPt++) {
                double np, nm, pp, pm;
                np = ms->GetNPlus(jPhi,jEta,jPt);
                nm = ms->GetNMinus(jPhi,jEta,jPt);
                pp = ms->GetPPlus(jPhi,jEta,jPt);
                pm = ms->GetPMinus(jPhi,jEta,jPt);
                nsum   = np + nm;
                ndiff  = np - nm;
                psum   = pp + pm;
                pdiff  = pp - pm;
                if ((jPtCent = getPtCentBin(jCent)) >= 0) {
                    occptNSum[jPtCent][jPt]->Fill(dp,de,nsum);
                    occptNPlus[jPtCent][jPt]->Fill(dp,de,np);
                    occptNMinus[jPtCent][jPt]->Fill(dp,de,nm);
                    occptNDiff[jPtCent][jPt]->Fill(dp,de,ndiff);
                    occptPSum[jPtCent][jPt]->Fill(dp,de,psum);
                    occptPPlus[jPtCent][jPt]->Fill(dp,de,pp);
                    occptPMinus[jPtCent][jPt]->Fill(dp,de,pm);
                    occptPDiff[jPtCent][jPt]->Fill(dp,de,pdiff);
                    occptPNSum[jPtCent][jPt]->Fill(dp,de,nsum*psum);
                    occptPNPlus[jPtCent][jPt]->Fill(dp,de,np*pp);
                    occptPNMinus[jPtCent][jPt]->Fill(dp,de,nm*pm);
                    occptPNDiff[jPtCent][jPt]->Fill(dp,de,ndiff*pdiff);
                }
                nplus  += np;
                nminus += nm;
                pplus  += pp;
                pminus += pm;
            }
            nsum   = nplus + nminus;
            ndiff  = nplus - nminus;
            psum   = pplus + pminus;
            pdiff  = pplus - pminus;
            occNSum[jCent]->Fill(dp,de,nsum);
            occNPlus[jCent]->Fill(dp,de,nplus);
            occNMinus[jCent]->Fill(dp,de,nminus);
            occNDiff[jCent]->Fill(dp,de,ndiff);
            occPSum[jCent]->Fill(dp,de,psum);
            occPPlus[jCent]->Fill(dp,de,pplus);
            occPMinus[jCent]->Fill(dp,de,pminus);
            occPDiff[jCent]->Fill(dp,de,pdiff);
            occPNSum[jCent]->Fill(dp,de,nsum*psum);
            occPNPlus[jCent]->Fill(dp,de,nplus*pplus);
            occPNMinus[jCent]->Fill(dp,de,nminus*pminus);
            occPNDiff[jCent]->Fill(dp,de,ndiff*pdiff);
        }
    }
}
void StEStructFluctuations::AddToPtBin( int jPhi, int jEta,
                                        int jPtCent, int jPt, int jBin,
                                        double Nplus,   double Nminus,
                                        double Pplus,   double Pminus ) {

    if (jPtCent < 0) {
        return;
    }

    double Nsum  = Nplus + Nminus;
    double Ndiff = Nplus - Nminus;
    double Psum  = Pplus + Pminus;

    int iBin = 1 + jBin + offset[jPhi][jEta];
    hptTotEvents[jPtCent][jPt][0]->Fill(iBin);
    if (Nsum > 0) {
        hptTotEvents[jPtCent][jPt][1]->Fill(iBin);
    }
    if (Nplus > 0) {
        hptTotEvents[jPtCent][jPt][2]->Fill(iBin);
    }
    if (Nminus > 0) {
        hptTotEvents[jPtCent][jPt][3]->Fill(iBin);
    }
    if ((Nplus > 0) && (Nminus > 0)) {
        hptTotEvents[jPtCent][jPt][4]->Fill(iBin);
    }

    hptNSum[jPtCent][jPt][0]->Fill(iBin,Nsum);
    hptNSum[jPtCent][jPt][1]->Fill(iBin,Nsum*Nsum);

    hptNDiff[jPtCent][jPt][0]->Fill(iBin,Ndiff);
    hptNDiff[jPtCent][jPt][1]->Fill(iBin,Ndiff*Ndiff);

    hptNPlus[jPtCent][jPt][0]->Fill(iBin,Nplus);
    hptNPlus[jPtCent][jPt][1]->Fill(iBin,Nplus*Nplus);

    hptNMinus[jPtCent][jPt][0]->Fill(iBin,Nminus);
    hptNMinus[jPtCent][jPt][1]->Fill(iBin,Nminus*Nminus);

    hptNPlusMinus[jPtCent][jPt]->Fill(iBin,Nplus*Nminus);

    // This routine is called in an inner loop.
    // Although the compiler should do the optimizations I try
    // minimizing number of sqrt() and eliminating pow().
    if (Nsum < 0) {
        return;
    }
    double sqs = sqrt(Nsum);
    double sqp = sqrt(Nplus);
    double sqm = sqrt(Nminus);
    double rs, rs2, rp, rp2, rm, rm2;
    if (Nsum > 0) {
        hptPSum[jPtCent][jPt][0]->Fill(iBin,Psum);
        rs = Psum/Nsum;
        rs2 = rs*rs;
        hptPSum[jPtCent][jPt][1]->Fill(iBin,Psum*rs);
        hptPSum[jPtCent][jPt][2]->Fill(iBin,rs2);
        hptPSum[jPtCent][jPt][3]->Fill(iBin,rs2*rs2);
    }

    if (Nplus > 0) {
        hptPPlus[jPtCent][jPt][0]->Fill(iBin,Pplus);
        rp = Pplus/Nplus;
        rp2 = rp*rp;
        hptPPlus[jPtCent][jPt][1]->Fill(iBin,Pplus*rp);
        hptPPlus[jPtCent][jPt][2]->Fill(iBin,rp2);
        hptPPlus[jPtCent][jPt][3]->Fill(iBin,rp2*rp2);
    }

    if (Nminus > 0) {
        hptPMinus[jPtCent][jPt][0]->Fill(iBin,Pminus);
        rm = Pminus/Nminus;
        rm2 = rm*rm;
        hptPMinus[jPtCent][jPt][1]->Fill(iBin,Pminus*rm);
        hptPMinus[jPtCent][jPt][2]->Fill(iBin,rm2);
        hptPMinus[jPtCent][jPt][3]->Fill(iBin,rm2*rm2);
    }

    if ((Nplus > 0) && (Nminus > 0)) {
        hptPPlusMinus[jPtCent][jPt][0]->Fill(iBin,sqp*sqm);
        hptPPlusMinus[jPtCent][jPt][1]->Fill(iBin,Pplus*sqm/sqp);
        hptPPlusMinus[jPtCent][jPt][2]->Fill(iBin,Pminus*sqp/sqm);
        hptPPlusMinus[jPtCent][jPt][3]->Fill(iBin,Pplus*Pminus/(sqp*sqm));
    }

    if (Nsum > 0) {
        hptPNSum[jPtCent][jPt][0]->Fill(iBin,sqs);
        hptPNSum[jPtCent][jPt][1]->Fill(iBin,Nsum*sqs);
        hptPNSum[jPtCent][jPt][2]->Fill(iBin,Psum/sqs);
        hptPNSum[jPtCent][jPt][3]->Fill(iBin,Psum*sqs);
    }

    if (Nplus > 0) {
        hptPNPlus[jPtCent][jPt][0]->Fill(iBin,sqp);
        hptPNPlus[jPtCent][jPt][1]->Fill(iBin,Nplus*sqp);
        hptPNPlus[jPtCent][jPt][2]->Fill(iBin,Pplus/sqp);
        hptPNPlus[jPtCent][jPt][3]->Fill(iBin,Pplus*sqp);
    }

    if (Nminus > 0) {
        hptPNMinus[jPtCent][jPt][0]->Fill(iBin,sqm);
        hptPNMinus[jPtCent][jPt][1]->Fill(iBin,Nminus*sqm);
        hptPNMinus[jPtCent][jPt][2]->Fill(iBin,Pminus/sqm);
        hptPNMinus[jPtCent][jPt][3]->Fill(iBin,Pminus*sqm);
    }

    if ((Nplus > 0) && (Nminus > 0)) {
        hptPNPlusMinus[jPtCent][jPt][0]->Fill(iBin,Pplus*Nminus/sqp);
        hptPNPlusMinus[jPtCent][jPt][1]->Fill(iBin,Nminus*sqp);
        hptPNPlusMinus[jPtCent][jPt][2]->Fill(iBin,Pminus*Nplus/sqm);
        hptPNPlusMinus[jPtCent][jPt][3]->Fill(iBin,Nplus*sqm);
    }
}
void StEStructFluctuations::AddToBin( int jPhi,       int jEta,
                                      int jCent,      int jBin,
                                      double Nplus,   double Nminus,
                                      double Pplus,   double Pminus ) {
    double Nsum  = Nplus + Nminus;
    double Ndiff = Nplus - Nminus;
    double Psum  = Pplus + Pminus;

#ifdef TERMINUSSTUDY
    sum[jPhi][jEta]->Fill(Nsum);
    plus[jPhi][jEta]->Fill(Nplus);
    minus[jPhi][jEta]->Fill(Nminus);
    diff[jPhi][jEta][jCent]->Fill(Ndiff);
#endif

    int iBin = 1 + jBin + offset[jPhi][jEta];
    hTotEvents[jCent][0]->Fill(iBin);
    if (Nsum > 0) {
        hTotEvents[jCent][1]->Fill(iBin);
    }
    if (Nplus > 0) {
        hTotEvents[jCent][2]->Fill(iBin);
    }
    if (Nminus > 0) {
        hTotEvents[jCent][3]->Fill(iBin);
    }
    if ((Nplus > 0) && (Nminus > 0)) {
        hTotEvents[jCent][4]->Fill(iBin);
    }

    hNSum[jCent][0]->Fill(iBin,Nsum);
    hNSum[jCent][1]->Fill(iBin,Nsum*Nsum);

    hNDiff[jCent][0]->Fill(iBin,Ndiff);
    hNDiff[jCent][1]->Fill(iBin,Ndiff*Ndiff);

    hNPlus[jCent][0]->Fill(iBin,Nplus);
    hNPlus[jCent][1]->Fill(iBin,Nplus*Nplus);

    hNMinus[jCent][0]->Fill(iBin,Nminus);
    hNMinus[jCent][1]->Fill(iBin,Nminus*Nminus);

    hNPlusMinus[jCent]->Fill(iBin,Nplus*Nminus);

    // This routine is called in an inner loop.
    // Although the compiler should do the optimizations I try
    // minimizing number of sqrt() and eliminating pow()
    if (Nsum < 0) {
        return;
    }
    double sqs = sqrt(Nsum);
    double sqp = sqrt(Nplus);
    double sqm = sqrt(Nminus);
    double rs, rs2, rp, rp2, rm, rm2;
    if (Nsum > 0) {
        hPSum[jCent][0]->Fill(iBin,Psum);
        rs  = Psum/Nsum;
        rs2 = rs*rs;
        hPSum[jCent][1]->Fill(iBin,Psum*rs);
        hPSum[jCent][2]->Fill(iBin,rs2);
        hPSum[jCent][3]->Fill(iBin,rs2*rs2);
    }

    if (Nplus > 0) {
        hPPlus[jCent][0]->Fill(iBin,Pplus);
        rp  = Pplus/Nplus;
        rp2 = rp*rp;
        hPPlus[jCent][1]->Fill(iBin,Pplus*rp);
        hPPlus[jCent][2]->Fill(iBin,rp2);
        hPPlus[jCent][3]->Fill(iBin,rp2*rp2);
    }

    if (Nminus > 0) {
        hPMinus[jCent][0]->Fill(iBin,Pminus);
        rm  = Pminus/Nminus;
        rm2 = rm*rm;
        hPMinus[jCent][1]->Fill(iBin,Pminus*rm);
        hPMinus[jCent][2]->Fill(iBin,rm2);
        hPMinus[jCent][3]->Fill(iBin,rm2*rm2);
    }

    if ((Nplus > 0) && (Nminus > 0)) {
        hPPlusMinus[jCent][0]->Fill(iBin,sqp*sqm);
        hPPlusMinus[jCent][1]->Fill(iBin,Pplus*sqm/sqp);
        hPPlusMinus[jCent][2]->Fill(iBin,Pminus*sqp/sqm);
        hPPlusMinus[jCent][3]->Fill(iBin,Pplus*Pminus/(sqp*sqm));
    }

    if (Nsum > 0) {
        hPNSum[jCent][0]->Fill(iBin,sqrt(Nsum));
        hPNSum[jCent][1]->Fill(iBin,Nsum*sqs);
        hPNSum[jCent][2]->Fill(iBin,Psum/sqs);
        hPNSum[jCent][3]->Fill(iBin,Psum*sqs);
    }

    if (Nplus > 0) {
        hPNPlus[jCent][0]->Fill(iBin,sqp);
        hPNPlus[jCent][1]->Fill(iBin,Nplus*sqp);
        hPNPlus[jCent][2]->Fill(iBin,Pplus/sqp);
        hPNPlus[jCent][3]->Fill(iBin,Pplus*sqp);
    }

    if (Nminus > 0) {
        hPNMinus[jCent][0]->Fill(iBin,sqm);
        hPNMinus[jCent][1]->Fill(iBin,Nminus*sqm);
        hPNMinus[jCent][2]->Fill(iBin,Pminus/sqm);
        hPNMinus[jCent][3]->Fill(iBin,Pminus*sqm);
    }

    if ((Nplus > 0) && (Nminus > 0)) {
        hPNPlusMinus[jCent][0]->Fill(iBin,Pplus*Nminus/sqp);
        hPNPlusMinus[jCent][1]->Fill(iBin,Nminus*sqp);
        hPNPlusMinus[jCent][2]->Fill(iBin,Pminus*Nplus/sqm);
        hPNPlusMinus[jCent][3]->Fill(iBin,Nplus*sqm);
    }
}
// iEta runs from 1 up to the number of etaBins that fit in,
// according to the binning mode.
// Return starting bin (first bin is called 0.)
// When iEta is too big return -1.
int StEStructFluctuations::getEtaStart( int iEta, int dEta ) {
    if (dEta > NETABINS) {
        return -1;
    }
    if (1 == summingMode) {
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
    } else if (2 == summingMode) {
        if (iEta+dEta <= NETABINS) {
            return iEta - 1;
        } else {
            return -1;
        }
    } else if (3 == summingMode) {
        if (iEta > 1) {
            return -1;
        } else {
            return 0;
        }
    } else if (4 == summingMode) {
        if (iEta > 1) {
            return -1;
        } else {
            return NETABINS-dEta-1;
        }
    } else if (5 == summingMode) {
        if (iEta > 1) {
            return -1;
        } else {
            return (NETABINS-dEta) / 2;
        }
    }
    return -1;
}
int StEStructFluctuations::getPhiStart( int iPhi, int dPhi ) {
    if (dPhi > NPHIBINS) {
        return -1;
    }
    if (1 == summingMode) {
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
    } else if (2 == summingMode) {
        if (iPhi+dPhi <= NPHIBINS) {
            return iPhi - 1;
        } else {
            return -1;
        }
    } else if (3 == summingMode) {
        if (iPhi > 1) {
            return -1;
        } else {
            return 0;
        }
    } else if (4 == summingMode) {
        if (iPhi > 1) {
            return -1;
        } else {
            return NPHIBINS-dPhi - 1;
        }
    } else if (5 == summingMode) {
        if (iPhi > 1) {
            return -1;
        } else {
            return (NPHIBINS-dPhi) / 2;
        }
    }
    return -1;
}
int StEStructFluctuations::getNumEtaBins( int dEta ) {
    if (1 == summingMode) {
        int nEta = NETABINS / dEta;
        int oEta = NETABINS % dEta;
        if ( 0 == oEta ) {
            return nEta;
        } else {
            return 2 * nEta;
        }
    } else if (2 == summingMode) {
        return NETABINS + 1 - dEta;
    } else if (3 == summingMode) {
        return 1;
    } else if (4 == summingMode) {
        return 1;
    } else if (5 == summingMode) {
        return 1;
    }
    return 0;
}
int StEStructFluctuations::getNumPhiBins( int dPhi ) {
    if (1 == summingMode) {
        int nPhi = NPHIBINS / dPhi;
        int oPhi = NPHIBINS % dPhi;
        if ( 0 == oPhi ) {
            return nPhi;
        } else {
            return 2 * nPhi;
        }
    } else if (2 == summingMode) {
        return NPHIBINS + 1 - dPhi;
    } else if (3 == summingMode) {
        return 1;
    } else if (4 == summingMode) {
        return 1;
    } else if (5 == summingMode) {
        return 1;
    }
    return 0;
}

//--------------------------------------------------------------------------
//
//------------ Below are init, delete, write functions -------///
//


//--------------------------------------------------------------------------
void StEStructFluctuations::writeHistograms(TFile* tf){

    tf->cd();
#ifdef TERMINUSSTUDY
    for (int jPhi=0;jPhi<NPHIBINS;jPhi++) {
        for (int jEta=0;jEta<NETABINS;jEta++) {
            sum[jPhi][jEta]->Write();
            plus[jPhi][jEta]->Write();
            minus[jPhi][jEta]->Write();
            plusminus[jPhi][jEta]->Write();
            for (int jCent=0;jCent<NCENTBINS;jCent++) {
                diff[jPhi][jEta][jCent]->Write();
            }
        }
    }
#endif

    // Here I write out the statistics needed to combine all jobs
    // and calculate \sigma^2/\bar n and the errors.
    // Write this out to a seperate file.

    hnBins->Write();
    hoffset->Write();
    hfUnique->Write();

    for (int jC=0;jC<NCENTBINS;jC++) {
        for (int jStat=0;jStat<5;jStat++) {
            hTotEvents[jC][jStat]->Write();
        }

        for (int jStat=0;jStat<2;jStat++) {
            hNSum[jC][jStat]->Write();
            hNDiff[jC][jStat]->Write();
            hNPlus[jC][jStat]->Write();
            hNMinus[jC][jStat]->Write();
        }
        hNPlusMinus[jC]->Write();
        for (int jStat=0;jStat<4;jStat++) {
            hPSum[jC][jStat]->Write();
            hPPlus[jC][jStat]->Write();
            hPMinus[jC][jStat]->Write();
        }
        for (int jStat=0;jStat<4;jStat++) {
            hPPlusMinus[jC][jStat]->Write();
        }
        for (int jStat=0;jStat<4;jStat++) {
            hPNSum[jC][jStat]->Write();
            hPNPlus[jC][jStat]->Write();
            hPNMinus[jC][jStat]->Write();
        }
        for (int jStat=0;jStat<4;jStat++) {
            hPNPlusMinus[jC][jStat]->Write();
        }
    }
    hPtSumSq[0]->Write();
    hPtSumSq[1]->Write();
    hPtSumSq[2]->Write();

    for (int jC=0;jC<NPTCENTBINS;jC++) {
        for (int jPt=0;jPt<NPTBINS;jPt++) {
            for (int jStat=0;jStat<5;jStat++) {
                hptTotEvents[jC][jPt][jStat]->Write();
            }
            for (int jStat=0;jStat<2;jStat++) {
                hptNSum[jC][jPt][jStat]->Write();
                hptNDiff[jC][jPt][jStat]->Write();
                hptNPlus[jC][jPt][jStat]->Write();
                hptNMinus[jC][jPt][jStat]->Write();
            }
            hptNPlusMinus[jC][jPt]->Write();
            for (int jStat=0;jStat<4;jStat++) {
                hptPSum[jC][jPt][jStat]->Write();
                hptPPlus[jC][jPt][jStat]->Write();
                hptPMinus[jC][jPt][jStat]->Write();
            }
            for (int jStat=0;jStat<4;jStat++) {
                hptPPlusMinus[jC][jPt][jStat]->Write();
            }
            for (int jStat=0;jStat<4;jStat++) {
                hptPNSum[jC][jPt][jStat]->Write();
                hptPNPlus[jC][jPt][jStat]->Write();
                hptPNMinus[jC][jPt][jStat]->Write();
            }
            for (int jStat=0;jStat<4;jStat++) {
                hptPNPlusMinus[jC][jPt][jStat]->Write();
            }
        }
    }
    for (int jPt=0;jPt<NPTBINS;jPt++) {
        hptPtSumSq[jPt][0]->Write();
        hptPtSumSq[jPt][1]->Write();
        hptPtSumSq[jPt][2]->Write();
    }

    cout << "For this analysis we used doingPairCuts = " << doingPairCuts << endl;
    cout << "(0 means don't invoke pair cuts, 1 means invoke pair cuts.)" << endl;
    cout << endl;
    cout << "For this analysis we have used summingMode = " << summingMode << endl;
    cout << "(1 = start at bin 0. Fit as many non-overlapping bins as possible" <<endl;
    cout << "   If it doesn't end evenly start at end and work back.)" << endl;
    cout << "(2 = start at bin 0, shift over one bin until we hit the end.)" << endl;
    cout << "(3 = use one bin at beginning.)" << endl;
    cout << "(4 = use one bin at end.)" << endl;
    cout << "(5 = use one bin near center.)" << endl;
    cout << endl;

    cout << "Looped over " << nTotEvents << " total events" << endl;
    cout << "  For each centrality have ";
    for (int jCent=0;jCent<NCENTBINS;jCent++) {
        cout << nCentEvents[jCent] << "  ";
    }
    cout << endl;
}
void StEStructFluctuations::writeQAHists(TFile* qatf) {

    qatf->cd();

    for (int i=0;i<NCENTBINS;i++) {
        occNSum[i]->Write();
        occNPlus[i]->Write();
        occNMinus[i]->Write();
        occNDiff[i]->Write();
        occPSum[i]->Write();
        occPPlus[i]->Write();
        occPMinus[i]->Write();
        occPDiff[i]->Write();
        occPNSum[i]->Write();
        occPNPlus[i]->Write();
        occPNMinus[i]->Write();
        occPNDiff[i]->Write();
    }
    for (int i=0;i<NPTCENTBINS;i++) {
        for (int j=0;j<NPTBINS;j++) {
            occptNSum[i][j]->Write();
            occptNPlus[i][j]->Write();
            occptNMinus[i][j]->Write();
            occptNDiff[i][j]->Write();
            occptPSum[i][j]->Write();
            occptPPlus[i][j]->Write();
            occptPMinus[i][j]->Write();
            occptPDiff[i][j]->Write();
            occptPNSum[i][j]->Write();
            occptPNPlus[i][j]->Write();
            occptPNMinus[i][j]->Write();
            occptPNDiff[i][j]->Write();
        }
    }

}

//--------------------------------------------------------------------------
void StEStructFluctuations::initArraysAndHistograms(){
    char line[255];

    nTotEvents = 0;
    for (int jCent=0;jCent<NCENTBINS;jCent++) {
        nCentEvents[jCent] = 0;
    }

    // Here are histograms for every acceptance we are going to look at.
#ifdef TERMINUSSTUDY
    for (int jPhi=0;jPhi<NPHIBINS;jPhi++) {
        for (int jEta=0;jEta<NETABINS;jEta++) {
            sprintf( line, "sum%i_%i", jPhi, jEta );
            sum[jPhi][jEta] = new TH1F(line,line, 1200,0.0,1200.0);

            sprintf( line, "plus%i_%i", jPhi, jEta );
            plus[jPhi][jEta] = new TH1F(line,line, 600,0.0,600.0);

            sprintf( line, "minus%i_%i", jPhi, jEta );
            minus[jPhi][jEta] = new TH1F(line,line, 600,0.0,600.0);

            sprintf( line, "plusminus%i_%i", jPhi, jEta );
            plusminus[jPhi][jEta] = new TH1F(line,line, 600,0.0,600.0);

            for (int j=0;j<NCENTBINS;j++) {
                sprintf( line, "diff%i_%i_%i", jPhi, jEta, j );
                diff[jPhi][jEta][j] = new TH1F(line,line, 200,-100.0, 100.0);
            }
        }
    }
#endif

    // Here are histograms toward uniform occupancy of bins.
    for (int i=0;i<NCENTBINS;i++) {
        sprintf( line, "occNSum_%i", i );
        occNSum[i] = new TH2F(line,line,NPHIBINS,-3.1415926,3.1415926,NETABINS,ETAMIN,ETAMAX);
        sprintf( line, "occNPlus_%i", i );
        occNPlus[i] = new TH2F(line,line,NPHIBINS,-3.1415926,3.1415926,NETABINS,ETAMIN,ETAMAX);
        sprintf( line, "occNMinus_%i", i );
        occNMinus[i] = new TH2F(line,line,NPHIBINS,-3.1415926,3.1415926,NETABINS,ETAMIN,ETAMAX);
        sprintf( line, "occNDiff_%i", i );
        occNDiff[i] = new TH2F(line,line,NPHIBINS,-3.1415926,3.1415926,NETABINS,ETAMIN,ETAMAX);
        sprintf( line, "occPSum_%i", i );
        occPSum[i] = new TH2F(line,line,NPHIBINS,-3.1415926,3.1415926,NETABINS,ETAMIN,ETAMAX);
        sprintf( line, "occPPlus_%i", i );
        occPPlus[i] = new TH2F(line,line,NPHIBINS,-3.1415926,3.1415926,NETABINS,ETAMIN,ETAMAX);
        sprintf( line, "occPMinus_%i", i );
        occPMinus[i] = new TH2F(line,line,NPHIBINS,-3.1415926,3.1415926,NETABINS,ETAMIN,ETAMAX);
        sprintf( line, "occPDiff_%i", i );
        occPDiff[i] = new TH2F(line,line,NPHIBINS,-3.1415926,3.1415926,NETABINS,ETAMIN,ETAMAX);
        sprintf( line, "occPNSum_%i", i );
        occPNSum[i] = new TH2F(line,line,NPHIBINS,-3.1415926,3.1415926,NETABINS,ETAMIN,ETAMAX);
        sprintf( line, "occPNPlus_%i", i );
        occPNPlus[i] = new TH2F(line,line,NPHIBINS,-3.1415926,3.1415926,NETABINS,ETAMIN,ETAMAX);
        sprintf( line, "occPNMinus_%i", i );
        occPNMinus[i] = new TH2F(line,line,NPHIBINS,-3.1415926,3.1415926,NETABINS,ETAMIN,ETAMAX);
        sprintf( line, "occPNDiff_%i", i );
        occPNDiff[i] = new TH2F(line,line,NPHIBINS,-3.1415926,3.1415926,NETABINS,ETAMIN,ETAMAX);
    }
    for (int i=0;i<NPTCENTBINS;i++) {
        for (int j=0;j<NPTBINS;j++) {
            sprintf( line, "occptNSum_%i_%i", i, j );
            occptNSum[i][j] = new TH2F(line,line,NPHIBINS,-3.1415926,3.1415926,NETABINS,ETAMIN,ETAMAX);
            sprintf( line, "occptNPlus_%i_%i", i, j );
            occptNPlus[i][j] = new TH2F(line,line,NPHIBINS,-3.1415926,3.1415926,NETABINS,ETAMIN,ETAMAX);
            sprintf( line, "occptNMinus_%i_%i", i, j );
            occptNMinus[i][j] = new TH2F(line,line,NPHIBINS,-3.1415926,3.1415926,NETABINS,ETAMIN,ETAMAX);
            sprintf( line, "occptNDiff_%i_%i", i, j );
            occptNDiff[i][j] = new TH2F(line,line,NPHIBINS,-3.1415926,3.1415926,NETABINS,ETAMIN,ETAMAX);
            sprintf( line, "occptPSum_%i_%i", i, j );
            occptPSum[i][j] = new TH2F(line,line,NPHIBINS,-3.1415926,3.1415926,NETABINS,ETAMIN,ETAMAX);
            sprintf( line, "occptPPlus_%i_%i", i, j );
            occptPPlus[i][j] = new TH2F(line,line,NPHIBINS,-3.1415926,3.1415926,NETABINS,ETAMIN,ETAMAX);
            sprintf( line, "occptPMinus_%i_%i", i, j );
            occptPMinus[i][j] = new TH2F(line,line,NPHIBINS,-3.1415926,3.1415926,NETABINS,ETAMIN,ETAMAX);
            sprintf( line, "occptPDiff_%i_%i", i, j );
            occptPDiff[i][j] = new TH2F(line,line,NPHIBINS,-3.1415926,3.1415926,NETABINS,ETAMIN,ETAMAX);
            sprintf( line, "occptPNSum_%i_%i", i, j );
            occptPNSum[i][j] = new TH2F(line,line,NPHIBINS,-3.1415926,3.1415926,NETABINS,ETAMIN,ETAMAX);
            sprintf( line, "occptPNPlus_%i_%i", i, j );
            occptPNPlus[i][j] = new TH2F(line,line,NPHIBINS,-3.1415926,3.1415926,NETABINS,ETAMIN,ETAMAX);
            sprintf( line, "occptPNMinus_%i_%i", i, j );
            occptPNMinus[i][j] = new TH2F(line,line,NPHIBINS,-3.1415926,3.1415926,NETABINS,ETAMIN,ETAMAX);
            sprintf( line, "occptPNDiff_%i_%i", i, j );
            occptPNDiff[i][j] = new TH2F(line,line,NPHIBINS,-3.1415926,3.1415926,NETABINS,ETAMIN,ETAMAX);
        }
    }

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
    int totBins = off;
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
cout << "Allocating histograms to store info." << endl;
    for (int jC=0;jC<NCENTBINS;jC++) {
        sprintf( line, "TotalEvents_%i", jC );
        hTotEvents[jC][0] = new TH1D(line,line,totBins,0.5,totBins+0.5);
        sprintf( line, "TotalSumEvents_%i", jC );
        hTotEvents[jC][1] = new TH1D(line,line,totBins,0.5,totBins+0.5);
        sprintf( line, "TotalPlusEvents_%i", jC );
        hTotEvents[jC][2] = new TH1D(line,line,totBins,0.5,totBins+0.5);
        sprintf( line, "TotalMinusEvents_%i", jC );
        hTotEvents[jC][3] = new TH1D(line,line,totBins,0.5,totBins+0.5);
        sprintf( line, "TotalPlusMinusEvents_%i", jC );
        hTotEvents[jC][4] = new TH1D(line,line,totBins,0.5,totBins+0.5);

        for (int jStat=0;jStat<2;jStat++) {
            sprintf( line, "NSum%i_%i", jC, jStat );
            hNSum[jC][jStat] = new TH1D(line,line,totBins,0.5,totBins+0.5);
            sprintf( line, "NDiff%i_%i", jC, jStat );
            hNDiff[jC][jStat] = new TH1D(line,line,totBins,0.5,totBins+0.5);
            sprintf( line, "NPlus%i_%i", jC, jStat );
            hNPlus[jC][jStat] = new TH1D(line,line,totBins,0.5,totBins+0.5);
            sprintf( line, "NMinus%i_%i", jC, jStat );
            hNMinus[jC][jStat] = new TH1D(line,line,totBins,0.5,totBins+0.5);
        }
        sprintf( line, "NPlusMinus%i_0", jC );
        hNPlusMinus[jC] = new TH1D(line,line,totBins,0.5,totBins+0.5);
        for (int jStat=0;jStat<4;jStat++) {
            sprintf( line, "PSum%i_%i", jC, jStat );
            hPSum[jC][jStat] = new TH1D(line,line,totBins,0.5,totBins+0.5);
            sprintf( line, "PPlus%i_%i", jC, jStat );
            hPPlus[jC][jStat] = new TH1D(line,line,totBins,0.5,totBins+0.5);
            sprintf( line, "PMinus%i_%i", jC, jStat );
            hPMinus[jC][jStat] = new TH1D(line,line,totBins,0.5,totBins+0.5);
        }
        for (int jStat=0;jStat<4;jStat++) {
            sprintf( line, "PPlusMinus%i_%i", jC, jStat );
            hPPlusMinus[jC][jStat] = new TH1D(line,line,totBins,0.5,totBins+0.5);
        }
        for (int jStat=0;jStat<4;jStat++) {
            sprintf( line, "PNSum%i_%i", jC, jStat );
            hPNSum[jC][jStat] = new TH1D(line,line,totBins,0.5,totBins+0.5);
            sprintf( line, "PNPlus%i_%i", jC, jStat );
            hPNPlus[jC][jStat] = new TH1D(line,line,totBins,0.5,totBins+0.5);
            sprintf( line, "PNMinus%i_%i", jC, jStat );
            hPNMinus[jC][jStat] = new TH1D(line,line,totBins,0.5,totBins+0.5);
        }
        for (int jStat=0;jStat<4;jStat++) {
            sprintf( line, "PNPlusMinus%i_%i", jC, jStat );
            hPNPlusMinus[jC][jStat] = new TH1D(line,line,totBins,0.5,totBins+0.5);
        }
    }
    hPtSumSq[0] = new TH1D("PtSumSq","PtSumSq",NCENTBINS,0.5,NCENTBINS+0.5);
    hPtSumSq[1] = new TH1D("PtPlusSq","PtMinusSq",NCENTBINS,0.5,NCENTBINS+0.5);
    hPtSumSq[2] = new TH1D("PtMinusSq","PtPlusSq",NCENTBINS,0.5,NCENTBINS+0.5);

cout << "Allocating histograms to store Pt info. " << endl;
    for (int jC=0;jC<NPTCENTBINS;jC++) {
        for (int jPt=0;jPt<NPTBINS;jPt++) {
            sprintf( line, "ptTotalEvents_%i_%i", jC, jPt );
            hptTotEvents[jC][jPt][0] = new TH1D(line,line,totBins,0.5,totBins+0.5);
            sprintf( line, "ptTotalSumEvents_%i_%i", jC, jPt );
            hptTotEvents[jC][jPt][1] = new TH1D(line,line,totBins,0.5,totBins+0.5);
            sprintf( line, "ptTotalPlusEvents_%i_%i", jC, jPt );
            hptTotEvents[jC][jPt][2] = new TH1D(line,line,totBins,0.5,totBins+0.5);
            sprintf( line, "ptTotalMinusEvents_%i_%i", jC, jPt );
            hptTotEvents[jC][jPt][3] = new TH1D(line,line,totBins,0.5,totBins+0.5);
            sprintf( line, "ptTotalPlusMinusEvents_%i_%i", jC, jPt );
            hptTotEvents[jC][jPt][4] = new TH1D(line,line,totBins,0.5,totBins+0.5);

            for (int jStat=0;jStat<2;jStat++) {
                sprintf( line, "ptNSum%i_%i_%i", jC, jPt, jStat );
                hptNSum[jC][jPt][jStat] = new TH1D(line,line,totBins,0.5,totBins+0.5);
                sprintf( line, "ptNDiff%i_%i_%i", jC, jPt, jStat );
                hptNDiff[jC][jPt][jStat] = new TH1D(line,line,totBins,0.5,totBins+0.5);
                sprintf( line, "ptNPlus%i_%i_%i", jC, jPt, jStat );
                hptNPlus[jC][jPt][jStat] = new TH1D(line,line,totBins,0.5,totBins+0.5);
                sprintf( line, "ptNMinus%i_%i_%i", jC, jPt, jStat );
                hptNMinus[jC][jPt][jStat] = new TH1D(line,line,totBins,0.5,totBins+0.5);
            }
            sprintf( line, "ptNPlusMinus%i_%i_0", jC, jPt );
            hptNPlusMinus[jC][jPt] = new TH1D(line,line,totBins,0.5,totBins+0.5);
            for (int jStat=0;jStat<4;jStat++) {
                sprintf( line, "ptPSum%i_%i_%i", jC, jPt, jStat );
                hptPSum[jC][jPt][jStat] = new TH1D(line,line,totBins,0.5,totBins+0.5);
                sprintf( line, "ptPPlus%i_%i_%i", jC, jPt, jStat );
                hptPPlus[jC][jPt][jStat] = new TH1D(line,line,totBins,0.5,totBins+0.5);
                sprintf( line, "ptPMinus%i_%i_%i", jC, jPt, jStat );
                hptPMinus[jC][jPt][jStat] = new TH1D(line,line,totBins,0.5,totBins+0.5);
            }
            for (int jStat=0;jStat<4;jStat++) {
                sprintf( line, "ptPPlusMinus%i_%i_%i", jC, jPt, jStat );
                hptPPlusMinus[jC][jPt][jStat] = new TH1D(line,line,totBins,0.5,totBins+0.5);
            }
            for (int jStat=0;jStat<4;jStat++) {
                sprintf( line, "ptPNSum%i_%i_%i", jC, jPt, jStat );
                hptPNSum[jC][jPt][jStat] = new TH1D(line,line,totBins,0.5,totBins+0.5);
                sprintf( line, "ptPNPlus%i_%i_%i", jC, jPt, jStat );
                hptPNPlus[jC][jPt][jStat] = new TH1D(line,line,totBins,0.5,totBins+0.5);
                sprintf( line, "ptPNMinus%i_%i_%i", jC, jPt, jStat );
                hptPNMinus[jC][jPt][jStat] = new TH1D(line,line,totBins,0.5,totBins+0.5);
            }
            for (int jStat=0;jStat<4;jStat++) {
                sprintf( line, "ptPNPlusMinus%i_%i_%i", jC, jPt, jStat );
                hptPNPlusMinus[jC][jPt][jStat] = new TH1D(line,line,totBins,0.5,totBins+0.5);
            }
        }
    }
    for (int jPt=0;jPt<NPTBINS;jPt++) {
        sprintf( line, "ptPtSumSq_%i", jPt );
        hptPtSumSq[jPt][0] = new TH1D(line,line,NPTCENTBINS,0.5,NPTCENTBINS+0.5);
        sprintf( line, "ptPtPlusSq_%i", jPt );
        hptPtSumSq[jPt][1] = new TH1D(line,line,NPTCENTBINS,0.5,NPTCENTBINS+0.5);
        sprintf( line, "ptPtMinusSq_%i", jPt );
        hptPtSumSq[jPt][2] = new TH1D(line,line,NPTCENTBINS,0.5,NPTCENTBINS+0.5);
    }
}

//--------------------------------------------------------------------------
void StEStructFluctuations::deleteArraysAndHistograms() {

#ifdef TERMINUSSTUDY
    for (int jPhi=0;jPhi<NPHIBINS;jPhi++) {
        for (int jEta=0;jEta<NETABINS;jEta++) {
            delete sum[jPhi][jEta];
            delete plus[jPhi][jEta];
            delete minus[jPhi][jEta];
            delete plusminus[jPhi][jEta];
            for (int j=0;j<NCENTBINS;j++) {
                delete diff[jPhi][jEta][j];
            }
        }
    }
#endif

cout << "Deleting occupancy histograms." << endl;
    for (int i=0;i<NCENTBINS;i++) {
        delete occNSum[i];
        delete occNPlus[i];
        delete occNMinus[i];
        delete occNDiff[i];
        delete occPSum[i];
        delete occPPlus[i];
        delete occPMinus[i];
        delete occPDiff[i];
        delete occPNSum[i];
        delete occPNPlus[i];
        delete occPNMinus[i];
        delete occPNDiff[i];
    }
    for (int i=0;i<NPTCENTBINS;i++) {
        for (int j=0;j<NPTBINS;j++) {
            delete occptNSum[i][j];
            delete occptNPlus[i][j];
            delete occptNMinus[i][j];
            delete occptNDiff[i][j];
            delete occptPSum[i][j];
            delete occptPPlus[i][j];
            delete occptPMinus[i][j];
            delete occptPDiff[i][j];
            delete occptPNSum[i][j];
            delete occptPNPlus[i][j];
            delete occptPNMinus[i][j];
            delete occptPNDiff[i][j];
        }
    }

    delete hnBins;
    delete hoffset;
    delete hfUnique;

cout << "freeing h Array histograms." << endl;
    for (int jC=0;jC<NCENTBINS;jC++) {
        for (int jStat=0;jStat<5;jStat++) {
            delete hTotEvents[jC][jStat];
        }

        for (int jStat=0;jStat<2;jStat++) {
            delete hNSum[jC][jStat];
            delete hNDiff[jC][jStat];
            delete hNPlus[jC][jStat];
            delete hNMinus[jC][jStat];
        }
        delete hNPlusMinus[jC];
        for (int jStat=0;jStat<4;jStat++) {
            delete hPSum[jC][jStat];
            delete hPPlus[jC][jStat];
            delete hPMinus[jC][jStat];
        }
        for (int jStat=0;jStat<4;jStat++) {
            delete hPPlusMinus[jC][jStat];
        }
        for (int jStat=0;jStat<4;jStat++) {
            delete hPNSum[jC][jStat];
            delete hPNPlus[jC][jStat];
            delete hPNMinus[jC][jStat];
        }
        for (int jStat=0;jStat<4;jStat++) {
            delete hPNPlusMinus[jC][jStat];
        }
    }
    for (int jType=0;jType<3;jType++) {
        delete hPtSumSq[jType];
    }

cout << "freeing hpt Array histograms." << endl;
    for (int jC=0;jC<NPTCENTBINS;jC++) {
        for (int jPt=0;jPt<NPTBINS;jPt++) {
            for (int jStat=0;jStat<5;jStat++) {
                delete hptTotEvents[jC][jPt][jStat];
            }

            for (int jStat=0;jStat<2;jStat++) {
                delete hptNSum[jC][jPt][jStat];
                delete hptNDiff[jC][jPt][jStat];
                delete hptNPlus[jC][jPt][jStat];
                delete hptNMinus[jC][jPt][jStat];
            }
            delete hptNPlusMinus[jC][jPt];
            for (int jStat=0;jStat<4;jStat++) {
                delete hptPSum[jC][jPt][jStat];
                delete hptPPlus[jC][jPt][jStat];
                delete hptPMinus[jC][jPt][jStat];
            }
            for (int jStat=0;jStat<4;jStat++) {
                delete hptPPlusMinus[jC][jPt][jStat];
            }
            for (int jStat=0;jStat<4;jStat++) {
                delete hptPNSum[jC][jPt][jStat];
                delete hptPNPlus[jC][jPt][jStat];
                delete hptPNMinus[jC][jPt][jStat];
            }
            for (int jStat=0;jStat<4;jStat++) {
                delete hptPNPlusMinus[jC][jPt][jStat];
            }
        }
    }
    for (int jPt=0;jPt<NPTBINS;jPt++) {
        for (int jType=0;jType<3;jType++) {
            delete hptPtSumSq[jPt][jType];
        }
    }
}




int StEStructFluctuations::getCentBin( int mult ) {
//>>>>> pp - AuAu difference.
// The next line is for 130GeV Au-Au data for tracks in |eta| < 1
//    int multCut[] = {10, 35, 91, 191, 351, 591, 960};
// The next line is for common mult for 200GeV data using refMult.
//    int multCut[] = { 2, 7, 14, 30, 56, 94, 146, 217, 312, 431, 510, 1000};
// The next line is for 200GeV Au-Au data for tracks in |eta| < 1
#ifndef USEREFMULT
#ifdef AUAUDATA
    int multCut[] = { 4, 14, 30, 60, 120, 195, 285, 420, 570, 780, 900, 2000};
#endif
#ifdef PPDATA
// The next line is for common mult for 200GeV pp data using refMult.
// I use the same cuts for tracks in |eta| < 1.
//    int multCut[] = { 2, 5, 8, 11, 14, 25};
    int multCut[] = { 3, 8, 25};
#endif
#endif
#ifdef USEREFMULT
    int multCut[] = { 2, 8, 14, 30, 56, 94, 146, 217, 312, 431, 510, 1000};
#endif

    if (mult < multCut[0]) {
        return -1;
    }
    for (int j=0;j<NCENTBINS;j++) {
        if ((multCut[j] <= mult) && (mult < multCut[j+1])) {
            return j;
        }
    }
    return -1;
}
int StEStructFluctuations::getPtCentBin( int jCent ) {
//>>>>> pp - AuAu difference.
    // For AuAu we have three pt dependent centrality bins.
    // We ignore the two most peripheral bins.
#ifdef AUAUDATA
    if (jCent >= 2) {
        return (jCent-2)/3;
    } else {
        return -1;
    }
#endif
#ifdef PPDATA
    // For pp we have two pt dependent centrality bins.
    if (jCent < 2) {
        return 0;
    } else {
        return 1;
    }
#endif
}
int StEStructFluctuations::getPtBin( float pt ) {
//    float ptCut[] = {0.15, 0.22, 0.28, 0.34, 0.40, 0.48,
//                       0.58, 0.70, 0.88, 1.20, 2.00, 5.00};
//    float ptCut[] = {0.15, 0.34, 0.58, 1.20, 5.00};
    float ptCut[] = {0.15, 0.5, 5.00};

    for (int j=0;j<NPTBINS;j++) {
        if ((ptCut[j] <= pt) && (pt < ptCut[j+1])) {
            return j;
        }
    }
    return -1;
}

