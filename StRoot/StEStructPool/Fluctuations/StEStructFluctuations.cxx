
#include "StEStructFluctuations.h"

#include "TH1F.h"
#include "TH2F.h"
#include "TFile.h"

#include "StEStructPool/EventMaker/StEStructEvent.h"
#include "StEStructPool/EventMaker/StEStructTrack.h"
#include "StTimer.hh"

#include <stdlib.h>


ClassImp(StEStructFluctuations)

//--------------------------------------------------------------------------
StEStructFluctuations::StEStructFluctuations(int mode, int invokePairCuts,
            int etaSumMode, int phiSumMode): manalysisMode(mode), mskipPairCuts(false), mdoPairCutHistograms(false) {
    doingPairCuts = invokePairCuts;
    etaSummingMode   = etaSumMode;
    phiSummingMode   = phiSumMode;
    init();
}

//--------------------------------------------------------------------------
StEStructFluctuations::StEStructFluctuations(const char* cutFileName, int mode, int invokePairCuts,
                                             int etaSumMode, int phiSumMode): manalysisMode(mode), mskipPairCuts(false), mdoPairCutHistograms(false), mPair(cutFileName) {
    doingPairCuts = invokePairCuts;
    etaSummingMode   = etaSumMode;
    phiSummingMode   = phiSumMode;
    init();
}

//--------------------------------------------------------------------------
StEStructFluctuations::~StEStructFluctuations() {
    cleanUp();
}


void StEStructFluctuations::init() {

  mCurrentEvent=NULL;
  mtimer=NULL;

  if(manalysisMode & 1) {
     mskipPairCuts=true;
  } else if(manalysisMode & 2){
    mdoPairCutHistograms=true;
  }

  ms  = new multStruct();
  initArrays();
}


void StEStructFluctuations::cleanUp() {
    delete ms;
    deleteArrays();
    deleteHistograms();
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
    int nPlus = 0, nMinus = 0;
    int nPlusAdded = 0, nMinusAdded = 0;

    ms->NewEvent(mCurrentEvent->Vx(), mCurrentEvent->Vy(), mCurrentEvent->Vz() );

    tc = mCurrentEvent->TrackCollectionP();
    for(Iter=tc->begin(); Iter!=tc->end(); ++Iter){
        nPlus++;
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

        nPlusAdded++;
        ms->AddTrack( iPhi, iEta, iPt, +1, t->Pt() );
    }
    tc = mCurrentEvent->TrackCollectionM();
    for(Iter=tc->begin(); Iter!=tc->end(); ++Iter){
        nMinus++;
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

        nMinusAdded++;
        ms->AddTrack( iPhi, iEta, iPt, -1, t->Pt() );
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

    if ((jCent = getCentBin(ms->GetRefMult())) < 0) {
        return;
    }
    nCentEvents[jCent]++;
    jPtCent = getPtCentBin(jCent);

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
                    for (jPt=0;jPt<NPTBINS;jPt++) {
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
                        AddToPtBin( jPtCent, jPt, iBin,
                                    ptNPlus,   ptNMinus,
                                    ptPtPlus,  ptPtMinus,
                                    ptPt2Plus, ptPt2Minus );
                        NPlus    += ptNPlus;
                        NMinus   += ptNMinus;
                        PtPlus   += ptPtPlus;
                        PtMinus  += ptPtMinus;
                        Pt2Plus  += ptPt2Plus;
                        Pt2Minus += ptPt2Minus;
                    }

                    AddToBin( jCent,      iBin,
                              NPlus,   NMinus,
                              PtPlus,  PtMinus,
                              Pt2Plus, Pt2Minus );
#ifdef TERMINUSSTUDY
                    sum[jPhi][jEta]->Fill(NPlus+NMinus);
                    plus[jPhi][jEta]->Fill(NPlus);
                    minus[jPhi][jEta]->Fill(NMinus);
                    diff[jPhi][jEta][jCent]->Fill(NPlus-NMinus);
#endif

                    iEta++;
                }
                iPhi++;
            }
        }
    }

    // Increment occupancy histograms.
    delEta = (ETAMAX-ETAMIN) / NETABINS;
    delPhi = 2.0*3.1415926 / NPHIBINS;
    double nsum[NCENTBINS],  ndiff[NCENTBINS],  nplus[NCENTBINS],  nminus[NCENTBINS];
    double psum[NCENTBINS],  pdiff[NCENTBINS],  pplus[NCENTBINS],  pminus[NCENTBINS];
    double ptnsum[NPTCENTBINS][NPTBINS],  ptndiff[NPTCENTBINS][NPTBINS],  ptnplus[NPTCENTBINS][NPTBINS],  ptnminus[NPTCENTBINS][NPTBINS];
    double ptpsum[NPTCENTBINS][NPTBINS],  ptpdiff[NPTCENTBINS][NPTBINS],  ptpplus[NPTCENTBINS][NPTBINS],  ptpminus[NPTCENTBINS][NPTBINS];
    for (int ic=0;ic<NCENTBINS;ic++) {
        nsum[ic]   = 0;
        ndiff[ic]  = 0;
        nplus[ic]  = 0;
        nminus[ic] = 0;
        psum[ic]   = 0;
        pdiff[ic]  = 0;
        pplus[ic]  = 0;
        pminus[ic] = 0;
    }
    for (int ic=0;ic<NPTCENTBINS;ic++) {
        for (int ipt=0;ipt<NPTBINS;ipt++) {
            ptnsum[ic][ipt]   = 0;
            ptndiff[ic][ipt]  = 0;
            ptnplus[ic][ipt]  = 0;
            ptnminus[ic][ipt] = 0;
            ptpsum[ic][ipt]   = 0;
            ptpdiff[ic][ipt]  = 0;
            ptpplus[ic][ipt]  = 0;
            ptpminus[ic][ipt] = 0;
        }
    }
    for (int jPhi=0;jPhi<NPHIBINS;jPhi++) {
        double dp = -3.1415926 + delPhi*(jPhi+0.5);
        for (int jEta=0;jEta<NETABINS;jEta++) {
            double de = ETAMIN + delEta*(jEta+0.5);
            double ns, nd, ps, pd;
            double nPlus = 0, nMinus = 0, pPlus = 0, pMinus = 0;
            for (int jPt=0;jPt<NPTBINS;jPt++) {
                double np, nm, pp, pm;
                np = ms->GetNPlus(jPhi,jEta,jPt);
                nm = ms->GetNMinus(jPhi,jEta,jPt);
                pp = ms->GetPtPlus(jPhi,jEta,jPt);
                pm = ms->GetPtMinus(jPhi,jEta,jPt);
                ns = np + nm;
                nd = np - nm;
                ps = pp + pm;
                pd = pp - pm;
                if (jPtCent >= 0) {
                    occptNSum[jPtCent][jPt]->Fill(dp,de,ns);
                    occptNPlus[jPtCent][jPt]->Fill(dp,de,np);
                    occptNMinus[jPtCent][jPt]->Fill(dp,de,nm);
                    occptNDiff[jPtCent][jPt]->Fill(dp,de,nd);
                    occptPSum[jPtCent][jPt]->Fill(dp,de,ps);
                    occptPPlus[jPtCent][jPt]->Fill(dp,de,pp);
                    occptPMinus[jPtCent][jPt]->Fill(dp,de,pm);
                    occptPDiff[jPtCent][jPt]->Fill(dp,de,pd);
                    occptPNSum[jPtCent][jPt]->Fill(dp,de,ns*ps);
                    occptPNPlus[jPtCent][jPt]->Fill(dp,de,np*pp);
                    occptPNMinus[jPtCent][jPt]->Fill(dp,de,nm*pm);
                    occptPNDiff[jPtCent][jPt]->Fill(dp,de,nd*pd);

                    ptnsum[jPtCent][jPt]    += ns;
                    ptndiff[jPtCent][jPt]   += nd;
                    ptnplus[jPtCent][jPt]   += np;
                    ptnminus[jPtCent][jPt]  += nm;
                    ptpsum[jPtCent][jPt]    += ps;
                    ptpdiff[jPtCent][jPt]   += pd;
                    ptpplus[jPtCent][jPt]   += pp;
                    ptpminus[jPtCent][jPt]  += pm;
                }
                nPlus  += np;
                nMinus += nm;
                pPlus  += pp;
                pMinus += pm;
            }
            ns = nPlus + nMinus;
            nd = nPlus - nMinus;
            ps = pPlus + pMinus;
            pd = pPlus - pMinus;
            occNSum[jCent]->Fill(dp,de,ns);
            occNPlus[jCent]->Fill(dp,de,nPlus);
            occNMinus[jCent]->Fill(dp,de,nMinus);
            occNDiff[jCent]->Fill(dp,de,nd);
            occPSum[jCent]->Fill(dp,de,ps);
            occPPlus[jCent]->Fill(dp,de,pPlus);
            occPMinus[jCent]->Fill(dp,de,pMinus);
            occPDiff[jCent]->Fill(dp,de,pd);
            occPNSum[jCent]->Fill(dp,de,ns*ps);
            occPNPlus[jCent]->Fill(dp,de,nPlus*pPlus);
            occPNMinus[jCent]->Fill(dp,de,nMinus*pMinus);
            occPNDiff[jCent]->Fill(nd*pd);

            nsum[jCent]    += ns;
            ndiff[jCent]   += nd;
            nplus[jCent]   += nPlus;
            nminus[jCent]  += nMinus;
            psum[jCent]    += ps;
            pdiff[jCent]   += pd;
            pplus[jCent]   += pPlus;
            pminus[jCent]  += pMinus;
        }
    }
    multNSum[jCent]->Fill(nsum[jCent]);
    multNPlus[jCent]->Fill(nplus[jCent]);
    multNMinus[jCent]->Fill(nminus[jCent]);
    multNDiff[jCent]->Fill(ndiff[jCent]);
    multPSum[jCent]->Fill(psum[jCent]);
    multPPlus[jCent]->Fill(pplus[jCent]);
    multPMinus[jCent]->Fill(pminus[jCent]);
    multPDiff[jCent]->Fill(pdiff[jCent]);
    if (jPtCent >= 0) {
        for (int jPt=0;jPt<NPTBINS;jPt++) {
            multptNSum[jPtCent][jPt]->Fill(ptnsum[jPtCent][jPt]);
            multptNPlus[jPtCent][jPt]->Fill(ptnplus[jPtCent][jPt]);
            multptNMinus[jPtCent][jPt]->Fill(ptnminus[jPtCent][jPt]);
            multptNDiff[jPtCent][jPt]->Fill(ptndiff[jPtCent][jPt]);
            multptPSum[jPtCent][jPt]->Fill(ptpsum[jPtCent][jPt]);
            multptPPlus[jPtCent][jPt]->Fill(ptpplus[jPtCent][jPt]);
            multptPMinus[jPtCent][jPt]->Fill(ptpminus[jPtCent][jPt]);
            multptPDiff[jPtCent][jPt]->Fill(ptpdiff[jPtCent][jPt]);
        }
    }
}
void StEStructFluctuations::AddToPtBin( int jPtCent, int jPt, int iBin,
                                        double Nplus,    double Nminus,
                                        double Ptplus,   double Ptminus,
                                        double PtSqplus, double PtSqminus ) {

    if ((jPtCent < 0) || (NPTCENTBINS <= jPtCent)) {
        return;
    }
    if ((jPt < 0) || (NPTBINS <= jPt)) {
        return;
    }

    int jBin = iBin - 1;
    ptTotEvents[jPtCent][jPt][0][jBin]++;

    // This routine is called in an inner loop.
    // Although the compiler should do the optimizations I try
    // minimizing number of sqrt() and eliminating pow().
    double Nsum  = Nplus + Nminus;
    if (Nsum < 0) {
        return;
    }

    double Ndiff   = Nplus - Nminus;
    double Ptsum   = Ptplus + Ptminus;
    double Ptdiff  = Ptplus - Ptminus;
    double PtSqsum = PtSqplus + PtSqminus;
    double sqs = sqrt(Nsum);
    double sqp = sqrt(Nplus);
    double sqm = sqrt(Nminus);
    double r;

    if (Nsum > 0) {
        ptTotEvents[jPtCent][jPt][1][jBin]++;

        ptNSum[jPtCent][jPt][0][jBin] += Nsum;
        ptNSum[jPtCent][jPt][1][jBin] += Nsum*Nsum;

        r = Ptsum*Ptsum/Nsum;
        ptPSum[jPtCent][jPt][0][jBin] += Ptsum;
        ptPSum[jPtCent][jPt][1][jBin] += r;
        ptPSum[jPtCent][jPt][2][jBin] += r*Ptsum/Nsum;
        ptPSum[jPtCent][jPt][3][jBin] += r*r/Nsum;
        ptPSum[jPtCent][jPt][4][jBin] += PtSqsum;

        ptPNSum[jPtCent][jPt][0][jBin] += sqs;
        ptPNSum[jPtCent][jPt][1][jBin] += Nsum*sqs;
        ptPNSum[jPtCent][jPt][2][jBin] += Ptsum/sqs;
        ptPNSum[jPtCent][jPt][3][jBin] += Ptsum*sqs;

        ptNDiff[jPtCent][jPt][0][jBin] += Ndiff;
        ptNDiff[jPtCent][jPt][1][jBin] += Ndiff*Ndiff;

        double r1 = Ptdiff*Ptdiff/Nsum;
        double r2 = Ptdiff*Ndiff/Nsum;
        double r3 = Ndiff*Ndiff/Nsum;
        ptPDiff[jPtCent][jPt][0][jBin] += r1;
        ptPDiff[jPtCent][jPt][1][jBin] += r2;
        ptPDiff[jPtCent][jPt][2][jBin] += r3;
        ptPDiff[jPtCent][jPt][3][jBin] += r1*r1/Nsum;
        ptPDiff[jPtCent][jPt][4][jBin] += r1*r2/Nsum;
        ptPDiff[jPtCent][jPt][5][jBin] += r1*r3/Nsum;
        ptPDiff[jPtCent][jPt][6][jBin] += r2*r3/Nsum;
        ptPDiff[jPtCent][jPt][7][jBin] += r3*r3/Nsum;

        ptPNDiff[jPtCent][jPt][0][jBin] += Ndiff/sqrt(Nsum);
        ptPNDiff[jPtCent][jPt][1][jBin] += Ndiff*Ndiff/sqrt(Nsum);
        ptPNDiff[jPtCent][jPt][2][jBin] += Ptdiff/sqrt(Nsum);
        ptPNDiff[jPtCent][jPt][3][jBin] += Ptdiff*Ndiff/sqrt(Nsum);
    }

    if (Nplus > 0) {
        ptTotEvents[jPtCent][jPt][2][jBin]++;

        ptNPlus[jPtCent][jPt][0][jBin] += Nplus;
        ptNPlus[jPtCent][jPt][1][jBin] += Nplus*Nplus;

        r = Ptplus*Ptplus/Nplus;
        ptPPlus[jPtCent][jPt][0][jBin] += Ptplus;
        ptPPlus[jPtCent][jPt][1][jBin] += r;
        ptPPlus[jPtCent][jPt][2][jBin] += r*Ptplus/Nplus;
        ptPPlus[jPtCent][jPt][3][jBin] += r*r/Nplus;
        ptPPlus[jPtCent][jPt][4][jBin] += PtSqplus;

        ptPNPlus[jPtCent][jPt][0][jBin] += sqp;
        ptPNPlus[jPtCent][jPt][1][jBin] += Nplus*sqp;
        ptPNPlus[jPtCent][jPt][2][jBin] += Ptplus/sqp;
        ptPNPlus[jPtCent][jPt][3][jBin] += Ptplus*sqp;
    }

    if (Nminus > 0) {
        ptTotEvents[jPtCent][jPt][3][jBin]++;

        ptNMinus[jPtCent][jPt][0][jBin] += Nminus;
        ptNMinus[jPtCent][jPt][1][jBin] += Nminus*Nminus;

        r = Ptminus*Ptminus/Nminus;
        ptPMinus[jPtCent][jPt][0][jBin] += Ptminus;
        ptPMinus[jPtCent][jPt][1][jBin] += r;
        ptPMinus[jPtCent][jPt][2][jBin] += r*Ptminus/Nminus;
        ptPMinus[jPtCent][jPt][3][jBin] += r*r/Nminus;
        ptPMinus[jPtCent][jPt][4][jBin] += PtSqminus;

        ptPNMinus[jPtCent][jPt][0][jBin] += sqm;
        ptPNMinus[jPtCent][jPt][1][jBin] += Nminus*sqm;
        ptPNMinus[jPtCent][jPt][2][jBin] += Ptminus/sqm;
        ptPNMinus[jPtCent][jPt][3][jBin] += Ptminus*sqm;
    }

    if ((Nplus > 0) && (Nminus > 0)) {
        ptTotEvents[jPtCent][jPt][4][jBin]++;

        ptNPlusMinus[jPtCent][jPt][jBin] += Nplus*Nminus;

        ptPPlusMinus[jPtCent][jPt][0][jBin] += sqp*sqm;
        ptPPlusMinus[jPtCent][jPt][1][jBin] += Ptplus*sqm/sqp;
        ptPPlusMinus[jPtCent][jPt][2][jBin] += Ptminus*sqp/sqm;
        ptPPlusMinus[jPtCent][jPt][3][jBin] += Ptplus*Ptminus/(sqp*sqm);
        ptPPlusMinus[jPtCent][jPt][4][jBin] += Nplus;
        ptPPlusMinus[jPtCent][jPt][5][jBin] += Ptplus;
        ptPPlusMinus[jPtCent][jPt][6][jBin] += Nminus;
        ptPPlusMinus[jPtCent][jPt][7][jBin] += Ptminus;

        ptPNPlusMinus[jPtCent][jPt][0][jBin] += sqp;
        ptPNPlusMinus[jPtCent][jPt][1][jBin] += Nminus*sqp;
        ptPNPlusMinus[jPtCent][jPt][2][jBin] += Ptplus/sqp;
        ptPNPlusMinus[jPtCent][jPt][3][jBin] += Ptplus*Nminus/sqp;
        ptPNPlusMinus[jPtCent][jPt][4][jBin] += sqm;
        ptPNPlusMinus[jPtCent][jPt][5][jBin] += Nplus*sqm;
        ptPNPlusMinus[jPtCent][jPt][6][jBin] += Ptminus/sqm;
        ptPNPlusMinus[jPtCent][jPt][7][jBin] += Ptminus*Nplus/sqm;
    }
}
void StEStructFluctuations::AddToBin( int jCent,       int iBin,
                                      double Nplus,    double Nminus,
                                      double Ptplus,   double Ptminus,
                                      double PtSqplus, double PtSqminus ) {

    if ((jCent < 0) || (NCENTBINS <= jCent)) {
        return;
    }

    int jBin = iBin - 1;
    TotEvents[jCent][0][jBin]++;

    // This routine is called in an inner loop.
    // Although the compiler should do the optimizations I try
    // minimizing number of sqrt() and eliminating pow()
    double Nsum = Nplus + Nminus;
    if (Nsum < 0) {
        return;
    }

    double Ndiff    = Nplus - Nminus;
    double Ptsum    = Ptplus   + Ptminus;
    double Ptdiff   = Ptplus   - Ptminus;
    double PtSqsum  = PtSqplus + PtSqminus;
    double sqs = sqrt(Nsum);
    double sqp = sqrt(Nplus);
    double sqm = sqrt(Nminus);
    double r;

    if (Nsum > 0) {
        TotEvents[jCent][1][jBin]++;

        NSum[jCent][0][jBin] += Nsum;
        NSum[jCent][1][jBin] += Nsum*Nsum;

        r  = Ptsum*Ptsum/Nsum;
        PSum[jCent][0][jBin] += Ptsum;
        PSum[jCent][1][jBin] += r;
        PSum[jCent][2][jBin] += r*Ptsum/Nsum;
        PSum[jCent][3][jBin] += r*r/Nsum;
        PSum[jCent][4][jBin] += PtSqsum;

        PNSum[jCent][0][jBin] += sqrt(Nsum);
        PNSum[jCent][1][jBin] += Nsum*sqs;
        PNSum[jCent][2][jBin] += Ptsum/sqs;
        PNSum[jCent][3][jBin] += Ptsum*sqs;

        NDiff[jCent][0][jBin] += Ndiff;
        NDiff[jCent][1][jBin] += Ndiff*Ndiff;

        double r1 = Ptdiff*Ptdiff/Nsum;
        double r2 = Ptdiff*Ndiff/Nsum;
        double r3 = Ndiff*Ndiff/Nsum;
        PDiff[jCent][0][jBin] += r1;
        PDiff[jCent][1][jBin] += r2;
        PDiff[jCent][2][jBin] += r3;
        PDiff[jCent][3][jBin] += r1*r1/Nsum;
        PDiff[jCent][4][jBin] += r1*r2/Nsum;
        PDiff[jCent][5][jBin] += r1*r3/Nsum;
        PDiff[jCent][6][jBin] += r2*r3/Nsum;
        PDiff[jCent][7][jBin] += r3*r3/Nsum;

        PNDiff[jCent][0][jBin] += Ndiff/sqrt(Nsum);
        PNDiff[jCent][1][jBin] += Ndiff*Ndiff/sqrt(Nsum);
        PNDiff[jCent][2][jBin] += Ptdiff/sqrt(Nsum);
        PNDiff[jCent][3][jBin] += Ptdiff*Ndiff/sqrt(Nsum);
    }

    if (Nplus > 0) {
        TotEvents[jCent][2][jBin]++;

        NPlus[jCent][0][jBin] += Nplus;
        NPlus[jCent][1][jBin] += Nplus*Nplus;

        r  = Ptplus*Ptplus/Nplus;
        PPlus[jCent][0][jBin] += Ptplus;
        PPlus[jCent][1][jBin] += r;
        PPlus[jCent][2][jBin] += r*Ptplus/Nplus;
        PPlus[jCent][3][jBin] += r*r/Nplus;
        PPlus[jCent][4][jBin] += PtSqplus;

        PNPlus[jCent][0][jBin] += sqp;
        PNPlus[jCent][1][jBin] += Nplus*sqp;
        PNPlus[jCent][2][jBin] += Ptplus/sqp;
        PNPlus[jCent][3][jBin] += Ptplus*sqp;
    }

    if (Nminus > 0) {
        TotEvents[jCent][3][jBin]++;

        NMinus[jCent][0][jBin] += Nminus;
        NMinus[jCent][1][jBin] += Nminus*Nminus;

        r  = Ptminus*Ptminus/Nminus;
        PMinus[jCent][0][jBin] += Ptminus;
        PMinus[jCent][1][jBin] += r;
        PMinus[jCent][2][jBin] += r*Ptminus/Nminus;
        PMinus[jCent][3][jBin] += r*r/Nminus;
        PMinus[jCent][4][jBin] += PtSqminus;

        PNMinus[jCent][0][jBin] += sqm;
        PNMinus[jCent][1][jBin] += Nminus*sqm;
        PNMinus[jCent][2][jBin] += Ptminus/sqm;
        PNMinus[jCent][3][jBin] += Ptminus*sqm;
    }

    if ((Nplus > 0) && (Nminus > 0)) {
        TotEvents[jCent][4][jBin]++;

        NPlusMinus[jCent][jBin] += Nplus*Nminus;

        PPlusMinus[jCent][0][jBin] += sqp*sqm;
        PPlusMinus[jCent][1][jBin] += Ptplus*sqm/sqp;
        PPlusMinus[jCent][2][jBin] += Ptminus*sqp/sqm;
        PPlusMinus[jCent][3][jBin] += Ptplus*Ptminus/(sqp*sqm);
        PPlusMinus[jCent][4][jBin] += Nplus;
        PPlusMinus[jCent][5][jBin] += Ptplus;
        PPlusMinus[jCent][6][jBin] += Nminus;
        PPlusMinus[jCent][7][jBin] += Ptminus;

        PNPlusMinus[jCent][0][jBin] += sqp;
        PNPlusMinus[jCent][1][jBin] += Nminus*sqp;
        PNPlusMinus[jCent][2][jBin] += Ptplus/sqp;
        PNPlusMinus[jCent][3][jBin] += Ptplus*Nminus/sqp;
        PNPlusMinus[jCent][4][jBin] += sqm;
        PNPlusMinus[jCent][5][jBin] += Nplus*sqm;
        PNPlusMinus[jCent][6][jBin] += Ptminus/sqm;
        PNPlusMinus[jCent][7][jBin] += Ptminus*Nplus/sqm;
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
int StEStructFluctuations::getPhiStart( int iPhi, int dPhi ) {
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
int StEStructFluctuations::getNumEtaBins( int dEta ) {
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
int StEStructFluctuations::getNumPhiBins( int dPhi ) {
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

//--------------------------------------------------------------------------
//
//------------ Below are init, delete, write functions -------///
//


//--------------------------------------------------------------------------
void StEStructFluctuations::setOutputFileName(const char* outFileName) {
}
void StEStructFluctuations::writeHistograms(TFile* tf){

    tf->cd();
    initHistograms();
    fillHistograms();

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
        for (int jStat=0;jStat<5;jStat++) {
            hPSum[jC][jStat]->Write();
            hPPlus[jC][jStat]->Write();
            hPMinus[jC][jStat]->Write();
        }
        for (int jStat=0;jStat<8;jStat++) {
            hPDiff[jC][jStat]->Write();
        }
        for (int jStat=0;jStat<8;jStat++) {
            hPPlusMinus[jC][jStat]->Write();
        }
        for (int jStat=0;jStat<4;jStat++) {
            hPNSum[jC][jStat]->Write();
            hPNDiff[jC][jStat]->Write();
            hPNPlus[jC][jStat]->Write();
            hPNMinus[jC][jStat]->Write();
        }
        for (int jStat=0;jStat<8;jStat++) {
            hPNPlusMinus[jC][jStat]->Write();
        }
    }

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
            for (int jStat=0;jStat<5;jStat++) {
                hptPSum[jC][jPt][jStat]->Write();
                hptPPlus[jC][jPt][jStat]->Write();
                hptPMinus[jC][jPt][jStat]->Write();
            }
            for (int jStat=0;jStat<8;jStat++) {
                hptPDiff[jC][jPt][jStat]->Write();
            }
            for (int jStat=0;jStat<8;jStat++) {
                hptPPlusMinus[jC][jPt][jStat]->Write();
            }
            for (int jStat=0;jStat<4;jStat++) {
                hptPNSum[jC][jPt][jStat]->Write();
                hptPNDiff[jC][jPt][jStat]->Write();
                hptPNPlus[jC][jPt][jStat]->Write();
                hptPNMinus[jC][jPt][jStat]->Write();
            }
            for (int jStat=0;jStat<8;jStat++) {
                hptPNPlusMinus[jC][jPt][jStat]->Write();
            }
        }
    }

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

    cout << "Looped over " << nTotEvents << " total events" << endl;
    cout << "  For each centrality have ";
    for (int jCent=0;jCent<NCENTBINS;jCent++) {
        cout << nCentEvents[jCent] << "  ";
    }
    cout << endl;
}
void StEStructFluctuations::fillHistograms(){

    // Here I copy from arrays top histograms so I can write the histograms.

    for (int jC=0;jC<NCENTBINS;jC++) {
        for (int jStat=0;jStat<5;jStat++) {
            for (int iBin=0;iBin<TOTBINS;iBin++) {
                 hTotEvents[jC][jStat]->SetBinContent(iBin+1,TotEvents[jC][jStat][iBin]);
            }
        }

        for (int jStat=0;jStat<2;jStat++) {
            for (int iBin=0;iBin<TOTBINS;iBin++) {
                hNSum[jC][jStat]->SetBinContent(iBin+1,NSum[jC][jStat][iBin]);
                hNDiff[jC][jStat]->SetBinContent(iBin+1,NDiff[jC][jStat][iBin]);
                hNPlus[jC][jStat]->SetBinContent(iBin+1,NPlus[jC][jStat][iBin]);
                hNMinus[jC][jStat]->SetBinContent(iBin+1,NMinus[jC][jStat][iBin]);
            }
        }
        for (int iBin=0;iBin<TOTBINS;iBin++) {
            hNPlusMinus[jC]->SetBinContent(iBin+1,NPlusMinus[jC][iBin]);
        }
        for (int jStat=0;jStat<5;jStat++) {
            for (int iBin=0;iBin<TOTBINS;iBin++) {
                hPSum[jC][jStat]->SetBinContent(iBin+1,PSum[jC][jStat][iBin]);
                hPPlus[jC][jStat]->SetBinContent(iBin+1,PPlus[jC][jStat][iBin]);
                hPMinus[jC][jStat]->SetBinContent(iBin+1,PMinus[jC][jStat][iBin]);
            }
        }
        for (int jStat=0;jStat<8;jStat++) {
            for (int iBin=0;iBin<TOTBINS;iBin++) {
                hPDiff[jC][jStat]->SetBinContent(iBin+1,PDiff[jC][jStat][iBin]);
            }
        }
        for (int jStat=0;jStat<8;jStat++) {
            for (int iBin=0;iBin<TOTBINS;iBin++) {
                hPPlusMinus[jC][jStat]->SetBinContent(iBin+1,PPlusMinus[jC][jStat][iBin]);
            }
        }
        for (int jStat=0;jStat<4;jStat++) {
            for (int iBin=0;iBin<TOTBINS;iBin++) {
                hPNSum[jC][jStat]->SetBinContent(iBin+1,PNSum[jC][jStat][iBin]);
                hPNDiff[jC][jStat]->SetBinContent(iBin+1,PNDiff[jC][jStat][iBin]);
                hPNPlus[jC][jStat]->SetBinContent(iBin+1,PNPlus[jC][jStat][iBin]);
                hPNMinus[jC][jStat]->SetBinContent(iBin+1,PNMinus[jC][jStat][iBin]);
            }
        }
        for (int jStat=0;jStat<8;jStat++) {
            for (int iBin=0;iBin<TOTBINS;iBin++) {
                hPNPlusMinus[jC][jStat]->SetBinContent(iBin+1,PNPlusMinus[jC][jStat][iBin]);
            }
        }
    }

    for (int jC=0;jC<NPTCENTBINS;jC++) {
        for (int jPt=0;jPt<NPTBINS;jPt++) {
            for (int jStat=0;jStat<5;jStat++) {
                for (int iBin=0;iBin<TOTBINS;iBin++) {
                    hptTotEvents[jC][jPt][jStat]->SetBinContent(iBin+1,ptTotEvents[jC][jPt][jStat][iBin]);
                }
            }
            for (int jStat=0;jStat<2;jStat++) {
                for (int iBin=0;iBin<TOTBINS;iBin++) {
                    hptNSum[jC][jPt][jStat]->SetBinContent(iBin+1,ptNSum[jC][jPt][jStat][iBin]);
                    hptNDiff[jC][jPt][jStat]->SetBinContent(iBin+1,ptNDiff[jC][jPt][jStat][iBin]);
                    hptNPlus[jC][jPt][jStat]->SetBinContent(iBin+1,ptNPlus[jC][jPt][jStat][iBin]);
                    hptNMinus[jC][jPt][jStat]->SetBinContent(iBin+1,ptNMinus[jC][jPt][jStat][iBin]);
                }
            }
            for (int iBin=0;iBin<TOTBINS;iBin++) {
                hptNPlusMinus[jC][jPt]->SetBinContent(iBin+1,ptNPlusMinus[jC][jPt][iBin]);
            }
            for (int jStat=0;jStat<5;jStat++) {
                for (int iBin=0;iBin<TOTBINS;iBin++) {
                    hptPSum[jC][jPt][jStat]->SetBinContent(iBin+1,ptPSum[jC][jPt][jStat][iBin]);
                    hptPPlus[jC][jPt][jStat]->SetBinContent(iBin+1,ptPPlus[jC][jPt][jStat][iBin]);
                    hptPMinus[jC][jPt][jStat]->SetBinContent(iBin+1,ptPMinus[jC][jPt][jStat][iBin]);
                }
            }
            for (int jStat=0;jStat<8;jStat++) {
                for (int iBin=0;iBin<TOTBINS;iBin++) {
                    hptPDiff[jC][jPt][jStat]->SetBinContent(iBin+1,ptPDiff[jC][jPt][jStat][iBin]);
                }
            }
            for (int jStat=0;jStat<8;jStat++) {
                for (int iBin=0;iBin<TOTBINS;iBin++) {
                    hptPPlusMinus[jC][jPt][jStat]->SetBinContent(iBin+1,ptPPlusMinus[jC][jPt][jStat][iBin]);
                }
            }
            for (int jStat=0;jStat<4;jStat++) {
                for (int iBin=0;iBin<TOTBINS;iBin++) {
                    hptPNSum[jC][jPt][jStat]->SetBinContent(iBin+1,ptPNSum[jC][jPt][jStat][iBin]);
                    hptPNDiff[jC][jPt][jStat]->SetBinContent(iBin+1,ptPNDiff[jC][jPt][jStat][iBin]);
                    hptPNPlus[jC][jPt][jStat]->SetBinContent(iBin+1,ptPNPlus[jC][jPt][jStat][iBin]);
                    hptPNMinus[jC][jPt][jStat]->SetBinContent(iBin+1,ptPNMinus[jC][jPt][jStat][iBin]);
                }
            }
            for (int jStat=0;jStat<8;jStat++) {
                for (int iBin=0;iBin<TOTBINS;iBin++) {
                    hptPNPlusMinus[jC][jPt][jStat]->SetBinContent(iBin+1,ptPNPlusMinus[jC][jPt][jStat][iBin]);
                }
            }
        }
    }
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
        multNSum[i]->Write();
        multNPlus[i]->Write();
        multNMinus[i]->Write();
        multNDiff[i]->Write();
        multPSum[i]->Write();
        multPPlus[i]->Write();
        multPMinus[i]->Write();
        multPDiff[i]->Write();
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
            multptNSum[i][j]->Write();
            multptNPlus[i][j]->Write();
            multptNMinus[i][j]->Write();
            multptNDiff[i][j]->Write();
            multptPSum[i][j]->Write();
            multptPPlus[i][j]->Write();
            multptPMinus[i][j]->Write();
            multptPDiff[i][j]->Write();
        }
    }

}

//--------------------------------------------------------------------------
void StEStructFluctuations::initArrays(){
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

        sprintf( line, "multNSum_%i", i );
        multNSum[i] = new TH1F(line,line,150,0,1500);
        sprintf( line, "multNPlus_%i", i );
        multNPlus[i] = new TH1F(line,line,100,0,1000);
        sprintf( line, "multNMinus_%i", i );
        multNMinus[i] = new TH1F(line,line,100,0,1000);
        sprintf( line, "multNDiff_%i", i );
        multNDiff[i] = new TH1F(line,line,100,-200,200);
        sprintf( line, "multPSum_%i", i );
        multPSum[i] = new TH1F(line,line,100,0,1000);
        sprintf( line, "multPPlus_%i", i );
        multPPlus[i] = new TH1F(line,line,100,0,1000);
        sprintf( line, "multPMinus_%i", i );
        multPMinus[i] = new TH1F(line,line,100,0,1000);
        sprintf( line, "multPDiff_%i", i );
        multPDiff[i] = new TH1F(line,line,100,-200,200);
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
            sprintf( line, "multptNSum_%i_%i", i, j );
            multptNSum[i][j] = new TH1F(line,line,100,0,1000);
            sprintf( line, "multptNPlus_%i_%i", i, j );
            multptNPlus[i][j] = new TH1F(line,line,100,0,1000);
            sprintf( line, "multptNMinus_%i_%i", i, j );
            multptNMinus[i][j] = new TH1F(line,line,100,0,1000);
            sprintf( line, "multptNDiff_%i_%i", i, j );
            multptNDiff[i][j] = new TH1F(line,line,100,-200,200);
            sprintf( line, "multptPSum_%i_%i", i, j );
            multptPSum[i][j] = new TH1F(line,line,100,0,1000);
            sprintf( line, "multptPPlus_%i_%i", i, j );
            multptPPlus[i][j] = new TH1F(line,line,100,0,1000);
            sprintf( line, "multptPMinus_%i_%i", i, j );
            multptPMinus[i][j] = new TH1F(line,line,100,0,1000);
            sprintf( line, "multptPDiff_%i_%i", i, j );
            multptPDiff[i][j] = new TH1F(line,line,100,-200,200);
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
    TOTBINS = off;
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
cout << "Allocating arrays to store info." << endl;
    for (int jC=0;jC<NCENTBINS;jC++) {
        for (int jStat=0;jStat<5;jStat++) {
            TotEvents[jC][jStat] = (double *) malloc( sizeof(double) * TOTBINS );
        }

        for (int jStat=0;jStat<2;jStat++) {
            NSum[jC][jStat] = (double *) malloc( sizeof(double) * TOTBINS );
            NDiff[jC][jStat] = (double *) malloc( sizeof(double) * TOTBINS );
            NPlus[jC][jStat] = (double *) malloc( sizeof(double) * TOTBINS );
            NMinus[jC][jStat] = (double *) malloc( sizeof(double) * TOTBINS );
        }
        NPlusMinus[jC] = (double *) malloc( sizeof(double) * TOTBINS );
        for (int jStat=0;jStat<5;jStat++) {
            PSum[jC][jStat] = (double *) malloc( sizeof(double) * TOTBINS );
            PPlus[jC][jStat] = (double *) malloc( sizeof(double) * TOTBINS );
            PMinus[jC][jStat] = (double *) malloc( sizeof(double) * TOTBINS );
        }
        for (int jStat=0;jStat<8;jStat++) {
            PDiff[jC][jStat] = (double *) malloc( sizeof(double) * TOTBINS );
        }
        for (int jStat=0;jStat<8;jStat++) {
            PPlusMinus[jC][jStat] = (double *) malloc( sizeof(double) * TOTBINS );
        }
        for (int jStat=0;jStat<4;jStat++) {
            PNSum[jC][jStat] = (double *) malloc( sizeof(double) * TOTBINS );
            PNDiff[jC][jStat] = (double *) malloc( sizeof(double) * TOTBINS );
            PNPlus[jC][jStat] = (double *) malloc( sizeof(double) * TOTBINS );
            PNMinus[jC][jStat] = (double *) malloc( sizeof(double) * TOTBINS );
        }
        for (int jStat=0;jStat<8;jStat++) {
            PNPlusMinus[jC][jStat] = (double *) malloc( sizeof(double) * TOTBINS );
        }
    }

cout << "Allocating arrays to store Pt info. " << endl;
    for (int jC=0;jC<NPTCENTBINS;jC++) {
        for (int jPt=0;jPt<NPTBINS;jPt++) {
            for (int jStat=0;jStat<5;jStat++) {
                ptTotEvents[jC][jPt][jStat] = (double *) malloc( sizeof(double) * TOTBINS );
            }

            for (int jStat=0;jStat<2;jStat++) {
                ptNSum[jC][jPt][jStat] = (double *) malloc( sizeof(double) * TOTBINS );
                ptNDiff[jC][jPt][jStat] = (double *) malloc( sizeof(double) * TOTBINS );
                ptNPlus[jC][jPt][jStat] = (double *) malloc( sizeof(double) * TOTBINS );
                ptNMinus[jC][jPt][jStat] = (double *) malloc( sizeof(double) * TOTBINS );
            }
            ptNPlusMinus[jC][jPt] = (double *) malloc( sizeof(double) * TOTBINS );
            for (int jStat=0;jStat<5;jStat++) {
                ptPSum[jC][jPt][jStat] = (double *) malloc( sizeof(double) * TOTBINS );
                ptPPlus[jC][jPt][jStat] = (double *) malloc( sizeof(double) * TOTBINS );
                ptPMinus[jC][jPt][jStat] = (double *) malloc( sizeof(double) * TOTBINS );
            }
            for (int jStat=0;jStat<8;jStat++) {
                ptPDiff[jC][jPt][jStat] = (double *) malloc( sizeof(double) * TOTBINS );
            }
            for (int jStat=0;jStat<8;jStat++) {
                ptPPlusMinus[jC][jPt][jStat] = (double *) malloc( sizeof(double) * TOTBINS );
            }
            for (int jStat=0;jStat<4;jStat++) {
                ptPNSum[jC][jPt][jStat] = (double *) malloc( sizeof(double) * TOTBINS );
                ptPNDiff[jC][jPt][jStat] = (double *) malloc( sizeof(double) * TOTBINS );
                ptPNPlus[jC][jPt][jStat] = (double *) malloc( sizeof(double) * TOTBINS );
                ptPNMinus[jC][jPt][jStat] = (double *) malloc( sizeof(double) * TOTBINS );
            }
            for (int jStat=0;jStat<8;jStat++) {
                ptPNPlusMinus[jC][jPt][jStat] = (double *) malloc( sizeof(double) * TOTBINS );
            }
        }
    }
}

//--------------------------------------------------------------------------
void StEStructFluctuations::deleteArrays() {

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
        delete multNSum[i];
        delete multNPlus[i];
        delete multNMinus[i];
        delete multNDiff[i];
        delete multPSum[i];
        delete multPPlus[i];
        delete multPMinus[i];
        delete multPDiff[i];
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
            delete multptNSum[i][j];
            delete multptNPlus[i][j];
            delete multptNMinus[i][j];
            delete multptNDiff[i][j];
            delete multptPSum[i][j];
            delete multptPPlus[i][j];
            delete multptPMinus[i][j];
            delete multptPDiff[i][j];
        }
    }

    delete hnBins;
    delete hoffset;
    delete hfUnique;

cout << "freeing Arrays." << endl;
    for (int jC=0;jC<NCENTBINS;jC++) {
        for (int jStat=0;jStat<5;jStat++) {
            free(TotEvents[jC][jStat]);
        }

        for (int jStat=0;jStat<2;jStat++) {
            free(NSum[jC][jStat]);
            free(NDiff[jC][jStat]);
            free(NPlus[jC][jStat]);
            free(NMinus[jC][jStat]);
        }
        free(hNPlusMinus[jC]);
        for (int jStat=0;jStat<5;jStat++) {
            free(PSum[jC][jStat]);
            free(PPlus[jC][jStat]);
            free(PMinus[jC][jStat]);
        }
        for (int jStat=0;jStat<8;jStat++) {
            free(PDiff[jC][jStat]);
        }
        for (int jStat=0;jStat<8;jStat++) {
            free(PPlusMinus[jC][jStat]);
        }
        for (int jStat=0;jStat<4;jStat++) {
            free(PNSum[jC][jStat]);
            free(PNDiff[jC][jStat]);
            free(PNPlus[jC][jStat]);
            free(PNMinus[jC][jStat]);
        }
        for (int jStat=0;jStat<8;jStat++) {
            free(PNPlusMinus[jC][jStat]);
        }
    }

cout << "free(ng pt Arrays." << endl;
    for (int jC=0;jC<NPTCENTBINS;jC++) {
        for (int jPt=0;jPt<NPTBINS;jPt++) {
            for (int jStat=0;jStat<5;jStat++) {
                free(ptTotEvents[jC][jPt][jStat]);
            }

            for (int jStat=0;jStat<2;jStat++) {
                free(ptNSum[jC][jPt][jStat]);
                free(ptNDiff[jC][jPt][jStat]);
                free(ptNPlus[jC][jPt][jStat]);
                free(ptNMinus[jC][jPt][jStat]);
            }
            free(ptNPlusMinus[jC][jPt]);
            for (int jStat=0;jStat<5;jStat++) {
                free(ptPSum[jC][jPt][jStat]);
                free(ptPPlus[jC][jPt][jStat]);
                free(ptPMinus[jC][jPt][jStat]);
            }
            for (int jStat=0;jStat<8;jStat++) {
                free(ptPDiff[jC][jPt][jStat]);
            }
            for (int jStat=0;jStat<8;jStat++) {
                free(ptPPlusMinus[jC][jPt][jStat]);
            }
            for (int jStat=0;jStat<4;jStat++) {
                free(ptPNSum[jC][jPt][jStat]);
                free(ptPNDiff[jC][jPt][jStat]);
                free(ptPNPlus[jC][jPt][jStat]);
                free(ptPNMinus[jC][jPt][jStat]);
            }
            for (int jStat=0;jStat<8;jStat++) {
                free(ptPNPlusMinus[jC][jPt][jStat]);
            }
        }
    }
}


void StEStructFluctuations::initHistograms(){
    char line[255];

cout << "Allocating histograms for I/O." << endl;
    for (int jC=0;jC<NCENTBINS;jC++) {
        sprintf( line, "TotalEvents_%i", jC );
        hTotEvents[jC][0] = new TH1D(line,line,TOTBINS,0.5,TOTBINS+0.5);
        sprintf( line, "TotalSumEvents_%i", jC );
        hTotEvents[jC][1] = new TH1D(line,line,TOTBINS,0.5,TOTBINS+0.5);
        sprintf( line, "TotalPlusEvents_%i", jC );
        hTotEvents[jC][2] = new TH1D(line,line,TOTBINS,0.5,TOTBINS+0.5);
        sprintf( line, "TotalMinusEvents_%i", jC );
        hTotEvents[jC][3] = new TH1D(line,line,TOTBINS,0.5,TOTBINS+0.5);
        sprintf( line, "TotalPlusMinusEvents_%i", jC );
        hTotEvents[jC][4] = new TH1D(line,line,TOTBINS,0.5,TOTBINS+0.5);

        for (int jStat=0;jStat<2;jStat++) {
            sprintf( line, "NSum%i_%i", jC, jStat );
            hNSum[jC][jStat] = new TH1D(line,line,TOTBINS,0.5,TOTBINS+0.5);
            sprintf( line, "NDiff%i_%i", jC, jStat );
            hNDiff[jC][jStat] = new TH1D(line,line,TOTBINS,0.5,TOTBINS+0.5);
            sprintf( line, "NPlus%i_%i", jC, jStat );
            hNPlus[jC][jStat] = new TH1D(line,line,TOTBINS,0.5,TOTBINS+0.5);
            sprintf( line, "NMinus%i_%i", jC, jStat );
            hNMinus[jC][jStat] = new TH1D(line,line,TOTBINS,0.5,TOTBINS+0.5);
        }
        sprintf( line, "NPlusMinus%i_0", jC );
        hNPlusMinus[jC] = new TH1D(line,line,TOTBINS,0.5,TOTBINS+0.5);
        for (int jStat=0;jStat<5;jStat++) {
            sprintf( line, "PSum%i_%i", jC, jStat );
            hPSum[jC][jStat] = new TH1D(line,line,TOTBINS,0.5,TOTBINS+0.5);
            sprintf( line, "PPlus%i_%i", jC, jStat );
            hPPlus[jC][jStat] = new TH1D(line,line,TOTBINS,0.5,TOTBINS+0.5);
            sprintf( line, "PMinus%i_%i", jC, jStat );
            hPMinus[jC][jStat] = new TH1D(line,line,TOTBINS,0.5,TOTBINS+0.5);
        }
        for (int jStat=0;jStat<8;jStat++) {
            sprintf( line, "PDiff%i_%i", jC, jStat );
            hPDiff[jC][jStat] = new TH1D(line,line,TOTBINS,0.5,TOTBINS+0.5);
        }
        for (int jStat=0;jStat<8;jStat++) {
            sprintf( line, "PPlusMinus%i_%i", jC, jStat );
            hPPlusMinus[jC][jStat] = new TH1D(line,line,TOTBINS,0.5,TOTBINS+0.5);
        }
        for (int jStat=0;jStat<4;jStat++) {
            sprintf( line, "PNSum%i_%i", jC, jStat );
            hPNSum[jC][jStat] = new TH1D(line,line,TOTBINS,0.5,TOTBINS+0.5);
            sprintf( line, "PNDiff%i_%i", jC, jStat );
            hPNDiff[jC][jStat] = new TH1D(line,line,TOTBINS,0.5,TOTBINS+0.5);
            sprintf( line, "PNPlus%i_%i", jC, jStat );
            hPNPlus[jC][jStat] = new TH1D(line,line,TOTBINS,0.5,TOTBINS+0.5);
            sprintf( line, "PNMinus%i_%i", jC, jStat );
            hPNMinus[jC][jStat] = new TH1D(line,line,TOTBINS,0.5,TOTBINS+0.5);
        }
        for (int jStat=0;jStat<8;jStat++) {
            sprintf( line, "PNPlusMinus%i_%i", jC, jStat );
            hPNPlusMinus[jC][jStat] = new TH1D(line,line,TOTBINS,0.5,TOTBINS+0.5);
        }
    }

cout << "Allocating histograms for Pt I/O. " << endl;
    for (int jC=0;jC<NPTCENTBINS;jC++) {
        for (int jPt=0;jPt<NPTBINS;jPt++) {
            sprintf( line, "ptTotalEvents_%i_%i", jC, jPt );
            hptTotEvents[jC][jPt][0] = new TH1D(line,line,TOTBINS,0.5,TOTBINS+0.5);
            sprintf( line, "ptTotalSumEvents_%i_%i", jC, jPt );
            hptTotEvents[jC][jPt][1] = new TH1D(line,line,TOTBINS,0.5,TOTBINS+0.5);
            sprintf( line, "ptTotalPlusEvents_%i_%i", jC, jPt );
            hptTotEvents[jC][jPt][2] = new TH1D(line,line,TOTBINS,0.5,TOTBINS+0.5);
            sprintf( line, "ptTotalMinusEvents_%i_%i", jC, jPt );
            hptTotEvents[jC][jPt][3] = new TH1D(line,line,TOTBINS,0.5,TOTBINS+0.5);
            sprintf( line, "ptTotalPlusMinusEvents_%i_%i", jC, jPt );
            hptTotEvents[jC][jPt][4] = new TH1D(line,line,TOTBINS,0.5,TOTBINS+0.5);

            for (int jStat=0;jStat<2;jStat++) {
                sprintf( line, "ptNSum%i_%i_%i", jC, jPt, jStat );
                hptNSum[jC][jPt][jStat] = new TH1D(line,line,TOTBINS,0.5,TOTBINS+0.5);
                sprintf( line, "ptNDiff%i_%i_%i", jC, jPt, jStat );
                hptNDiff[jC][jPt][jStat] = new TH1D(line,line,TOTBINS,0.5,TOTBINS+0.5);
                sprintf( line, "ptNPlus%i_%i_%i", jC, jPt, jStat );
                hptNPlus[jC][jPt][jStat] = new TH1D(line,line,TOTBINS,0.5,TOTBINS+0.5);
                sprintf( line, "ptNMinus%i_%i_%i", jC, jPt, jStat );
                hptNMinus[jC][jPt][jStat] = new TH1D(line,line,TOTBINS,0.5,TOTBINS+0.5);
            }
            sprintf( line, "ptNPlusMinus%i_%i_0", jC, jPt );
            hptNPlusMinus[jC][jPt] = new TH1D(line,line,TOTBINS,0.5,TOTBINS+0.5);
            for (int jStat=0;jStat<5;jStat++) {
                sprintf( line, "ptPSum%i_%i_%i", jC, jPt, jStat );
                hptPSum[jC][jPt][jStat] = new TH1D(line,line,TOTBINS,0.5,TOTBINS+0.5);
                sprintf( line, "ptPPlus%i_%i_%i", jC, jPt, jStat );
                hptPPlus[jC][jPt][jStat] = new TH1D(line,line,TOTBINS,0.5,TOTBINS+0.5);
                sprintf( line, "ptPMinus%i_%i_%i", jC, jPt, jStat );
                hptPMinus[jC][jPt][jStat] = new TH1D(line,line,TOTBINS,0.5,TOTBINS+0.5);
            }
            for (int jStat=0;jStat<8;jStat++) {
                sprintf( line, "ptPDiff%i_%i_%i", jC, jPt, jStat );
                hptPDiff[jC][jPt][jStat] = new TH1D(line,line,TOTBINS,0.5,TOTBINS+0.5);
            }
            for (int jStat=0;jStat<8;jStat++) {
                sprintf( line, "ptPPlusMinus%i_%i_%i", jC, jPt, jStat );
                hptPPlusMinus[jC][jPt][jStat] = new TH1D(line,line,TOTBINS,0.5,TOTBINS+0.5);
            }
            for (int jStat=0;jStat<4;jStat++) {
                sprintf( line, "ptPNSum%i_%i_%i", jC, jPt, jStat );
                hptPNSum[jC][jPt][jStat] = new TH1D(line,line,TOTBINS,0.5,TOTBINS+0.5);
                sprintf( line, "ptPNDiff%i_%i_%i", jC, jPt, jStat );
                hptPNDiff[jC][jPt][jStat] = new TH1D(line,line,TOTBINS,0.5,TOTBINS+0.5);
                sprintf( line, "ptPNPlus%i_%i_%i", jC, jPt, jStat );
                hptPNPlus[jC][jPt][jStat] = new TH1D(line,line,TOTBINS,0.5,TOTBINS+0.5);
                sprintf( line, "ptPNMinus%i_%i_%i", jC, jPt, jStat );
                hptPNMinus[jC][jPt][jStat] = new TH1D(line,line,TOTBINS,0.5,TOTBINS+0.5);
            }
            for (int jStat=0;jStat<8;jStat++) {
                sprintf( line, "ptPNPlusMinus%i_%i_%i", jC, jPt, jStat );
                hptPNPlusMinus[jC][jPt][jStat] = new TH1D(line,line,TOTBINS,0.5,TOTBINS+0.5);
            }
        }
    }
}

//--------------------------------------------------------------------------
void StEStructFluctuations::deleteHistograms() {

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
        for (int jStat=0;jStat<5;jStat++) {
            delete hPSum[jC][jStat];
            delete hPPlus[jC][jStat];
            delete hPMinus[jC][jStat];
        }
        for (int jStat=0;jStat<8;jStat++) {
            delete hPPlusMinus[jC][jStat];
        }
        for (int jStat=0;jStat<4;jStat++) {
            delete hPNSum[jC][jStat];
            delete hPNPlus[jC][jStat];
            delete hPNMinus[jC][jStat];
        }
        for (int jStat=0;jStat<8;jStat++) {
            delete hPNPlusMinus[jC][jStat];
        }
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
            for (int jStat=0;jStat<5;jStat++) {
                delete hptPSum[jC][jPt][jStat];
                delete hptPPlus[jC][jPt][jStat];
                delete hptPMinus[jC][jPt][jStat];
            }
            for (int jStat=0;jStat<8;jStat++) {
                delete hptPPlusMinus[jC][jPt][jStat];
            }
            for (int jStat=0;jStat<4;jStat++) {
                delete hptPNSum[jC][jPt][jStat];
                delete hptPNPlus[jC][jPt][jStat];
                delete hptPNMinus[jC][jPt][jStat];
            }
            for (int jStat=0;jStat<8;jStat++) {
                delete hptPNPlusMinus[jC][jPt][jStat];
            }
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
#ifdef HIJING
    int multCut[] = {};
    int iCent = (int) mCurrentEvent->Centrality();
    if ((iCent < 0) || (NCENTBINS <= iCent)) {
        cout << "Centrality " << iCent << " is out of range!!!!" << endl;
        iCent = -1;
    }
    return iCent;
#endif
#ifdef PPDATA
// The next line is for common mult for 200GeV pp data using refMult.
// I use the same cuts for tracks in |eta| < 1.
//    int multCut[] = { 2, 5, 8, 11, 14, 25};
//    int multCut[] = { 3, 7, 13, 21, 26, 50};
    int multCut[] = { 1, 25};
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
#ifdef HIJING
    if (jCent <= 2) {
        return 0;
    } else {
        return 1;
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

