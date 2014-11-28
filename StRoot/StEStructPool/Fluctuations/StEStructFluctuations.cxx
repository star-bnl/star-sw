
#include <math.h>
#include "StEStructFluctuations.h"

#include "TH1F.h"
#include "TH2F.h"
#include "TFile.h"

#include <stdlib.h>


ClassImp(StEStructFluct)

//--------------------------------------------------------------------------
StEStructFluct::StEStructFluct( char *key, int totBins,
                                float EtaMin, float EtaMax,
                                float PtMin,  float PtMax ) {
    mKey     = strdup(key);
    mTotBins = totBins;
    mEtaMin  = EtaMin;
    mEtaMax  = EtaMax;
    mPtMin   = PtMin;
    mPtMax   = PtMax;
    initArrays();
}

//--------------------------------------------------------------------------
StEStructFluct::~StEStructFluct() {
    deleteArrays();
    deleteHistograms();
    free( mKey );
}

void StEStructFluct::AddToBin( int    jBin,
                               double Nplus,    double Nminus,
                               double Ptplus,   double Ptminus,
                               double PtSqplus, double PtSqminus ) {
    TotEvents[0][jBin]++;

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

// Doing some tests for Jamie to alleive his concernes over nu_dynamical calculation.
    if (Nsum > 0) {
        TotEvents[1][jBin]++;

        NSum[0][jBin] += Nplus;
        NSum[1][jBin] += Nminus;
        NSum[2][jBin] += Nplus*Nplus;
        NSum[3][jBin] += Nplus*Nminus;
        NSum[4][jBin] += Nminus*Nminus;
        NSum[5][jBin] += Nplus*Nplus/Nsum;
        NSum[6][jBin] += Nplus*Nminus/Nsum;
        NSum[7][jBin] += Nminus*Nminus/Nsum;
        NSum[8][jBin] += Nplus*Nplus/(Nsum*Nsum);
        NSum[9][jBin] += Nplus*Nminus/(Nsum*Nsum);
        NSum[10][jBin] += Nminus*Nminus/(Nsum*Nsum);
        NSum[11][jBin] += Nplus/sqs;
        NSum[12][jBin] += Nplus*sqs;
        NSum[13][jBin] += Nminus/sqs;
        NSum[14][jBin] += Nminus*sqs;

        r  = Ptsum*Ptsum/Nsum;
        PSum[0][jBin] += PtSqsum;
        PSum[1][jBin] += Ptplus;
        PSum[2][jBin] += Ptminus;
        PSum[3][jBin] += Nplus*Ptsum;
        PSum[4][jBin] += Nminus*Ptsum;
        PSum[5][jBin] += Ptsum*Ptsum;
        PSum[6][jBin] += Nplus*Ptsum/Nsum;
        PSum[7][jBin] += Nminus*Ptsum/Nsum;
        PSum[8][jBin] += Ptsum*Ptsum/Nsum;
        PSum[12][jBin] += r*Ptsum/Nsum;
        PSum[13][jBin] += r*r/Nsum;
        PSum[14][jBin] += Ptsum/sqs;
        PSum[15][jBin] += Ptsum*sqs;


        NDiff[0][jBin] += Nplus*Ndiff/sqs;
        NDiff[1][jBin] += Nminus*Ndiff/sqs;

        double r1 = Ptdiff*Ptdiff/Nsum;
        double r2 = Ptdiff*Ndiff/Nsum;
        double r3 = Ndiff*Ndiff/Nsum;
        PDiff[0][jBin] += Nplus*Ptdiff;
        PDiff[1][jBin] += Nminus*Ptdiff;
        PDiff[2][jBin] += Ptdiff*Ptdiff;
        PDiff[3][jBin] += Nplus*Ptdiff/Nsum;
        PDiff[4][jBin] += Nminus*Ptdiff/Nsum;
        PDiff[5][jBin] += Ptdiff*Ptdiff/Nsum;
        PDiff[6][jBin] += Nplus*Ptdiff/(Nsum*Nsum);
        PDiff[7][jBin] += Nminus*Ptdiff/(Nsum*Nsum);
        PDiff[8][jBin] += Ptdiff*Ptdiff/(Nsum*Nsum);
        PDiff[9][jBin] += Ptdiff/sqs;
        PDiff[10][jBin] += Ndiff*Ptdiff/sqs;
        PDiff[11][jBin] += r1*r1/Nsum;
        PDiff[12][jBin] += r1*r2/Nsum;
        PDiff[13][jBin] += r1*r3/Nsum;
        PDiff[14][jBin] += r2*r3/Nsum;
        PDiff[15][jBin] += r3*r3/Nsum;
    }
    if (Nsum > 1) {
        TotEvents[5][jBin]++;
        NSum[15][jBin] += 1.0/(Nsum-1.0);
        PSum[9][jBin]  += Nsum/(Nsum-1.0);
        PSum[10][jBin] += Ptsum/(Nsum-1.0);
        PSum[11][jBin] += Ptsum*Ptsum/(Nsum*(Nsum-1.0));
        PSum[16][jBin] += Nsum;
        PSum[17][jBin] += Ptsum;
        PSum[18][jBin] += PtSqsum;
        PSum[19][jBin] += PtSqsum/(Nsum*(Nsum-1.0));
        PSum[20][jBin] += Ptsum/(Nsum*(Nsum-1.0));
    }

    if (Nplus > 0) {
        TotEvents[2][jBin]++;

        NPlus[0][jBin] += Nplus;
        NPlus[1][jBin] += Nplus*Nplus;
        NPlus[2][jBin] += sqp;
        NPlus[3][jBin] += Nplus*sqp;
        NPlus[4][jBin] += 1/Nplus;

        r  = Ptplus*Ptplus/Nplus;
        PPlus[0][jBin] += PtSqplus;
        PPlus[1][jBin] += Ptplus;
        PPlus[2][jBin] += Ptplus*Nplus;
        PPlus[3][jBin] += Ptplus/Nplus;
        PPlus[4][jBin] += Ptplus*Ptplus;
        PPlus[5][jBin] += r;
        PPlus[6][jBin] += r/Nplus;
        PPlus[7][jBin] += r*Ptplus/Nplus;
        PPlus[8][jBin] += r*r/Nplus;
        PPlus[9][jBin] += Ptplus/sqp;
        PPlus[10][jBin] += Ptplus*sqp;
    }

    if (Nminus > 0) {
        TotEvents[3][jBin]++;

        NMinus[0][jBin] += Nminus;
        NMinus[1][jBin] += Nminus*Nminus;
        NMinus[2][jBin] += sqm;
        NMinus[3][jBin] += Nminus*sqm;
        NMinus[4][jBin] += 1/Nminus;

        r  = Ptminus*Ptminus/Nminus;
        PMinus[0][jBin] += PtSqminus;
        PMinus[1][jBin] += Ptminus;
        PMinus[2][jBin] += Ptminus*Nminus;
        PMinus[3][jBin] += Ptminus/Nminus;
        PMinus[4][jBin] += Ptminus*Ptminus;
        PMinus[5][jBin] += r;
        PMinus[6][jBin] += r/Nminus;
        PMinus[7][jBin] += r*Ptminus/Nminus;
        PMinus[8][jBin] += r*r/Nminus;
        PMinus[9][jBin] += Ptminus/sqm;
        PMinus[10][jBin] += Ptminus*sqm;
    }

    if ((Nplus > 0) || (Nminus > 0)) {
        TotEvents[4][jBin]++;

        NPlusMinus[0][jBin] += Nplus;
        NPlusMinus[1][jBin] += Nminus;
        NPlusMinus[2][jBin] += Nplus*Nminus;
        NPlusMinus[3][jBin] += sqp;
        NPlusMinus[4][jBin] += sqm;
        NPlusMinus[5][jBin] += sqp*sqm;
        NPlusMinus[6][jBin] += Nminus*sqp;
        NPlusMinus[7][jBin] += Nplus*sqm;

        PPlusMinus[0][jBin] += Ptplus;
        PPlusMinus[1][jBin] += Ptminus;
        PPlusMinus[4][jBin] += Nplus*Ptminus;
        PPlusMinus[5][jBin] += Nminus*Ptplus;
        PPlusMinus[14][jBin] += Ptplus*Ptminus;
        if (Nplus > 0) {
            PPlusMinus[2][jBin] += Ptplus/Nplus;
            PPlusMinus[6][jBin] += Ptplus/sqp;
            PPlusMinus[8][jBin] += Ptplus*Nminus/Nplus;
            PPlusMinus[10][jBin] += Ptplus*sqm/sqp;
            PPlusMinus[12][jBin] += Ptplus*Nminus/sqp;
        }
        if (Nminus > 0) {
            PPlusMinus[3][jBin] += Ptminus/Nminus;
            PPlusMinus[7][jBin] += Ptminus/sqm;
            PPlusMinus[9][jBin] += Ptminus*Nplus/Nminus;
            PPlusMinus[11][jBin] += Ptminus*sqp/sqm;
            PPlusMinus[13][jBin] += Ptminus*Nplus/sqm;
        }
        if ((Nplus > 0) && (Nminus > 0)) {
            PPlusMinus[15][jBin] += Ptplus*Ptminus/(Nplus*Nminus);
            PPlusMinus[16][jBin] += Ptplus*Ptminus/(sqp*sqm);
        }
    }
}
void StEStructFluct::fillOccupancies( double dPhi,  double dEta,
                                      double nPlus, double nMinus,
                                      double pPlus, double pMinus ) {
    double ns = nPlus + nMinus;
    double nd = nPlus - nMinus;
    double ps = pPlus + pMinus;
    double pd = pPlus - pMinus;
    occNSum->Fill(dPhi,dEta,ns);
    occNPlus->Fill(dPhi,dEta,nPlus);
    occNMinus->Fill(dPhi,dEta,nMinus);
    occNDiff->Fill(dPhi,dEta,nd);
    occPSum->Fill(dPhi,dEta,ps);
    occPPlus->Fill(dPhi,dEta,pPlus);
    occPMinus->Fill(dPhi,dEta,pMinus);
    occPDiff->Fill(dPhi,dEta,pd);
    occPNSum->Fill(dPhi,dEta,ns*ps);
    occPNPlus->Fill(dPhi,dEta,nPlus*pPlus);
    occPNMinus->Fill(dPhi,dEta,nMinus*pMinus);
    occPNDiff->Fill(dPhi,dEta,nd*pd);
}
void StEStructFluct::fillMults( double nPlus, double nMinus,
                                double pPlus, double pMinus ) {
    double ns = nPlus + nMinus;
    double nd = nPlus - nMinus;
    double ps = pPlus + pMinus;
    double pd = pPlus - pMinus;
    multNSum->Fill(ns);
    multNPlus->Fill(nPlus);
    multNMinus->Fill(nMinus);
    multNDiff->Fill(nd);
    multPSum->Fill(ps);
    multPPlus->Fill(pPlus);
    multPMinus->Fill(pMinus);
    multPDiff->Fill(pd);
}
void StEStructFluct::fillEtaZ( float z, float eta,
                               int maxFitPoints, int foundPoints, int nFitPoints,
                               int iF, int iL ) {
    EtaZNTracks->Fill(z,eta);
    EtaZNMaxPoints->Fill(z,eta,maxFitPoints);
    EtaZNFoundPoints->Fill(z,eta,foundPoints);
    EtaZNFitPoints->Fill(z,eta,nFitPoints);
    InnerRow->Fill(z,eta,iF);
    OuterRow->Fill(z,eta,iL);
}
void StEStructFluct::fillPtHist( double Pt, int sign ) {
    ptAll->Fill(Pt);
    if (sign > 0) {
        ptPlus->Fill(Pt);
    }
    if (sign < 0) {
        ptMinus->Fill(Pt);
    }
}
void StEStructFluct::fillPhiHist( double Phi, int sign ) {
    if (sign > 0) {
        phiPlus->Fill(Phi);
    }
    if (sign < 0) {
        phiMinus->Fill(Phi);
    }
}
void StEStructFluct::fillEtaHist( double Eta, int sign ) {
    if (sign > 0) {
        etaPlus->Fill(Eta);
    }
    if (sign < 0) {
        etaMinus->Fill(Eta);
    }
}
//--------------------------------------------------------------------------
//
//------------ Below are init, delete, write functions -------///
//


void StEStructFluct::writeHistograms() {

    initHistograms();
    fillHistograms();

    for (int jStat=0;jStat<6;jStat++) {
        hTotEvents[jStat]->Write();
    }

    for (int jStat=0;jStat<16;jStat++) {
        hNSum[jStat]->Write();
    }
    for (int jStat=0;jStat<2;jStat++) {
        hNDiff[jStat]->Write();
    }
    for (int jStat=0;jStat<5;jStat++) {
        hNPlus[jStat]->Write();
        hNMinus[jStat]->Write();
    }
    for (int jStat=0;jStat<8;jStat++) {
        hNPlusMinus[jStat]->Write();
    }
    for (int jStat=0;jStat<21;jStat++) {
        hPSum[jStat]->Write();
    }
    for (int jStat=0;jStat<16;jStat++) {
        hPDiff[jStat]->Write();
    }
    for (int jStat=0;jStat<11;jStat++) {
        hPPlus[jStat]->Write();
        hPMinus[jStat]->Write();
    }
    for (int jStat=0;jStat<17;jStat++) {
        hPPlusMinus[jStat]->Write();
    }
}
void StEStructFluct::fillHistograms() {

    // Here I copy from arrays top histograms so I can write the histograms.

    for (int jStat=0;jStat<6;jStat++) {
        for (int jBin=0;jBin<mTotBins;jBin++) {
             hTotEvents[jStat]->SetBinContent(jBin+1,TotEvents[jStat][jBin]);
        }
    }

    for (int jStat=0;jStat<16;jStat++) {
        for (int jBin=0;jBin<mTotBins;jBin++) {
            hNSum[jStat]->SetBinContent(jBin+1,NSum[jStat][jBin]);
        }
    }
    for (int jStat=0;jStat<2;jStat++) {
        for (int jBin=0;jBin<mTotBins;jBin++) {
            hNDiff[jStat]->SetBinContent(jBin+1,NDiff[jStat][jBin]);
        }
    }
    for (int jStat=0;jStat<5;jStat++) {
        for (int jBin=0;jBin<mTotBins;jBin++) {
            hNPlus[jStat]->SetBinContent(jBin+1,NPlus[jStat][jBin]);
            hNMinus[jStat]->SetBinContent(jBin+1,NMinus[jStat][jBin]);
        }
    }
    for (int jStat=0;jStat<8;jStat++) {
        for (int jBin=0;jBin<mTotBins;jBin++) {
            hNPlusMinus[jStat]->SetBinContent(jBin+1,NPlusMinus[jStat][jBin]);
        }
    }
    for (int jStat=0;jStat<21;jStat++) {
        for (int jBin=0;jBin<mTotBins;jBin++) {
            hPSum[jStat]->SetBinContent(jBin+1,PSum[jStat][jBin]);
        }
    }
    for (int jStat=0;jStat<16;jStat++) {
        for (int jBin=0;jBin<mTotBins;jBin++) {
            hPDiff[jStat]->SetBinContent(jBin+1,PDiff[jStat][jBin]);
        }
    }
    for (int jStat=0;jStat<11;jStat++) {
        for (int jBin=0;jBin<mTotBins;jBin++) {
            hPPlus[jStat]->SetBinContent(jBin+1,PPlus[jStat][jBin]);
            hPMinus[jStat]->SetBinContent(jBin+1,PMinus[jStat][jBin]);
        }
    }
    for (int jStat=0;jStat<17;jStat++) {
        for (int jBin=0;jBin<mTotBins;jBin++) {
            hPPlusMinus[jStat]->SetBinContent(jBin+1,PPlusMinus[jStat][jBin]);
        }
    }
}
void StEStructFluct::writeQAHistograms() {
    occNSum->Write();
    occNPlus->Write();
    occNMinus->Write();
    occNDiff->Write();
    occPSum->Write();
    occPPlus->Write();
    occPMinus->Write();
    occPDiff->Write();
    occPNSum->Write();
    occPNPlus->Write();
    occPNMinus->Write();
    occPNDiff->Write();
    EtaZNTracks->Write();
    EtaZNFitPoints->Write();
    EtaZNMaxPoints->Write();
    EtaZNFoundPoints->Write();
    InnerRow->Write();
    OuterRow->Write();
    multNSum->Write();
    multNPlus->Write();
    multNMinus->Write();
    multNDiff->Write();
    multPSum->Write();
    multPPlus->Write();
    multPMinus->Write();
    multPDiff->Write();
    ptAll->Write();
    ptPlus->Write();
    ptMinus->Write();
    phiPlus->Write();
    phiMinus->Write();
    etaPlus->Write();
    etaMinus->Write();
}

//--------------------------------------------------------------------------
void StEStructFluct::initArrays() {
    char line[255];

    // Here are histograms toward uniform occupancy of bins.
    sprintf( line, "occNSum_%s", mKey );
    occNSum = new TH2F(line,line,NPHIBINS,-3.1415926,3.1415926,NETABINS,mEtaMin,mEtaMax);
    sprintf( line, "occNPlus_%s", mKey );
    occNPlus = new TH2F(line,line,NPHIBINS,-3.1415926,3.1415926,NETABINS,mEtaMin,mEtaMax);
    sprintf( line, "occNMinus_%s", mKey );
    occNMinus = new TH2F(line,line,NPHIBINS,-3.1415926,3.1415926,NETABINS,mEtaMin,mEtaMax);
    sprintf( line, "occNDiff_%s", mKey );
    occNDiff = new TH2F(line,line,NPHIBINS,-3.1415926,3.1415926,NETABINS,mEtaMin,mEtaMax);
    sprintf( line, "occPSum_%s", mKey );
    occPSum = new TH2F(line,line,NPHIBINS,-3.1415926,3.1415926,NETABINS,mEtaMin,mEtaMax);
    sprintf( line, "occPPlus_%s", mKey );
    occPPlus = new TH2F(line,line,NPHIBINS,-3.1415926,3.1415926,NETABINS,mEtaMin,mEtaMax);
    sprintf( line, "occPMinus_%s", mKey );
    occPMinus = new TH2F(line,line,NPHIBINS,-3.1415926,3.1415926,NETABINS,mEtaMin,mEtaMax);
    sprintf( line, "occPDiff_%s", mKey );
    occPDiff = new TH2F(line,line,NPHIBINS,-3.1415926,3.1415926,NETABINS,mEtaMin,mEtaMax);
    sprintf( line, "occPNSum_%s", mKey );
    occPNSum = new TH2F(line,line,NPHIBINS,-3.1415926,3.1415926,NETABINS,mEtaMin,mEtaMax);
    sprintf( line, "occPNPlus_%s", mKey );
    occPNPlus = new TH2F(line,line,NPHIBINS,-3.1415926,3.1415926,NETABINS,mEtaMin,mEtaMax);
    sprintf( line, "occPNMinus_%s", mKey );
    occPNMinus = new TH2F(line,line,NPHIBINS,-3.1415926,3.1415926,NETABINS,mEtaMin,mEtaMax);
    sprintf( line, "occPNDiff_%s", mKey );
    occPNDiff = new TH2F(line,line,NPHIBINS,-3.1415926,3.1415926,NETABINS,mEtaMin,mEtaMax);

    sprintf( line, "EtaZNTracks_%s", mKey );
    EtaZNTracks = new TH2F(line,line,60,-30,30,50,mEtaMin,mEtaMax);
    sprintf( line, "EtaZNFitPoints_%s", mKey );
    EtaZNFitPoints = new TH2F(line,line,60,-30,30,50,mEtaMin,mEtaMax);
    sprintf( line, "EtaZNMaxPoints_%s", mKey );
    EtaZNMaxPoints = new TH2F(line,line,60,-30,30,50,mEtaMin,mEtaMax);
    sprintf( line, "EtaZNFoundPoints_%s", mKey );
    EtaZNFoundPoints = new TH2F(line,line,60,-30,30,50,mEtaMin,mEtaMax);
    sprintf( line, "InnerRow_%s", mKey );
    InnerRow = new TH2F(line,line,60,-30,30,50,mEtaMin,mEtaMax);
    sprintf( line, "OuterRow_%s", mKey );
    OuterRow = new TH2F(line,line,60,-30,30,50,mEtaMin,mEtaMax);

    sprintf( line, "multNSum_%s", mKey );
    multNSum = new TH1F(line,line,150,0,1500);
    sprintf( line, "multNPlus_%s", mKey );
    multNPlus = new TH1F(line,line,100,0,1000);
    sprintf( line, "multNMinus_%s", mKey );
    multNMinus = new TH1F(line,line,100,0,1000);
    sprintf( line, "multNDiff_%s", mKey );
    multNDiff = new TH1F(line,line,100,-200,200);
    sprintf( line, "multPSum_%s", mKey );
    multPSum = new TH1F(line,line,100,0,1000);
    sprintf( line, "multPPlus_%s", mKey );
    multPPlus = new TH1F(line,line,100,0,1000);
    sprintf( line, "multPMinus_%s", mKey );
    multPMinus = new TH1F(line,line,100,0,1000);
    sprintf( line, "multPDiff_%s", mKey );
    multPDiff = new TH1F(line,line,100,-200,200);

    sprintf( line, "ptAll_%s", mKey );
    ptAll = new TH1F(line,line,100,mPtMin,mPtMax);
    sprintf( line, "ptPlus_%s", mKey );
    ptPlus = new TH1F(line,line,100,mPtMin,mPtMax);
    sprintf( line, "ptMinus_%s", mKey );
    ptMinus = new TH1F(line,line,100,mPtMin,mPtMax);
    sprintf( line, "phiPlus_%s", mKey );
    phiPlus = new TH1F(line,line,100,-3.1415926,3.1415926);
    sprintf( line, "phiMinus_%s", mKey );
    phiMinus = new TH1F(line,line,100,-3.1415926,3.1415926);
    sprintf( line, "etaPlus_%s", mKey );
    etaPlus = new TH1F(line,line,100,mEtaMin,mEtaMax);
    sprintf( line, "etaMinus_%s", mKey );
    etaMinus = new TH1F(line,line,100,mEtaMin,mEtaMax);

    printf("Allocating arrays to store info.\n");
    for (int jStat=0;jStat<6;jStat++) {
        TotEvents[jStat] = new double[mTotBins];
        memset(TotEvents[jStat], 0, sizeof(double)*mTotBins );
    }

    for (int jStat=0;jStat<16;jStat++) {
        NSum[jStat]   = new double[mTotBins];
        memset(NSum[jStat], 0, sizeof(double)*mTotBins );
    }
    for (int jStat=0;jStat<2;jStat++) {
        NDiff[jStat]  = new double[mTotBins];
        memset(NDiff[jStat], 0, sizeof(double)*mTotBins );
    }
    for (int jStat=0;jStat<5;jStat++) {
        NPlus[jStat]  = new double[mTotBins];
        memset(NPlus[jStat], 0, sizeof(double)*mTotBins );
        NMinus[jStat] = new double[mTotBins];
        memset(NMinus[jStat], 0, sizeof(double)*mTotBins );
    }
    for (int jStat=0;jStat<8;jStat++) {
        NPlusMinus[jStat] = new double[mTotBins];
        memset(NPlusMinus[jStat], 0, sizeof(double)*mTotBins );
    }
    for (int jStat=0;jStat<21;jStat++) {
        PSum[jStat]   = new double[mTotBins];
        memset(PSum[jStat], 0, sizeof(double)*mTotBins );
    }
    for (int jStat=0;jStat<16;jStat++) {
        PDiff[jStat] = new double[mTotBins];
        memset(PDiff[jStat], 0, sizeof(double)*mTotBins );
    }
    for (int jStat=0;jStat<11;jStat++) {
        PPlus[jStat]  = new double[mTotBins];
        memset(PPlus[jStat], 0, sizeof(double)*mTotBins );
        PMinus[jStat] = new double[mTotBins];
        memset(PMinus[jStat], 0, sizeof(double)*mTotBins );
    }
    for (int jStat=0;jStat<17;jStat++) {
        PPlusMinus[jStat] = new double[mTotBins];
        memset(PPlusMinus[jStat], 0, sizeof(double)*mTotBins );
    }
}

//--------------------------------------------------------------------------
void StEStructFluct::deleteArrays() {

    printf("Deleting occupancy histograms.\n");
    delete occNSum;
    delete occNPlus;
    delete occNMinus;
    delete occNDiff;
    delete occPSum;
    delete occPPlus;
    delete occPMinus;
    delete occPDiff;
    delete occPNSum;
    delete occPNPlus;
    delete occPNMinus;
    delete occPNDiff;
    delete EtaZNTracks;
    delete EtaZNFitPoints;
    delete EtaZNMaxPoints;
    delete EtaZNFoundPoints;
    delete InnerRow;
    delete OuterRow;
    delete multNSum;
    delete multNPlus;
    delete multNMinus;
    delete multNDiff;
    delete multPSum;
    delete multPPlus;
    delete multPMinus;
    delete multPDiff;
    delete ptAll;
    delete ptPlus;
    delete ptMinus;
    delete phiPlus;
    delete phiMinus;
    delete etaPlus;
    delete etaMinus;

    printf("freeing Arrays.\n");
    for (int jStat=0;jStat<6;jStat++) {
        delete [] TotEvents[jStat];
    }

    for (int jStat=0;jStat<16;jStat++) {
        delete [] NSum[jStat];
    }
    for (int jStat=0;jStat<2;jStat++) {
        delete [] NDiff[jStat];
    }
    for (int jStat=0;jStat<5;jStat++) {
        delete [] NPlus[jStat];
        delete [] NMinus[jStat];
    }
    for (int jStat=0;jStat<8;jStat++) {
        delete [] hNPlusMinus[jStat];
    }
    for (int jStat=0;jStat<21;jStat++) {
        delete [] PSum[jStat];
    }
    for (int jStat=0;jStat<16;jStat++) {
        delete [] PDiff[jStat];
    }
    for (int jStat=0;jStat<11;jStat++) {
        delete [] PPlus[jStat];
        delete [] PMinus[jStat];
    }
    for (int jStat=0;jStat<17;jStat++) {
        delete [] PPlusMinus[jStat];
    }
}


void StEStructFluct::initHistograms() {
    char line[255];

    printf("Allocating histograms for I/O for key %s\n", mKey);
    sprintf( line, "TotalEvents_%s", mKey);
    hTotEvents[0] = new TH1D(line,line,mTotBins,0.5,mTotBins+0.5);
    sprintf( line, "TotalSumEvents_%s", mKey);
    hTotEvents[1] = new TH1D(line,line,mTotBins,0.5,mTotBins+0.5);
    sprintf( line, "TotalPlusEvents_%s", mKey);
    hTotEvents[2] = new TH1D(line,line,mTotBins,0.5,mTotBins+0.5);
    sprintf( line, "TotalMinusEvents_%s", mKey);
    hTotEvents[3] = new TH1D(line,line,mTotBins,0.5,mTotBins+0.5);
    sprintf( line, "TotalPlusMinusEvents_%s", mKey);
    hTotEvents[4] = new TH1D(line,line,mTotBins,0.5,mTotBins+0.5);
    sprintf( line, "TotalEventsForSig2Dyn_%s", mKey);
    hTotEvents[5] = new TH1D(line,line,mTotBins,0.5,mTotBins+0.5);

    for (int jStat=0;jStat<16;jStat++) {
        sprintf( line, "NSum%s_%i", mKey, jStat );
        hNSum[jStat] = new TH1D(line,line,mTotBins,0.5,mTotBins+0.5);
    }
    for (int jStat=0;jStat<2;jStat++) {
        sprintf( line, "NDiff%s_%i", mKey, jStat );
        hNDiff[jStat] = new TH1D(line,line,mTotBins,0.5,mTotBins+0.5);
    }
    for (int jStat=0;jStat<5;jStat++) {
        sprintf( line, "NPlus%s_%i", mKey, jStat );
        hNPlus[jStat] = new TH1D(line,line,mTotBins,0.5,mTotBins+0.5);
        sprintf( line, "NMinus%s_%i", mKey, jStat );
        hNMinus[jStat] = new TH1D(line,line,mTotBins,0.5,mTotBins+0.5);
    }
    for (int jStat=0;jStat<8;jStat++) {
        sprintf( line, "NPlusMinus%s_%i", mKey, jStat );
        hNPlusMinus[jStat] = new TH1D(line,line,mTotBins,0.5,mTotBins+0.5);
    }
    for (int jStat=0;jStat<21;jStat++) {
        sprintf( line, "PSum%s_%i", mKey, jStat );
        hPSum[jStat] = new TH1D(line,line,mTotBins,0.5,mTotBins+0.5);
    }
    for (int jStat=0;jStat<16;jStat++) {
        sprintf( line, "PDiff%s_%i", mKey, jStat );
        hPDiff[jStat] = new TH1D(line,line,mTotBins,0.5,mTotBins+0.5);
    }
    for (int jStat=0;jStat<11;jStat++) {
        sprintf( line, "PPlus%s_%i", mKey, jStat );
        hPPlus[jStat] = new TH1D(line,line,mTotBins,0.5,mTotBins+0.5);
        sprintf( line, "PMinus%s_%i", mKey, jStat );
        hPMinus[jStat] = new TH1D(line,line,mTotBins,0.5,mTotBins+0.5);
    }
    for (int jStat=0;jStat<17;jStat++) {
        sprintf( line, "PPlusMinus%s_%i", mKey, jStat );
        hPPlusMinus[jStat] = new TH1D(line,line,mTotBins,0.5,mTotBins+0.5);
    }
}

//--------------------------------------------------------------------------
void StEStructFluct::deleteHistograms() {

    printf("freeing h Array histograms.\n");
    for (int jStat=0;jStat<6;jStat++) {
        delete hTotEvents[jStat];
    }

    for (int jStat=0;jStat<16;jStat++) {
        delete hNSum[jStat];
    }
    for (int jStat=0;jStat<2;jStat++) {
        delete hNDiff[jStat];
    }
    for (int jStat=0;jStat<5;jStat++) {
        delete hNPlus[jStat];
        delete hNMinus[jStat];
    }
    for (int jStat=0;jStat<8;jStat++) {
        delete hNPlusMinus[jStat];
    }
    for (int jStat=0;jStat<21;jStat++) {
        delete hPSum[jStat];
    }
    for (int jStat=0;jStat<16;jStat++) {
        delete hPDiff[jStat];
    }
    for (int jStat=0;jStat<11;jStat++) {
        delete hPPlus[jStat];
        delete hPMinus[jStat];
    }
    for (int jStat=0;jStat<17;jStat++) {
        delete hPPlusMinus[jStat];
    }
}
