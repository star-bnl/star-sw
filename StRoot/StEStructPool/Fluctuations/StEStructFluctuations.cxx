
#include "StEStructFluctuations.h"

#include "TH1F.h"
#include "TH2F.h"
#include "TFile.h"

#include "StEStructPool/EventMaker/StEStructEvent.h"
#include "StEStructPool/EventMaker/StEStructTrack.h"
#include "StTimer.hh"

#include <stdlib.h>


ClassImp(StEStructFluct)

//--------------------------------------------------------------------------
StEStructFluct::StEStructFluct( char *key, int totBins ) {
    mKey = strdup(key);
    mTotBins = totBins;
    initArrays();
}

//--------------------------------------------------------------------------
StEStructFluct::~StEStructFluct() {
    deleteArrays();
    deleteHistograms();
    free( mKey );
}

void StEStructFluct::AddToBin( int    iBin,
                               double Nplus,    double Nminus,
                               double Ptplus,   double Ptminus,
                               double PtSqplus, double PtSqminus ) {
    int jBin = iBin - 1;
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

    if (Nsum > 0) {
        TotEvents[1][jBin]++;

        NSum[0][jBin] += Nsum;
        NSum[1][jBin] += Nsum*Nsum;

        r  = Ptsum*Ptsum/Nsum;
        PSum[0][jBin] += Ptsum;
        PSum[1][jBin] += r;
        PSum[2][jBin] += r*Ptsum/Nsum;
        PSum[3][jBin] += r*r/Nsum;
        PSum[4][jBin] += PtSqsum;

        PNSum[0][jBin] += sqrt(Nsum);
        PNSum[1][jBin] += Nsum*sqs;
        PNSum[2][jBin] += Ptsum/sqs;
        PNSum[3][jBin] += Ptsum*sqs;

        NDiff[0][jBin] += Ndiff;
        NDiff[1][jBin] += Ndiff*Ndiff;

        double r1 = Ptdiff*Ptdiff/Nsum;
        double r2 = Ptdiff*Ndiff/Nsum;
        double r3 = Ndiff*Ndiff/Nsum;
        PDiff[0][jBin] += r1;
        PDiff[1][jBin] += r2;
        PDiff[2][jBin] += r3;
        PDiff[3][jBin] += r1*r1/Nsum;
        PDiff[4][jBin] += r1*r2/Nsum;
        PDiff[5][jBin] += r1*r3/Nsum;
        PDiff[6][jBin] += r2*r3/Nsum;
        PDiff[7][jBin] += r3*r3/Nsum;

        PNDiff[0][jBin] += Ndiff/sqrt(Nsum);
        PNDiff[1][jBin] += Ndiff*Ndiff/sqrt(Nsum);
        PNDiff[2][jBin] += Ptdiff/sqrt(Nsum);
        PNDiff[3][jBin] += Ptdiff*Ndiff/sqrt(Nsum);
    }

    if (Nplus > 0) {
        TotEvents[2][jBin]++;

        NPlus[0][jBin] += Nplus;
        NPlus[1][jBin] += Nplus*Nplus;

        r  = Ptplus*Ptplus/Nplus;
        PPlus[0][jBin] += Ptplus;
        PPlus[1][jBin] += r;
        PPlus[2][jBin] += r*Ptplus/Nplus;
        PPlus[3][jBin] += r*r/Nplus;
        PPlus[4][jBin] += PtSqplus;

        PNPlus[0][jBin] += sqp;
        PNPlus[1][jBin] += Nplus*sqp;
        PNPlus[2][jBin] += Ptplus/sqp;
        PNPlus[3][jBin] += Ptplus*sqp;
    }

    if (Nminus > 0) {
        TotEvents[3][jBin]++;

        NMinus[0][jBin] += Nminus;
        NMinus[1][jBin] += Nminus*Nminus;

        r  = Ptminus*Ptminus/Nminus;
        PMinus[0][jBin] += Ptminus;
        PMinus[1][jBin] += r;
        PMinus[2][jBin] += r*Ptminus/Nminus;
        PMinus[3][jBin] += r*r/Nminus;
        PMinus[4][jBin] += PtSqminus;

        PNMinus[0][jBin] += sqm;
        PNMinus[1][jBin] += Nminus*sqm;
        PNMinus[2][jBin] += Ptminus/sqm;
        PNMinus[3][jBin] += Ptminus*sqm;
    }

    if ((Nplus > 0) && (Nminus > 0)) {
        TotEvents[4][jBin]++;

        NPlusMinus[jBin] += Nplus*Nminus;

        PPlusMinus[0][jBin] += sqp*sqm;
        PPlusMinus[1][jBin] += Ptplus*sqm/sqp;
        PPlusMinus[2][jBin] += Ptminus*sqp/sqm;
        PPlusMinus[3][jBin] += Ptplus*Ptminus/(sqp*sqm);
        PPlusMinus[4][jBin] += Nplus;
        PPlusMinus[5][jBin] += Ptplus;
        PPlusMinus[6][jBin] += Nminus;
        PPlusMinus[7][jBin] += Ptminus;

        PNPlusMinus[0][jBin] += sqp;
        PNPlusMinus[1][jBin] += Nminus*sqp;
        PNPlusMinus[2][jBin] += Ptplus/sqp;
        PNPlusMinus[3][jBin] += Ptplus*Nminus/sqp;
        PNPlusMinus[4][jBin] += sqm;
        PNPlusMinus[5][jBin] += Nplus*sqm;
        PNPlusMinus[6][jBin] += Ptminus/sqm;
        PNPlusMinus[7][jBin] += Ptminus*Nplus/sqm;
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
    occPNDiff->Fill(nd*pd);
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

//--------------------------------------------------------------------------
//
//------------ Below are init, delete, write functions -------///
//


void StEStructFluct::writeHistograms() {

    initHistograms();
    fillHistograms();

    for (int jStat=0;jStat<5;jStat++) {
        hTotEvents[jStat]->Write();
    }

    for (int jStat=0;jStat<2;jStat++) {
        hNSum[jStat]->Write();
        hNDiff[jStat]->Write();
        hNPlus[jStat]->Write();
        hNMinus[jStat]->Write();
    }
    hNPlusMinus->Write();
    for (int jStat=0;jStat<5;jStat++) {
        hPSum[jStat]->Write();
        hPPlus[jStat]->Write();
        hPMinus[jStat]->Write();
    }
    for (int jStat=0;jStat<8;jStat++) {
        hPDiff[jStat]->Write();
    }
    for (int jStat=0;jStat<8;jStat++) {
        hPPlusMinus[jStat]->Write();
    }
    for (int jStat=0;jStat<4;jStat++) {
        hPNSum[jStat]->Write();
        hPNDiff[jStat]->Write();
        hPNPlus[jStat]->Write();
        hPNMinus[jStat]->Write();
    }
    for (int jStat=0;jStat<8;jStat++) {
        hPNPlusMinus[jStat]->Write();
    }
}
void StEStructFluct::fillHistograms() {

    // Here I copy from arrays top histograms so I can write the histograms.

    for (int jStat=0;jStat<5;jStat++) {
        for (int iBin=0;iBin<mTotBins;iBin++) {
             hTotEvents[jStat]->SetBinContent(iBin+1,TotEvents[jStat][iBin]);
        }
    }

    for (int jStat=0;jStat<2;jStat++) {
        for (int iBin=0;iBin<mTotBins;iBin++) {
            hNSum[jStat]->SetBinContent(iBin+1,NSum[jStat][iBin]);
            hNDiff[jStat]->SetBinContent(iBin+1,NDiff[jStat][iBin]);
            hNPlus[jStat]->SetBinContent(iBin+1,NPlus[jStat][iBin]);
            hNMinus[jStat]->SetBinContent(iBin+1,NMinus[jStat][iBin]);
        }
    }
    for (int iBin=0;iBin<mTotBins;iBin++) {
        hNPlusMinus->SetBinContent(iBin+1,NPlusMinus[iBin]);
    }
    for (int jStat=0;jStat<5;jStat++) {
        for (int iBin=0;iBin<mTotBins;iBin++) {
            hPSum[jStat]->SetBinContent(iBin+1,PSum[jStat][iBin]);
            hPPlus[jStat]->SetBinContent(iBin+1,PPlus[jStat][iBin]);
            hPMinus[jStat]->SetBinContent(iBin+1,PMinus[jStat][iBin]);
        }
    }
    for (int jStat=0;jStat<8;jStat++) {
        for (int iBin=0;iBin<mTotBins;iBin++) {
            hPDiff[jStat]->SetBinContent(iBin+1,PDiff[jStat][iBin]);
        }
    }
    for (int jStat=0;jStat<8;jStat++) {
        for (int iBin=0;iBin<mTotBins;iBin++) {
            hPPlusMinus[jStat]->SetBinContent(iBin+1,PPlusMinus[jStat][iBin]);
        }
    }
    for (int jStat=0;jStat<4;jStat++) {
        for (int iBin=0;iBin<mTotBins;iBin++) {
            hPNSum[jStat]->SetBinContent(iBin+1,PNSum[jStat][iBin]);
            hPNDiff[jStat]->SetBinContent(iBin+1,PNDiff[jStat][iBin]);
            hPNPlus[jStat]->SetBinContent(iBin+1,PNPlus[jStat][iBin]);
            hPNMinus[jStat]->SetBinContent(iBin+1,PNMinus[jStat][iBin]);
        }
    }
    for (int jStat=0;jStat<8;jStat++) {
        for (int iBin=0;iBin<mTotBins;iBin++) {
            hPNPlusMinus[jStat]->SetBinContent(iBin+1,PNPlusMinus[jStat][iBin]);
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
    multNSum->Write();
    multNPlus->Write();
    multNMinus->Write();
    multNDiff->Write();
    multPSum->Write();
    multPPlus->Write();
    multPMinus->Write();
    multPDiff->Write();
}

//--------------------------------------------------------------------------
void StEStructFluct::initArrays() {
    char line[255];

    // Here are histograms toward uniform occupancy of bins.
    sprintf( line, "occNSum_%s", mKey );
    occNSum = new TH2F(line,line,NPHIBINS,-3.1415926,3.1415926,NETABINS,ETAMIN,ETAMAX);
    sprintf( line, "occNPlus_%s", mKey );
    occNPlus = new TH2F(line,line,NPHIBINS,-3.1415926,3.1415926,NETABINS,ETAMIN,ETAMAX);
    sprintf( line, "occNMinus_%s", mKey );
    occNMinus = new TH2F(line,line,NPHIBINS,-3.1415926,3.1415926,NETABINS,ETAMIN,ETAMAX);
    sprintf( line, "occNDiff_%s", mKey );
    occNDiff = new TH2F(line,line,NPHIBINS,-3.1415926,3.1415926,NETABINS,ETAMIN,ETAMAX);
    sprintf( line, "occPSum_%s", mKey );
    occPSum = new TH2F(line,line,NPHIBINS,-3.1415926,3.1415926,NETABINS,ETAMIN,ETAMAX);
    sprintf( line, "occPPlus_%s", mKey );
    occPPlus = new TH2F(line,line,NPHIBINS,-3.1415926,3.1415926,NETABINS,ETAMIN,ETAMAX);
    sprintf( line, "occPMinus_%s", mKey );
    occPMinus = new TH2F(line,line,NPHIBINS,-3.1415926,3.1415926,NETABINS,ETAMIN,ETAMAX);
    sprintf( line, "occPDiff_%s", mKey );
    occPDiff = new TH2F(line,line,NPHIBINS,-3.1415926,3.1415926,NETABINS,ETAMIN,ETAMAX);
    sprintf( line, "occPNSum_%s", mKey );
    occPNSum = new TH2F(line,line,NPHIBINS,-3.1415926,3.1415926,NETABINS,ETAMIN,ETAMAX);
    sprintf( line, "occPNPlus_%s", mKey );
    occPNPlus = new TH2F(line,line,NPHIBINS,-3.1415926,3.1415926,NETABINS,ETAMIN,ETAMAX);
    sprintf( line, "occPNMinus_%s", mKey );
    occPNMinus = new TH2F(line,line,NPHIBINS,-3.1415926,3.1415926,NETABINS,ETAMIN,ETAMAX);
    sprintf( line, "occPNDiff_%s", mKey );
    occPNDiff = new TH2F(line,line,NPHIBINS,-3.1415926,3.1415926,NETABINS,ETAMIN,ETAMAX);

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

cout << "Allocating arrays to store info." << endl;
    for (int jStat=0;jStat<5;jStat++) {
        TotEvents[jStat] = (double *) malloc( sizeof(double) * mTotBins );
    }

    for (int jStat=0;jStat<2;jStat++) {
        NSum[jStat] = (double *) malloc( sizeof(double) * mTotBins );
        NDiff[jStat] = (double *) malloc( sizeof(double) * mTotBins );
        NPlus[jStat] = (double *) malloc( sizeof(double) * mTotBins );
        NMinus[jStat] = (double *) malloc( sizeof(double) * mTotBins );
    }
    NPlusMinus = (double *) malloc( sizeof(double) * mTotBins );
    for (int jStat=0;jStat<5;jStat++) {
        PSum[jStat] = (double *) malloc( sizeof(double) * mTotBins );
        PPlus[jStat] = (double *) malloc( sizeof(double) * mTotBins );
        PMinus[jStat] = (double *) malloc( sizeof(double) * mTotBins );
    }
    for (int jStat=0;jStat<8;jStat++) {
        PDiff[jStat] = (double *) malloc( sizeof(double) * mTotBins );
    }
    for (int jStat=0;jStat<8;jStat++) {
        PPlusMinus[jStat] = (double *) malloc( sizeof(double) * mTotBins );
    }
    for (int jStat=0;jStat<4;jStat++) {
        PNSum[jStat] = (double *) malloc( sizeof(double) * mTotBins );
        PNDiff[jStat] = (double *) malloc( sizeof(double) * mTotBins );
        PNPlus[jStat] = (double *) malloc( sizeof(double) * mTotBins );
        PNMinus[jStat] = (double *) malloc( sizeof(double) * mTotBins );
    }
    for (int jStat=0;jStat<8;jStat++) {
        PNPlusMinus[jStat] = (double *) malloc( sizeof(double) * mTotBins );
    }
}

//--------------------------------------------------------------------------
void StEStructFluct::deleteArrays() {

cout << "Deleting occupancy histograms." << endl;
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
    delete multNSum;
    delete multNPlus;
    delete multNMinus;
    delete multNDiff;
    delete multPSum;
    delete multPPlus;
    delete multPMinus;
    delete multPDiff;

cout << "freeing Arrays." << endl;
    for (int jStat=0;jStat<5;jStat++) {
        free(TotEvents[jStat]);
    }

    for (int jStat=0;jStat<2;jStat++) {
        free(NSum[jStat]);
        free(NDiff[jStat]);
        free(NPlus[jStat]);
        free(NMinus[jStat]);
    }
    free(hNPlusMinus);
    for (int jStat=0;jStat<5;jStat++) {
        free(PSum[jStat]);
        free(PPlus[jStat]);
        free(PMinus[jStat]);
    }
    for (int jStat=0;jStat<8;jStat++) {
        free(PDiff[jStat]);
    }
    for (int jStat=0;jStat<8;jStat++) {
        free(PPlusMinus[jStat]);
    }
    for (int jStat=0;jStat<4;jStat++) {
        free(PNSum[jStat]);
        free(PNDiff[jStat]);
        free(PNPlus[jStat]);
        free(PNMinus[jStat]);
    }
    for (int jStat=0;jStat<8;jStat++) {
        free(PNPlusMinus[jStat]);
    }
}


void StEStructFluct::initHistograms() {
    char line[255];

cout << "Allocating histograms for I/O for key " << mKey << endl;
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

    for (int jStat=0;jStat<2;jStat++) {
        sprintf( line, "NSum%s_%i", mKey, jStat );
        hNSum[jStat] = new TH1D(line,line,mTotBins,0.5,mTotBins+0.5);
        sprintf( line, "NDiff%s_%i", mKey, jStat );
        hNDiff[jStat] = new TH1D(line,line,mTotBins,0.5,mTotBins+0.5);
        sprintf( line, "NPlus%s_%i", mKey, jStat );
        hNPlus[jStat] = new TH1D(line,line,mTotBins,0.5,mTotBins+0.5);
        sprintf( line, "NMinus%s_%i", mKey, jStat );
        hNMinus[jStat] = new TH1D(line,line,mTotBins,0.5,mTotBins+0.5);
    }
    sprintf( line, "NPlusMinus%s_0", mKey );
    hNPlusMinus = new TH1D(line,line,mTotBins,0.5,mTotBins+0.5);
    for (int jStat=0;jStat<5;jStat++) {
        sprintf( line, "PSum%s_%i", mKey, jStat );
        hPSum[jStat] = new TH1D(line,line,mTotBins,0.5,mTotBins+0.5);
        sprintf( line, "PPlus%s_%i", mKey, jStat );
        hPPlus[jStat] = new TH1D(line,line,mTotBins,0.5,mTotBins+0.5);
        sprintf( line, "PMinus%s_%i", mKey, jStat );
        hPMinus[jStat] = new TH1D(line,line,mTotBins,0.5,mTotBins+0.5);
    }
    for (int jStat=0;jStat<8;jStat++) {
        sprintf( line, "PDiff%s_%i", mKey, jStat );
        hPDiff[jStat] = new TH1D(line,line,mTotBins,0.5,mTotBins+0.5);
    }
    for (int jStat=0;jStat<8;jStat++) {
        sprintf( line, "PPlusMinus%s_%i", mKey, jStat );
        hPPlusMinus[jStat] = new TH1D(line,line,mTotBins,0.5,mTotBins+0.5);
    }
    for (int jStat=0;jStat<4;jStat++) {
        sprintf( line, "PNSum%s_%i", mKey, jStat );
        hPNSum[jStat] = new TH1D(line,line,mTotBins,0.5,mTotBins+0.5);
        sprintf( line, "PNDiff%s_%i", mKey, jStat );
        hPNDiff[jStat] = new TH1D(line,line,mTotBins,0.5,mTotBins+0.5);
        sprintf( line, "PNPlus%s_%i", mKey, jStat );
        hPNPlus[jStat] = new TH1D(line,line,mTotBins,0.5,mTotBins+0.5);
        sprintf( line, "PNMinus%s_%i", mKey, jStat );
        hPNMinus[jStat] = new TH1D(line,line,mTotBins,0.5,mTotBins+0.5);
    }
    for (int jStat=0;jStat<8;jStat++) {
        sprintf( line, "PNPlusMinus%s_%i", mKey, jStat );
        hPNPlusMinus[jStat] = new TH1D(line,line,mTotBins,0.5,mTotBins+0.5);
    }
}

//--------------------------------------------------------------------------
void StEStructFluct::deleteHistograms() {

cout << "freeing h Array histograms." << endl;
    for (int jStat=0;jStat<5;jStat++) {
        delete hTotEvents[jStat];
    }

    for (int jStat=0;jStat<2;jStat++) {
        delete hNSum[jStat];
        delete hNDiff[jStat];
        delete hNPlus[jStat];
        delete hNMinus[jStat];
    }
    delete hNPlusMinus;
    for (int jStat=0;jStat<5;jStat++) {
        delete hPSum[jStat];
        delete hPPlus[jStat];
        delete hPMinus[jStat];
    }
    for (int jStat=0;jStat<8;jStat++) {
        delete hPPlusMinus[jStat];
    }
    for (int jStat=0;jStat<4;jStat++) {
        delete hPNSum[jStat];
        delete hPNPlus[jStat];
        delete hPNMinus[jStat];
    }
    for (int jStat=0;jStat<8;jStat++) {
        delete hPNPlusMinus[jStat];
    }
}
