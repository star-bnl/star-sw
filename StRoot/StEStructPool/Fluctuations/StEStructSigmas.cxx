
#include "StEStructSigmas.h"

#include "TH1F.h"
#include "TH2F.h"
#include "TFile.h"


ClassImp(StEStructSigmas)

//--------------------------------------------------------------------------
StEStructSigmas::StEStructSigmas( char *key,
                                  int   nPhi,   int   nEta, 
                                  float EtaMin, float EtaMax, 
                                  const char *preFix ) {
    mKey = strdup(key);
    mNPhiBins = nPhi;
    mNEtaBins = nEta;
    mEtaMin = EtaMin;
    mEtaMax = EtaMax;
    mpreFix = strdup(preFix);
    initHistograms();
}
//--------------------------------------------------------------------------
StEStructSigmas::~StEStructSigmas() {
    deleteHistograms();
    free(mKey);
    free(mpreFix);
}

void StEStructSigmas::fillHistograms() {
    NHistograms();
    PHistograms();
    PNHistograms();
}


//--------------------------------------------------------------------------
void StEStructSigmas::NHistograms() {
    char buffer[255];

    TH2F *hnBins   = (TH2F *) gDirectory->Get("nBins");
    TH2F *hoffset = (TH2F *) gDirectory->Get("offset");
    TH2F *hfUnique = (TH2F *) gDirectory->Get("fUnique");

    // We have accumulated over events.
    // For every bin at each scale we have summed up all the small bins.
    // Now accumulate sum, n, n^2 for all bins.

    NSig->Reset();
    NDel->Reset();
    NPlus->Reset();
    NMinus->Reset();
    NPlusMinus->Reset();

    NSigErrors->Reset();
    NDelErrors->Reset();
    NPlusErrors->Reset();
    NMinusErrors->Reset();
    NPlusMinusErrors->Reset();

    TH1D *hTotEvents[5];
    TH1D *hNSum[16];
    TH1D *hNDiff[2];
    TH1D *hNPlus[5];
    TH1D *hNMinus[5];
    TH1D *hNPlusMinus[8];

    sprintf(buffer,"%sTotalEvents_%s", mpreFix, mKey);
    hTotEvents[0] = (TH1D *) gDirectory->Get(buffer);
    sprintf(buffer,"%sTotalSumEvents_%s", mpreFix, mKey);
    hTotEvents[1] = (TH1D *) gDirectory->Get(buffer);
    sprintf(buffer,"%sTotalPlusEvents_%s", mpreFix, mKey);
    hTotEvents[2] = (TH1D *) gDirectory->Get(buffer);
    sprintf(buffer,"%sTotalMinusEvents_%s", mpreFix, mKey);
    hTotEvents[3] = (TH1D *) gDirectory->Get(buffer);
    sprintf(buffer,"%sTotalPlusMinusEvents_%s", mpreFix, mKey);
    hTotEvents[4] = (TH1D *) gDirectory->Get(buffer);

    for (int jStat=0;jStat<16;jStat++) {
        sprintf( buffer,"%sNSum%s_%i", mpreFix, mKey, jStat );
        hNSum[jStat] = (TH1D *) gDirectory->Get(buffer);
    }
    for (int jStat=0;jStat<2;jStat++) {
        sprintf( buffer,"%sNDiff%s_%i", mpreFix, mKey, jStat );
        hNDiff[jStat] = (TH1D *) gDirectory->Get(buffer);
    }
    for (int jStat=0;jStat<5;jStat++) {
        sprintf( buffer,"%sNPlus%s_%i", mpreFix, mKey, jStat );
        hNPlus[jStat] = (TH1D *) gDirectory->Get(buffer);
    }
    for (int jStat=0;jStat<5;jStat++) {
        sprintf( buffer,"%sNMinus%s_%i", mpreFix, mKey, jStat );
        hNMinus[jStat] = (TH1D *) gDirectory->Get(buffer);
    }
    for (int jStat=0;jStat<8;jStat++) {
        sprintf( buffer,"%sNPlusMinus%s_%i", mpreFix, mKey, jStat );
        hNPlusMinus[jStat] = (TH1D *) gDirectory->Get(buffer);
    }

    int mBin, oBin;
    for (int iPhi=1;iPhi<=mNPhiBins;iPhi++) {
        for (int iEta=1;iEta<=mNEtaBins;iEta++) {
            double NS[16], ND[2], NP[5], NM[5], NC[8];
            double sigSq, psigSq, msigSq;
            double Ss = 0, nSs = 0, DSs = 0, nDSs = 0;
            double Sd = 0, nSd = 0, DSd = 0, nDSd = 0;
            double Sp = 0, nSp = 0, DSp = 0, nDSp = 0;
            double Sm = 0, nSm = 0, DSm = 0, nDSm = 0;
            double Sc = 0, nSc = 0, DSc = 0, nDSc = 0;
            double sumEvents, plusEvents, minusEvents, plusMinusEvents;

            mBin = (int) hnBins->GetBinContent(iPhi,iEta);
            oBin = (int) hoffset->GetBinContent(iPhi,iEta);

            for (int iBin=oBin+1;iBin<=oBin+mBin;iBin++) {
                sumEvents = hTotEvents[0]->GetBinContent(iBin);
                if (sumEvents > 0) {
                    for (int jStat=0;jStat<16;jStat++) {
                        NS[jStat] = hNSum[jStat]->GetBinContent(iBin) / sumEvents;
                    }
                    for (int jStat=0;jStat<2;jStat++) {
                        ND[jStat] = hNDiff[jStat]->GetBinContent(iBin) / sumEvents;
                    }
                    double NSum = NS[0] + NS[1];
                    if (NSum > 0) {
                        double SumSq = NS[2] + 2*NS[3] + NS[4];
                        sigSq = (SumSq - NSum*NSum) / NSum;
                        Ss   += sigSq - 1;
                        nSs  += 1;
                        DSs  += (4 + sigSq/NSum) * sigSq / sumEvents;
                        nDSs += sqrt(hfUnique->GetBinContent(iPhi,iEta));

                        double NDiff = NS[0] - NS[1];
                        double DiffSq = NS[2] - 2*NS[3] + NS[4];
                        sigSq = (DiffSq - NDiff*NDiff) / NSum;
                        Sd   += sigSq - 1;
                        nSd  += 1;
                        DSd  += (4 + sigSq/NSum) * sigSq / sumEvents;
                        nDSd += sqrt(hfUnique->GetBinContent(iPhi,iEta));
                    }
                }

                psigSq = 0;
                plusEvents = hTotEvents[0]->GetBinContent(iBin);
                if (plusEvents > 0) {
                    for (int jStat=0;jStat<5;jStat++) {
                        NP[jStat] = hNPlus[jStat]->GetBinContent(iBin) / plusEvents;
                    }
                    if (NP[0] > 0) {
                        sigSq = (NP[1] - NP[0]*NP[0]) / NP[0];
                        Sp   += sigSq - 1;
                        nSp  += 1;
                        DSp  += (4 + sigSq/NP[0]) * sigSq / plusEvents;
                        nDSp += sqrt(hfUnique->GetBinContent(iPhi,iEta));
                        psigSq = sigSq;
                    }
                }

                msigSq = 0;
                minusEvents = hTotEvents[0]->GetBinContent(iBin);
                if (minusEvents > 0) {
                    for (int jStat=0;jStat<5;jStat++) {
                        NM[jStat] = hNMinus[jStat]->GetBinContent(iBin) / minusEvents;
                    }
                    if (NM[0] > 0) {
                        sigSq = (NM[1] - NM[0]*NM[0]) / NM[0];
                        Sm   += sigSq - 1;
                        nSm  += 1;
                        DSm  += (4 + sigSq/NM[0]) * sigSq / minusEvents;
                        nDSm += sqrt(hfUnique->GetBinContent(iPhi,iEta));
                        msigSq = sigSq;
                    }
                }

                plusMinusEvents = hTotEvents[0]->GetBinContent(iBin);
                if (plusMinusEvents > 0) {
                    for (int jStat=0;jStat<8;jStat++) {
                        NC[jStat] = hNPlusMinus[jStat]->GetBinContent(iBin) / plusMinusEvents;
                    }
                    if (NC[0]*NC[1] > 0) {
                        sigSq = (NC[2] - NC[0]*NC[1])/sqrt(NC[0]*NC[1]);
                        Sc   += sigSq;
                        nSc  += 1;
                        DSc  += (psigSq + msigSq) / plusMinusEvents +
                                    (1/NC[0]+1/NC[1])*sigSq*sigSq/4;
                        nDSc += sqrt(hfUnique->GetBinContent(iPhi,iEta));
                    }
                }
            }

            if (nSs > 0) {
                NSig->SetBinContent(iPhi,iEta,Ss/nSs);
                NSigErrors->SetBinContent(iPhi,iEta,sqrt(DSs)/nDSs);
            }
            if (nSd > 0) {
                NDel->SetBinContent(iPhi,iEta,Sd/nSs);
                NDelErrors->SetBinContent(iPhi,iEta,sqrt(DSd)/nDSd);
            }
            if (nSp > 0) {
                NPlus->SetBinContent(iPhi,iEta,Sp/nSp);
                NPlusErrors->SetBinContent(iPhi,iEta,sqrt(DSp)/nDSp);
            }
            if (nSm > 0) {
                NMinus->SetBinContent(iPhi,iEta,Sm/nSm);
                NMinusErrors->SetBinContent(iPhi,iEta,sqrt(DSm)/nDSm);
            }
            if (nSc > 0) {
                NPlusMinus->SetBinContent(iPhi,iEta,Sc/nSc);
                NPlusMinusErrors->SetBinContent(iPhi,iEta,sqrt(DSc)/nDSc);
            }
        }
    }
}
void StEStructSigmas::PHistograms() {
    char buffer[255];

    TH2F *hnBins   = (TH2F *) gDirectory->Get("nBins");
    TH2F *hoffset = (TH2F *) gDirectory->Get("offset");
    TH2F *hfUnique = (TH2F *) gDirectory->Get("fUnique");

    // We have accumulated over events.
    // For every bin at each scale we have summed up all the small bins.
    // Now accumulate sum, n, n^2 for all bins.

    for (int iType=0;iType<3;iType++) {
        PSig[iType]->Reset();
        PDel[iType]->Reset();
        PPlus[iType]->Reset();
        PMinus[iType]->Reset();
        PPlusMinus[iType]->Reset();
    }

    PSigErrors->Reset();
    PDelErrors->Reset();
    PPlusErrors->Reset();
    PMinusErrors->Reset();
    PPlusMinusErrors->Reset();

    SPtHat->Reset();
    PPtHat->Reset();
    MPtHat->Reset();
    sigSPtHat->Reset();
    sigPPtHat->Reset();
    sigMPtHat->Reset();

    sigSPtHatErrors->Reset();
    sigPPtHatErrors->Reset();
    sigMPtHatErrors->Reset();


    TH1D *hTotEvents[5];

    TH1D *hNSum[16];
    TH1D *hNDiff[2];
    TH1D *hNPlus[5];
    TH1D *hNMinus[5];
    TH1D *hNPlusMinus[8];

    TH1D *hPSum[16];
    TH1D *hPDiff[16];
    TH1D *hPPlus[11];
    TH1D *hPMinus[11];
    TH1D *hPPlusMinus[17];

    sprintf(buffer,"%sTotalEvents_%s", mpreFix, mKey);
    hTotEvents[0] = (TH1D *) gDirectory->Get(buffer);
    sprintf(buffer,"%sTotalSumEvents_%s", mpreFix, mKey);
    hTotEvents[1] = (TH1D *) gDirectory->Get(buffer);
    sprintf(buffer,"%sTotalPlusEvents_%s", mpreFix, mKey);
    hTotEvents[2] = (TH1D *) gDirectory->Get(buffer);
    sprintf(buffer,"%sTotalMinusEvents_%s", mpreFix, mKey);
    hTotEvents[3] = (TH1D *) gDirectory->Get(buffer);
    sprintf(buffer,"%sTotalPlusMinusEvents_%s", mpreFix, mKey);
    hTotEvents[4] = (TH1D *) gDirectory->Get(buffer);


    for (int jStat=0;jStat<16;jStat++) {
        sprintf( buffer,"%sNSum%s_%i", mpreFix, mKey, jStat );
        hNSum[jStat] = (TH1D *) gDirectory->Get(buffer);
    }
    for (int jStat=0;jStat<2;jStat++) {
        sprintf( buffer,"%sNDiff%s_%i", mpreFix, mKey, jStat );
        hNDiff[jStat] = (TH1D *) gDirectory->Get(buffer);
    }
    for (int jStat=0;jStat<5;jStat++) {
        sprintf( buffer,"%sNPlus%s_%i", mpreFix, mKey, jStat );
        hNPlus[jStat] = (TH1D *) gDirectory->Get(buffer);
    }
    for (int jStat=0;jStat<5;jStat++) {
        sprintf( buffer,"%sNMinus%s_%i", mpreFix, mKey, jStat );
        hNMinus[jStat] = (TH1D *) gDirectory->Get(buffer);
    }
    for (int jStat=0;jStat<8;jStat++) {
        sprintf( buffer,"%sNPlusMinus%s_%i", mpreFix, mKey, jStat );
        hNPlusMinus[jStat] = (TH1D *) gDirectory->Get(buffer);
    }


    for (int jStat=0;jStat<16;jStat++) {
        sprintf(buffer,"%sPSum%s_%i", mpreFix, mKey, jStat);
        hPSum[jStat] = (TH1D *) gDirectory->Get(buffer);
    }
    for (int jStat=0;jStat<16;jStat++) {
        sprintf(buffer,"%sPDiff%s_%i", mpreFix, mKey, jStat);
        hPDiff[jStat] = (TH1D *) gDirectory->Get(buffer);
    }
    for (int jStat=0;jStat<11;jStat++) {
        sprintf(buffer,"%sPPlus%s_%i", mpreFix, mKey, jStat);
        hPPlus[jStat] = (TH1D *) gDirectory->Get(buffer);
    }
    for (int jStat=0;jStat<11;jStat++) {
        sprintf(buffer,"%sPMinus%s_%i", mpreFix, mKey, jStat);
        hPMinus[jStat] = (TH1D *) gDirectory->Get(buffer);
    }
    for (int jStat=0;jStat<17;jStat++) {
        sprintf(buffer,"%sPPlusMinus%s_%i", mpreFix, mKey, jStat);
        hPPlusMinus[jStat] = (TH1D *) gDirectory->Get(buffer);
    }

    int mBin, oBin;

    for (int iPhi=1;iPhi<=mNPhiBins;iPhi++) {
        for (int iEta=1;iEta<=mNEtaBins;iEta++) {
            double NS[16], ND[2],  NP[5],  NM[5],  NC[8];
            double PS[16], PD[16], PP[11], PM[11], PC[17];

            double pHat[3], pHat2[3];
            double sigSq, sigHat, DsigHat;
            double Ss[] = {0, 0, 0}, nSs = 0, DSs = 0, nDSs = 0, hSs = 0, hDSs = 0;
            double Sd[] = {0, 0, 0}, nSd = 0, DSd = 0, nDSd = 0;
            double Sp[] = {0, 0, 0}, nSp = 0, DSp = 0, nDSp = 0, hSp = 0, hDSp = 0;
            double Sm[] = {0, 0, 0}, nSm = 0, DSm = 0, nDSm = 0, hSm = 0, hDSm = 0;
            double Sc[] = {0, 0, 0}, nSc = 0, nDSc = 0;

            double hatSs = 0;
            double hatSp = 0;
            double hatSm = 0;

            double totEvents, sumEvents, plusEvents, minusEvents, plusMinusEvents;

            mBin = (int) hnBins->GetBinContent(iPhi,iEta);
            oBin = (int) hoffset->GetBinContent(iPhi,iEta);

            for (int iBin=oBin+1;iBin<=oBin+mBin;iBin++) {
                totEvents = hTotEvents[0]->GetBinContent(iBin);
                sumEvents = hTotEvents[1]->GetBinContent(iBin);
                if (sumEvents > 0) {
                    double r = sumEvents / totEvents;
                    r = 1;
                    for (int jStat=0;jStat<16;jStat++) {
                        NS[jStat] = hNSum[jStat]->GetBinContent(iBin) / sumEvents;
                    }
                    for (int jStat=0;jStat<2;jStat++) {
                        ND[jStat] = hNDiff[jStat]->GetBinContent(iBin) / sumEvents;
                    }
                    for (int jStat=0;jStat<16;jStat++) {
                        PS[jStat] = hPSum[jStat]->GetBinContent(iBin) / sumEvents;
                    }
                    for (int jStat=0;jStat<16;jStat++) {
                        PD[jStat] = hPDiff[jStat]->GetBinContent(iBin) / sumEvents;
                    }
                    double NSum = NS[0] + NS[1];
                    double PSum = PS[1] + PS[2];
                    if (NSum > 0) {
                        pHat[0]  = PSum / NSum;
                        pHat2[0] = pHat[0]*pHat[0];
                        if (NS[0] > 0) {
                            pHat[1]  = PS[1] / NS[0];
                        } else {
                            pHat[1] = 0;
                        }
                        pHat2[1] = pHat[1]*pHat[1];
                        if (NS[1] > 0) {
                            pHat[2]  = PS[2] / NS[1];
                        } else {
                            pHat[2] = 0;
                        }
                        pHat2[2] = pHat[2]*pHat[2];

                        hatSs   += pHat[0];
                        sigHat   = PS[0]/NSum - pHat[0]*pHat[0];
                        hSs     += sigHat;
                        DsigHat  = 4*pHat2[0]*sigHat / (NSum*sumEvents);
                        hDSs    += DsigHat;
                        sigSq    = PS[8] - 2*pHat[1]*PS[6] - 2*pHat[2]*PS[7]
                                     + pHat2[1]*NS[5] + 2*pHat[1]*pHat[2]*NS[6] + pHat2[2]*NS[7];
                        Ss[0]   += (sigSq - sigHat) * r;
                        sigSq    = PS[5] - 2*pHat[1]*PS[3] - 2*pHat[2]*PS[4]
                                     + pHat2[1]*NS[2] + 2*pHat[1]*pHat[2]*NS[3] + pHat2[2]*NS[4];
                        Ss[1]   += (sigSq/NSum - sigHat) * r;
                        sigSq    = PS[11] - 2*pHat[1]*PS[9] - 2*pHat[2]*PS[10]
                                     + pHat2[1]*NS[8] + 2*pHat[1]*pHat[2]*NS[9] + pHat2[2]*NS[10];
                        Ss[2]   += NSum * (sigSq - sigHat*NS[15]) * r;
                        nSs     += 1;

                        DSs      += (PS[13] - 4*pHat[0]*PS[12] + 6*pHat2[0]*PS[8]
                                   - 4*pHat[0]*pHat2[0]*PSum + pHat2[0]*pHat2[0]*NSum) *NSum/sumEvents;
                        nDSs     += sqrt(hfUnique->GetBinContent(iPhi,iEta));

                        sigSq       = PD[5] - 2*pHat[1]*PD[3] + 2*pHat[2]*PD[4]
                                     + pHat2[1]*NS[5] - 2*pHat[1]*pHat[2]*NS[6] + pHat2[2]*NS[7];
                        Sd[0]    += (sigSq - sigHat) * r;
                        sigSq       = PD[2] - 2*pHat[1]*PD[0] + 2*pHat[2]*PD[1]
                                     + pHat2[1]*NS[2] - 2*pHat[1]*pHat[2]*NS[3] + pHat2[2]*NS[4];
                        Sd[1]    += (sigSq/NSum - sigHat) * r;
                        sigSq       = PD[8] - 2*pHat[1]*PD[6] + 2*pHat[2]*PD[7]
                                     + pHat2[1]*NS[8] - 2*pHat[1]*pHat[2]*NS[9] + pHat2[2]*NS[10];
                        Sd[2]    += NSum * (sigSq - sigHat*NS[15]) * r;
                        nSd      += 1;

                        DSd      += (PD[11] - 4*pHat[0]*PD[12] + 6*pHat2[0]*PD[13]
                                          - 4*pHat[0]*pHat2[0]*PD[14] + pHat2[0]*pHat2[0]*PD[15]) *NSum/sumEvents;
                        nDSd     += sqrt(hfUnique->GetBinContent(iPhi,iEta));
                    }
                }

                plusEvents = hTotEvents[2]->GetBinContent(iBin);
                if (plusEvents > 0) {
                    double r = plusEvents / totEvents;
                    r = 1;
                    for (int jStat=0;jStat<5;jStat++) {
                        NP[jStat] = hNPlus[jStat]->GetBinContent(iBin) / plusEvents;
                    }
                    for (int jStat=0;jStat<11;jStat++) {
                        PP[jStat] = hPPlus[jStat]->GetBinContent(iBin) / plusEvents;
                    }
                    if (NP[0] > 0) {
                        pHat[1]  = PP[1] / NP[0];
                        pHat2[1] = pHat[1]*pHat[1];
                        hatSp   += pHat[1];
                        sigHat   = PP[0]/NP[0] - pHat[1]*pHat[1];
                        hSp     += sigHat;
                        DsigHat  = 4*pHat2[1]*sigHat / (NP[0]*plusEvents);
                        hDSp    += DsigHat;
                        sigSq    = PP[5] - PP[1]*PP[1]/NP[0];
                        Sp[0]   += sigSq - sigHat;
                        sigSq    = PP[4] - 2*pHat[1]*PP[2] + pHat2[1]*NP[1];
                        Sp[1]   += sigSq/NP[0] - sigHat;
                        sigSq    = PP[6] - 2*pHat[1]*PP[3] + pHat2[1];
                        Sp[2]   += NP[0] * (sigSq - sigHat*NP[4]) * r;
                        nSp     += 1;

                        DSp     += (PP[8] - 4*pHat[1]*PP[7] + 6*pHat2[1]*PP[5]
                                          - 4*pHat[1]*pHat2[1]*PP[1] + pHat2[1]*pHat2[1]*NP[0]) *NP[0]/plusEvents;
                        nDSp    += sqrt(hfUnique->GetBinContent(iPhi,iEta));
                    }
                }

                minusEvents = hTotEvents[3]->GetBinContent(iBin);
                if (minusEvents > 0) {
                    double r = minusEvents / sumEvents;
                    r = 1;
                    for (int jStat=0;jStat<5;jStat++) {
                        NM[jStat] = hNMinus[jStat]->GetBinContent(iBin) / minusEvents;
                    }
                    for (int jStat=0;jStat<11;jStat++) {
                        PM[jStat] = hPMinus[jStat]->GetBinContent(iBin) / minusEvents;
                    }
                    if (NM[0] > 0) {
                        pHat[2]  = PM[1] / NM[0];
                        pHat2[2] = pHat[2]*pHat[2];
                        hatSm   += pHat[2];
                        sigHat   = PM[0]/NM[0] - pHat[2]*pHat[2];
                        hSm     += sigHat;
                        DsigHat  = 4*pHat2[2]*sigHat / (NM[0]*minusEvents);
                        hDSm    += DsigHat;
                        sigSq    = PM[5] - PM[1]*PM[1]/NM[0];
                        Sm[0]   += sigSq - sigHat;
                        sigSq    = PM[4] - 2*pHat[2]*PM[2] + pHat2[2]*NM[1];
                        Sm[1]   += sigSq/NM[0] - sigHat;
                        sigSq    = PM[6] - 2*pHat[2]*PM[3] + pHat2[2];
                        Sm[2]   += NM[0] * (sigSq - sigHat*NM[4]) * r;
                        nSm     += 1;

                        DSm     += (PM[8] - 4*pHat[2]*PM[7] + 6*pHat2[2]*PM[5]
                                          - 4*pHat[2]*pHat2[2]*PM[1] + pHat2[2]*pHat2[2]*NM[0]) * NM[0] / minusEvents;
                        nDSm    += sqrt(hfUnique->GetBinContent(iPhi,iEta));
                    }
                }

                plusMinusEvents = sumEvents;
                if (plusMinusEvents > 0) {
                    for (int jStat=0;jStat<8;jStat++) {
                        NC[jStat] = hNPlusMinus[jStat]->GetBinContent(iBin) / plusMinusEvents;
                    }
                    for (int jStat=0;jStat<17;jStat++) {
                        PC[jStat] = hPPlusMinus[jStat]->GetBinContent(iBin) / plusMinusEvents;
                    }
                    if (NC[0] > 0) {
                        pHat[1] = PC[0] / NC[0];
                    } else {
                        pHat[1] = 0;
                    }
                    if (NC[1] > 0) {
                        pHat[2] = PC[1] / NC[1];
                    } else {
                        pHat[2] = 0;
                    }
                    sigSq   = PC[16] - pHat[2]*PC[10] - pHat[1]*PC[11] + pHat[1]*pHat[2]*NC[5];
                    Sc[0]  += sigSq;
                    sigSq   = PC[14] - pHat[2]*PC[5]  - pHat[1]*PC[4]  + pHat[1]*pHat[2]*NC[2];
                    Sc[1]  += sigSq/sqrt(NC[0]*NC[1]);
                    sigSq   = PC[15] - pHat[2]*PC[2]  - pHat[1]*PC[3]  + pHat[1]*pHat[2];
                    Sc[2]  += sigSq*sqrt(NC[0]*NC[1]);
                    nSc    += 1;
                    nDSc    += sqrt(hfUnique->GetBinContent(iPhi,iEta));
                }
            }

            if (nSs > 0) {
                PSig[0]->SetBinContent(iPhi,iEta,Ss[0]/nSs);
                PSig[1]->SetBinContent(iPhi,iEta,Ss[1]/nSs);
                PSig[2]->SetBinContent(iPhi,iEta,Ss[2]/nSs);
                PSigErrors->SetBinContent(iPhi,iEta,sqrt(DSs)/nDSs);
                SPtHat->SetBinContent(iPhi,iEta,hatSs/nSs);
                sigSPtHat->SetBinContent(iPhi,iEta,hSs/nSs);
                sigSPtHatErrors->SetBinContent(iPhi,iEta,sqrt(hDSs)/nDSs);
            }
            if (nSd > 0) {
                PDel[0]->SetBinContent(iPhi,iEta,Sd[0]/nSd);
                PDel[1]->SetBinContent(iPhi,iEta,Sd[1]/nSd);
                PDel[2]->SetBinContent(iPhi,iEta,Sd[2]/nSd);
                PDelErrors->SetBinContent(iPhi,iEta,sqrt(DSd)/nDSd);
            }

            if (nSp > 0) {
                PPlus[0]->SetBinContent(iPhi,iEta,Sp[0]/nSp);
                PPlus[1]->SetBinContent(iPhi,iEta,Sp[1]/nSp);
                PPlus[2]->SetBinContent(iPhi,iEta,Sp[2]/nSp);
                PPlusErrors->SetBinContent(iPhi,iEta,sqrt(DSp)/nDSp);
                PPtHat->SetBinContent(iPhi,iEta,hatSp/nSp);
                sigPPtHat->SetBinContent(iPhi,iEta,hSp/nSp);
                sigPPtHatErrors->SetBinContent(iPhi,iEta,sqrt(hDSp)/nDSp);
            }

            if (nSm > 0) {
                PMinus[0]->SetBinContent(iPhi,iEta,Sm[0]/nSm);
                PMinus[1]->SetBinContent(iPhi,iEta,Sm[1]/nSm);
                PMinus[2]->SetBinContent(iPhi,iEta,Sm[2]/nSm);
                PMinusErrors->SetBinContent(iPhi,iEta,sqrt(DSm)/nDSm);
                MPtHat->SetBinContent(iPhi,iEta,hatSm/nSm);
                sigMPtHat->SetBinContent(iPhi,iEta,hSm/nSm);
                sigMPtHatErrors->SetBinContent(iPhi,iEta,sqrt(hDSm)/nDSm);
            }

            if (nSc > 0) {
                PPlusMinus[0]->SetBinContent(iPhi,iEta,Sc[0]/nSc);
                PPlusMinus[1]->SetBinContent(iPhi,iEta,Sc[1]/nSc);
                PPlusMinus[2]->SetBinContent(iPhi,iEta,Sc[2]/nSc);
// Haven't found good approximation for error on +- covariance.
// Use average of errors on + and - to have something.
                PPlusMinusErrors->SetBinContent(iPhi,iEta,sqrt((DSp+DSm)/2)/nDSs);
            }
        }
    }
}
void StEStructSigmas::PNHistograms() {
    char buffer[255];

    TH2F *hnBins   = (TH2F *) gDirectory->Get("nBins");
    TH2F *hoffset = (TH2F *) gDirectory->Get("offset");

    // We have accumulated over events.
    // For every bin at each scale we have summed up all the small bins.
    // Now accumulate sum, n, n^2 for all bins.

    for (int iType=0;iType<3;iType++) {
        PNSig[iType]->Reset();
        PNDel[iType]->Reset();
        PNPlus[iType]->Reset();
        PNMinus[iType]->Reset();
        PNPlusMinus[iType]->Reset();
        PNMinusPlus[iType]->Reset();
    }

    PNSigErrors->Reset();
    PNDelErrors->Reset();
    PNPlusErrors->Reset();
    PNMinusErrors->Reset();
    PNPlusMinusErrors->Reset();
    PNMinusPlusErrors->Reset();

    TH1D *hTotEvents[5];

    TH1D *hNSum[16];
    TH1D *hNDiff[2];
    TH1D *hNPlus[5];
    TH1D *hNMinus[5];
    TH1D *hNPlusMinus[8];

    TH1D *hPSum[16];
    TH1D *hPDiff[16];
    TH1D *hPPlus[11];
    TH1D *hPMinus[11];
    TH1D *hPPlusMinus[17];

    sprintf(buffer,"%sTotalEvents_%s", mpreFix, mKey);
    hTotEvents[0] = (TH1D *) gDirectory->Get(buffer);
    sprintf(buffer,"%sTotalSumEvents_%s", mpreFix, mKey);
    hTotEvents[1] = (TH1D *) gDirectory->Get(buffer);
    sprintf(buffer,"%sTotalPlusEvents_%s", mpreFix, mKey);
    hTotEvents[2] = (TH1D *) gDirectory->Get(buffer);
    sprintf(buffer,"%sTotalMinusEvents_%s", mpreFix, mKey);
    hTotEvents[3] = (TH1D *) gDirectory->Get(buffer);
    sprintf(buffer,"%sTotalPlusMinusEvents_%s", mpreFix, mKey);
    hTotEvents[4] = (TH1D *) gDirectory->Get(buffer);

    for (int jStat=0;jStat<16;jStat++) {
        sprintf( buffer,"%sNSum%s_%i", mpreFix, mKey, jStat );
        hNSum[jStat] = (TH1D *) gDirectory->Get(buffer);
    }
    for (int jStat=0;jStat<2;jStat++) {
        sprintf( buffer,"%sNDiff%s_%i", mpreFix, mKey, jStat );
        hNDiff[jStat] = (TH1D *) gDirectory->Get(buffer);
    }
    for (int jStat=0;jStat<5;jStat++) {
        sprintf( buffer,"%sNPlus%s_%i", mpreFix, mKey, jStat );
        hNPlus[jStat] = (TH1D *) gDirectory->Get(buffer);
    }
    for (int jStat=0;jStat<5;jStat++) {
        sprintf( buffer,"%sNMinus%s_%i", mpreFix, mKey, jStat );
        hNMinus[jStat] = (TH1D *) gDirectory->Get(buffer);
    }
    for (int jStat=0;jStat<8;jStat++) {
        sprintf( buffer,"%sNPlusMinus%s_%i", mpreFix, mKey, jStat );
        hNPlusMinus[jStat] = (TH1D *) gDirectory->Get(buffer);
    }


    for (int jStat=0;jStat<16;jStat++) {
        sprintf(buffer,"%sPSum%s_%i", mpreFix, mKey, jStat);
        hPSum[jStat] = (TH1D *) gDirectory->Get(buffer);
    }
    for (int jStat=0;jStat<16;jStat++) {
        sprintf(buffer,"%sPDiff%s_%i", mpreFix, mKey, jStat);
        hPDiff[jStat] = (TH1D *) gDirectory->Get(buffer);
    }
    for (int jStat=0;jStat<11;jStat++) {
        sprintf(buffer,"%sPPlus%s_%i", mpreFix, mKey, jStat);
        hPPlus[jStat] = (TH1D *) gDirectory->Get(buffer);
    }
    for (int jStat=0;jStat<11;jStat++) {
        sprintf(buffer,"%sPMinus%s_%i", mpreFix, mKey, jStat);
        hPMinus[jStat] = (TH1D *) gDirectory->Get(buffer);
    }
    for (int jStat=0;jStat<17;jStat++) {
        sprintf(buffer,"%sPPlusMinus%s_%i", mpreFix, mKey, jStat);
        hPPlusMinus[jStat] = (TH1D *) gDirectory->Get(buffer);
    }


    int mBin, oBin;

    for (int iPhi=1;iPhi<=mNPhiBins;iPhi++) {
        for (int iEta=1;iEta<=mNEtaBins;iEta++) {
            double NS[16], ND[2],  NP[5],  NM[5],  NC[8];
            double PS[16], PD[16], PP[11], PM[11], PC[17];

            double sigSq, pHat[3];
            double Ss[] = {0, 0, 0}, nSs = 0;
            double Sd[] = {0, 0, 0}, nSd = 0;
            double Sp[] = {0, 0, 0}, nSp = 0;
            double Sm[] = {0, 0, 0}, nSm = 0;
            double Sc1[] = {0, 0, 0}, nSc1 = 0;
            double Sc2[] = {0, 0, 0}, nSc2 = 0;

            double totEvents, sumEvents, plusEvents, minusEvents, plusMinusEvents;
            double nErr, pErr;

            mBin = (int) hnBins->GetBinContent(iPhi,iEta);
            oBin = (int) hoffset->GetBinContent(iPhi,iEta);

            for (int iBin=oBin+1;iBin<=oBin+mBin;iBin++) {
                totEvents = hTotEvents[0]->GetBinContent(iBin);
                sumEvents = hTotEvents[1]->GetBinContent(iBin);
                if (sumEvents > 0) {
                    for (int jStat=0;jStat<16;jStat++) {
                        NS[jStat] = hNSum[jStat]->GetBinContent(iBin) / sumEvents;
                    }
                    for (int jStat=0;jStat<2;jStat++) {
                        ND[jStat] = hNDiff[jStat]->GetBinContent(iBin) / sumEvents;
                    }
                    for (int jStat=0;jStat<16;jStat++) {
                        PS[jStat] = hPSum[jStat]->GetBinContent(iBin) / sumEvents;
                    }
                    for (int jStat=0;jStat<16;jStat++) {
                        PD[jStat] = hPDiff[jStat]->GetBinContent(iBin) / sumEvents;
                    }
                    double NSum = NS[0] + NS[1];
                    if (NSum > 0) {
                        if (NS[0] > 0) {
                            pHat[1]  = PS[1] / NS[0];
                        } else {
                            pHat[1] = 0;
                        }
                        if (NS[1] > 0) {
                            pHat[2]  = PS[2] / NS[1];
                        } else {
                            pHat[2] = 0;
                        }
                        sigSq   = (PS[15] - NSum*PS[14]
                                  - pHat[1]*(NS[12] - NSum*NS[11])
                                  - pHat[2]*(NS[14] - NSum*NS[13])) / sqrt(NSum);
                        Ss[0]  += sigSq;
                        sigSq   = (PS[3]+PS[4]
                                  - pHat[1]*(NS[2] + NS[3])
                                  - pHat[2]*(NS[3] + NS[4])) / NSum;
                        Ss[1]  += sigSq;
                        sigSq   = (-PS[9] - PS[10]
                                   + pHat[1]*(NS[8] + NS[9])
                                   + pHat[2]*(NS[9] + NS[10])) * NSum;
                        Ss[2]  += sigSq;
                        nSs +=1;

                        double NDiff = NS[0] - NS[1];
                        sigSq   = PD[10] - NDiff*PD[9]
                                  - pHat[1]*(ND[0]-NDiff*NS[11])
                                  + pHat[2]*(ND[1]-NDiff*NS[13]);
                        Sd[0]  += sigSq / sqrt(NSum);
                        sigSq   = PD[0] - PD[1]
                                  - pHat[1]*(NS[2] - NS[3])
                                  + pHat[2]*(NS[3] - NS[4]);
                        Sd[1]  += sigSq / NSum;
                        sigSq   = PD[3] - PD[4] - NDiff*(PD[6]+PD[7])
                                  - pHat[1]*(NS[5]-NS[6] - NDiff*(NS[8]+NS[9]))
                                  + pHat[2]*(NS[6]-NS[7] - NDiff*(NS[9]+NS[10]));
                        Sd[2]  += sigSq;
                        nSd += 1;
                    }
                }

                plusEvents = hTotEvents[2]->GetBinContent(iBin);
                if (plusEvents > 0) {
                    for (int jStat=0;jStat<5;jStat++) {
                        NP[jStat] = hNPlus[jStat]->GetBinContent(iBin) / plusEvents;
                    }
                    for (int jStat=0;jStat<11;jStat++) {
                        PP[jStat] = hPPlus[jStat]->GetBinContent(iBin) / plusEvents;
                    }
                    if (NP[0] > 0) {
                        pHat[1] = PP[1] / NP[0];
                        sigSq  = PP[10] - NP[0]*PP[9]
                                  - pHat[1]*NP[3] + PP[1]*NP[2];
                        Sp[0]  += sigSq / sqrt(NP[0]);
                        sigSq  = PP[2] - pHat[1]*NP[1];
                        Sp[1]  += sigSq / NP[0];
                        sigSq  = PP[1] - NP[0]*PP[3];
                        Sp[2]  += sigSq;
                        nSp    += 1;
                    }
                }

                minusEvents = hTotEvents[3]->GetBinContent(iBin);
                if (minusEvents > 0) {
                    for (int jStat=0;jStat<5;jStat++) {
                        NM[jStat] = hNMinus[jStat]->GetBinContent(iBin) / minusEvents;
                    }
                    for (int jStat=0;jStat<11;jStat++) {
                        PM[jStat] = hPMinus[jStat]->GetBinContent(iBin) / minusEvents;
                    }
                    if (NM[0] > 0) {
                        pHat[2] = PM[1] / NM[0];
                        sigSq  = PM[10] - NM[0]*PM[9]
                                  - pHat[2]*NM[3] + PM[1]*NM[2];
                        Sm[0]  += sigSq / sqrt(NM[0]);
                        sigSq  = PM[2] - pHat[2]*NM[1];
                        Sm[1]  += sigSq / NM[0];
                        sigSq  = PM[1] - NM[0]*PM[3];
                        Sm[2]  += sigSq;
                        nSm    += 1;
                    }
                }

                plusMinusEvents = hTotEvents[4]->GetBinContent(iBin);
                if (plusMinusEvents > 0) {
                    for (int jStat=0;jStat<8;jStat++) {
                        NC[jStat] = hNPlusMinus[jStat]->GetBinContent(iBin) / plusMinusEvents;
                    }
                    for (int jStat=0;jStat<17;jStat++) {
                        PC[jStat] = hPPlusMinus[jStat]->GetBinContent(iBin) / plusMinusEvents;
                    }
                    if (NC[0] > 0) {
                        pHat[1] = PC[0] / NC[0];
                    } else {
                        pHat[1] = 0;
                    }
                    if (NC[1] > 0) {
                        pHat[2] = PC[1] / NC[1];
                    } else {
                        pHat[2] = 0;
                    }

                    sigSq = (PC[12] - NC[1]*PC[6]
                              - pHat[1]*NC[6] + pHat[1]*NC[1]*NC[3]) / sqrt(NC[1]);
                    Sc1[0] += sigSq;
                    sigSq = (PC[5] - NC[1]*PC[0]
                              - pHat[1]*NC[2] + PC[0]*NC[1]) / sqrt(NC[0]*NC[1]);
                    Sc1[1] += sigSq;
                    sigSq = (PC[8] - NC[1]*PC[2]) * sqrt(NC[0]/NC[1]);
                    Sc1[2] += sigSq;
                    nSc1 += 1;

                    sigSq = (PC[13] - NC[0]*PC[7]
                              - pHat[2]*NC[7] + pHat[2]*NC[0]*NC[4]) / sqrt(NC[0]);
                    Sc2[0] += sigSq;
                    sigSq = (PC[4] - NC[0]*PC[1]
                              - pHat[2]*NC[2] + PC[1]*NC[0]) / sqrt(NC[0]*NC[1]);
                    Sc2[1] += sigSq;
                    sigSq = (PC[9] - NC[0]*PC[3]) * sqrt(NC[1]/NC[0]);
                    Sc2[2] += sigSq;
                    nSc2 += 1;
                }
            }

            if (nSs > 0) {
                PNSig[0]->SetBinContent(iPhi,iEta,Ss[0]/nSs);
                PNSig[1]->SetBinContent(iPhi,iEta,Ss[1]/nSs);
                PNSig[2]->SetBinContent(iPhi,iEta,Ss[2]/nSs);
            }
            if (nSd > 0) {
                PNDel[0]->SetBinContent(iPhi,iEta,Sd[0]/nSd);
                PNDel[1]->SetBinContent(iPhi,iEta,Sd[1]/nSd);
                PNDel[2]->SetBinContent(iPhi,iEta,Sd[2]/nSd);
            }
            if (nSp > 0) {
                PNPlus[0]->SetBinContent(iPhi,iEta,Sp[0]/nSp);
                PNPlus[1]->SetBinContent(iPhi,iEta,Sp[1]/nSp);
                PNPlus[2]->SetBinContent(iPhi,iEta,Sp[2]/nSp);
            }
            if (nSm > 0) {
                PNMinus[0]->SetBinContent(iPhi,iEta,Sm[0]/nSm);
                PNMinus[1]->SetBinContent(iPhi,iEta,Sm[1]/nSm);
                PNMinus[2]->SetBinContent(iPhi,iEta,Sm[2]/nSm);
            }
            if (nSc1) {
                PNPlusMinus[0]->SetBinContent(iPhi,iEta,Sc1[0]/nSc1);
                PNPlusMinus[1]->SetBinContent(iPhi,iEta,Sc1[1]/nSc1);
                PNPlusMinus[2]->SetBinContent(iPhi,iEta,Sc1[2]/nSc1);
            }
            if (nSc2) {
                PNMinusPlus[0]->SetBinContent(iPhi,iEta,Sc2[0]/nSc2);
                PNMinusPlus[1]->SetBinContent(iPhi,iEta,Sc2[1]/nSc2);
                PNMinusPlus[2]->SetBinContent(iPhi,iEta,Sc2[2]/nSc2);
            }
// I haven't been able to find a reasonable approximation to the errors
// on the Pt-N covariance terms. My best guess is the errors factor,
// and since we already have the N and the Pt errors I use those.
            nErr = NSigErrors->GetBinContent(iPhi,iEta);
            pErr = PSigErrors->GetBinContent(iPhi,iEta);
            PNSigErrors->SetBinContent(iPhi,iEta,sqrt(nErr*pErr));
            nErr = NDelErrors->GetBinContent(iPhi,iEta);
            pErr = PDelErrors->GetBinContent(iPhi,iEta);
            PNDelErrors->SetBinContent(iPhi,iEta,sqrt(nErr*pErr));
            nErr = NPlusErrors->GetBinContent(iPhi,iEta);
            pErr = PPlusErrors->GetBinContent(iPhi,iEta);
            PNPlusErrors->SetBinContent(iPhi,iEta,sqrt(nErr*pErr));
            nErr = NMinusErrors->GetBinContent(iPhi,iEta);
            pErr = PMinusErrors->GetBinContent(iPhi,iEta);
            PNMinusErrors->SetBinContent(iPhi,iEta,sqrt(nErr*pErr));
            nErr = NPlusMinusErrors->GetBinContent(iPhi,iEta);
            pErr = PPlusMinusErrors->GetBinContent(iPhi,iEta);
            PNPlusMinusErrors->SetBinContent(iPhi,iEta,sqrt(nErr*pErr));
            PNMinusPlusErrors->SetBinContent(iPhi,iEta,sqrt(nErr*pErr));
        }
    }
}

//
//------------ Below are init, delete, write functions -------///
//


//--------------------------------------------------------------------------
void StEStructSigmas::writeHistograms() {
    NSig->Write();
    NDel->Write();
    NPlus->Write();
    NMinus->Write();
    NPlusMinus->Write();
    for (int iType=0;iType<3;iType++) {
        PSig[iType]->Write();
        PDel[iType]->Write();
        PPlus[iType]->Write();
        PMinus[iType]->Write();
        PPlusMinus[iType]->Write();
        PNSig[iType]->Write();
        PNDel[iType]->Write();
        PNPlus[iType]->Write();
        PNMinus[iType]->Write();
        PNPlusMinus[iType]->Write();
        PNMinusPlus[iType]->Write();
    }

    SPtHat->Write();
    PPtHat->Write();
    MPtHat->Write();
    sigSPtHat->Write();
    sigPPtHat->Write();
    sigMPtHat->Write();

    NSigErrors->Write();
    NDelErrors->Write();
    NPlusErrors->Write();
    NMinusErrors->Write();
    NPlusMinusErrors->Write();
    PSigErrors->Write();
    PDelErrors->Write();
    PPlusErrors->Write();
    PMinusErrors->Write();
    PPlusMinusErrors->Write();
    PNSigErrors->Write();
    PNDelErrors->Write();
    PNPlusErrors->Write();
    PNMinusErrors->Write();
    PNPlusMinusErrors->Write();
    PNMinusPlusErrors->Write();

    sigSPtHatErrors->Write();
    sigPPtHatErrors->Write();
    sigMPtHatErrors->Write();
}

//--------------------------------------------------------------------------
void StEStructSigmas::initHistograms() {
    char line[255];

    // Here are histograms for storing variances and errors of variances..
    sprintf( line, "NSig_%s", mKey );
    NSig = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,mEtaMax-mEtaMin);
    sprintf( line, "NDel_%s", mKey );
    NDel = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,mEtaMax-mEtaMin);
    sprintf( line, "NPlus_%s", mKey );
    NPlus = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,mEtaMax-mEtaMin);
    sprintf( line, "NMinus_%s", mKey );
    NMinus = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,mEtaMax-mEtaMin);
    sprintf( line, "NPlusMinus_%s", mKey );
    NPlusMinus = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,mEtaMax-mEtaMin);

    for (int iType=0;iType<3;iType++) {
        sprintf( line, "PSig%i_%s", iType, mKey );
        PSig[iType] = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,mEtaMax-mEtaMin);
        sprintf( line, "PDel%i_%s", iType, mKey );
        PDel[iType] = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,mEtaMax-mEtaMin);
        sprintf( line, "PPlus%i_%s", iType, mKey );
        PPlus[iType] = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,mEtaMax-mEtaMin);
        sprintf( line, "PMinus%i_%s", iType, mKey );
        PMinus[iType] = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,mEtaMax-mEtaMin);
        sprintf( line, "PPlusMinus%i_%s", iType, mKey );
        PPlusMinus[iType] = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,mEtaMax-mEtaMin);
        sprintf( line, "PNSig%i_%s", iType, mKey );
        PNSig[iType] = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,mEtaMax-mEtaMin);
        sprintf( line, "PNDel%i_%s", iType, mKey );
        PNDel[iType] = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,mEtaMax-mEtaMin);
        sprintf( line, "PNPlus%i_%s", iType, mKey );
        PNPlus[iType] = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,mEtaMax-mEtaMin);
        sprintf( line, "PNMinus%i_%s", iType, mKey );
        PNMinus[iType] = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,mEtaMax-mEtaMin);
        sprintf( line, "PNPlusMinus%i_%s", iType, mKey );
        PNPlusMinus[iType] = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,mEtaMax-mEtaMin);
        sprintf( line, "PNMinusPlus%i_%s", iType, mKey );
        PNMinusPlus[iType] = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,mEtaMax-mEtaMin);
    }

    sprintf( line, "SptHat_%s", mKey );
    SPtHat    = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,mEtaMax-mEtaMin);
    sprintf( line, "PptHat_%s", mKey );
    PPtHat    = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,mEtaMax-mEtaMin);
    sprintf( line, "MptHat_%s", mKey );
    MPtHat    = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,mEtaMax-mEtaMin);
    sprintf( line, "SsigPtHat_%s", mKey );
    sigSPtHat = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,mEtaMax-mEtaMin);
    sprintf( line, "PsigPtHat_%s", mKey );
    sigPPtHat = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,mEtaMax-mEtaMin);
    sprintf( line, "MsigPtHat_%s", mKey );
    sigMPtHat = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,mEtaMax-mEtaMin);

    sprintf( line, "NSigErrors_%s", mKey );
    NSigErrors = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,mEtaMax-mEtaMin);
    sprintf( line, "NDelErrors_%s", mKey );
    NDelErrors = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,mEtaMax-mEtaMin);
    sprintf( line, "NPlusErrors_%s", mKey );
    NPlusErrors = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,mEtaMax-mEtaMin);
    sprintf( line, "NMinusErrors_%s", mKey );
    NMinusErrors = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,mEtaMax-mEtaMin);
    sprintf( line, "NPlusMinusErrors_%s", mKey );
    NPlusMinusErrors = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,mEtaMax-mEtaMin);
    sprintf( line, "PSigErrors_%s", mKey );
    PSigErrors = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,mEtaMax-mEtaMin);
    sprintf( line, "PDelErrors_%s", mKey );
    PDelErrors = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,mEtaMax-mEtaMin);
    sprintf( line, "PPlusErrors_%s", mKey );
    PPlusErrors = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,mEtaMax-mEtaMin);
    sprintf( line, "PMinusErrors_%s", mKey );
    PMinusErrors = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,mEtaMax-mEtaMin);
    sprintf( line, "PPlusMinusErrors_%s", mKey );
    PPlusMinusErrors = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,mEtaMax-mEtaMin);
    sprintf( line, "PNSigErrors_%s", mKey );
    PNSigErrors = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,mEtaMax-mEtaMin);
    sprintf( line, "PNDelErrors_%s", mKey );
    PNDelErrors = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,mEtaMax-mEtaMin);
    sprintf( line, "PNPlusErrors_%s", mKey );
    PNPlusErrors = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,mEtaMax-mEtaMin);
    sprintf( line, "PNMinusErrors_%s", mKey );
    PNMinusErrors = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,mEtaMax-mEtaMin);
    sprintf( line, "PNPlusMinusErrors_%s", mKey );
    PNPlusMinusErrors = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,mEtaMax-mEtaMin);
    sprintf( line, "PNMinusPlusErrors_%s", mKey );
    PNMinusPlusErrors = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,mEtaMax-mEtaMin);

    sprintf( line, "SsigPtHatErrors_%s", mKey );
    sigSPtHatErrors = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,mEtaMax-mEtaMin);
    sprintf( line, "PsigPtHatErrors_%s", mKey );
    sigPPtHatErrors = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,mEtaMax-mEtaMin);
    sprintf( line, "MsigPtHatErrors_%s", mKey );
    sigMPtHatErrors = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,mEtaMax-mEtaMin);
}

//--------------------------------------------------------------------------
void StEStructSigmas::deleteHistograms() {

cout << "Deleting bin***Var2, bin***Errors histograms." << endl;
    delete NSig;
    delete NDel;
    delete NPlus;
    delete NMinus;
    delete NPlusMinus;
    for (int iType=0;iType<3;iType++) {
        delete PSig[iType];
        delete PDel[iType];
        delete PPlus[iType];
        delete PMinus[iType];
        delete PPlusMinus[iType];
        delete PNSig[iType];
        delete PNDel[iType];
        delete PNPlus[iType];
        delete PNMinus[iType];
        delete PNPlusMinus[iType];
        delete PNMinusPlus[iType];
    }

    delete SPtHat;
    delete PPtHat;
    delete MPtHat;
    delete sigSPtHat;
    delete sigPPtHat;
    delete sigMPtHat;

    delete NSigErrors;
    delete NDelErrors;
    delete NPlusErrors;
    delete NMinusErrors;
    delete NPlusMinusErrors;
    delete PSigErrors;
    delete PDelErrors;
    delete PPlusErrors;
    delete PMinusErrors;
    delete PPlusMinusErrors;
    delete PNSigErrors;
    delete PNDelErrors;
    delete PNPlusErrors;
    delete PNMinusErrors;
    delete PNPlusMinusErrors;
    delete PNMinusPlusErrors;

    delete sigSPtHatErrors;
    delete sigPPtHatErrors;
    delete sigMPtHatErrors;
}
