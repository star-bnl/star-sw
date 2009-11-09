
#include "StEStructSigmas.h"

#include "TMath.h"
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
    mEtaSumMode = 1;
    mPhiSumMode = 1;
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
    binTupleStruct   bTuple;
    scaleTupleStruct sTuple;
    sumTupleStruct   STuple;

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

    NSigCorrection->Reset();
    NDelCorrection->Reset();
    NPlusCorrection->Reset();
    NMinusCorrection->Reset();
    NPlusMinusCorrection->Reset();

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

    double vecCI[NPHIBINS][NETABINS][3];
    double vecCD[NPHIBINS][NETABINS][3];
    double vecPP[NPHIBINS][NETABINS][3];
    double vecMM[NPHIBINS][NETABINS][3];
    double vecPM[NPHIBINS][NETABINS][3];
    double BCI = 0, nBCI = 0, BCD = 0, nBCD = 0, BPP = 0, nBPP = 0;
    double BMM = 0, nBMM = 0, BPM = 0, nBPM = 0;
    int mBin, oBin;
    for (int iPhi=1;iPhi<=mNPhiBins;iPhi++) {
        int jPhi = iPhi - 1;
        for (int iEta=1;iEta<=mNEtaBins;iEta++) {
            int jEta = iEta - 1;
            for (int i=0;i<3;i++) {
                vecCI[jPhi][jEta][i] = 0;
                vecCD[jPhi][jEta][i] = 0;
                vecPP[jPhi][jEta][i] = 0;
                vecMM[jPhi][jEta][i] = 0;
                vecPM[jPhi][jEta][i] = 0;
            }

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

            int iEtaBin = 0, iPhiBin = 1;
            double mCI[2][2], mCD[2][2], mPP[2][2], mMM[2][2], mPM[2][2];
            double vCI[2],    vCD[2],    vPP[2],    vMM[2],    vPM[2];
            double mInv[2][2], mDet;
            int nEtaBin = getNumEtaBins(iEta);
//            int nPhiBin = getNumPhiBins(iEta);
            float phi1, phi2, eta1, eta2;
            double f3;
            for (int i=0;i<2;i++) {
                vCI[i] = 0;
                vCD[i] = 0;
                vPP[i] = 0;
                vMM[i] = 0;
                vPM[i] = 0;
                for (int j=0;j<2;j++) {
                    mCI[i][j] = 0;
                    mCD[i][j] = 0;
                    mPP[i][j] = 0;
                    mMM[i][j] = 0;
                    mPM[i][j] = 0;
                }
            }
            for (int iBin=oBin+1;iBin<=oBin+mBin;iBin++) {
                iEtaBin++;
                if (iEtaBin > nEtaBin) {
                    iEtaBin = 1;
                    iPhiBin++;
                }
                phi1 = 0 + 2*3.1415926*getPhiStart(iPhiBin,iPhi)/mNPhiBins;
                phi2 = phi1 + iPhi * 2*3.1415926 / mNPhiBins;
                eta1 = mEtaMin + (mEtaMax-mEtaMin)*getEtaStart(iEtaBin,iEta)/mNEtaBins;
                eta2 = eta1 + iEta * (mEtaMax-mEtaMin) / mNEtaBins;
                f3   = (pow(eta2,3)-pow(eta1,3))/(3*(eta2-eta1));
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
                        bTuple.type     = 1;
                        bTuple.phiScale = iPhi;
                        bTuple.etaScale = iEta;
                        bTuple.phi      = (phi1+phi2)/2;
                        bTuple.eta      = (eta1+eta2)/2;
                        bTuple.sig2     = sigSq-1;
                        bTuple.sig2_1   = 0;
                        bTuple.sig2_2   = 0;
                        bTuple.nbar     = NSum;
                        bTuple.events   = sumEvents;
                        bTuple.f3       = f3;
                        binTuple->Fill(&bTuple.type);
                        vecCI[jPhi][jEta][0] += 1;
                        vecCI[jPhi][jEta][1] += sigSq-1;
                        vecCI[jPhi][jEta][2] += f3;
                        if (nEtaBin > 3) {
                            mCI[0][0] += 1;
                            mCI[0][1] += f3;
                            mCI[1][0] += f3;
                            mCI[1][1] += f3*f3;
                            vCI[0]    += sigSq-1;
                            vCI[1]    += (sigSq-1)*f3;
                        }

                        double NDiff = NS[0] - NS[1];
                        double DiffSq = NS[2] - 2*NS[3] + NS[4];
                        sigSq = (DiffSq - NDiff*NDiff) / NSum;
                        Sd   += sigSq - 1;
                        nSd  += 1;
                        DSd  += (4 + sigSq/NSum) * sigSq / sumEvents;
                        nDSd += sqrt(hfUnique->GetBinContent(iPhi,iEta));
                        bTuple.type     = 2;
                        bTuple.phiScale = iPhi;
                        bTuple.etaScale = iEta;
                        bTuple.phi      = (phi1+phi2)/2;
                        bTuple.eta      = (eta1+eta2)/2;
                        bTuple.sig2     = sigSq-1;
                        bTuple.sig2_1   = 0;
                        bTuple.sig2_2   = 0;
                        bTuple.nbar     = NSum;
                        bTuple.events   = sumEvents;
                        bTuple.f3       = f3;
                        binTuple->Fill(&bTuple.type);
                        vecCD[jPhi][jEta][0] += 1;
                        vecCD[jPhi][jEta][1] += sigSq-1;
                        vecCD[jPhi][jEta][2] += f3;
                        if (nEtaBin > 3) {
                            mCD[0][0] += 1;
                            mCD[0][1] += f3;
                            mCD[1][0] += f3;
                            mCD[1][1] += f3*f3;
                            vCD[0]    += sigSq-1;
                            vCD[1]    += (sigSq-1)*f3;
                        }
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
                        bTuple.type     = 3;
                        bTuple.phiScale = iPhi;
                        bTuple.etaScale = iEta;
                        bTuple.phi      = (phi1+phi2)/2;
                        bTuple.eta      = (eta1+eta2)/2;
                        bTuple.sig2     = sigSq-1;
                        bTuple.sig2_1   = 0;
                        bTuple.sig2_2   = 0;
                        bTuple.nbar     = NP[0];
                        bTuple.events   = plusEvents;
                        bTuple.f3       = f3;
                        binTuple->Fill(&bTuple.type);
                        vecPP[jPhi][jEta][0] += 1;
                        vecPP[jPhi][jEta][1] += sigSq-1;
                        vecPP[jPhi][jEta][2] += f3;
                        if (nEtaBin > 3) {
                            mPP[0][0] += 1;
                            mPP[0][1] += f3;
                            mPP[1][0] += f3;
                            mPP[1][1] += f3*f3;
                            vPP[0]    += sigSq-1;
                            vPP[1]    += (sigSq-1)*f3;
                        }
                    }
                } else {
                    for (int jStat=0;jStat<5;jStat++) {
                        NP[jStat] = 0;
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
                        bTuple.type     = 4;
                        bTuple.phiScale = iPhi;
                        bTuple.etaScale = iEta;
                        bTuple.phi      = (phi1+phi2)/2;
                        bTuple.eta      = (eta1+eta2)/2;
                        bTuple.sig2     = sigSq-1;
                        bTuple.sig2_1   = 0;
                        bTuple.sig2_2   = 0;
                        bTuple.nbar     = NM[0];
                        bTuple.events   = minusEvents;
                        bTuple.f3       = f3;
                        binTuple->Fill(&bTuple.type);
                        vecMM[jPhi][jEta][0] += 1;
                        vecMM[jPhi][jEta][1] += sigSq-1;
                        vecMM[jPhi][jEta][2] += f3;
                        if (nEtaBin > 3) {
                            mMM[0][0] += 1;
                            mMM[0][1] += f3;
                            mMM[1][0] += f3;
                            mMM[1][1] += f3*f3;
                            vMM[0]    += sigSq-1;
                            vMM[1]    += (sigSq-1)*f3;
                        }
                    }
                } else {
                    for (int jStat=0;jStat<5;jStat++) {
                        NM[jStat] = 0;
                    }
                }

// Accumulation of hNPlusMinus required (n+ > 0) && (n- > 0)
// I think this is wrong. Instead if I require (n+ > 0) || (n- > 0)
// I can retrieve appropriate terms from NP and NM, at least for the
// phiPt case.
                plusMinusEvents = hTotEvents[0]->GetBinContent(iBin);
                if (plusMinusEvents > 0) {
                    for (int jStat=0;jStat<8;jStat++) {
                        NC[jStat] = hNPlusMinus[jStat]->GetBinContent(iBin) / plusMinusEvents;
                    }
                    if (NP[0]*NM[0] > 0) {
                        sigSq = (NC[2] - NP[0]*NM[0])/sqrt(NP[0]*NM[0]);
                        Sc   += sigSq;
                        nSc  += 1;
                        DSc  += (psigSq + msigSq) / plusMinusEvents +
                                    (1/NC[0]+1/NC[1])*sigSq*sigSq/4;
                        nDSc += sqrt(hfUnique->GetBinContent(iPhi,iEta));
                        bTuple.type     = 5;
                        bTuple.phiScale = iPhi;
                        bTuple.etaScale = iEta;
                        bTuple.phi      = (phi1+phi2)/2;
                        bTuple.eta      = (eta1+eta2)/2;
                        bTuple.sig2     = sigSq;
                        bTuple.sig2_1   = 0;
                        bTuple.sig2_2   = 0;
                        bTuple.nbar     = sqrt(NP[0]*NM[0]);
                        bTuple.events   = plusMinusEvents;
                        bTuple.f3       = f3;
                        binTuple->Fill(&bTuple.type);
                        vecPM[jPhi][jEta][0] += 1;
                        vecPM[jPhi][jEta][1] += sigSq;
                        vecPM[jPhi][jEta][2] += f3;
                        if (nEtaBin > 3) {
                            mPM[0][0] += 1;
                            mPM[0][1] += f3;
                            mPM[1][0] += f3;
                            mPM[1][1] += f3*f3;
                            vPM[0]    += sigSq;
                            vPM[1]    += sigSq*f3;
                        }
                    }
                }
//                plusMinusEvents = hTotEvents[0]->GetBinContent(iBin);
//                if (plusMinusEvents > 0) {
//                    for (int jStat=0;jStat<8;jStat++) {
//                        NC[jStat] = hNPlusMinus[jStat]->GetBinContent(iBin) / plusMinusEvents;
//                    }
//                    if (NC[0]*NC[1] > 0) {
//                        sigSq = (NC[2] - NC[0]*NC[1])/sqrt(NC[0]*NC[1]);
//                        Sc   += sigSq;
//                        nSc  += 1;
//                        DSc  += (psigSq + msigSq) / plusMinusEvents +
//                                    (1/NC[0]+1/NC[1])*sigSq*sigSq/4;
//                        nDSc += sqrt(hfUnique->GetBinContent(iPhi,iEta));
//                    }
//                }
            }

            sTuple.phiScale = iPhi;
            sTuple.etaScale = iEta;
            mDet = mCI[0][0]*mCI[1][1] - mCI[0][1]*mCI[1][0];
            if (mDet > 0.00000001) {
                mInv[0][0] = +mCI[1][1] / mDet;
                mInv[1][0] = -mCI[1][0] / mDet;
                mInv[0][1] = -mCI[0][1] / mDet;
                mInv[1][1] = +mCI[0][0] / mDet;
                sTuple.type = 1;
                sTuple.A    = (vCI[0]*mInv[0][0] + vCI[1]*mInv[1][0]) / (iPhi*iEta);
                sTuple.B    = (vCI[0]*mInv[0][1] + vCI[1]*mInv[1][1]) / (iPhi*iEta);
                sTuple.nBins  = mCI[0][0];
                sTuple.f3     = mCI[0][1];
                sTuple.f3sq   = mCI[1][1];
                sTuple.sig2   = vCI[0];
                sTuple.sig2f3 = vCI[1];
                scaleTuple->Fill(&sTuple.type);
                BCI  += sTuple.B;
                nBCI += 1;
            }
            mDet = mCD[0][0]*mCD[1][1] - mCD[0][1]*mCD[1][0];
            if (mDet > 0.00000001) {
                mInv[0][0] = +mCD[1][1] / mDet;
                mInv[1][0] = -mCD[1][0] / mDet;
                mInv[0][1] = -mCD[0][1] / mDet;
                mInv[1][1] = +mCD[0][0] / mDet;
                sTuple.type = 2;
                sTuple.A    = (vCD[0]*mInv[0][0] + vCD[1]*mInv[1][0]) / (iPhi*iEta);
                sTuple.B    = (vCD[0]*mInv[0][1] + vCD[1]*mInv[1][1]) / (iPhi*iEta);
                sTuple.nBins  = mCD[0][0];
                sTuple.f3     = mCD[0][1];
                sTuple.f3sq   = mCD[1][1];
                sTuple.sig2   = vCD[0];
                sTuple.sig2f3 = vCD[1];
                scaleTuple->Fill(&sTuple.type);
                BCD  += sTuple.B;
                nBCD += 1;
            }
            mDet = mPP[0][0]*mPP[1][1] - mPP[0][1]*mPP[1][0];
            if (mDet > 0.00000001) {
                mInv[0][0] = +mPP[1][1] / mDet;
                mInv[1][0] = -mPP[1][0] / mDet;
                mInv[0][1] = -mPP[0][1] / mDet;
                mInv[1][1] = +mPP[0][0] / mDet;
                sTuple.type = 3;
                sTuple.A    = (vPP[0]*mInv[0][0] + vPP[1]*mInv[1][0]) / (iPhi*iEta);
                sTuple.B    = (vPP[0]*mInv[0][1] + vPP[1]*mInv[1][1]) / (iPhi*iEta);
                sTuple.nBins  = mPP[0][0];
                sTuple.f3     = mPP[0][1];
                sTuple.f3sq   = mPP[1][1];
                sTuple.sig2   = vPP[0];
                sTuple.sig2f3 = vPP[1];
                scaleTuple->Fill(&sTuple.type);
                BPP  += sTuple.B;
                nBPP += 1;
            }
            mDet = mMM[0][0]*mMM[1][1] - mMM[0][1]*mMM[1][0];
            if (mDet > 0.00000001) {
                mInv[0][0] = +mMM[1][1] / mDet;
                mInv[1][0] = -mMM[1][0] / mDet;
                mInv[0][1] = -mMM[0][1] / mDet;
                mInv[1][1] = +mMM[0][0] / mDet;
                sTuple.type = 4;
                sTuple.A    = (vMM[0]*mInv[0][0] + vMM[1]*mInv[1][0]) / (iPhi*iEta);
                sTuple.B    = (vMM[0]*mInv[0][1] + vMM[1]*mInv[1][1]) / (iPhi*iEta);
                sTuple.nBins  = mMM[0][0];
                sTuple.f3     = mMM[0][1];
                sTuple.f3sq   = mMM[1][1];
                sTuple.sig2   = vMM[0];
                sTuple.sig2f3 = vMM[1];
                scaleTuple->Fill(&sTuple.type);
                BMM  += sTuple.B;
                nBMM += 1;
            }
            mDet = mPM[0][0]*mPM[1][1] - mPM[0][1]*mPM[1][0];
            if (mDet > 0.00000001) {
                mInv[0][0] = +mPM[1][1] / mDet;
                mInv[1][0] = -mPM[1][0] / mDet;
                mInv[0][1] = -mPM[0][1] / mDet;
                mInv[1][1] = +mPM[0][0] / mDet;
                sTuple.type = 5;
                sTuple.A    = (vPM[0]*mInv[0][0] + vPM[1]*mInv[1][0]) / (iPhi*iEta);
                sTuple.B    = (vPM[0]*mInv[0][1] + vPM[1]*mInv[1][1]) / (iPhi*iEta);
                sTuple.nBins  = mPM[0][0];
                sTuple.f3     = mPM[0][1];
                sTuple.f3sq   = mPM[1][1];
                sTuple.sig2   = vPM[0];
                sTuple.sig2f3 = vPM[1];
                scaleTuple->Fill(&sTuple.type);
                BPM  += sTuple.B;
                nBPM += 1;
            }
            if (nSs > 0) {
//                NSig->SetBinContent(iPhi,iEta,Ss/nSs);
                NSigErrors->SetBinContent(iPhi,iEta,sqrt(DSs)/nDSs);
            }
            if (nSd > 0) {
//                NDel->SetBinContent(iPhi,iEta,Sd/nSs);
                NDelErrors->SetBinContent(iPhi,iEta,sqrt(DSd)/nDSd);
            }
            if (nSp > 0) {
//                NPlus->SetBinContent(iPhi,iEta,Sp/nSp);
                NPlusErrors->SetBinContent(iPhi,iEta,sqrt(DSp)/nDSp);
            }
            if (nSm > 0) {
//                NMinus->SetBinContent(iPhi,iEta,Sm/nSm);
                NMinusErrors->SetBinContent(iPhi,iEta,sqrt(DSm)/nDSm);
            }
            if (nSc > 0) {
//                NPlusMinus->SetBinContent(iPhi,iEta,Sc/nSc);
                NPlusMinusErrors->SetBinContent(iPhi,iEta,sqrt(DSc)/nDSc);
            }
        }
    }
    STuple.type  = 1;
    STuple.B     = BCI;
    STuple.nBins = nBCI;
    sumTuple->Fill(&STuple.type);
    STuple.type  = 2;
    STuple.B     = BCD;
    STuple.nBins = nBCD;
    sumTuple->Fill(&STuple.type);
    STuple.type  = 3;
    STuple.B     = BPP;
    STuple.nBins = nBPP;
    sumTuple->Fill(&STuple.type);
    STuple.type  = 4;
    STuple.B     = BMM;
    STuple.nBins = nBMM;
    sumTuple->Fill(&STuple.type);
    STuple.type  = 5;
    STuple.B     = BPM;
    STuple.nBins = nBPM;
    sumTuple->Fill(&STuple.type);

    BCI = BCI / nBCI;
    BCD = BCD / nBCD;
    BPP = BPP / nBPP;
    BMM = BMM / nBMM;
    BPM = BPM / nBPM;
    double delS, corr, a, b, f;
    double fFull = (pow(mEtaMax,3)-pow(mEtaMin,3))/(3*(mEtaMax-mEtaMin));
    for (int jPhi=0;jPhi<mNPhiBins;jPhi++) {
        int iPhi = jPhi + 1;
        for (int jEta=0;jEta<mNEtaBins;jEta++) {
            int iEta = jEta + 1;
            if (vecCI[jPhi][jEta][0] > 0) {
                a = vecCI[jPhi][jEta][1]/vecCI[jPhi][jEta][0];
                b = BCI * iPhi*iEta;
                f = vecCI[jPhi][jEta][2]/vecCI[jPhi][jEta][0];
//                delS = a + b*(1.0/3 - f);
                corr = b*(f - fFull);
//                corr = b*f;
                delS = a - corr;
                NSig->SetBinContent(iPhi,iEta,delS);
                NSigCorrection->SetBinContent(iPhi,iEta,corr);
            } else {
                NSig->SetBinContent(iPhi,iEta,0);
            }
            if (vecCD[jPhi][jEta][0] > 0) {
                a = vecCD[jPhi][jEta][1]/vecCD[jPhi][jEta][0];
                b = BCD * iPhi*iEta;
                f = vecCD[jPhi][jEta][2]/vecCD[jPhi][jEta][0];
//                delS = a + b*(1.0/3 - f);
                corr = b*(f - fFull);
//                corr = b*f;
                delS = a - corr;
                NDel->SetBinContent(iPhi,iEta,delS);
                NDelCorrection->SetBinContent(iPhi,iEta,corr);
            } else {
                NDel->SetBinContent(iPhi,iEta,0);
            }
            if (vecPP[jPhi][jEta][0] > 0) {
                a = vecPP[jPhi][jEta][1]/vecPP[jPhi][jEta][0];
                b = BPP * iPhi*iEta;
                f = vecPP[jPhi][jEta][2]/vecPP[jPhi][jEta][0];
//                delS = a + b*(1.0/3 - f);
                corr = b*(f - fFull);
//                corr = b*f;
                delS = a - corr;
                NPlus->SetBinContent(iPhi,iEta,delS);
                NPlusCorrection->SetBinContent(iPhi,iEta,corr);
            } else {
                NPlus->SetBinContent(iPhi,iEta,0);
            }
            if (vecMM[jPhi][jEta][0] > 0) {
                a = vecMM[jPhi][jEta][1]/vecMM[jPhi][jEta][0];
                b = BMM * iPhi*iEta;
                f = vecMM[jPhi][jEta][2]/vecMM[jPhi][jEta][0];
//                delS = a + b*(1.0/3 - f);
                corr = b*(f - fFull);
//                corr = b*f;
                delS = a - corr;
                NMinus->SetBinContent(iPhi,iEta,delS);
                NMinusCorrection->SetBinContent(iPhi,iEta,corr);
            } else {
                NMinus->SetBinContent(iPhi,iEta,0);
            }
            if (vecPM[jPhi][jEta][0] > 0) {
                a = vecPM[jPhi][jEta][1]/vecPM[jPhi][jEta][0];
                b = BPM * iPhi*iEta;
                f = vecPM[jPhi][jEta][2]/vecPM[jPhi][jEta][0];
//                delS = a + b*(1.0/3 - f);
                corr = b*(f - fFull);
//                corr = b*f;
                delS = a - corr;
                NPlusMinus->SetBinContent(iPhi,iEta,delS);
                NPlusMinusCorrection->SetBinContent(iPhi,iEta,corr);
            } else {
                NPlusMinus->SetBinContent(iPhi,iEta,0);
            }
        }
    }
}
void StEStructSigmas::PHistograms() {
    char buffer[255];
    binTupleStruct bTuple;

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
    PSig[3]->Reset();

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


    TH1D *hTotEvents[6];

    TH1D *hNSum[16];
    TH1D *hNDiff[2];
    TH1D *hNPlus[5];
    TH1D *hNMinus[5];
    TH1D *hNPlusMinus[8];

    TH1D *hPSum[21];
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
    sprintf(buffer, "%sTotalEventsForSig2Dyn_%s", mpreFix, mKey);
    hTotEvents[5] = (TH1D *) gDirectory->Get(buffer);


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


    for (int jStat=0;jStat<21;jStat++) {
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
    int nBad = 0;

    // Currently working with `small' event samples and calculating \hat{p_t}
    // for small bins of peripheral events is problematic. Since we are dealing
    // with Hijing I try using an inclusive \hat{p_t}
    // Maybe bad for data where mean p_t may depend on detector location.

    
    int iBinFS = (int) (hnBins->GetBinContent(mNPhiBins,mNEtaBins)+hoffset->GetBinContent(mNPhiBins,mNEtaBins));
    double nplus  = (double) hNSum[0]->GetBinContent(iBinFS);
    double nminus = (double) hNSum[1]->GetBinContent(iBinFS);
    double pplus  = (double) hPSum[1]->GetBinContent(iBinFS);
    double pminus = (double) hPSum[2]->GetBinContent(iBinFS);
    double psq    = (double) hPSum[0]->GetBinContent(iBinFS);
    double psig   = (double) hPSum[17]->GetBinContent(iBinFS);
    double nsig   = (double) hPSum[16]->GetBinContent(iBinFS);
    double psigsq = (double) hPSum[18]->GetBinContent(iBinFS);
    double nsum = nplus + nminus;
    double psum = pplus + pminus;

    for (int iPhi=1;iPhi<=mNPhiBins;iPhi++) {
        for (int iEta=1;iEta<=mNEtaBins;iEta++) {
            double NS[16], ND[2],  NP[5],  NM[5],  NC[8];
            double PS[21], PD[16], PP[11], PM[11], PC[17];

            double pHat[3], pHat2[3], pHatSig2, sigHatSig2;
            double sigSq0, sigSq1, sigSq2, sigHat, DsigHat;
            double Ss[] = {0, 0, 0, 0}, nSs = 0, nS1 = 0, nS3 = 0, DSs = 0, nDSs = 0, hSs = 0, hDSs = 0;
            double Sd[] = {0, 0, 0}, nSd = 0, DSd = 0, nDSd = 0;
            double Sp[] = {0, 0, 0}, nSp = 0, DSp = 0, nDSp = 0, hSp = 0, hDSp = 0;
            double Sm[] = {0, 0, 0}, nSm = 0, DSm = 0, nDSm = 0, hSm = 0, hDSm = 0;
            double Sc[] = {0, 0, 0}, nSc = 0, nDSc = 0;

            double hatSs = 0;
            double hatSp = 0;
            double hatSm = 0;

            double totEvents, sumEvents, sig2Events, plusEvents, minusEvents, plusMinusEvents;

            mBin = (int) hnBins->GetBinContent(iPhi,iEta);
            oBin = (int) hoffset->GetBinContent(iPhi,iEta);

            int iEtaBin = 0, iPhiBin = 1;
            int nEtaBin = getNumEtaBins(iEta);
            float phi1, phi2, eta1, eta2;
            double nTracksTotal = 0, nBinsTotal = 0;
            for (int iBin=oBin+1;iBin<=oBin+mBin;iBin++) {
                iEtaBin++;
                if (iEtaBin > nEtaBin) {
                    iEtaBin = 1;
                    iPhiBin++;
                }
                phi1 = 0 + 2*3.1415926*getPhiStart(iPhiBin,iPhi)/mNPhiBins;
                phi2 = phi1 + iPhi * 2*3.1415926 / mNPhiBins;
                eta1 = mEtaMin + (mEtaMax-mEtaMin)*getEtaStart(iEtaBin,iEta)/mNEtaBins;
                eta2 = eta1 + iEta * (mEtaMax-mEtaMin) / mNEtaBins;
                totEvents = hTotEvents[0]->GetBinContent(iBin);
                sumEvents = hTotEvents[1]->GetBinContent(iBin);
                sig2Events = hTotEvents[5]->GetBinContent(iBin);
                if (sumEvents > 0) {
                    for (int jStat=0;jStat<16;jStat++) {
                        NS[jStat] = hNSum[jStat]->GetBinContent(iBin);
                    }
                    for (int jStat=0;jStat<2;jStat++) {
                        ND[jStat] = hNDiff[jStat]->GetBinContent(iBin);
                    }
                    for (int jStat=0;jStat<21;jStat++) {
                        PS[jStat] = hPSum[jStat]->GetBinContent(iBin);
                    }
                    for (int jStat=0;jStat<16;jStat++) {
                        PD[jStat] = hPDiff[jStat]->GetBinContent(iBin);
                    }
                    double NSumSig2 = PS[16];
                    double NPlus  = NS[0];
                    double PPlus  = PS[1];
                    double NMinus = NS[1];
                    double PMinus = PS[2];
                    double NSum   = NPlus + NMinus;
                    double PSum   = PPlus + PMinus;
                    nTracksTotal += NSum;
                    nBinsTotal   += totEvents;
                    if (NSum > 0) {
                        // Lower case values are for largest bin at this centrality.
                        // Upper case are for the current bin.
                        if (nsum > 0) {
//                            pHat[0]  = PSum / NSum;
                            pHat[0]  = psum / nsum;
                            pHat2[0] = pHat[0]*pHat[0];
                            if (nplus > 0) {
//                                pHat[1]  = PPlus / NPlus;
                                pHat[1]  = pplus / nplus;
                            } else {
                                pHat[1] = 0;
                            }
                            pHat2[1] = pHat[1]*pHat[1];
                            if (nminus > 0) {
//                                pHat[2]  = PMinus / NMinus;
                                pHat[2]  = pminus / nminus;
                            } else {
                                pHat[2] = 0;
                            }
                            pHat2[2] = pHat[2]*pHat[2];
                            // Using all tracks for this centrality in calculating \sigma^2_{\hat{p_t}}
                            // is fine when we sum over all bins.
                            sigHat   = psq/nsum - (psum/nsum)*(psum/nsum);

//                            if (NSig > 0) {
//                                pHatSig2   = PSig / NSig;
//                            } else {
//                                pHatSig2 = 0;
//                            }
                            if (nsig > 0) {
                                pHatSig2   = psig / nsig;
                                sigHatSig2 = psigsq / nsig - (psig/nsig)*(psig/nsig);
                            } else {
                                pHatSig2 = 0;
                                sigHatSig2 = 0;
                            }
                        }


                        hatSs   += pHat[0];
                        hSs     += sigHat;
                        DsigHat  = 4*pHat2[0]*sigHat / (NSum*sumEvents);
                        hDSs    += DsigHat;
                        sigSq0   = PS[8] - 2*pHat[1]*PS[6] - 2*pHat[2]*PS[7]
                                     + pHat2[1]*NS[5] + 2*pHat[1]*pHat[2]*NS[6] + pHat2[2]*NS[7];
                        Ss[0]   += sigSq0 - sigHat;
                        sigSq1   = PS[5] - 2*pHat[1]*PS[3] - 2*pHat[2]*PS[4]
                                     + pHat2[1]*NS[2] + 2*pHat[1]*pHat[2]*NS[3] + pHat2[2]*NS[4];
                        // Create \Delta^\sigma^2 / \bar n to compare with <dpt dpt>
                        if (NSum > 1) {
                            Ss[1]   += sigSq1/NSum - sigHat;
//                            Ss[1]   += (sigSq1/NSum - sigHat) / (NSum/totEvents);
                            nS1    += 1;
                        } else {
                            nBad += 1;
                        }
// Doing some tests for Jamie to alleive his concerns over nu_dynamical calculation.
// My original comparison in 2004 was reasonable, but in comparing to to EbyE
// energy dependence paper I should have used their newer definition, which
// here I am calling 2005.
                        if (NSumSig2 > 0) {  // Here is calculation with old reference.
                            sigSq2   = PS[11] - 2*pHatSig2*PS[10] + pHatSig2*pHatSig2*PS[9];
                            Ss[2]   += (NSumSig2 -1)* (sigSq2 - sigHatSig2*NS[15]);
                        }
                        if (NSumSig2 > 1) {  // Here is calculation with 2005 reference.
                            // Create <dpt dpt> histograms with or without \bar{n} normalization.
                            // Choose global or local pHat when calculating pHatSig2 above.
                            sigSq2   = PS[11] - 2*pHatSig2*PS[10] + pHatSig2*pHatSig2*PS[9];
//                            Ss[3]   += (NSum/totEvents)*(sigSq2 - (PS[19] -2*pHatSig2*PS[20] + pHatSig2*pHatSig2*NS[15]))/sig2Events;
                            Ss[3]   += (sigSq2 - (PS[19] -2*pHatSig2*PS[20] + pHatSig2*pHatSig2*NS[15]))/sig2Events;
                            nS3 += 1;
                        }
                        nSs     += 1;
                        bTuple.type     = 6;
                        bTuple.phiScale = iPhi;
                        bTuple.etaScale = iEta;
                        bTuple.phi      = (phi1+phi2)/2;
                        bTuple.eta      = (eta1+eta2)/2;
                        bTuple.sig2     = sigSq0 - sigHat;
                        bTuple.sig2_1   = sigSq1/NSum - sigHat;
                        bTuple.sig2_2   = NSum * (sigSq2 - sigHat*NS[15]);
                        bTuple.nbar     = NSum;
                        bTuple.events   = sumEvents;
                        bTuple.f3       = 0;
                        binTuple->Fill(&bTuple.type);

                        DSs      += (PS[13] - 4*pHat[0]*PS[12] + 6*pHat2[0]*PS[8]
                                   - 4*pHat[0]*pHat2[0]*PSum + pHat2[0]*pHat2[0]*NSum) *NSum/sumEvents;
                        nDSs     += sqrt(hfUnique->GetBinContent(iPhi,iEta));

                        sigSq0    = PD[5] - 2*pHat[1]*PD[3] + 2*pHat[2]*PD[4]
                                     + pHat2[1]*NS[5] - 2*pHat[1]*pHat[2]*NS[6] + pHat2[2]*NS[7];
                        Sd[0]    += sigSq0 - sigHat;
                        sigSq1    = PD[2] - 2*pHat[1]*PD[0] + 2*pHat[2]*PD[1]
                                     + pHat2[1]*NS[2] - 2*pHat[1]*pHat[2]*NS[3] + pHat2[2]*NS[4];
                        Sd[1]    += sigSq1/NSum - sigHat;
                        sigSq2    = PD[8] - 2*pHat[1]*PD[6] + 2*pHat[2]*PD[7]
                                     + pHat2[1]*NS[8] - 2*pHat[1]*pHat[2]*NS[9] + pHat2[2]*NS[10];
                        Sd[2]    += NSum * (sigSq2 - sigHat*NS[15]);
                        nSd      += 1;
// Ss[0] += PD[2]/NSum;
// Ss[1] += 2*pHat[1]*PD[0]/NSum;
// Ss[2] += 2*pHat[2]*PD[1]/NSum;
// Sd[0] += pHat2[1]*NS[2]/NSum;
// Sd[1] += 2*pHat[1]*pHat[2]*NS[3]/NSum;
// Sd[2] += pHat2[2]*NS[4]/NSum;

                        DSd      += (PD[11] - 4*pHat[0]*PD[12] + 6*pHat2[0]*PD[13]
                                          - 4*pHat[0]*pHat2[0]*PD[14] + pHat2[0]*pHat2[0]*PD[15]) *NSum/sumEvents;
                        nDSd     += sqrt(hfUnique->GetBinContent(iPhi,iEta));
                        bTuple.type     = 7;
                        bTuple.phiScale = iPhi;
                        bTuple.etaScale = iEta;
                        bTuple.phi      = (phi1+phi2)/2;
                        bTuple.eta      = (eta1+eta2)/2;
                        bTuple.sig2     = sigSq0 - sigHat;
                        bTuple.sig2_1   = sigSq1/NSum - sigHat;
                        bTuple.sig2_2   = NSum * (sigSq2 - sigHat*NS[15]);
                        bTuple.nbar     = NSum;
                        bTuple.events   = sumEvents;
                        bTuple.f3       = 0;
                        binTuple->Fill(&bTuple.type);
                    }
                }

                plusEvents = hTotEvents[2]->GetBinContent(iBin);
                pHat[1] = 0;
                if (plusEvents > 0) {
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
                        sigSq0   = PP[5] - PP[1]*PP[1]/NP[0];
                        Sp[0]   += sigSq0 - sigHat;
                        sigSq1   = PP[4] - 2*pHat[1]*PP[2] + pHat2[1]*NP[1];
                        Sp[1]   += sigSq1/NP[0] - sigHat;
                        sigSq2   = PP[6] - 2*pHat[1]*PP[3] + pHat2[1];
                        Sp[2]   += NP[0] * (sigSq2 - sigHat*NP[4]);
                        nSp     += 1;

                        DSp     += (PP[8] - 4*pHat[1]*PP[7] + 6*pHat2[1]*PP[5]
                                          - 4*pHat[1]*pHat2[1]*PP[1] + pHat2[1]*pHat2[1]*NP[0]) *NP[0]/plusEvents;
                        nDSp    += sqrt(hfUnique->GetBinContent(iPhi,iEta));
                        bTuple.type     = 8;
                        bTuple.phiScale = iPhi;
                        bTuple.etaScale = iEta;
                        bTuple.phi      = (phi1+phi2)/2;
                        bTuple.eta      = (eta1+eta2)/2;
                        bTuple.sig2     = sigSq0 - sigHat;
                        bTuple.sig2_1   = sigSq1/NP[0] - sigHat;
                        bTuple.sig2_2   = NP[0] * (sigSq2 - sigHat*NP[4]);
                        bTuple.nbar     = NP[0];
                        bTuple.events   = plusEvents;
                        bTuple.f3       = 0;
                        binTuple->Fill(&bTuple.type);
                    }
                } else {
                    for (int jStat=0;jStat<5;jStat++) {
                        NP[jStat] = 0;
                    }
                    for (int jStat=0;jStat<11;jStat++) {
                        PP[jStat] = 0;
                    }
                }

                minusEvents = hTotEvents[3]->GetBinContent(iBin);
                pHat[2] = 0;
                if (minusEvents > 0) {
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
                        sigSq0   = PM[5] - PM[1]*PM[1]/NM[0];
                        Sm[0]   += sigSq0 - sigHat;
                        sigSq1   = PM[4] - 2*pHat[2]*PM[2] + pHat2[2]*NM[1];
                        Sm[1]   += sigSq1/NM[0] - sigHat;
                        sigSq2   = PM[6] - 2*pHat[2]*PM[3] + pHat2[2];
                        Sm[2]   += NM[0] * (sigSq2 - sigHat*NM[4]);
                        nSm     += 1;

                        DSm     += (PM[8] - 4*pHat[2]*PM[7] + 6*pHat2[2]*PM[5]
                                          - 4*pHat[2]*pHat2[2]*PM[1] + pHat2[2]*pHat2[2]*NM[0]) * NM[0] / minusEvents;
                        nDSm    += sqrt(hfUnique->GetBinContent(iPhi,iEta));
                        bTuple.type     = 9;
                        bTuple.phiScale = iPhi;
                        bTuple.etaScale = iEta;
                        bTuple.phi      = (phi1+phi2)/2;
                        bTuple.eta      = (eta1+eta2)/2;
                        bTuple.sig2     = sigSq0 - sigHat;
                        bTuple.sig2_1   = sigSq1/NM[0] - sigHat;
                        bTuple.sig2_2   = NM[0] * (sigSq2 - sigHat*NM[4]);
                        bTuple.nbar     = NM[0];
                        bTuple.events   = minusEvents;
                        bTuple.f3       = 0;
                        binTuple->Fill(&bTuple.type);
                    }
                } else {
                    for (int jStat=0;jStat<5;jStat++) {
                        NM[jStat] = 0;
                    }
                    for (int jStat=0;jStat<11;jStat++) {
                        PM[jStat] = 0;
                    }
                }

// Accumulation of hNPlusMinus required (n+ > 0) && (n- > 0)
// I think this is wrong. Instead if I require (n+ > 0) || (n- > 0)
// I can retrieve appropriate terms from NP and NM, at least for the
// phiPt case.
                plusMinusEvents = hTotEvents[1]->GetBinContent(iBin);
                if (plusMinusEvents > 0) {
                    for (int jStat=0;jStat<8;jStat++) {
                        NC[jStat] = hNPlusMinus[jStat]->GetBinContent(iBin) / plusMinusEvents;
                    }
                    for (int jStat=0;jStat<17;jStat++) {
                        PC[jStat] = hPPlusMinus[jStat]->GetBinContent(iBin) / plusMinusEvents;
                    }

                    sigSq0  = PC[16] - pHat[2]*PC[10] - pHat[1]*PC[11] + pHat[1]*pHat[2]*NC[5];
                    Sc[0]  += sigSq0;
                    sigSq1  = 0;
                    if (NP[0]*NM[0] > 0) {
                        sigSq1  = PC[14] - pHat[2]*PC[5]  - pHat[1]*PC[4]  + pHat[1]*pHat[2]*NC[2];
                        Sc[1]  += sigSq1/sqrt(NP[0]*NM[0]);
                    }
                    sigSq2  = PC[15] - pHat[2]*PC[2]  - pHat[1]*PC[3]  + pHat[1]*pHat[2];
                    Sc[2]  += sigSq2*sqrt(NP[0]*NM[0]);
                    nSc    += 1;
                    nDSc    += sqrt(hfUnique->GetBinContent(iPhi,iEta));
                    bTuple.type     = 10;
                    bTuple.phiScale = iPhi;
                    bTuple.etaScale = iEta;
                    bTuple.phi      = (phi1+phi2)/2;
                    bTuple.eta      = (eta1+eta2)/2;
                    bTuple.sig2     = sigSq0;
                    bTuple.sig2_1   = sigSq1/sqrt(NP[0]*NM[0]);
                    bTuple.sig2_2   = sigSq2*sqrt(NP[0]*NM[0]);
                    bTuple.nbar     = sqrt(NP[0]*NM[0]);
                    bTuple.events   = plusMinusEvents;
                    bTuple.f3       = 0;
                    binTuple->Fill(&bTuple.type);
                }
//                plusMinusEvents = hTotEvents[1]->GetBinContent(iBin);
//                if (plusMinusEvents > 0) {
//                    for (int jStat=0;jStat<8;jStat++) {
//                        NC[jStat] = hNPlusMinus[jStat]->GetBinContent(iBin) / plusMinusEvents;
//                    }
//                    for (int jStat=0;jStat<17;jStat++) {
//                        PC[jStat] = hPPlusMinus[jStat]->GetBinContent(iBin) / plusMinusEvents;
//                    }
//                    if (NC[0] > 0) {
//                        pHat[1] = PC[0] / NC[0];
//                    } else {
//                        pHat[1] = 0;
//                    }
//                    if (NC[1] > 0) {
//                        pHat[2] = PC[1] / NC[1];
//                    } else {
//                        pHat[2] = 0;
//                    }
//                    sigSq   = PC[16] - pHat[2]*PC[10] - pHat[1]*PC[11] + pHat[1]*pHat[2]*NC[5];
//                    Sc[0]  += sigSq;
//                    sigSq   = PC[14] - pHat[2]*PC[5]  - pHat[1]*PC[4]  + pHat[1]*pHat[2]*NC[2];
//                    Sc[1]  += sigSq/sqrt(NC[0]*NC[1]);
//                    sigSq   = PC[15] - pHat[2]*PC[2]  - pHat[1]*PC[3]  + pHat[1]*pHat[2];
//                    Sc[2]  += sigSq*sqrt(NC[0]*NC[1]);
//                    nSc    += 1;
//                    nDSc    += sqrt(hfUnique->GetBinContent(iPhi,iEta));
//                }
            }

            if (nSs > 0) {
                PSig[0]->SetBinContent(iPhi,iEta,Ss[0]/nSs);
                PSigErrors->SetBinContent(iPhi,iEta,sqrt(DSs)/nDSs);
                SPtHat->SetBinContent(iPhi,iEta,hatSs/nSs);
                sigSPtHat->SetBinContent(iPhi,iEta,hSs/nSs);
                sigSPtHatErrors->SetBinContent(iPhi,iEta,sqrt(hDSs)/nDSs);
            }
            if (nS1 > 0) {
                PSig[1]->SetBinContent(iPhi,iEta,Ss[1]/nS1);
//                PSig[1]->SetBinContent(iPhi,iEta,(Ss[1]/nS1)*1.0/(nTracksTotal/nBinsTotal));
            }
            if (nS3 > 0) {
                PSig[2]->SetBinContent(iPhi,iEta,Ss[2]/nSs);
                PSig[3]->SetBinContent(iPhi,iEta,Ss[3]/nS3);
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
    binTupleStruct bTuple;

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

    TH1D *hPSum[21];
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


    for (int jStat=0;jStat<21;jStat++) {
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
            double PS[21], PD[16], PP[11], PM[11], PC[17];

            double sigSq0, sigSq1, sigSq2, pHat[3];
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

            int iEtaBin = 0, iPhiBin = 1;
            int nEtaBin = getNumEtaBins(iEta);
            float phi1, phi2, eta1, eta2;
            for (int iBin=oBin+1;iBin<=oBin+mBin;iBin++) {
                iEtaBin++;
                if (iEtaBin > nEtaBin) {
                    iEtaBin = 1;
                    iPhiBin++;
                }
                phi1 = 0 + 2*3.1415926*getPhiStart(iPhiBin,iPhi)/mNPhiBins;
                phi2 = phi1 + iPhi * 2*3.1415926 / mNPhiBins;
                eta1 = mEtaMin + (mEtaMax-mEtaMin)*getEtaStart(iEtaBin,iEta)/mNEtaBins;
                eta2 = eta1 + iEta * (mEtaMax-mEtaMin) / mNEtaBins;
                totEvents = hTotEvents[0]->GetBinContent(iBin);
                sumEvents = hTotEvents[1]->GetBinContent(iBin);
                if (sumEvents > 0) {
                    for (int jStat=0;jStat<16;jStat++) {
                        NS[jStat] = hNSum[jStat]->GetBinContent(iBin) / sumEvents;
                    }
                    for (int jStat=0;jStat<2;jStat++) {
                        ND[jStat] = hNDiff[jStat]->GetBinContent(iBin) / sumEvents;
                    }
                    for (int jStat=0;jStat<21;jStat++) {
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
                        sigSq0  = (PS[15] - NSum*PS[14]
                                  - pHat[1]*(NS[12] - NSum*NS[11])
                                  - pHat[2]*(NS[14] - NSum*NS[13])) / sqrt(NSum);
                        Ss[0]  += sigSq0;
                        sigSq1  = (PS[3]+PS[4]
                                  - pHat[1]*(NS[2] + NS[3])
                                  - pHat[2]*(NS[3] + NS[4])) / NSum;
                        Ss[1]  += sigSq1;
                        sigSq2  = (-PS[9] - PS[10]
                                   + pHat[1]*(NS[8] + NS[9])
                                   + pHat[2]*(NS[9] + NS[10])) * NSum;
                        Ss[2]  += sigSq2;
                        nSs +=1;
                        bTuple.type     = 11;
                        bTuple.phiScale = iPhi;
                        bTuple.etaScale = iEta;
                        bTuple.phi      = (phi1+phi2)/2;
                        bTuple.eta      = (eta1+eta2)/2;
                        bTuple.sig2     = sigSq0;
                        bTuple.sig2_1   = sigSq1;
                        bTuple.sig2_2   = sigSq2;
                        bTuple.nbar     = NSum;
                        bTuple.events   = sumEvents;
                        bTuple.f3       = 0;
                        binTuple->Fill(&bTuple.type);

                        double NDiff = NS[0] - NS[1];
                        sigSq0  = PD[10] - NDiff*PD[9]
                                  - pHat[1]*(ND[0]-NDiff*NS[11])
                                  + pHat[2]*(ND[1]-NDiff*NS[13]);
                        Sd[0]  += sigSq0 / sqrt(NSum);
                        sigSq1  = PD[0] - PD[1]
                                  - pHat[1]*(NS[2] - NS[3])
                                  + pHat[2]*(NS[3] - NS[4]);
                        Sd[1]  += sigSq1 / NSum;
                        sigSq2  = PD[3] - PD[4] - NDiff*(PD[6]+PD[7])
                                  - pHat[1]*(NS[5]-NS[6] - NDiff*(NS[8]+NS[9]))
                                  + pHat[2]*(NS[6]-NS[7] - NDiff*(NS[9]+NS[10]));
                        Sd[2]  += sigSq2;
                        nSd += 1;
                        bTuple.type     = 12;
                        bTuple.phiScale = iPhi;
                        bTuple.etaScale = iEta;
                        bTuple.phi      = (phi1+phi2)/2;
                        bTuple.eta      = (eta1+eta2)/2;
                        bTuple.sig2     = sigSq0/sqrt(NSum);
                        bTuple.sig2_1   = sigSq1/NSum;
                        bTuple.sig2_2   = sigSq2;
                        bTuple.nbar     = NSum;
                        bTuple.events   = sumEvents;
                        bTuple.f3       = 0;
                        binTuple->Fill(&bTuple.type);
                    }
                }

                plusEvents = hTotEvents[2]->GetBinContent(iBin);
                pHat[1] = 0;
                if (plusEvents > 0) {
                    for (int jStat=0;jStat<5;jStat++) {
                        NP[jStat] = hNPlus[jStat]->GetBinContent(iBin) / plusEvents;
                    }
                    for (int jStat=0;jStat<11;jStat++) {
                        PP[jStat] = hPPlus[jStat]->GetBinContent(iBin) / plusEvents;
                    }
                    if (NP[0] > 0) {
                        pHat[1] = PP[1] / NP[0];
                        sigSq0 = PP[10] - NP[0]*PP[9]
                                  - pHat[1]*NP[3] + PP[1]*NP[2];
                        Sp[0]  += sigSq0 / sqrt(NP[0]);
                        sigSq1 = PP[2] - pHat[1]*NP[1];
                        Sp[1]  += sigSq1 / NP[0];
                        sigSq2 = PP[1] - NP[0]*PP[3];
                        Sp[2]  += sigSq2;
                        nSp    += 1;
                        bTuple.type     = 13;
                        bTuple.phiScale = iPhi;
                        bTuple.etaScale = iEta;
                        bTuple.phi      = (phi1+phi2)/2;
                        bTuple.eta      = (eta1+eta2)/2;
                        bTuple.sig2     = sigSq0/sqrt(NP[0]);
                        bTuple.sig2_1   = sigSq1/NP[0];
                        bTuple.sig2_2   = sigSq2;
                        bTuple.nbar     = NP[0];
                        bTuple.events   = plusEvents;
                        bTuple.f3       = 0;
                        binTuple->Fill(&bTuple.type);
                    }
                } else {
                    for (int jStat=0;jStat<5;jStat++) {
                        NP[jStat] = 0;
                    }
                    for (int jStat=0;jStat<11;jStat++) {
                        PP[jStat] = 0;
                    }
                }

                minusEvents = hTotEvents[3]->GetBinContent(iBin);
                pHat[2] = 0;
                if (minusEvents > 0) {
                    for (int jStat=0;jStat<5;jStat++) {
                        NM[jStat] = hNMinus[jStat]->GetBinContent(iBin) / minusEvents;
                    }
                    for (int jStat=0;jStat<11;jStat++) {
                        PM[jStat] = hPMinus[jStat]->GetBinContent(iBin) / minusEvents;
                    }
                    if (NM[0] > 0) {
                        pHat[2] = PM[1] / NM[0];
                        sigSq0 = PM[10] - NM[0]*PM[9]
                                  - pHat[2]*NM[3] + PM[1]*NM[2];
                        Sm[0]  += sigSq0 / sqrt(NM[0]);
                        sigSq1 = PM[2] - pHat[2]*NM[1];
                        Sm[1]  += sigSq1 / NM[0];
                        sigSq2 = PM[1] - NM[0]*PM[3];
                        Sm[2]  += sigSq2;
                        nSm    += 1;
                        bTuple.type     = 14;
                        bTuple.phiScale = iPhi;
                        bTuple.etaScale = iEta;
                        bTuple.phi      = (phi1+phi2)/2;
                        bTuple.eta      = (eta1+eta2)/2;
                        bTuple.sig2     = sigSq0/sqrt(NM[0]);
                        bTuple.sig2_1   = sigSq1/NM[0];
                        bTuple.sig2_2   = sigSq2;
                        bTuple.nbar     = NM[0];
                        bTuple.events   = minusEvents;
                        bTuple.f3       = 0;
                        binTuple->Fill(&bTuple.type);
                    }
                } else {
                    for (int jStat=0;jStat<5;jStat++) {
                        NM[jStat] = 0;
                    }
                    for (int jStat=0;jStat<11;jStat++) {
                        PM[jStat] = 0;
                    }
                }

// Accumulation of hNPlusMinus required (n+ > 0) && (n- > 0)
// I think this is wrong. Instead if I require (n+ > 0) || (n- > 0)
// I can retrieve appropriate terms from NP and NM, at least for the
// phiPt case.
                plusMinusEvents = hTotEvents[1]->GetBinContent(iBin);
                if (plusMinusEvents > 0) {
                    for (int jStat=0;jStat<8;jStat++) {
                        NC[jStat] = hNPlusMinus[jStat]->GetBinContent(iBin) / plusMinusEvents;
                    }
                    for (int jStat=0;jStat<17;jStat++) {
                        PC[jStat] = hPPlusMinus[jStat]->GetBinContent(iBin) / plusMinusEvents;
                    }
                    sigSq0 = 0;
                    if (NM[0] > 0) {
                        sigSq0 = (PC[12] - NC[1]*PC[6]
                                  - pHat[1]*NC[6] + pHat[1]*NC[1]*NC[3]) / sqrt(NM[0]);
                        Sc1[0] += sigSq0;
                    }
                    sigSq1 = 0;
                    if (NP[0]*NM[0] > 0) {
                        sigSq1 = (PC[5] - pHat[1]*NC[2]) / sqrt(NP[0]*NM[0]);
                        Sc1[1] += sigSq1;
                    }
                    sigSq2 = 0;
                    if (NM[0] > 0) {
                        sigSq2 = (PC[8] - NC[1]*PC[2]) * sqrt(NP[0]/NM[0]);
                        Sc1[2] += sigSq2;
                    }
                    nSc1 += 1;
                    bTuple.type     = 15;
                    bTuple.phiScale = iPhi;
                    bTuple.etaScale = iEta;
                    bTuple.phi      = (phi1+phi2)/2;
                    bTuple.eta      = (eta1+eta2)/2;
                    bTuple.sig2     = sigSq0;
                    bTuple.sig2_1   = sigSq1;
                    bTuple.sig2_2   = sigSq2;
                    bTuple.nbar     = sqrt(NM[0]*NP[0]);
                    bTuple.events   = plusMinusEvents;
                    bTuple.f3       = 0;
                    binTuple->Fill(&bTuple.type);

                    sigSq0 = 0;
                    if (NP[0] > 0) {
                        sigSq0 = (PC[13] - NC[0]*PC[7]
                                  - pHat[2]*NC[7] + pHat[2]*NC[0]*NC[4]) / sqrt(NP[0]);
                        Sc2[0] += sigSq0;
                    }
                    sigSq1 = 0;
                    if (NP[0]*NM[0] > 0) {
                        sigSq1 = (PC[4] - pHat[2]*NC[2]) / sqrt(NP[0]*NM[0]);
                        Sc2[1] += sigSq1;
                    }
                    sigSq2 = 0;
                    if (NP[0] > 0) {
                        sigSq2 = (PC[9] - NC[0]*PC[3]) * sqrt(NM[0]/NP[0]);
                        Sc2[2] += sigSq2;
                    }
                    nSc2 += 1;
                    bTuple.type     = 16;
                    bTuple.phiScale = iPhi;
                    bTuple.etaScale = iEta;
                    bTuple.phi      = (phi1+phi2)/2;
                    bTuple.eta      = (eta1+eta2)/2;
                    bTuple.sig2     = sigSq0;
                    bTuple.sig2_1   = sigSq1;
                    bTuple.sig2_2   = sigSq2;
                    bTuple.nbar     = sqrt(NM[0]*NP[0]);
                    bTuple.events   = plusMinusEvents;
                    bTuple.f3       = 0;
                    binTuple->Fill(&bTuple.type);
                }
//                plusMinusEvents = hTotEvents[4]->GetBinContent(iBin);
//                if (plusMinusEvents > 0) {
//                    for (int jStat=0;jStat<8;jStat++) {
//                        NC[jStat] = hNPlusMinus[jStat]->GetBinContent(iBin) / plusMinusEvents;
//                    }
//                    for (int jStat=0;jStat<17;jStat++) {
//                        PC[jStat] = hPPlusMinus[jStat]->GetBinContent(iBin) / plusMinusEvents;
//                    }
//                    if (NC[0] > 0) {
//                        pHat[1] = PC[0] / NC[0];
//                    } else {
//                        pHat[1] = 0;
//                    }
//                    if (NC[1] > 0) {
//                        pHat[2] = PC[1] / NC[1];
//                    } else {
//                        pHat[2] = 0;
//                    }
//
//                    sigSq = (PC[12] - NC[1]*PC[6]
//                              - pHat[1]*NC[6] + pHat[1]*NC[1]*NC[3]) / sqrt(NC[1]);
//                    Sc1[0] += sigSq;
//                    sigSq = (PC[5] - NC[1]*PC[0]
//                              - pHat[1]*NC[2] + PC[0]*NC[1]) / sqrt(NC[0]*NC[1]);
//                    Sc1[1] += sigSq;
//                    sigSq = (PC[8] - NC[1]*PC[2]) * sqrt(NC[0]/NC[1]);
//                    Sc1[2] += sigSq;
//                    nSc1 += 1;
//
//                    sigSq = (PC[13] - NC[0]*PC[7]
//                              - pHat[2]*NC[7] + pHat[2]*NC[0]*NC[4]) / sqrt(NC[0]);
//                    Sc2[0] += sigSq;
//                    sigSq = (PC[4] - NC[0]*PC[1]
//                              - pHat[2]*NC[2] + PC[1]*NC[0]) / sqrt(NC[0]*NC[1]);
//                    Sc2[1] += sigSq;
//                    sigSq = (PC[9] - NC[0]*PC[3]) * sqrt(NC[1]/NC[0]);
//                    Sc2[2] += sigSq;
//                    nSc2 += 1;
//                }
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
    NSigCorrection->Write();
    NDelCorrection->Write();
    NPlusCorrection->Write();
    NMinusCorrection->Write();
    NPlusMinusCorrection->Write();
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
    PSig[3]->Write();

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
    binTuple->Write();
    scaleTuple->Write();
    sumTuple->Write();
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
    sprintf( line, "NSigCorrection_%s", mKey );
    NSigCorrection = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,mEtaMax-mEtaMin);
    sprintf( line, "NDelCorrection_%s", mKey );
    NDelCorrection = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,mEtaMax-mEtaMin);
    sprintf( line, "NPlusCorrection_%s", mKey );
    NPlusCorrection = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,mEtaMax-mEtaMin);
    sprintf( line, "NMinusCorrection_%s", mKey );
    NMinusCorrection = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,mEtaMax-mEtaMin);
    sprintf( line, "NPlusMinusCorrection_%s", mKey );
    NPlusMinusCorrection = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,mEtaMax-mEtaMin);

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
    sprintf( line, "PSig%i_%s", 3, mKey );
    PSig[3] = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,mEtaMax-mEtaMin);

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

    sprintf( line, "binTuple_%s", mKey );
    binTuple = new TNtuple(line,"sigma dependence on eta",
            "type:phiScale:etaScale:phi:eta:sig2:sig2_1:sig2_2:nbar:events:f3");
    sprintf( line, "scaleTuple_%s", mKey );
    scaleTuple = new TNtuple(line,"fitting dependence on eta",
            "type:phiScale:etaScale:A:B:nBins:f3:f3sq:sig2:sig2f3");
    sprintf( line, "sumTuple_%s", mKey );
    sumTuple = new TNtuple(line,"fit parameters for eta",
            "type:B:nBins");
}

//--------------------------------------------------------------------------
void StEStructSigmas::deleteHistograms() {

printf("Deleting bin***Var2, bin***Errors histograms.\n");
    delete NSig;
    delete NDel;
    delete NPlus;
    delete NMinus;
    delete NPlusMinus;
    delete NSigCorrection;
    delete NDelCorrection;
    delete NPlusCorrection;
    delete NMinusCorrection;
    delete NPlusMinusCorrection;
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
    delete PSig[3];

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

    delete binTuple;
    delete scaleTuple;
    delete sumTuple;
}
//
// Copy these from StEStructFluctAnal.cxx.
// Probably should declare these routines static
// and leave them in that file.
//
// iEta runs from 1 up to the number of etaBins that fit in,
// according to the binning mode.
// Return starting bin (first bin is called 0.)
// When iEta is too big return -1.
int StEStructSigmas::getEtaStart( int iEta, int dEta ) {
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
int StEStructSigmas::getPhiStart( int iPhi, int dPhi ) {
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
int StEStructSigmas::getNumEtaBins( int dEta ) {
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
int StEStructSigmas::getNumPhiBins( int dPhi ) {
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
