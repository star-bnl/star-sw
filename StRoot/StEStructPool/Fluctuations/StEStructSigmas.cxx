
#include "StEStructSigmas.h"

#include "TH1F.h"
#include "TH2F.h"
#include "TFile.h"


ClassImp(StEStructSigmas)

//--------------------------------------------------------------------------
StEStructSigmas::StEStructSigmas( char *key, int nPhi, int nEta, const char *preFix ) {
    mKey = strdup(key);
    mNPhiBins = nPhi;
    mNEtaBins = nEta;
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
    TH1D *hNSum[2];
    TH1D *hNDiff[2];
    TH1D *hNPlus[2];
    TH1D *hNMinus[2];
    TH1D *hNPlusMinus[1];

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

    for (int jStat=0;jStat<2;jStat++) {
        sprintf( buffer,"%sNSum%s_%i", mpreFix, mKey, jStat );
        hNSum[jStat] = (TH1D *) gDirectory->Get(buffer);

        sprintf( buffer,"%sNDiff%s_%i", mpreFix, mKey, jStat );
        hNDiff[jStat] = (TH1D *) gDirectory->Get(buffer);

        sprintf( buffer,"%sNPlus%s_%i", mpreFix, mKey, jStat );
        hNPlus[jStat] = (TH1D *) gDirectory->Get(buffer);

        sprintf( buffer,"%sNMinus%s_%i", mpreFix, mKey, jStat );
        hNMinus[jStat] = (TH1D *) gDirectory->Get(buffer);
    }
    sprintf( buffer,"%sNPlusMinus%s_0", mpreFix, mKey );
    hNPlusMinus[0] = (TH1D *) gDirectory->Get(buffer);

    int mBin, oBin;
    for (int iPhi=1;iPhi<=mNPhiBins;iPhi++) {
        for (int iEta=1;iEta<=mNEtaBins;iEta++) {
            double NS1, NS2,  NSSig  = 0, NDSSig  = 0, SSig  = 0, DSSig  = 0, nSig;
            double ND1, ND2,  NDSig  = 0, NDDSig  = 0, DSig  = 0, DDSig  = 0, qSig;
            double NP1, NP2,  NPSig  = 0, NDPSig  = 0, PSig  = 0, DPSig  = 0, pSig;
            double NM1, NM2,  NMSig  = 0, NDMSig  = 0, MSig  = 0, DMSig  = 0, mSig;
            double      NPM2, NPMSig = 0, NDPMSig = 0, PMSig = 0, DPMSig = 0, pmSig;
            double sumEvents, plusEvents, minusEvents, plusMinusEvents;

            mBin = (int) hnBins->GetBinContent(iPhi,iEta);
            oBin = (int) hoffset->GetBinContent(iPhi,iEta);

            for (int iBin=oBin+1;iBin<=oBin+mBin;iBin++) {
                sumEvents = hTotEvents[0]->GetBinContent(iBin);
                NS1 = 0;
                if (sumEvents > 0) {
                    NS1 = hNSum[0]->GetBinContent(iBin);
                    NS2 = hNSum[1]->GetBinContent(iBin);
                    ND1 = hNDiff[0]->GetBinContent(iBin);
                    ND2 = hNDiff[1]->GetBinContent(iBin);
                    if (NS1 > 0) {
                        nSig    = (NS2 - NS1*NS1/sumEvents) / NS1;
                        SSig   += nSig - 1;
                        NSSig  += 1;
                        DSSig  += (4 + nSig/(NS1/sumEvents)) * nSig / sumEvents;
                        NDSSig += sqrt(hfUnique->GetBinContent(iPhi,iEta));

                        qSig    = (ND2 - ND1*ND1/sumEvents) / NS1;
                        DSig   += qSig - 1;
                        NDSig  += 1;
                        DDSig  += (4 + qSig/(NS1/sumEvents)) * qSig / sumEvents;
                        NDDSig += sqrt(hfUnique->GetBinContent(iPhi,iEta));
                    }
                }

                plusEvents = hTotEvents[0]->GetBinContent(iBin);
                NP1  = 0;
                NP2  = 0;
                pSig = 0;
                if (plusEvents > 0) {
                    NP1  = hNPlus[0]->GetBinContent(iBin);
                    NP2  = hNPlus[1]->GetBinContent(iBin);
                    if (NP1 > 0) {
                        pSig    = (NP2 - NP1*NP1/plusEvents) / NP1;
                        PSig   += pSig - 1;
                        NPSig  += 1;
                        DPSig  += (4+pSig/(NP1/sumEvents))*pSig/sumEvents;
                        NDPSig += sqrt(hfUnique->GetBinContent(iPhi,iEta));
                    }
                }

                minusEvents = hTotEvents[0]->GetBinContent(iBin);
                NM1  = 0;
                NM2  = 0;
                mSig = 0;
                if (minusEvents > 0) {
                    NM1  = hNMinus[0]->GetBinContent(iBin);
                    NM2  = hNMinus[1]->GetBinContent(iBin);
                    if (NM1 > 0) {
                        mSig    = (NM2 - NM1*NM1/minusEvents) / NM1;
                        MSig   += mSig - 1;
                        NMSig  += 1;
                        DMSig  += (4+mSig/(NM1/sumEvents))*mSig/sumEvents;
                        NDMSig += sqrt(hfUnique->GetBinContent(iPhi,iEta));
                    }
                }

                plusMinusEvents = hTotEvents[0]->GetBinContent(iBin);
                if (plusMinusEvents > 0) {
                    NPM2     = hNPlusMinus[0]->GetBinContent(iBin);
                    pmSig    = NPM2/sqrt(NP1*NM1) - sqrt(NP1*NM1)/plusMinusEvents;
                    PMSig   += pmSig;
                    NPMSig  += 1;
                    DPMSig  += (pSig + mSig) / plusMinusEvents +
                                (1/NP1+1/NM1)*pmSig*pmSig/4;
                    NDPMSig += sqrt(hfUnique->GetBinContent(iPhi,iEta));
                }
            }

            if (NSSig > 0) {
                NSig->SetBinContent(iPhi,iEta,SSig/NSSig);
                NSigErrors->SetBinContent(iPhi,iEta,sqrt(DSSig)/NDSSig);
            }
            if (NDSig > 0) {
                NDel->SetBinContent(iPhi,iEta,DSig/NSSig);
                NDelErrors->SetBinContent(iPhi,iEta,sqrt(DDSig)/NDSSig);
            }
            if (NPSig > 0) {
                NPlus->SetBinContent(iPhi,iEta,PSig/NPSig);
                NPlusErrors->SetBinContent(iPhi,iEta,sqrt(DPSig)/NDPSig);
            }
            if (NMSig > 0) {
                NMinus->SetBinContent(iPhi,iEta,MSig/NMSig);
                NMinusErrors->SetBinContent(iPhi,iEta,sqrt(DMSig)/NDMSig);
            }
            if (NPMSig > 0) {
                NPlusMinus->SetBinContent(iPhi,iEta,PMSig/NPMSig);
                NPlusMinusErrors->SetBinContent(iPhi,iEta,sqrt(DPMSig)/NDPMSig);
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

    PSig->Reset();
    PDel->Reset();
    PPlus->Reset();
    PMinus->Reset();
    PPlusMinus->Reset();

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
    TH1D *hNSum[2];
    TH1D *hNPlus[2];
    TH1D *hNMinus[2];
    TH1D *hPSum[5];
    TH1D *hPDiff[8];
    TH1D *hPPlus[5];
    TH1D *hPMinus[5];
    TH1D *hPPlusMinus[8];

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

    for (int jStat=0;jStat<2;jStat++) {
        sprintf(buffer,"%sNSum%s_%i", mpreFix, mKey, jStat);
        hNSum[jStat] = (TH1D *) gDirectory->Get(buffer);

        sprintf(buffer,"%sNPlus%s_%i", mpreFix, mKey, jStat);
        hNPlus[jStat] = (TH1D *) gDirectory->Get(buffer);

        sprintf(buffer,"%sNMinus%s_%i", mpreFix, mKey, jStat);
        hNMinus[jStat] = (TH1D *) gDirectory->Get(buffer);
    }

    for (int jStat=0;jStat<5;jStat++) {
        sprintf(buffer,"%sPSum%s_%i", mpreFix, mKey, jStat);
        hPSum[jStat] = (TH1D *) gDirectory->Get(buffer);

        sprintf(buffer,"%sPPlus%s_%i", mpreFix, mKey, jStat);
        hPPlus[jStat] = (TH1D *) gDirectory->Get(buffer);

        sprintf(buffer,"%sPMinus%s_%i", mpreFix, mKey, jStat);
        hPMinus[jStat] = (TH1D *) gDirectory->Get(buffer);
    }
    for (int jStat=0;jStat<8;jStat++) {
        sprintf(buffer,"%sPDiff%s_%i", mpreFix, mKey, jStat);
        hPDiff[jStat] = (TH1D *) gDirectory->Get(buffer);
    }
    for (int jStat=0;jStat<8;jStat++) {
        sprintf(buffer,"%sPPlusMinus%s_%i", mpreFix, mKey, jStat);
        hPPlusMinus[jStat] = (TH1D *) gDirectory->Get(buffer);
    }

    int mBin, oBin;

    for (int iPhi=1;iPhi<=mNPhiBins;iPhi++) {
        for (int iEta=1;iEta<=mNEtaBins;iEta++) {
            double NS0, NS1, PS0, PS1, PS2, PS3, PS4, DSsig = 0, NDSsig = 0;
            double ssig, Ssig = 0, NSsig = 0, ptSHat = 0, sigptSHat = 0;
            double PD0, PD1, PD2, PD3, PD4, PD5, PD6, PD7;
            double dsig, Dsig = 0, NDsig = 0, DDsig = 0, NDDsig = 0;
            double NP0, NP1, PP0, PP1, PP2, PP3, PP4, DPsig = 0, NDPsig = 0;
            double psig, Psig = 0, NPsig = 0, ptPHat = 0, sigptPHat = 0;
            double NM0, NM1, PM0, PM1, PM2, PM3, PM4, DMsig = 0, NDMsig = 0;
            double msig, Msig = 0, NMsig = 0, ptMHat = 0, sigptMHat = 0;
            double PPM1, PPM2, PPM3,  PPM4, PMsig = 0, NPMsig = 0, NDPMsig = 0;
            double sigPtHat, DsigptHat, DsigptSHat = 0, DsigptPHat = 0, DsigptMHat = 0;
            double totEvents, sumEvents, plusEvents, minusEvents, plusMinusEvents;

            mBin = (int) hnBins->GetBinContent(iPhi,iEta);
            oBin = (int) hoffset->GetBinContent(iPhi,iEta);


            for (int iBin=oBin+1;iBin<=oBin+mBin;iBin++) {
                totEvents = hTotEvents[0]->GetBinContent(iBin);
                sumEvents = hTotEvents[1]->GetBinContent(iBin);
                if (sumEvents > 0) {
                    NS0 = hNSum[0]->GetBinContent(iBin) / sumEvents;
                    NS1 = hNSum[1]->GetBinContent(iBin) / sumEvents;
                    PS0 = hPSum[0]->GetBinContent(iBin) / sumEvents;
                    PS1 = hPSum[1]->GetBinContent(iBin) / sumEvents;
                    PS2 = hPSum[2]->GetBinContent(iBin) / sumEvents;
                    PS3 = hPSum[3]->GetBinContent(iBin) / sumEvents;
                    PS4 = hPSum[4]->GetBinContent(iBin) / sumEvents;

                    PD0 = hPDiff[0]->GetBinContent(iBin) / sumEvents;
                    PD1 = hPDiff[1]->GetBinContent(iBin) / sumEvents;
                    PD2 = hPDiff[2]->GetBinContent(iBin) / sumEvents;
                    PD3 = hPDiff[3]->GetBinContent(iBin) / sumEvents;
                    PD4 = hPDiff[4]->GetBinContent(iBin) / sumEvents;
                    PD5 = hPDiff[5]->GetBinContent(iBin) / sumEvents;
                    PD6 = hPDiff[6]->GetBinContent(iBin) / sumEvents;
                    PD7 = hPDiff[7]->GetBinContent(iBin) / sumEvents;
                    if (NS0 > 0) {
                        double pHat  = PS0 / NS0;
                        double pHat2 = pHat*pHat;
                        ptSHat    += pHat;
                        sigPtHat   = PS4/NS0 - pHat*pHat;
                        sigptSHat += sigPtHat;
                        DsigptHat  = 4*pHat2*sigPtHat / (NS0*sumEvents);
                        DsigptSHat += DsigptHat;
                        ssig       = PS1 - pHat*PS0;
                        Ssig      += ssig - sigPtHat;
                        NSsig     += 1;

                        DSsig     += (PS3 - 4*pHat*PS2       + 6*pHat2*PS1
                                          - 4*pHat*pHat2*PS0 + pHat2*pHat2*NS0) *NS0/sumEvents;
                        NDSsig    += sqrt(hfUnique->GetBinContent(iPhi,iEta));

                        dsig       = PD0 - 2*pHat*PD1 + pHat2*PD2;
                        Dsig      += dsig - sigPtHat;
                        NDsig     += 1;

                        DDsig     += (PD3 - 4*pHat*PD4       + 6*pHat2*PD5
                                          - 4*pHat*pHat2*PD6 + pHat2*pHat2*PD7) *NS0/sumEvents;
                        NDDsig    += sqrt(hfUnique->GetBinContent(iPhi,iEta));

                    }
                }

                plusEvents = hTotEvents[2]->GetBinContent(iBin);
                if (plusEvents > 0) {
                    NP0 = hNPlus[0]->GetBinContent(iBin) / plusEvents;
                    NP1 = hNPlus[1]->GetBinContent(iBin) / plusEvents;
                    PP0 = hPPlus[0]->GetBinContent(iBin) / plusEvents;
                    PP1 = hPPlus[1]->GetBinContent(iBin) / plusEvents;
                    PP2 = hPPlus[2]->GetBinContent(iBin) / plusEvents;
                    PP3 = hPPlus[3]->GetBinContent(iBin) / plusEvents;
                    PP4 = hPPlus[4]->GetBinContent(iBin) / plusEvents;
                    if (NP0 > 0) {
                        double pHat  = PP0 / NP0;
                        double pHat2  = pHat*pHat;
                        ptPHat    += pHat;
                        sigPtHat   = PP4/NP0 - pHat*pHat;
                        sigptPHat += sigPtHat;
                        DsigptHat  = 4*pHat2*sigPtHat / (NP0*plusEvents);
                        DsigptPHat += DsigptHat;
                        psig       = PP1 - pHat*PP0;
                        Psig      += psig - sigPtHat;
                        NPsig     += 1;

                        DPsig     += (PP3 - 4*pHat*PP2       + 6*pHat2*PP1
                                          - 4*pHat*pHat2*PP0 + pHat2*pHat2*NP0) *NP0/plusEvents;
                        NDPsig    += sqrt(hfUnique->GetBinContent(iPhi,iEta));
                    }
                }

                minusEvents = hTotEvents[3]->GetBinContent(iBin);
                if (minusEvents > 0) {
                    NM0 = hNMinus[0]->GetBinContent(iBin) / minusEvents;
                    NM1 = hNMinus[1]->GetBinContent(iBin) / minusEvents;
                    PM0 = hPMinus[0]->GetBinContent(iBin) / minusEvents;
                    PM1 = hPMinus[1]->GetBinContent(iBin) / minusEvents;
                    PM2 = hPMinus[2]->GetBinContent(iBin) / minusEvents;
                    PM3 = hPMinus[3]->GetBinContent(iBin) / minusEvents;
                    PM4 = hPMinus[4]->GetBinContent(iBin) / minusEvents;
                    if (NM0 > 0) {
                        double pHat  = PM0 / NM0;
                        double pHat2  = pHat*pHat;
                        ptMHat    += pHat;
                        sigPtHat   = PM4/NM0 - pHat*pHat;
                        sigptMHat += sigPtHat;
                        DsigptHat  = 4*pHat2*sigPtHat / (NM0*minusEvents);
                        DsigptMHat += DsigptHat;
                        msig       = PM1 - pHat*PM0;
                        Msig      += msig - sigPtHat;
                        NMsig     += 1;

                        DMsig     += (PM3 - 4*pHat*PM2       + 6*pHat2*PM1
                                          - 4*pHat*pHat2*PM0 + pHat2*pHat2*NM0) * NM0 / minusEvents;
                        NDMsig    += sqrt(hfUnique->GetBinContent(iPhi,iEta));
                    }
                }

                plusMinusEvents = hTotEvents[4]->GetBinContent(iBin);
                if (plusMinusEvents > 0) {
                    NP1  = hPPlusMinus[4]->GetBinContent(iBin);
                    PP1  = hPPlusMinus[5]->GetBinContent(iBin);
                    double pHat = PP1 / NP1;
                    NM1  = hPPlusMinus[6]->GetBinContent(iBin);
                    PM1  = hPPlusMinus[7]->GetBinContent(iBin);
                    double mHat = PM1 / NM1;
                    PPM1 = pHat*mHat*hPPlusMinus[0]->GetBinContent(iBin);
                    PPM2 = mHat*hPPlusMinus[1]->GetBinContent(iBin);
                    PPM3 = pHat*hPPlusMinus[2]->GetBinContent(iBin);
                    PPM4 = hPPlusMinus[3]->GetBinContent(iBin);
                    PMsig  += (PPM1 - PPM2 - PPM3 + PPM4) / plusMinusEvents;
                    NPMsig += 1;
                    NDPMsig    += sqrt(hfUnique->GetBinContent(iPhi,iEta));
                }
            }

            if (NSsig > 0) {
                PSig->SetBinContent(iPhi,iEta,Ssig/NSsig);
                PSigErrors->SetBinContent(iPhi,iEta,sqrt(DSsig)/NDSsig);
                SPtHat->SetBinContent(iPhi,iEta,ptSHat/NSsig);
                sigSPtHat->SetBinContent(iPhi,iEta,sigptSHat/NSsig);
                sigSPtHatErrors->SetBinContent(iPhi,iEta,sqrt(DsigptSHat)/NDSsig);
            }
            if (NDsig > 0) {
                PDel->SetBinContent(iPhi,iEta,Dsig/NDsig);
                PDelErrors->SetBinContent(iPhi,iEta,sqrt(DDsig)/NDDsig);
            }

            if (NPsig > 0) {
                PPlus->SetBinContent(iPhi,iEta,Psig/NPsig);
                PPlusErrors->SetBinContent(iPhi,iEta,sqrt(DPsig)/NDPsig);
                PPtHat->SetBinContent(iPhi,iEta,ptPHat/NPsig);
                sigPPtHat->SetBinContent(iPhi,iEta,sigptPHat/NPsig);
                sigPPtHatErrors->SetBinContent(iPhi,iEta,sqrt(DsigptPHat)/NDPsig);
            }

            if (NMsig > 0) {
                PMinus->SetBinContent(iPhi,iEta,Msig/NMsig);
                PMinusErrors->SetBinContent(iPhi,iEta,sqrt(DMsig)/NDMsig);
                MPtHat->SetBinContent(iPhi,iEta,ptMHat/NMsig);
                sigMPtHat->SetBinContent(iPhi,iEta,sigptMHat/NMsig);
                sigMPtHatErrors->SetBinContent(iPhi,iEta,sqrt(DsigptMHat)/NDMsig);
            }

            if (NPMsig > 0) {
                PPlusMinus->SetBinContent(iPhi,iEta,PMsig/NPMsig);
// Haven't found good approximation for error on +- covariance.
// Use average of errors on + and - to have something.
                PPlusMinusErrors->SetBinContent(iPhi,iEta,sqrt((DPsig+DMsig)/2)/NDSsig);
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

    PNSig->Reset();
    PNDel->Reset();
    PNPlus->Reset();
    PNMinus->Reset();
    PNPlusMinus->Reset();
    PNMinusPlus->Reset();

    PNSigErrors->Reset();
    PNDelErrors->Reset();
    PNPlusErrors->Reset();
    PNMinusErrors->Reset();
    PNPlusMinusErrors->Reset();
    PNMinusPlusErrors->Reset();

    TH1D *hTotEvents[5];
    TH1D *hNSum[2];
    TH1D *hPSum;
    TH1D *hPNSum[4];
    TH1D *hNDiff;
    TH1D *hPNDiff[4];
    TH1D *hNPlus[2];
    TH1D *hPPlus;
    TH1D *hPNPlus[4];
    TH1D *hNMinus[2];
    TH1D *hPMinus;
    TH1D *hPPlusMinus[8];
    TH1D *hPNMinus[4];
    TH1D *hPNPlusMinus[8];

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

    for (int jStat=0;jStat<2;jStat++) {
        sprintf(buffer,"%sNSum%s_%i", mpreFix, mKey, jStat);
        hNSum[jStat] = (TH1D *) gDirectory->Get(buffer);

        sprintf(buffer,"%sNPlus%s_%i", mpreFix, mKey, jStat);
        hNPlus[jStat] = (TH1D *) gDirectory->Get(buffer);

        sprintf(buffer,"%sNMinus%s_%i", mpreFix, mKey, jStat);
        hNMinus[jStat] = (TH1D *) gDirectory->Get(buffer);
    }
    sprintf(buffer,"%sNDiff%s_0", mpreFix, mKey);
    hNDiff = (TH1D *) gDirectory->Get(buffer);

    sprintf(buffer,"%sPSum%s_0", mpreFix, mKey);
    hPSum = (TH1D *) gDirectory->Get(buffer);

    sprintf(buffer,"%sPPlus%s_0", mpreFix, mKey);
    hPPlus = (TH1D *) gDirectory->Get(buffer);

    sprintf(buffer,"%sPMinus%s_0", mpreFix, mKey);
    hPMinus = (TH1D *) gDirectory->Get(buffer);

    for (int jStat=0;jStat<8;jStat++) {
        sprintf(buffer,"%sPPlusMinus%s_%i", mpreFix, mKey, jStat);
        hPPlusMinus[jStat] = (TH1D *) gDirectory->Get(buffer);
    }
    for (int jStat=0;jStat<4;jStat++) {
        sprintf(buffer,"%sPNSum%s_%i", mpreFix, mKey, jStat);
        hPNSum[jStat] = (TH1D *) gDirectory->Get(buffer);

        sprintf(buffer,"%sPNDiff%s_%i", mpreFix, mKey, jStat);
        hPNDiff[jStat] = (TH1D *) gDirectory->Get(buffer);

        sprintf(buffer,"%sPNPlus%s_%i", mpreFix, mKey, jStat);
        hPNPlus[jStat] = (TH1D *) gDirectory->Get(buffer);

        sprintf(buffer,"%sPNMinus%s_%i", mpreFix, mKey, jStat);
        hPNMinus[jStat] = (TH1D *) gDirectory->Get(buffer);
    }
    for (int jStat=0;jStat<8;jStat++) {
        sprintf(buffer,"%sPNPlusMinus%s_%i", mpreFix, mKey, jStat);
        hPNPlusMinus[jStat] = (TH1D *) gDirectory->Get(buffer);
    }

    int mBin, oBin;

    for (int iPhi=1;iPhi<=mNPhiBins;iPhi++) {
        for (int iEta=1;iEta<=mNEtaBins;iEta++) {
            double sumEvents, plusEvents, minusEvents, plusMinusEvents;
            double NS1, PS1, PNS1, PNS2, PNS3, PNS4, Ssig = 0, NSsig = 0;
            double ND1,      PND1, PND2, PND3, PND4, Dsig = 0, NDsig = 0;;
            double NP1, PP1, PNP1, PNP2, PNP3, PNP4, Psig = 0, NPsig = 0;
            double NM1, PM1, PNM1, PNM2, PNM3, PNM4, Msig = 0, NMsig = 0;
            double PNPM1, PNPM2, PNPM3, PNPM4, PMsig = 0, NPMsig = 0;
            double PNMP1, PNMP2, PNMP3, PNMP4, MPsig = 0, NMPsig = 0;
            double nErr, pErr;

            mBin = (int) hnBins->GetBinContent(iPhi,iEta);
            oBin = (int) hoffset->GetBinContent(iPhi,iEta);

            for (int iBin=oBin+1;iBin<=oBin+mBin;iBin++) {
                sumEvents = hTotEvents[1]->GetBinContent(iBin);
                if (sumEvents > 0) {
                    NS1  = hNSum[0]->GetBinContent(iBin) / sumEvents;
                    PS1  = hPSum->GetBinContent(iBin) / sumEvents;
                    PNS1 = hPNSum[0]->GetBinContent(iBin) / sumEvents;
                    PNS2 = hPNSum[1]->GetBinContent(iBin) / sumEvents;
                    PNS3 = hPNSum[2]->GetBinContent(iBin) / sumEvents;
                    PNS4 = hPNSum[3]->GetBinContent(iBin) / sumEvents;
                    ND1  = hNDiff->GetBinContent(iBin) / sumEvents;
                    PND1 = hPNDiff[0]->GetBinContent(iBin) / sumEvents;
                    PND2 = hPNDiff[1]->GetBinContent(iBin) / sumEvents;
                    PND3 = hPNDiff[2]->GetBinContent(iBin) / sumEvents;
                    PND4 = hPNDiff[3]->GetBinContent(iBin) / sumEvents;
                    if (NS1 > 0) {
                        double R1 = PS1 / NS1;
                        Ssig  += (PNS1*R1*NS1 - PNS2*R1 - PNS3*NS1 + PNS4) / sqrt(NS1);
                        NSsig +=1;

                        Dsig  += (PND1*R1*ND1 - PND2*R1 - PND3*ND1 + PND4) / sqrt(NS1);
                        NDsig += 1;
                    }
                }

                plusEvents = hTotEvents[2]->GetBinContent(iBin);
                if (plusEvents > 0) {
                    NP1 = hNPlus[0]->GetBinContent(iBin) / plusEvents;
                    PP1 = hPPlus->GetBinContent(iBin) / plusEvents;
                    PNP1 = hPNPlus[0]->GetBinContent(iBin) / plusEvents;
                    PNP2 = hPNPlus[1]->GetBinContent(iBin) / plusEvents;
                    PNP3 = hPNPlus[2]->GetBinContent(iBin) / plusEvents;
                    PNP4 = hPNPlus[3]->GetBinContent(iBin) / plusEvents;
                    if (NP1 > 0) {
                        double R1 = PP1 / NP1;
                        Psig  += (PNP1*R1*NP1 - PNP2*R1 - PNP3*NP1 + PNP4) / sqrt(NP1);
                        NPsig += 1;
                    }
                }

                minusEvents = hTotEvents[3]->GetBinContent(iBin);
                if (minusEvents > 0) {
                    NM1 = hNMinus[0]->GetBinContent(iBin) / minusEvents;
                    PM1 = hPMinus->GetBinContent(iBin) / minusEvents;
                    PNM1 = hPNMinus[0]->GetBinContent(iBin) / minusEvents;
                    PNM2 = hPNMinus[1]->GetBinContent(iBin) / minusEvents;
                    PNM3 = hPNMinus[2]->GetBinContent(iBin) / minusEvents;
                    PNM4 = hPNMinus[3]->GetBinContent(iBin) / minusEvents;
                    if (NM1 > 0) {
                        double R1 = PM1 / NM1;
                        Msig  += (PNM1*R1*NM1 - PNM2*R1 - PNM3*NM1 + PNM4) / sqrt(NM1);
                        NMsig += 1;
                    }
                }

                plusMinusEvents = hTotEvents[4]->GetBinContent(iBin);
                if (plusMinusEvents > 0) {
                    NP1 = hPPlusMinus[4]->GetBinContent(iBin) / plusMinusEvents;
                    PP1 = hPPlusMinus[5]->GetBinContent(iBin) / plusMinusEvents;
                    NM1 = hPPlusMinus[6]->GetBinContent(iBin) / plusMinusEvents;
                    PM1 = hPPlusMinus[7]->GetBinContent(iBin) / plusMinusEvents;

                    PNPM1 = hPNPlusMinus[0]->GetBinContent(iBin) / plusMinusEvents;
                    PNPM2 = hPNPlusMinus[1]->GetBinContent(iBin) / plusMinusEvents;
                    PNPM3 = hPNPlusMinus[2]->GetBinContent(iBin) / plusMinusEvents;
                    PNPM4 = hPNPlusMinus[3]->GetBinContent(iBin) / plusMinusEvents;
                    if (NM1 > 0) {
                        double R1 = PP1 / NP1;
                        PMsig  += (PNPM1*R1*NM1 - PNPM2*R1 - PNPM3*NM1 + PNPM4) / sqrt(NM1);
                        NPMsig += 1;
                    }

                    PNMP1 = hPNPlusMinus[4]->GetBinContent(iBin) / plusMinusEvents;
                    PNMP2 = hPNPlusMinus[5]->GetBinContent(iBin) / plusMinusEvents;
                    PNMP3 = hPNPlusMinus[6]->GetBinContent(iBin) / plusMinusEvents;
                    PNMP4 = hPNPlusMinus[7]->GetBinContent(iBin) / plusMinusEvents;
                    if (NP1 > 0) {
                        double R1 = PM1 / NM1;
                        MPsig  += (PNMP1*R1*NP1 - PNMP2*R1 - PNMP3*NP1 + PNMP4) / sqrt(NP1);
                        NMPsig += 1;
                    }
                }
            }

            if (NSsig > 0) {
                PNSig->SetBinContent(iPhi,iEta,Ssig/NSsig);
            }
            if (NDsig > 0) {
                PNDel->SetBinContent(iPhi,iEta,Dsig/NDsig);
            }
            if (NPsig > 0) {
                PNPlus->SetBinContent(iPhi,iEta,Psig/NPsig);
            }
            if (NMsig > 0) {
                PNMinus->SetBinContent(iPhi,iEta,Msig/NMsig);
            }
            if (NPMsig) {
                PNPlusMinus->SetBinContent(iPhi,iEta,PMsig/NPMsig);
            }
            if (NMPsig) {
                PNMinusPlus->SetBinContent(iPhi,iEta,MPsig/NMPsig);
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
    PSig->Write();
    PDel->Write();
    PPlus->Write();
    PMinus->Write();
    PPlusMinus->Write();
    PNSig->Write();
    PNDel->Write();
    PNPlus->Write();
    PNMinus->Write();
    PNPlusMinus->Write();
    PNMinusPlus->Write();

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
    NSig = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,SETAMAX-SETAMIN);
    sprintf( line, "NDel_%s", mKey );
    NDel = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,SETAMAX-SETAMIN);
    sprintf( line, "NPlus_%s", mKey );
    NPlus = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,SETAMAX-SETAMIN);
    sprintf( line, "NMinus_%s", mKey );
    NMinus = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,SETAMAX-SETAMIN);
    sprintf( line, "NPlusMinus_%s", mKey );
    NPlusMinus = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,SETAMAX-SETAMIN);
    sprintf( line, "PSig_%s", mKey );
    PSig = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,SETAMAX-SETAMIN);
    sprintf( line, "PDel_%s", mKey );
    PDel = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,SETAMAX-SETAMIN);
    sprintf( line, "PPlus_%s", mKey );
    PPlus = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,SETAMAX-SETAMIN);
    sprintf( line, "PMinus_%s", mKey );
    PMinus = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,SETAMAX-SETAMIN);
    sprintf( line, "PPlusMinus_%s", mKey );
    PPlusMinus = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,SETAMAX-SETAMIN);
    sprintf( line, "PNSig_%s", mKey );
    PNSig = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,SETAMAX-SETAMIN);
    sprintf( line, "PNDel_%s", mKey );
    PNDel = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,SETAMAX-SETAMIN);
    sprintf( line, "PNPlus_%s", mKey );
    PNPlus = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,SETAMAX-SETAMIN);
    sprintf( line, "PNMinus_%s", mKey );
    PNMinus = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,SETAMAX-SETAMIN);
    sprintf( line, "PNPlusMinus_%s", mKey );
    PNPlusMinus = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,SETAMAX-SETAMIN);
    sprintf( line, "PNMinusPlus_%s", mKey );
    PNMinusPlus = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,SETAMAX-SETAMIN);

    sprintf( line, "SptHat_%s", mKey );
    SPtHat    = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,SETAMAX-SETAMIN);
    sprintf( line, "PptHat_%s", mKey );
    PPtHat    = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,SETAMAX-SETAMIN);
    sprintf( line, "MptHat_%s", mKey );
    MPtHat    = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,SETAMAX-SETAMIN);
    sprintf( line, "SsigPtHat_%s", mKey );
    sigSPtHat = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,SETAMAX-SETAMIN);
    sprintf( line, "PsigPtHat_%s", mKey );
    sigPPtHat = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,SETAMAX-SETAMIN);
    sprintf( line, "MsigPtHat_%s", mKey );
    sigMPtHat = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,SETAMAX-SETAMIN);

    sprintf( line, "NSigErrors_%s", mKey );
    NSigErrors = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,SETAMAX-SETAMIN);
    sprintf( line, "NDelErrors_%s", mKey );
    NDelErrors = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,SETAMAX-SETAMIN);
    sprintf( line, "NPlusErrors_%s", mKey );
    NPlusErrors = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,SETAMAX-SETAMIN);
    sprintf( line, "NMinusErrors_%s", mKey );
    NMinusErrors = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,SETAMAX-SETAMIN);
    sprintf( line, "NPlusMinusErrors_%s", mKey );
    NPlusMinusErrors = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,SETAMAX-SETAMIN);
    sprintf( line, "PSigErrors_%s", mKey );
    PSigErrors = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,SETAMAX-SETAMIN);
    sprintf( line, "PDelErrors_%s", mKey );
    PDelErrors = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,SETAMAX-SETAMIN);
    sprintf( line, "PPlusErrors_%s", mKey );
    PPlusErrors = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,SETAMAX-SETAMIN);
    sprintf( line, "PMinusErrors_%s", mKey );
    PMinusErrors = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,SETAMAX-SETAMIN);
    sprintf( line, "PPlusMinusErrors_%s", mKey );
    PPlusMinusErrors = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,SETAMAX-SETAMIN);
    sprintf( line, "PNSigErrors_%s", mKey );
    PNSigErrors = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,SETAMAX-SETAMIN);
    sprintf( line, "PNDelErrors_%s", mKey );
    PNDelErrors = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,SETAMAX-SETAMIN);
    sprintf( line, "PNPlusErrors_%s", mKey );
    PNPlusErrors = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,SETAMAX-SETAMIN);
    sprintf( line, "PNMinusErrors_%s", mKey );
    PNMinusErrors = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,SETAMAX-SETAMIN);
    sprintf( line, "PNPlusMinusErrors_%s", mKey );
    PNPlusMinusErrors = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,SETAMAX-SETAMIN);
    sprintf( line, "PNMinusPlusErrors_%s", mKey );
    PNMinusPlusErrors = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,SETAMAX-SETAMIN);

    sprintf( line, "SsigPtHatErrors_%s", mKey );
    sigSPtHatErrors = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,SETAMAX-SETAMIN);
    sprintf( line, "PsigPtHatErrors_%s", mKey );
    sigPPtHatErrors = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,SETAMAX-SETAMIN);
    sprintf( line, "MsigPtHatErrors_%s", mKey );
    sigMPtHatErrors = new TH2D(line,line,mNPhiBins,0.0,6.2831852,mNEtaBins,0.0,SETAMAX-SETAMIN);
}

//--------------------------------------------------------------------------
void StEStructSigmas::deleteHistograms() {

cout << "Deleting bin***Var2, bin***Errors histograms." << endl;
    delete NSig;
    delete NDel;
    delete NPlus;
    delete NMinus;
    delete NPlusMinus;
    delete PSig;
    delete PDel;
    delete PPlus;
    delete PMinus;
    delete PPlusMinus;
    delete PNSig;
    delete PNDel;
    delete PNPlus;
    delete PNMinus;
    delete PNPlusMinus;
    delete PNMinusPlus;

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
