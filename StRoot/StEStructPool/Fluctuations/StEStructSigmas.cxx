
#include "StEStructSigmas.h"

#include "TH1F.h"
#include "TH2F.h"
#include "TFile.h"


ClassImp(StEStructSigmas)

//--------------------------------------------------------------------------
StEStructSigmas::StEStructSigmas() {
    histosFilled = 0;
    initArraysAndHistograms();
}
//--------------------------------------------------------------------------
StEStructSigmas::~StEStructSigmas() {
    deleteArraysAndHistograms();
}

void StEStructSigmas::fillHistograms() {
    // I call this while saving QA histograms.
    // In case I forget to save QA histograms I need to fill before saving data.
    if (histosFilled) {
        return;
    }
    histosFilled = 1;
cout << "About to fill histograms." << endl;
cout << "Found NPhiBins = " << NPhiBins << " and NEtaBins = " << NEtaBins << endl;
    NHistograms();
    PHistograms();
    PNHistograms();
    ptNHistograms();
    ptPHistograms();
    ptPNHistograms();
}


//--------------------------------------------------------------------------
void StEStructSigmas::NHistograms() {
    char buffer[255];

    // We have accumulated over events.
    // For every bin at each scale we have summed up all the small bins.
    // Now accumulate sum, n, n^2 for all bins.

cout << "About to reset N histograms." << endl;
    for (int jCent=0;jCent<NSCENTBINS;jCent++) {
        NSig[jCent]->Reset();
        NDel[jCent]->Reset();
        NPlus[jCent]->Reset();
        NMinus[jCent]->Reset();
        NPlusMinus[jCent]->Reset();

        NSigErrors[jCent]->Reset();
        NDelErrors[jCent]->Reset();
        NPlusErrors[jCent]->Reset();
        NMinusErrors[jCent]->Reset();
        NPlusMinusErrors[jCent]->Reset();
    }

    // If we have summed histogram files the number of bins, offsets and
    // fraction of unique tracks ended up getting multiplied by the number
    // of files. Assume largest scale has only one bin per event, so its
    // content is equal to the number of files.
    // If we have not summed histogram files, or if we have already rescaled,
    // nFiles will be equal to 1.
    // Rescale here.
    TH2F *hnBins   = (TH2F *) gDirectory->Get("nBins");
    float nFiles = hnBins->GetBinContent(NPhiBins,NEtaBins);
    hnBins->Scale( 1.0/nFiles );
    TH2F *hoffset = (TH2F *) gDirectory->Get("offset");
    hoffset->Scale( 1.0/nFiles );
    TH2F *hfUnique = (TH2F *) gDirectory->Get("fUnique");
    hfUnique->Scale( 1.0/nFiles );


    TH1D *hTotEvents[5];
    TH1D *hNSum[2];
    TH1D *hNDiff[2];
    TH1D *hNPlus[2];
    TH1D *hNMinus[2];
    TH1D *hNPlusMinus[1];

    for (int jCent=0;jCent<NSCENTBINS;jCent++) {
        sprintf(buffer,"TotalEvents_%i",jCent);
        hTotEvents[0] = (TH1D *) gDirectory->Get(buffer);
        sprintf(buffer,"TotalSumEvents_%i",jCent);
        hTotEvents[1] = (TH1D *) gDirectory->Get(buffer);
        sprintf(buffer,"TotalPlusEvents_%i",jCent);
        hTotEvents[2] = (TH1D *) gDirectory->Get(buffer);
        sprintf(buffer,"TotalMinusEvents_%i",jCent);
        hTotEvents[3] = (TH1D *) gDirectory->Get(buffer);
        sprintf(buffer,"TotalPlusMinusEvents_%i",jCent);
        hTotEvents[4] = (TH1D *) gDirectory->Get(buffer);

        for (int jStat=0;jStat<2;jStat++) {
            sprintf(buffer,"NSum%i_%i",jCent,jStat);
            hNSum[jStat] = (TH1D *) gDirectory->Get(buffer);

            sprintf(buffer,"NDiff%i_%i",jCent,jStat);
            hNDiff[jStat] = (TH1D *) gDirectory->Get(buffer);

            sprintf(buffer,"NPlus%i_%i",jCent,jStat);
            hNPlus[jStat] = (TH1D *) gDirectory->Get(buffer);

            sprintf(buffer,"NMinus%i_%i",jCent,jStat);
            hNMinus[jStat] = (TH1D *) gDirectory->Get(buffer);
        }
        sprintf(buffer,"NPlusMinus%i_0",jCent);
        hNPlusMinus[0] = (TH1D *) gDirectory->Get(buffer);

        int mBin, oBin;
        for (int iPhi=1;iPhi<=NPhiBins;iPhi++) {
            for (int iEta=1;iEta<=NEtaBins;iEta++) {
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
                    NSig[jCent]->SetBinContent(iPhi,iEta,SSig/NSSig);
                    NSigErrors[jCent]->SetBinContent(iPhi,iEta,sqrt(DSSig)/NDSSig);
                }
                if (NDSig > 0) {
                    NDel[jCent]->SetBinContent(iPhi,iEta,DSig/NSSig);
                    NDelErrors[jCent]->SetBinContent(iPhi,iEta,sqrt(DDSig)/NDSSig);
                }
                if (NPSig > 0) {
                    NPlus[jCent]->SetBinContent(iPhi,iEta,PSig/NPSig);
                    NPlusErrors[jCent]->SetBinContent(iPhi,iEta,sqrt(DPSig)/NDPSig);
                }
                if (NMSig > 0) {
                    NMinus[jCent]->SetBinContent(iPhi,iEta,MSig/NMSig);
                    NMinusErrors[jCent]->SetBinContent(iPhi,iEta,sqrt(DMSig)/NDMSig);
                }
                if (NPMSig > 0) {
                    NPlusMinus[jCent]->SetBinContent(iPhi,iEta,PMSig/NPMSig);
                    NPlusMinusErrors[jCent]->SetBinContent(iPhi,iEta,sqrt(DPMSig)/NDPMSig);
                }
            }
        }
    }
}
void StEStructSigmas::PHistograms() {
    char buffer[255];

    // We have accumulated over events.
    // For every bin at each scale we have summed up all the small bins.
    // Now accumulate sum, n, n^2 for all bins.

    for (int jCent=0;jCent<NSCENTBINS;jCent++) {
        PSig[jCent]->Reset();
        PDel[jCent]->Reset();
        PPlus[jCent]->Reset();
        PMinus[jCent]->Reset();
        PPlusMinus[jCent]->Reset();

        PSigErrors[jCent]->Reset();
        PDelErrors[jCent]->Reset();
        PPlusErrors[jCent]->Reset();
        PMinusErrors[jCent]->Reset();
        PPlusMinusErrors[jCent]->Reset();

        SPtHat[jCent]->Reset();
        PPtHat[jCent]->Reset();
        MPtHat[jCent]->Reset();
        sigSPtHat[jCent]->Reset();
        sigPPtHat[jCent]->Reset();
        sigMPtHat[jCent]->Reset();

        sigSPtHatErrors[jCent]->Reset();
        sigPPtHatErrors[jCent]->Reset();
        sigMPtHatErrors[jCent]->Reset();
    }

    // If we have summed histogram files the number of bins, offsets and
    // fraction of unique tracks ended up getting multiplied by the number
    // of files. Assume largest scale has only one bin per event, so its
    // content is equal to the number of files.
    // If we have not summed histogram files, or if we have already rescaled,
    // nFiles will be equal to 1.
    // Rescale here.
    TH2F *hnBins   = (TH2F *) gDirectory->Get("nBins");
    float nFiles = hnBins->GetBinContent(NPhiBins,NEtaBins);
    hnBins->Scale( 1.0/nFiles );
    TH2F *hoffset = (TH2F *) gDirectory->Get("offset");
    hoffset->Scale( 1.0/nFiles );
    TH2F *hfUnique = (TH2F *) gDirectory->Get("fUnique");
    hfUnique->Scale( 1.0/nFiles );


    TH1D *hTotEvents[5];
    TH1D *hNSum[2];
    TH1D *hNPlus[2];
    TH1D *hNMinus[2];
    TH1D *hPSum[5];
    TH1D *hPDiff[8];
    TH1D *hPPlus[5];
    TH1D *hPMinus[5];
    TH1D *hPPlusMinus[8];

    for (int jCent=0;jCent<NSCENTBINS;jCent++) {
        sprintf(buffer,"TotalEvents_%i",jCent);
        hTotEvents[0] = (TH1D *) gDirectory->Get(buffer);
        sprintf(buffer,"TotalSumEvents_%i",jCent);
        hTotEvents[1] = (TH1D *) gDirectory->Get(buffer);
        sprintf(buffer,"TotalPlusEvents_%i",jCent);
        hTotEvents[2] = (TH1D *) gDirectory->Get(buffer);
        sprintf(buffer,"TotalMinusEvents_%i",jCent);
        hTotEvents[3] = (TH1D *) gDirectory->Get(buffer);
        sprintf(buffer,"TotalPlusMinusEvents_%i",jCent);
        hTotEvents[4] = (TH1D *) gDirectory->Get(buffer);

        for (int jStat=0;jStat<2;jStat++) {
            sprintf(buffer,"NSum%i_%i",jCent,jStat);
            hNSum[jStat] = (TH1D *) gDirectory->Get(buffer);

            sprintf(buffer,"NPlus%i_%i",jCent,jStat);
            hNPlus[jStat] = (TH1D *) gDirectory->Get(buffer);

            sprintf(buffer,"NMinus%i_%i",jCent,jStat);
            hNMinus[jStat] = (TH1D *) gDirectory->Get(buffer);
        }

        for (int jStat=0;jStat<5;jStat++) {
            sprintf(buffer,"PSum%i_%i",jCent,jStat);
            hPSum[jStat] = (TH1D *) gDirectory->Get(buffer);

            sprintf(buffer,"PPlus%i_%i",jCent,jStat);
            hPPlus[jStat] = (TH1D *) gDirectory->Get(buffer);

            sprintf(buffer,"PMinus%i_%i",jCent,jStat);
            hPMinus[jStat] = (TH1D *) gDirectory->Get(buffer);
        }
        for (int jStat=0;jStat<8;jStat++) {
            sprintf(buffer,"PDiff%i_%i",jCent,jStat);
            hPDiff[jStat] = (TH1D *) gDirectory->Get(buffer);
        }
        for (int jStat=0;jStat<8;jStat++) {
            sprintf(buffer,"PPlusMinus%i_%i",jCent,jStat);
            hPPlusMinus[jStat] = (TH1D *) gDirectory->Get(buffer);
        }

        int mBin, oBin;

        for (int iPhi=1;iPhi<=NPhiBins;iPhi++) {
            for (int iEta=1;iEta<=NEtaBins;iEta++) {
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
                    PSig[jCent]->SetBinContent(iPhi,iEta,Ssig/NSsig);
                    PSigErrors[jCent]->SetBinContent(iPhi,iEta,sqrt(DSsig)/NDSsig);
                    SPtHat[jCent]->SetBinContent(iPhi,iEta,ptSHat/NSsig);
                    sigSPtHat[jCent]->SetBinContent(iPhi,iEta,sigptSHat/NSsig);
                    sigSPtHatErrors[jCent]->SetBinContent(iPhi,iEta,sqrt(DsigptSHat)/NDSsig);
                }
                if (NDsig > 0) {
                    PDel[jCent]->SetBinContent(iPhi,iEta,Dsig/NDsig);
                    PDelErrors[jCent]->SetBinContent(iPhi,iEta,sqrt(DDsig)/NDDsig);
                }

                if (NPsig > 0) {
                    PPlus[jCent]->SetBinContent(iPhi,iEta,Psig/NPsig);
                    PPlusErrors[jCent]->SetBinContent(iPhi,iEta,sqrt(DPsig)/NDPsig);
                    PPtHat[jCent]->SetBinContent(iPhi,iEta,ptPHat/NPsig);
                    sigPPtHat[jCent]->SetBinContent(iPhi,iEta,sigptPHat/NPsig);
                    sigPPtHatErrors[jCent]->SetBinContent(iPhi,iEta,sqrt(DsigptPHat)/NDPsig);
                }

                if (NMsig > 0) {
                    PMinus[jCent]->SetBinContent(iPhi,iEta,Msig/NMsig);
                    PMinusErrors[jCent]->SetBinContent(iPhi,iEta,sqrt(DMsig)/NDMsig);
                    MPtHat[jCent]->SetBinContent(iPhi,iEta,ptMHat/NMsig);
                    sigMPtHat[jCent]->SetBinContent(iPhi,iEta,sigptMHat/NMsig);
                    sigMPtHatErrors[jCent]->SetBinContent(iPhi,iEta,sqrt(DsigptMHat)/NDMsig);
                }

                if (NPMsig > 0) {
                    PPlusMinus[jCent]->SetBinContent(iPhi,iEta,PMsig/NPMsig);
// Haven't found good approximation for error on +- covariance.
// Use average of errors on + and - to have something.
                    PPlusMinusErrors[jCent]->SetBinContent(iPhi,iEta,sqrt((DPsig+DMsig)/2)/NDSsig);
                }
            }
        }
    }
}
void StEStructSigmas::PNHistograms() {
    char buffer[255];

    // We have accumulated over events.
    // For every bin at each scale we have summed up all the small bins.
    // Now accumulate sum, n, n^2 for all bins.

    for (int jCent=0;jCent<NSCENTBINS;jCent++) {
        PNSig[jCent]->Reset();
        PNDel[jCent]->Reset();
        PNPlus[jCent]->Reset();
        PNMinus[jCent]->Reset();
        PNPlusMinus[jCent]->Reset();
        PNMinusPlus[jCent]->Reset();

        PNSigErrors[jCent]->Reset();
        PNDelErrors[jCent]->Reset();
        PNPlusErrors[jCent]->Reset();
        PNMinusErrors[jCent]->Reset();
        PNPlusMinusErrors[jCent]->Reset();
        PNMinusPlusErrors[jCent]->Reset();
    }

    // If we have summed histogram files the number of bins, offsets and
    // fraction of unique tracks ended up getting multiplied by the number
    // of files. Assume largest scale has only one bin per event, so its
    // content is equal to the number of files.
    // If we have not summed histogram files, or if we have already rescaled,
    // nFiles will be equal to 1.
    // Rescale here.
    TH2F *hnBins   = (TH2F *) gDirectory->Get("nBins");
    float nFiles = hnBins->GetBinContent(NPhiBins,NEtaBins);
    hnBins->Scale( 1.0/nFiles );
    TH2F *hoffset = (TH2F *) gDirectory->Get("offset");
    hoffset->Scale( 1.0/nFiles );
    TH2F *hfUnique = (TH2F *) gDirectory->Get("fUnique");
    hfUnique->Scale( 1.0/nFiles );


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

    for (int jCent=0;jCent<NSCENTBINS;jCent++) {
        sprintf(buffer,"TotalEvents_%i",jCent);
        hTotEvents[0] = (TH1D *) gDirectory->Get(buffer);
        sprintf(buffer,"TotalSumEvents_%i",jCent);
        hTotEvents[1] = (TH1D *) gDirectory->Get(buffer);
        sprintf(buffer,"TotalPlusEvents_%i",jCent);
        hTotEvents[2] = (TH1D *) gDirectory->Get(buffer);
        sprintf(buffer,"TotalMinusEvents_%i",jCent);
        hTotEvents[3] = (TH1D *) gDirectory->Get(buffer);
        sprintf(buffer,"TotalPlusMinusEvents_%i",jCent);
        hTotEvents[4] = (TH1D *) gDirectory->Get(buffer);

        for (int jStat=0;jStat<2;jStat++) {
            sprintf(buffer,"NSum%i_%i",jCent,jStat);
            hNSum[jStat] = (TH1D *) gDirectory->Get(buffer);

            sprintf(buffer,"NPlus%i_%i",jCent,jStat);
            hNPlus[jStat] = (TH1D *) gDirectory->Get(buffer);

            sprintf(buffer,"NMinus%i_%i",jCent,jStat);
            hNMinus[jStat] = (TH1D *) gDirectory->Get(buffer);
        }
        sprintf(buffer,"NDiff%i_0",jCent);
        hNDiff = (TH1D *) gDirectory->Get(buffer);

        sprintf(buffer,"PSum%i_0",jCent);
        hPSum = (TH1D *) gDirectory->Get(buffer);

        sprintf(buffer,"PPlus%i_0",jCent);
        hPPlus = (TH1D *) gDirectory->Get(buffer);

        sprintf(buffer,"PMinus%i_0",jCent);
        hPMinus = (TH1D *) gDirectory->Get(buffer);

        for (int jStat=0;jStat<8;jStat++) {
            sprintf(buffer,"PPlusMinus%i_%i",jCent,jStat);
            hPPlusMinus[jStat] = (TH1D *) gDirectory->Get(buffer);
        }
        for (int jStat=0;jStat<4;jStat++) {
            sprintf(buffer,"PNSum%i_%i",jCent,jStat);
            hPNSum[jStat] = (TH1D *) gDirectory->Get(buffer);

            sprintf(buffer,"PNDiff%i_%i",jCent,jStat);
            hPNDiff[jStat] = (TH1D *) gDirectory->Get(buffer);

            sprintf(buffer,"PNPlus%i_%i",jCent,jStat);
            hPNPlus[jStat] = (TH1D *) gDirectory->Get(buffer);

            sprintf(buffer,"PNMinus%i_%i",jCent,jStat);
            hPNMinus[jStat] = (TH1D *) gDirectory->Get(buffer);
        }
        for (int jStat=0;jStat<8;jStat++) {
            sprintf(buffer,"PNPlusMinus%i_%i",jCent,jStat);
            hPNPlusMinus[jStat] = (TH1D *) gDirectory->Get(buffer);
        }

        int mBin, oBin;

        for (int iPhi=1;iPhi<=NPhiBins;iPhi++) {
            for (int iEta=1;iEta<=NEtaBins;iEta++) {
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
                    PNSig[jCent]->SetBinContent(iPhi,iEta,Ssig/NSsig);
                }
                if (NDsig > 0) {
                    PNDel[jCent]->SetBinContent(iPhi,iEta,Dsig/NDsig);
                }
                if (NPsig > 0) {
                    PNPlus[jCent]->SetBinContent(iPhi,iEta,Psig/NPsig);
                }
                if (NMsig > 0) {
                    PNMinus[jCent]->SetBinContent(iPhi,iEta,Msig/NMsig);
                }
                if (NPMsig) {
                    PNPlusMinus[jCent]->SetBinContent(iPhi,iEta,PMsig/NPMsig);
                }
                if (NMPsig) {
                    PNMinusPlus[jCent]->SetBinContent(iPhi,iEta,MPsig/NMPsig);
                }
// I haven't been able to find a reasonable approximation to the errors
// on the Pt-N covariance terms. My best guess is the errors factor,
// and since we already have the N and the Pt errors I use those.
                nErr = NSigErrors[jCent]->GetBinContent(iPhi,iEta);
                pErr = PSigErrors[jCent]->GetBinContent(iPhi,iEta);
                PNSigErrors[jCent]->SetBinContent(iPhi,iEta,sqrt(nErr*pErr));
                nErr = NDelErrors[jCent]->GetBinContent(iPhi,iEta);
                pErr = PDelErrors[jCent]->GetBinContent(iPhi,iEta);
                PNDelErrors[jCent]->SetBinContent(iPhi,iEta,sqrt(nErr*pErr));
                nErr = NPlusErrors[jCent]->GetBinContent(iPhi,iEta);
                pErr = PPlusErrors[jCent]->GetBinContent(iPhi,iEta);
                PNPlusErrors[jCent]->SetBinContent(iPhi,iEta,sqrt(nErr*pErr));
                nErr = NMinusErrors[jCent]->GetBinContent(iPhi,iEta);
                pErr = PMinusErrors[jCent]->GetBinContent(iPhi,iEta);
                PNMinusErrors[jCent]->SetBinContent(iPhi,iEta,sqrt(nErr*pErr));
                nErr = NPlusMinusErrors[jCent]->GetBinContent(iPhi,iEta);
                pErr = PPlusMinusErrors[jCent]->GetBinContent(iPhi,iEta);
                PNPlusMinusErrors[jCent]->SetBinContent(iPhi,iEta,sqrt(nErr*pErr));
                PNMinusPlusErrors[jCent]->SetBinContent(iPhi,iEta,sqrt(nErr*pErr));
            }
        }
    }
}
void StEStructSigmas::ptNHistograms() {
    char buffer[255];

    for (int jPtCent=0;jPtCent<NPTSCENTBINS;jPtCent++) {
        for (int jPt=0;jPt<NPTBINS;jPt++) {
            ptNSig[jPtCent][jPt]->Reset();
            ptNDel[jPtCent][jPt]->Reset();
            ptNPlus[jPtCent][jPt]->Reset();
            ptNMinus[jPtCent][jPt]->Reset();
            ptNPlusMinus[jPtCent][jPt]->Reset();

            ptNSigErrors[jPtCent][jPt]->Reset();
            ptNDelErrors[jPtCent][jPt]->Reset();
            ptNPlusErrors[jPtCent][jPt]->Reset();
            ptNMinusErrors[jPtCent][jPt]->Reset();
            ptNPlusMinusErrors[jPtCent][jPt]->Reset();
        }
    }

    // If we have summed histogram files the number of bins, offsets and
    // fraction of unique tracks ended up getting multiplied by the number
    // of files. Assume largest scale has only one bin per event, so its
    // content is equal to the number of files.
    // If we have not summed histogram files, or if we have already rescaled,
    // nFiles will be equal to 1.
    // Rescale here.
    TH2F *hnBins   = (TH2F *) gDirectory->Get("nBins");
    float nFiles = hnBins->GetBinContent(NPhiBins,NEtaBins);
    hnBins->Scale( 1.0/nFiles );
    TH2F *hoffset = (TH2F *) gDirectory->Get("offset");
    hoffset->Scale( 1.0/nFiles );
    TH2F *hfUnique = (TH2F *) gDirectory->Get("fUnique");
    hfUnique->Scale( 1.0/nFiles );

    TH1D *hptTotEvents[5];
    TH1D *hptNSum[2];
    TH1D *hptNDiff[2];
    TH1D *hptNPlus[2];
    TH1D *hptNMinus[2];
    TH1D *hptNPlusMinus[1];

    for (int jPtCent=0;jPtCent<NPTSCENTBINS;jPtCent++) {
        for (int jPt=0;jPt<NPTBINS;jPt++) {

            sprintf(buffer,"ptTotalEvents_%i_%i",jPtCent,jPt);
            hptTotEvents[0] = (TH1D *) gDirectory->Get(buffer);
            sprintf(buffer,"ptTotalSumEvents_%i_%i",jPtCent,jPt);
            hptTotEvents[1] = (TH1D *) gDirectory->Get(buffer);
            sprintf(buffer,"ptTotalPlusEvents_%i_%i",jPtCent,jPt);
            hptTotEvents[2] = (TH1D *) gDirectory->Get(buffer);
            sprintf(buffer,"ptTotalMinusEvents_%i_%i",jPtCent,jPt);
            hptTotEvents[3] = (TH1D *) gDirectory->Get(buffer);
            sprintf(buffer,"ptTotalPlusMinusEvents_%i_%i",jPtCent,jPt);
            hptTotEvents[4] = (TH1D *) gDirectory->Get(buffer);

            for (int jStat=0;jStat<2;jStat++) {
                sprintf(buffer,"ptNSum%i_%i_%i",jPtCent,jPt,jStat);
                hptNSum[jStat] = (TH1D *) gDirectory->Get(buffer);

                sprintf(buffer,"ptNDiff%i_%i_%i",jPtCent,jPt,jStat);
                hptNDiff[jStat] = (TH1D *) gDirectory->Get(buffer);

                sprintf(buffer,"ptNPlus%i_%i_%i",jPtCent,jPt,jStat);
                hptNPlus[jStat] = (TH1D *) gDirectory->Get(buffer);

                sprintf(buffer,"ptNMinus%i_%i_%i",jPtCent,jPt,jStat);
                hptNMinus[jStat] = (TH1D *) gDirectory->Get(buffer);
            }
            sprintf(buffer,"ptNPlusMinus%i_%i_0",jPtCent,jPt);
            hptNPlusMinus[0] = (TH1D *) gDirectory->Get(buffer);


            int mBin, oBin;
            for (int iPhi=1;iPhi<=NPhiBins;iPhi++) {
                for (int iEta=1;iEta<=NEtaBins;iEta++) {
                    double NS1, NS2,  NSSig  = 0, NDSSig  = 0, SSig  = 0, DSSig  = 0, sSig;
                    double ND1, ND2,  NDSig  = 0, NDDSig  = 0, DSig  = 0, DDSig  = 0, qSig;
                    double NP1, NP2,  NPSig  = 0, NDPSig  = 0, PSig  = 0, DPSig  = 0, pSig;
                    double NM1, NM2,  NMSig  = 0, NDMSig  = 0, MSig  = 0, DMSig  = 0, mSig;
                    double      NPM2, NPMSig = 0, NDPMSig = 0, PMSig = 0, DPMSig = 0, pmSig;
                    double sumEvents, plusEvents, minusEvents, plusMinusEvents;

                    mBin = (int) hnBins->GetBinContent(iPhi,iEta);
                    oBin = (int) hoffset->GetBinContent(iPhi,iEta);

                    for (int iBin=oBin+1;iBin<=oBin+mBin;iBin++) {
                        sumEvents   = hptTotEvents[0]->GetBinContent(iBin);
                        if (sumEvents > 0) {
                            NS1 = hptNSum[0]->GetBinContent(iBin);
                            NS2 = hptNSum[1]->GetBinContent(iBin);
                            ND1 = hptNDiff[0]->GetBinContent(iBin);
                            ND2 = hptNDiff[1]->GetBinContent(iBin);
                            if (NS1 > 0) {
                                sSig    = (NS2 - NS1*NS1/sumEvents) / NS1;
                                SSig   += sSig - 1;
                                NSSig  += 1;
                                DSSig  += (4 + sSig/(NS1/sumEvents)) * sSig / sumEvents;
                                NDSSig += sqrt(hfUnique->GetBinContent(iPhi,iEta));

                                qSig    = (ND2 - ND1*ND1/sumEvents) / NS1;
                                DSig   += qSig - 1;
                                NDSig  += 1;
                                DDSig  += (4 + qSig/(NS1/sumEvents)) * qSig / sumEvents;
                                NDDSig += sqrt(hfUnique->GetBinContent(iPhi,iEta));
                            }
                        }

                        plusEvents  = hptTotEvents[0]->GetBinContent(iBin);
                        NP1  = 0;
                        NP2  = 0;
                        pSig = 0;
                        if (plusEvents > 0) {
                            NP1 = hptNPlus[0]->GetBinContent(iBin);
                            NP2 = hptNPlus[1]->GetBinContent(iBin);
                            if (NP1 > 0) {
                                pSig    = (NP2 - NP1*NP1/plusEvents) / NP1;
                                PSig   += pSig - 1;
                                NPSig  += 1;
                                DPSig  += (4 + pSig/(NP1/sumEvents)) * pSig / sumEvents;
                                NDPSig += sqrt(hfUnique->GetBinContent(iPhi,iEta));
                            }
                        }

                        minusEvents = hptTotEvents[0]->GetBinContent(iBin);
                        NM1  = 0;
                        NM2  = 0;
                        mSig = 0;
                        if (minusEvents > 0) {
                            NM1 = hptNMinus[0]->GetBinContent(iBin);
                            NM2 = hptNMinus[1]->GetBinContent(iBin);
                            if (NM1 > 0) {
                                mSig    = (NM2 - NM1*NM1/minusEvents) / NM1;
                                MSig   += mSig - 1;
                                NMSig  += 1;
                                DMSig  += (4 + mSig/(NM1/sumEvents)) * mSig / sumEvents;
                                NDMSig += sqrt(hfUnique->GetBinContent(iPhi,iEta));
                            }
                        }

                        plusMinusEvents = hptTotEvents[0]->GetBinContent(iBin);
                        if (plusMinusEvents > 0) {
                            NPM2     = hptNPlusMinus[0]->GetBinContent(iBin);
                            pmSig    = NPM2/sqrt(NP1*NM1) - sqrt(NP1*NM1)/plusMinusEvents;
                            PMSig   += pmSig;
                            NPMSig  += 1;
                            DPMSig  += (pSig + mSig)/ plusMinusEvents +
                                       (1/NP1+1/NM1)*pmSig*pmSig/4;
                            NDPMSig += sqrt(hfUnique->GetBinContent(iPhi,iEta));
                        }
                    }

                    if (NSSig > 0) {
                        ptNSig[jPtCent][jPt]->SetBinContent(iPhi,iEta,SSig/NSSig);
                        ptNSigErrors[jPtCent][jPt]->SetBinContent(iPhi,iEta,sqrt(DSSig)/NDSSig);
                    }
                    if (NDSig > 0) {
                        ptNDel[jPtCent][jPt]->SetBinContent(iPhi,iEta,DSig/NDSig);
                        ptNDelErrors[jPtCent][jPt]->SetBinContent(iPhi,iEta,sqrt(DDSig)/NDDSig);
                    }
                    if (NPSig > 0) {
                        ptNPlus[jPtCent][jPt]->SetBinContent(iPhi,iEta,PSig/NPSig);
                        ptNPlusErrors[jPtCent][jPt]->SetBinContent(iPhi,iEta,sqrt(DPSig)/NDPSig);
                    }
                    if (NMSig > 0) {
                        ptNMinus[jPtCent][jPt]->SetBinContent(iPhi,iEta,MSig/NMSig);
                        ptNMinusErrors[jPtCent][jPt]->SetBinContent(iPhi,iEta,sqrt(DMSig)/NDMSig);
                    }
                    if (NPMSig > 0) {
                        ptNPlusMinus[jPtCent][jPt]->SetBinContent(iPhi,iEta,PMSig/NPMSig);
                        ptNPlusMinusErrors[jPtCent][jPt]->SetBinContent(iPhi,iEta,sqrt(DPMSig)/NDPMSig);
                    }
                }
            }
        }
    }
}
void StEStructSigmas::ptPHistograms() {
    char buffer[255];

    for (int jPtCent=0;jPtCent<NPTSCENTBINS;jPtCent++) {
        for (int jPt=0;jPt<NPTBINS;jPt++) {
            ptPSig[jPtCent][jPt]->Reset();
            ptPDel[jPtCent][jPt]->Reset();
            ptPPlus[jPtCent][jPt]->Reset();
            ptPMinus[jPtCent][jPt]->Reset();
            ptPPlusMinus[jPtCent][jPt]->Reset();

            ptPSigErrors[jPtCent][jPt]->Reset();
            ptPDelErrors[jPtCent][jPt]->Reset();
            ptPPlusErrors[jPtCent][jPt]->Reset();
            ptPMinusErrors[jPtCent][jPt]->Reset();
            ptPPlusMinusErrors[jPtCent][jPt]->Reset();

            ptSPtHat[jPtCent][jPt]->Reset();
            ptPPtHat[jPtCent][jPt]->Reset();
            ptMPtHat[jPtCent][jPt]->Reset();
            ptsigSPtHat[jPtCent][jPt]->Reset();
            ptsigPPtHat[jPtCent][jPt]->Reset();
            ptsigMPtHat[jPtCent][jPt]->Reset();

            ptsigSPtHatErrors[jPtCent][jPt]->Reset();
            ptsigPPtHatErrors[jPtCent][jPt]->Reset();
            ptsigMPtHatErrors[jPtCent][jPt]->Reset();
        }
    }
    // If we have summed histogram files the number of bins, offsets and
    // fraction of unique tracks ended up getting multiplied by the number
    // of files. Assume largest scale has only one bin per event, so its
    // content is equal to the number of files.
    // If we have not summed histogram files, or if we have already rescaled,
    // nFiles will be equal to 1.
    // Rescale here.
    TH2F *hnBins   = (TH2F *) gDirectory->Get("nBins");
    float nFiles = hnBins->GetBinContent(NPhiBins,NEtaBins);
    hnBins->Scale( 1.0/nFiles );
    TH2F *hoffset = (TH2F *) gDirectory->Get("offset");
    hoffset->Scale( 1.0/nFiles );
    TH2F *hfUnique = (TH2F *) gDirectory->Get("fUnique");
    hfUnique->Scale( 1.0/nFiles );

    TH1D *hptTotEvents[5];
    TH1D *hptNSum[2];
    TH1D *hptNPlus[2];
    TH1D *hptNMinus[2];
    TH1D *hptPSum[5];
    TH1D *hptPDiff[8];
    TH1D *hptPPlus[5];
    TH1D *hptPMinus[5];
    TH1D *hptPPlusMinus[8];

    for (int jPtCent=0;jPtCent<NPTSCENTBINS;jPtCent++) {
        for (int jPt=0;jPt<NPTBINS;jPt++) {

            sprintf(buffer,"ptTotalEvents_%i_%i",jPtCent,jPt);
            hptTotEvents[0] = (TH1D *) gDirectory->Get(buffer);
            sprintf(buffer,"ptTotalSumEvents_%i_%i",jPtCent,jPt);
            hptTotEvents[1] = (TH1D *) gDirectory->Get(buffer);
            sprintf(buffer,"ptTotalPlusEvents_%i_%i",jPtCent,jPt);
            hptTotEvents[2] = (TH1D *) gDirectory->Get(buffer);
            sprintf(buffer,"ptTotalMinusEvents_%i_%i",jPtCent,jPt);
            hptTotEvents[3] = (TH1D *) gDirectory->Get(buffer);
            sprintf(buffer,"ptTotalPlusMinusEvents_%i_%i",jPtCent,jPt);
            hptTotEvents[4] = (TH1D *) gDirectory->Get(buffer);

            for (int jStat=0;jStat<2;jStat++) {
                sprintf(buffer,"ptNSum%i_%i_%i",jPtCent,jPt,jStat);
                hptNSum[jStat] = (TH1D *) gDirectory->Get(buffer);

                sprintf(buffer,"ptNPlus%i_%i_%i",jPtCent,jPt,jStat);
                hptNPlus[jStat] = (TH1D *) gDirectory->Get(buffer);

                sprintf(buffer,"ptNMinus%i_%i_%i",jPtCent,jPt,jStat);
                hptNMinus[jStat] = (TH1D *) gDirectory->Get(buffer);
            }

            for (int jStat=0;jStat<5;jStat++) {
                sprintf(buffer,"ptPSum%i_%i_%i",jPtCent,jPt,jStat);
                hptPSum[jStat] = (TH1D *) gDirectory->Get(buffer);

                sprintf(buffer,"ptPPlus%i_%i_%i",jPtCent,jPt,jStat);
                hptPPlus[jStat] = (TH1D *) gDirectory->Get(buffer);

                sprintf(buffer,"ptPMinus%i_%i_%i",jPtCent,jPt,jStat);
                hptPMinus[jStat] = (TH1D *) gDirectory->Get(buffer);
            }
            for (int jStat=0;jStat<8;jStat++) {
                sprintf(buffer,"ptPDiff%i_%i_%i",jPtCent,jPt,jStat);
                hptPDiff[jStat] = (TH1D *) gDirectory->Get(buffer);
            }
            for (int jStat=0;jStat<8;jStat++) {
                sprintf(buffer,"ptPPlusMinus%i_%i_%i",jPtCent,jPt,jStat);
                hptPPlusMinus[jStat] = (TH1D *) gDirectory->Get(buffer);
            }

            int mBin, oBin;

            for (int iPhi=1;iPhi<=NPhiBins;iPhi++) {
                for (int iEta=1;iEta<=NEtaBins;iEta++) {
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
                    double sumEvents, plusEvents, minusEvents, plusMinusEvents;

                    mBin = (int) hnBins->GetBinContent(iPhi,iEta);
                    oBin = (int) hoffset->GetBinContent(iPhi,iEta);

                    for (int iBin=oBin+1;iBin<=oBin+mBin;iBin++) {
                        sumEvents = hptTotEvents[1]->GetBinContent(iBin);
                        if (sumEvents > 0) {
                            NS0  = hptNSum[0]->GetBinContent(iBin) / sumEvents;
                            NS1  = hptNSum[1]->GetBinContent(iBin) / sumEvents;
                            PS0  = hptPSum[0]->GetBinContent(iBin) / sumEvents;
                            PS1  = hptPSum[1]->GetBinContent(iBin) / sumEvents;
                            PS2  = hptPSum[2]->GetBinContent(iBin) / sumEvents;
                            PS3  = hptPSum[3]->GetBinContent(iBin) / sumEvents;
                            PS4  = hptPSum[4]->GetBinContent(iBin) / sumEvents;

                            PD0  = hptPDiff[0]->GetBinContent(iBin) / sumEvents;
                            PD1  = hptPDiff[1]->GetBinContent(iBin) / sumEvents;
                            PD2  = hptPDiff[2]->GetBinContent(iBin) / sumEvents;
                            PD3  = hptPDiff[3]->GetBinContent(iBin) / sumEvents;
                            PD4  = hptPDiff[4]->GetBinContent(iBin) / sumEvents;
                            PD5  = hptPDiff[5]->GetBinContent(iBin) / sumEvents;
                            PD6  = hptPDiff[6]->GetBinContent(iBin) / sumEvents;
                            PD7  = hptPDiff[7]->GetBinContent(iBin) / sumEvents;
                            if (NS0 > 0) {
                                double pHat  = PS0 / NS0;
                                double pHat2  = pHat*pHat;
                                ptSHat    += pHat;
                                sigPtHat   = PS4/NS0 - pHat*pHat;
                                sigptSHat += sigPtHat;
                                DsigptHat  = 4*pHat2*sigPtHat / (NS0*sumEvents);
                                DsigptSHat += DsigptHat;
                                ssig       = PS1 - pHat*PS0;
                                Ssig      += ssig - sigPtHat;
                                NSsig     += 1;

                                DSsig     += (PS3 - 4*pHat*PS2       + 6*pHat2*PS1
                                                  - 4*pHat*pHat2*PS0 + pHat2*pHat2*NS0) *(NS0/sumEvents);
                                NDSsig    += sqrt(hfUnique->GetBinContent(iPhi,iEta));

                                dsig       = PD0 - 2*pHat*PD1 + pHat2*PD2;
                                Dsig      += dsig - sigPtHat;
                                NDsig     += 1;

                                DDsig     += (PD3 - 4*pHat*PD4       + 6*pHat2*PD5
                                                  - 4*pHat*pHat2*PD6 + pHat2*pHat2*PD7) *(NS0/sumEvents);
                                NDDsig    += sqrt(hfUnique->GetBinContent(iPhi,iEta));
                            }
                        }

                        plusEvents = hptTotEvents[2]->GetBinContent(iBin);
                        if (plusEvents > 0) {
                            NP0  = hptNPlus[0]->GetBinContent(iBin) / plusEvents;
                            NP1  = hptNPlus[1]->GetBinContent(iBin) / plusEvents;
                            PP0  = hptPPlus[0]->GetBinContent(iBin) / plusEvents;
                            PP1  = hptPPlus[1]->GetBinContent(iBin) / plusEvents;
                            PP2  = hptPPlus[2]->GetBinContent(iBin) / plusEvents;
                            PP3  = hptPPlus[3]->GetBinContent(iBin) / plusEvents;
                            PP4  = hptPPlus[4]->GetBinContent(iBin) / plusEvents;
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
                                                  - 4*pHat*pHat2*PP0 + pHat2*pHat2*NP0) *(NP0/plusEvents);
                                NDPsig    += sqrt(hfUnique->GetBinContent(iPhi,iEta));
                            }
                        }

                        minusEvents = hptTotEvents[3]->GetBinContent(iBin);
                        if (minusEvents > 0) {
                            NM0  = hptNMinus[0]->GetBinContent(iBin) / minusEvents;
                            NM1  = hptNMinus[1]->GetBinContent(iBin) / minusEvents;
                            PM0  = hptPMinus[0]->GetBinContent(iBin) / minusEvents;
                            PM1  = hptPMinus[1]->GetBinContent(iBin) / minusEvents;
                            PM2  = hptPMinus[2]->GetBinContent(iBin) / minusEvents;
                            PM3  = hptPMinus[3]->GetBinContent(iBin) / minusEvents;
                            PM4  = hptPMinus[4]->GetBinContent(iBin) / minusEvents;
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

                                DMsig     += (PM3 - 4*pHat*PM2       + 6*pHat2*PM1
                                                  - 4*pHat*pHat2*PM0 + pHat2*pHat2*NM0) *(NM0/minusEvents);
                                NMsig     += 1;
                                NDMsig    += sqrt(hfUnique->GetBinContent(iPhi,iEta));
                            }
                        }

                        plusMinusEvents = hptTotEvents[4]->GetBinContent(iBin);
                        if (plusMinusEvents > 0) {
                            NP1  = hptPPlusMinus[4]->GetBinContent(iBin);
                            PP1  = hptPPlusMinus[5]->GetBinContent(iBin);
                            double pHat = PP1 / NP1;
                            NM1  = hptPPlusMinus[6]->GetBinContent(iBin);
                            PM1  = hptPPlusMinus[7]->GetBinContent(iBin);
                            double mHat = PM1 / NM1;
                            PPM1 = pHat*mHat*hptPPlusMinus[0]->GetBinContent(iBin);
                            PPM2 = mHat*hptPPlusMinus[1]->GetBinContent(iBin);
                            PPM3 = pHat*hptPPlusMinus[2]->GetBinContent(iBin);
                            PPM4 = hptPPlusMinus[3]->GetBinContent(iBin);
                            PMsig  += (PPM1 - PPM2 - PPM3 + PPM4) / plusMinusEvents;
                            NPMsig += 1;
                            NDPMsig    += sqrt(hfUnique->GetBinContent(iPhi,iEta));
                        }
                    }

                    if (NSsig > 0) {
                        ptPSig[jPtCent][jPt]->SetBinContent(iPhi,iEta,Ssig/NSsig);
                        ptPSigErrors[jPtCent][jPt]->SetBinContent(iPhi,iEta,sqrt(DSsig)/NDSsig);
                        ptSPtHat[jPtCent][jPt]->SetBinContent(iPhi,iEta,ptSHat/NSsig);
                        ptsigSPtHat[jPtCent][jPt]->SetBinContent(iPhi,iEta,sigptSHat/NSsig);
                        ptsigSPtHatErrors[jPtCent][jPt]->SetBinContent(iPhi,iEta,sqrt(DsigptSHat)/NDSsig);
                    }
                    if (NDsig > 0) {
                        ptPDel[jPtCent][jPt]->SetBinContent(iPhi,iEta,Dsig/NDsig);
                        ptPDelErrors[jPtCent][jPt]->SetBinContent(iPhi,iEta,sqrt(DDsig)/NDDsig);
                    }

                    if (NPsig > 0) {
                        ptPPlus[jPtCent][jPt]->SetBinContent(iPhi,iEta,Psig/NPsig);
                        ptPPlusErrors[jPtCent][jPt]->SetBinContent(iPhi,iEta,sqrt(DPsig)/NDPsig);
                        ptPPtHat[jPtCent][jPt]->SetBinContent(iPhi,iEta,ptPHat/NPsig);
                        ptsigPPtHat[jPtCent][jPt]->SetBinContent(iPhi,iEta,sigptPHat/NPsig);
                        ptsigPPtHatErrors[jPtCent][jPt]->SetBinContent(iPhi,iEta,sqrt(DsigptPHat)/NDPsig);
                    }

                    if (NMsig > 0) {
                        ptPMinus[jPtCent][jPt]->SetBinContent(iPhi,iEta,Msig/NMsig);
                        ptPMinusErrors[jPtCent][jPt]->SetBinContent(iPhi,iEta,sqrt(DMsig)/NDMsig);
                        ptMPtHat[jPtCent][jPt]->SetBinContent(iPhi,iEta,ptMHat/NMsig);
                        ptsigMPtHat[jPtCent][jPt]->SetBinContent(iPhi,iEta,sigptMHat/NMsig);
                        ptsigMPtHatErrors[jPtCent][jPt]->SetBinContent(iPhi,iEta,sqrt(DsigptMHat)/NDMsig);
                    }

                    if (NPMsig > 0) {
                        ptPPlusMinus[jPtCent][jPt]->SetBinContent(iPhi,iEta,PMsig/NPMsig);
// Haven't found good approximation for error on +- covariance.
// Use average of errors on + and - to have something.
                        ptPPlusMinusErrors[jPtCent][jPt]->SetBinContent(iPhi,iEta,sqrt((DPsig+DMsig)/2)/NDPMsig);
                    }

                }
            }
        }
    }
}
void StEStructSigmas::ptPNHistograms() {
    char buffer[255];

    for (int jPtCent=0;jPtCent<NPTSCENTBINS;jPtCent++) {
        for (int j=0;j<NPTBINS;j++) {
            ptPNSig[jPtCent][j]->Reset();
            ptPNDel[jPtCent][j]->Reset();
            ptPNPlus[jPtCent][j]->Reset();
            ptPNMinus[jPtCent][j]->Reset();
            ptPNPlusMinus[jPtCent][j]->Reset();

            ptPNSigErrors[jPtCent][j]->Reset();
            ptPNDelErrors[jPtCent][j]->Reset();
            ptPNPlusErrors[jPtCent][j]->Reset();
            ptPNMinusErrors[jPtCent][j]->Reset();
            ptPNPlusMinusErrors[jPtCent][j]->Reset();
        }
    }

    // If we have summed histogram files the number of bins, offsets and
    // fraction of unique tracks ended up getting multiplied by the number
    // of files. Assume largest scale has only one bin per event, so its
    // content is equal to the number of files.
    // If we have not summed histogram files, or if we have already rescaled,
    // nFiles will be equal to 1.
    // Rescale here.
    TH2F *hnBins   = (TH2F *) gDirectory->Get("nBins");
    float nFiles = hnBins->GetBinContent(NPhiBins,NEtaBins);
    hnBins->Scale( 1.0/nFiles );
    TH2F *hoffset = (TH2F *) gDirectory->Get("offset");
    hoffset->Scale( 1.0/nFiles );
    TH2F *hfUnique = (TH2F *) gDirectory->Get("fUnique");
    hfUnique->Scale( 1.0/nFiles );

    TH1D *hptTotEvents[5];
    TH1D *hptNSum[2];
    TH1D *hptNDiff;
    TH1D *hptNPlus[2];
    TH1D *hptNMinus[2];
    TH1D *hptPSum[3];
    TH1D *hptPPlus[3];
    TH1D *hptPMinus[3];
    TH1D *hptPPlusMinus[8];
    TH1D *hptPNSum[4];
    TH1D *hptPNDiff[4];
    TH1D *hptPNPlus[4];
    TH1D *hptPNMinus[4];
    TH1D *hptPNPlusMinus[8];

    for (int jPtCent=0;jPtCent<NPTSCENTBINS;jPtCent++) {
        for (int jPt=0;jPt<NPTBINS;jPt++) {

            sprintf(buffer,"ptTotalEvents_%i_%i",jPtCent,jPt);
            hptTotEvents[0] = (TH1D *) gDirectory->Get(buffer);
            sprintf(buffer,"ptTotalSumEvents_%i_%i",jPtCent,jPt);
            hptTotEvents[1] = (TH1D *) gDirectory->Get(buffer);
            sprintf(buffer,"ptTotalPlusEvents_%i_%i",jPtCent,jPt);
            hptTotEvents[2] = (TH1D *) gDirectory->Get(buffer);
            sprintf(buffer,"ptTotalMinusEvents_%i_%i",jPtCent,jPt);
            hptTotEvents[3] = (TH1D *) gDirectory->Get(buffer);
            sprintf(buffer,"ptTotalPlusMinusEvents_%i_%i",jPtCent,jPt);
            hptTotEvents[4] = (TH1D *) gDirectory->Get(buffer);

            for (int jStat=0;jStat<2;jStat++) {
                sprintf(buffer,"ptNSum%i_%i_%i",jPtCent,jPt,jStat);
                hptNSum[jStat] = (TH1D *) gDirectory->Get(buffer);

                sprintf(buffer,"ptNPlus%i_%i_%i",jPtCent,jPt,jStat);
                hptNPlus[jStat] = (TH1D *) gDirectory->Get(buffer);

                sprintf(buffer,"ptNMinus%i_%i_%i",jPtCent,jPt,jStat);
                hptNMinus[jStat] = (TH1D *) gDirectory->Get(buffer);
            }
            sprintf(buffer,"ptNDiff%i_%i_0",jPtCent,jPt);
            hptNDiff = (TH1D *) gDirectory->Get(buffer);

            for (int jStat=0;jStat<3;jStat++) {
                sprintf(buffer,"ptPSum%i_%i_%i",jPtCent,jPt,jStat);
                hptPSum[jStat] = (TH1D *) gDirectory->Get(buffer);

                sprintf(buffer,"ptPPlus%i_%i_%i",jPtCent,jPt,jStat);
                hptPPlus[jStat] = (TH1D *) gDirectory->Get(buffer);

                sprintf(buffer,"ptPMinus%i_%i_%i",jPtCent,jPt,jStat);
                hptPMinus[jStat] = (TH1D *) gDirectory->Get(buffer);
            }
            for (int jStat=0;jStat<8;jStat++) {
                sprintf(buffer,"ptPPlusMinus%i_%i_%i",jPtCent,jPt,jStat);
                hptPPlusMinus[jStat] = (TH1D *) gDirectory->Get(buffer);
            }
            for (int jStat=0;jStat<4;jStat++) {
                sprintf(buffer,"ptPNSum%i_%i_%i",jPtCent,jPt,jStat);
                hptPNSum[jStat] = (TH1D *) gDirectory->Get(buffer);

                sprintf(buffer,"ptPNDiff%i_%i_%i",jPtCent,jPt,jStat);
                hptPNDiff[jStat] = (TH1D *) gDirectory->Get(buffer);

                sprintf(buffer,"ptPNPlus%i_%i_%i",jPtCent,jPt,jStat);
                hptPNPlus[jStat] = (TH1D *) gDirectory->Get(buffer);

                sprintf(buffer,"ptPNMinus%i_%i_%i",jPtCent,jPt,jStat);
                hptPNMinus[jStat] = (TH1D *) gDirectory->Get(buffer);
            }
            for (int jStat=0;jStat<8;jStat++) {
                sprintf(buffer,"ptPNPlusMinus%i_%i_%i",jPtCent,jPt,jStat);
                hptPNPlusMinus[jStat] = (TH1D *) gDirectory->Get(buffer);
            }

            int mBin, oBin;

            for (int iPhi=1;iPhi<=NPhiBins;iPhi++) {
                for (int iEta=1;iEta<=NEtaBins;iEta++) {
                    double NS1, PS1, PNS1, PNS2, PNS3, PNS4, Ssig = 0, NSsig = 0;;
                    double ND1,      PND1, PND2, PND3, PND4, Dsig = 0, NDsig = 0;;
                    double NP1, PP1, PNP1, PNP2, PNP3, PNP4, Psig = 0, NPsig = 0;;
                    double NM1, PM1, PNM1, PNM2, PNM3, PNM4, Msig = 0, NMsig = 0;
                    double PNPM1, PNPM2, PNPM3, PNPM4, PMsig = 0, NPMsig = 0;
                    double PNMP1, PNMP2, PNMP3, PNMP4, MPsig = 0, NMPsig = 0;
                    double sumEvents, plusEvents, minusEvents, plusMinusEvents;
                    double nErr, pErr;

                    mBin = (int) hnBins->GetBinContent(iPhi,iEta);
                    oBin = (int) hoffset->GetBinContent(iPhi,iEta);

                    for (int iBin=oBin+1;iBin<=oBin+mBin;iBin++) {
                        sumEvents = hptTotEvents[1]->GetBinContent(iBin);
                        if (sumEvents > 0) {
                            NS1  = hptNSum[0]->GetBinContent(iBin) / sumEvents;
                            PS1  = hptPSum[0]->GetBinContent(iBin) / sumEvents;
                            PNS1 = hptPNSum[0]->GetBinContent(iBin) / sumEvents;
                            PNS2 = hptPNSum[1]->GetBinContent(iBin) / sumEvents;
                            PNS3 = hptPNSum[2]->GetBinContent(iBin) / sumEvents;
                            PNS4 = hptPNSum[3]->GetBinContent(iBin) / sumEvents;
                            ND1  = hptNDiff->GetBinContent(iBin) / sumEvents;
                            PND1 = hptPNDiff[0]->GetBinContent(iBin) / sumEvents;
                            PND2 = hptPNDiff[1]->GetBinContent(iBin) / sumEvents;
                            PND3 = hptPNDiff[2]->GetBinContent(iBin) / sumEvents;
                            PND4 = hptPNDiff[3]->GetBinContent(iBin) / sumEvents;
                            if (NS1 > 0) {
                                double R1 = PS1 / NS1;
                                Ssig += (PNS1*R1*NS1 - PNS2*R1 - PNS3*NS1 + PNS4) / sqrt(NS1);
                                NSsig  += 1;

                                Dsig += (PND1*R1*ND1 - PND2*R1 - PND3*ND1 + PND4) / sqrt(NS1);
                                NDsig  += 1;
                            }
                        }

                        plusEvents = hptTotEvents[2]->GetBinContent(iBin);
                        if (plusEvents > 0) {
                            NP1  = hptNPlus[0]->GetBinContent(iBin) / plusEvents;
                            PP1  = hptPPlus[0]->GetBinContent(iBin) / plusEvents;
                            PNP1 = hptPNPlus[0]->GetBinContent(iBin) / plusEvents;
                            PNP2 = hptPNPlus[1]->GetBinContent(iBin) / plusEvents;
                            PNP3 = hptPNPlus[2]->GetBinContent(iBin) / plusEvents;
                            PNP4 = hptPNPlus[3]->GetBinContent(iBin) / plusEvents;
                            if (NP1 > 0) {
                                double R1 = PP1 / NP1;
                                Psig  += (PNP1*R1*NP1 - PNP2*R1 - PNP3*NP1 + PNP4) / sqrt(NP1);
                                NPsig += 1;
                            }
                        }

                        minusEvents = hptTotEvents[3]->GetBinContent(iBin);
                        if (minusEvents > 0) {
                            NM1  = hptNMinus[0]->GetBinContent(iBin) / minusEvents;
                            PM1  = hptPMinus[0]->GetBinContent(iBin) / minusEvents;
                            PNM1 = hptPNMinus[0]->GetBinContent(iBin) / minusEvents;
                            PNM2 = hptPNMinus[1]->GetBinContent(iBin) / minusEvents;
                            PNM3 = hptPNMinus[2]->GetBinContent(iBin) / minusEvents;
                            PNM4 = hptPNMinus[3]->GetBinContent(iBin) / minusEvents;
                            if (NM1 > 0) {
                                double R1 = PM1 / NM1;
                                Msig  += (PNM1*R1*NM1 - PNM2*R1 - PNM3*NM1 + PNM4) / sqrt(NM1);
                                NMsig += 1;
                            }
                        }

                        plusMinusEvents = hptTotEvents[4]->GetBinContent(iBin);
                        if (plusMinusEvents > 0) {
                            NP1   = hptPPlusMinus[4]->GetBinContent(iBin) / plusMinusEvents;
                            PP1   = hptPPlusMinus[5]->GetBinContent(iBin) / plusMinusEvents;
                            NM1   = hptPPlusMinus[6]->GetBinContent(iBin) / plusMinusEvents;
                            PM1   = hptPPlusMinus[7]->GetBinContent(iBin) / plusMinusEvents;

                            PNPM1 = hptPNPlusMinus[0]->GetBinContent(iBin) / plusMinusEvents;
                            PNPM2 = hptPNPlusMinus[1]->GetBinContent(iBin) / plusMinusEvents;
                            PNPM3 = hptPNPlusMinus[2]->GetBinContent(iBin) / plusMinusEvents;
                            PNPM4 = hptPNPlusMinus[3]->GetBinContent(iBin) / plusMinusEvents;
                            if (NM1 > 0) {
                                double R1 = PP1 / NP1;
                                PMsig  += (PNPM1*R1*NM1 - PNPM2*R1 - PNPM3*NM1 + PNPM4) / sqrt(NM1);
                                NPMsig += 1;
                            }
                            PNMP1 = hptPNPlusMinus[4]->GetBinContent(iBin) / plusMinusEvents;
                            PNMP2 = hptPNPlusMinus[5]->GetBinContent(iBin) / plusMinusEvents;
                            PNMP3 = hptPNPlusMinus[6]->GetBinContent(iBin) / plusMinusEvents;
                            PNMP4 = hptPNPlusMinus[7]->GetBinContent(iBin) / plusMinusEvents;
                            if (NP1 > 0) {
                                double R1 = PM1 / NM1;
                                MPsig  += (PNMP1*R1*NP1 - PNMP2*R1 - PNMP3*NP1 + PNMP4) / sqrt(NP1);
                                NMPsig += 1;
                            }
                        }
                    }

                    if (NSsig > 0) {
                        ptPNSig[jPtCent][jPt]->SetBinContent(iPhi,iEta,Ssig/NSsig);
                    }
                    if (NDsig > 0) {
                        ptPNDel[jPtCent][jPt]->SetBinContent(iPhi,iEta,Dsig/NDsig);
                    }
                    if (NPsig > 0) {
                        ptPNPlus[jPtCent][jPt]->SetBinContent(iPhi,iEta,Psig/NPsig);
                    }
                    if (NMsig > 0) {
                        ptPNMinus[jPtCent][jPt]->SetBinContent(iPhi,iEta,Msig/NMsig);
                    }
                    if (NPMsig > 0) {
                        ptPNPlusMinus[jPtCent][jPt]->SetBinContent(iPhi,iEta,PMsig/NPMsig);
                    }
                    if (NMPsig > 0) {
                        ptPNMinusPlus[jPtCent][jPt]->SetBinContent(iPhi,iEta,MPsig/NMPsig);
                    }
// I haven't been able to find a reasonable approximation to the errors
// on the Pt-N covariance terms. My best guess is the errors factor,
// and since we already have the N and the Pt errors I use those.
                    nErr = ptNSigErrors[jPtCent][jPt]->GetBinContent(iPhi,iEta);
                    pErr = ptPSigErrors[jPtCent][jPt]->GetBinContent(iPhi,iEta);
                    ptPNSigErrors[jPtCent][jPt]->SetBinContent(iPhi,iEta,sqrt(nErr*pErr));
                    nErr = ptNDelErrors[jPtCent][jPt]->GetBinContent(iPhi,iEta);
                    pErr = ptPDelErrors[jPtCent][jPt]->GetBinContent(iPhi,iEta);
                    ptPNDelErrors[jPtCent][jPt]->SetBinContent(iPhi,iEta,sqrt(nErr*pErr));
                    nErr = ptNPlusErrors[jPtCent][jPt]->GetBinContent(iPhi,iEta);
                    pErr = ptPPlusErrors[jPtCent][jPt]->GetBinContent(iPhi,iEta);
                    ptPNPlusErrors[jPtCent][jPt]->SetBinContent(iPhi,iEta,sqrt(nErr*pErr));
                    nErr = ptNMinusErrors[jPtCent][jPt]->GetBinContent(iPhi,iEta);
                    pErr = ptPMinusErrors[jPtCent][jPt]->GetBinContent(iPhi,iEta);
                    ptPNMinusErrors[jPtCent][jPt]->SetBinContent(iPhi,iEta,sqrt(nErr*pErr));
                    nErr = ptNPlusMinusErrors[jPtCent][jPt]->GetBinContent(iPhi,iEta);
                    pErr = ptPPlusMinusErrors[jPtCent][jPt]->GetBinContent(iPhi,iEta);
                    ptPNPlusMinusErrors[jPtCent][jPt]->SetBinContent(iPhi,iEta,sqrt(nErr*pErr));
                    ptPNMinusPlusErrors[jPtCent][jPt]->SetBinContent(iPhi,iEta,sqrt(nErr*pErr));
                }
            }
        }
    }
}

//
//------------ Below are init, delete, write functions -------///
//


//--------------------------------------------------------------------------
void StEStructSigmas::writeQAHists(TFile* qatf) {

    fillHistograms();

    for (int i=0;i<NSCENTBINS;i++) {
        NSig[i]->Write();
        NDel[i]->Write();
        NPlus[i]->Write();
        NMinus[i]->Write();
        NPlusMinus[i]->Write();
        PSig[i]->Write();
        PDel[i]->Write();
        PPlus[i]->Write();
        PMinus[i]->Write();
        PPlusMinus[i]->Write();
        PNSig[i]->Write();
        PNDel[i]->Write();
        PNPlus[i]->Write();
        PNMinus[i]->Write();
        PNPlusMinus[i]->Write();
        PNMinusPlus[i]->Write();

        SPtHat[i]->Write();
        PPtHat[i]->Write();
        MPtHat[i]->Write();
        sigSPtHat[i]->Write();
        sigPPtHat[i]->Write();
        sigMPtHat[i]->Write();

        NSigErrors[i]->Write();
        NDelErrors[i]->Write();
        NPlusErrors[i]->Write();
        NMinusErrors[i]->Write();
        NPlusMinusErrors[i]->Write();
        PSigErrors[i]->Write();
        PDelErrors[i]->Write();
        PPlusErrors[i]->Write();
        PMinusErrors[i]->Write();
        PPlusMinusErrors[i]->Write();
        PNSigErrors[i]->Write();
        PNDelErrors[i]->Write();
        PNPlusErrors[i]->Write();
        PNMinusErrors[i]->Write();
        PNPlusMinusErrors[i]->Write();
        PNMinusPlusErrors[i]->Write();

        sigSPtHatErrors[i]->Write();
        sigPPtHatErrors[i]->Write();
        sigMPtHatErrors[i]->Write();
    }

    for (int i=0;i<NPTSCENTBINS;i++) {
        for (int j=0;j<NPTBINS;j++) {
            ptNSig[i][j]->Write();
            ptNDel[i][j]->Write();
            ptNPlus[i][j]->Write();
            ptNMinus[i][j]->Write();
            ptNPlusMinus[i][j]->Write();
            ptPSig[i][j]->Write();
            ptPDel[i][j]->Write();
            ptPPlus[i][j]->Write();
            ptPMinus[i][j]->Write();
            ptPPlusMinus[i][j]->Write();
            ptPNSig[i][j]->Write();
            ptPNDel[i][j]->Write();
            ptPNPlus[i][j]->Write();
            ptPNMinus[i][j]->Write();
            ptPNPlusMinus[i][j]->Write();
            ptPNMinusPlus[i][j]->Write();

            ptSPtHat[i][j]->Write();
            ptPPtHat[i][j]->Write();
            ptMPtHat[i][j]->Write();
            ptsigSPtHat[i][j]->Write();
            ptsigPPtHat[i][j]->Write();
            ptsigMPtHat[i][j]->Write();

            ptNSigErrors[i][j]->Write();
            ptNDelErrors[i][j]->Write();
            ptNPlusErrors[i][j]->Write();
            ptNMinusErrors[i][j]->Write();
            ptNPlusMinusErrors[i][j]->Write();
            ptPSigErrors[i][j]->Write();
            ptPDelErrors[i][j]->Write();
            ptPPlusErrors[i][j]->Write();
            ptPMinusErrors[i][j]->Write();
            ptPPlusMinusErrors[i][j]->Write();
            ptPNSigErrors[i][j]->Write();
            ptPNDelErrors[i][j]->Write();
            ptPNPlusErrors[i][j]->Write();
            ptPNMinusErrors[i][j]->Write();
            ptPNPlusMinusErrors[i][j]->Write();
            ptPNMinusPlusErrors[i][j]->Write();

            ptsigSPtHatErrors[i][j]->Write();
            ptsigPPtHatErrors[i][j]->Write();
            ptsigMPtHatErrors[i][j]->Write();
        }
    }
}

//--------------------------------------------------------------------------
void StEStructSigmas::initArraysAndHistograms() {
    char line[255];

    // Assume histograms we extract information from already exist in memory.
    // Extract number of phi and eta bins from one of the histograms
    TH2F *hnBins   = (TH2F *) gDirectory->Get("nBins");
    NPhiBins = hnBins->GetNbinsX();
    NEtaBins = hnBins->GetNbinsY();


    // Here are histograms for storing variances and errors of variances..
    for (int i=0;i<NSCENTBINS;i++) {
        sprintf( line, "NSig_%i", i );
        NSig[i] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
        sprintf( line, "NDel_%i", i );
        NDel[i] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
        sprintf( line, "NPlus_%i", i );
        NPlus[i] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
        sprintf( line, "NMinus_%i", i );
        NMinus[i] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
        sprintf( line, "NPlusMinus_%i", i );
        NPlusMinus[i] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
        sprintf( line, "PSig_%i", i );
        PSig[i] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
        sprintf( line, "PDel_%i", i );
        PDel[i] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
        sprintf( line, "PPlus_%i", i );
        PPlus[i] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
        sprintf( line, "PMinus_%i", i );
        PMinus[i] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
        sprintf( line, "PPlusMinus_%i", i );
        PPlusMinus[i] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
        sprintf( line, "PNSig_%i", i );
        PNSig[i] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
        sprintf( line, "PNDel_%i", i );
        PNDel[i] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
        sprintf( line, "PNPlus_%i", i );
        PNPlus[i] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
        sprintf( line, "PNMinus_%i", i );
        PNMinus[i] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
        sprintf( line, "PNPlusMinus_%i", i );
        PNPlusMinus[i] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
        sprintf( line, "PNMinusPlus_%i", i );
        PNMinusPlus[i] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);

        sprintf( line, "SptHat_%i", i );
        SPtHat[i]    = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
        sprintf( line, "PptHat_%i", i );
        PPtHat[i]    = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
        sprintf( line, "MptHat_%i", i );
        MPtHat[i]    = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
        sprintf( line, "SsigPtHat_%i", i );
        sigSPtHat[i] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
        sprintf( line, "PsigPtHat_%i", i );
        sigPPtHat[i] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
        sprintf( line, "MsigPtHat_%i", i );
        sigMPtHat[i] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);

        sprintf( line, "NSigErrors_%i", i );
        NSigErrors[i] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
        sprintf( line, "NDelErrors_%i", i );
        NDelErrors[i] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
        sprintf( line, "NPlusErrors_%i", i );
        NPlusErrors[i] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
        sprintf( line, "NMinusErrors_%i", i );
        NMinusErrors[i] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
        sprintf( line, "NPlusMinusErrors_%i", i );
        NPlusMinusErrors[i] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
        sprintf( line, "PSigErrors_%i", i );
        PSigErrors[i] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
        sprintf( line, "PDelErrors_%i", i );
        PDelErrors[i] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
        sprintf( line, "PPlusErrors_%i", i );
        PPlusErrors[i] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
        sprintf( line, "PMinusErrors_%i", i );
        PMinusErrors[i] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
        sprintf( line, "PPlusMinusErrors_%i", i );
        PPlusMinusErrors[i] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
        sprintf( line, "PNSigErrors_%i", i );
        PNSigErrors[i] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
        sprintf( line, "PNDelErrors_%i", i );
        PNDelErrors[i] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
        sprintf( line, "PNPlusErrors_%i", i );
        PNPlusErrors[i] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
        sprintf( line, "PNMinusErrors_%i", i );
        PNMinusErrors[i] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
        sprintf( line, "PNPlusMinusErrors_%i", i );
        PNPlusMinusErrors[i] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
        sprintf( line, "PNMinusPlusErrors_%i", i );
        PNMinusPlusErrors[i] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);

        sprintf( line, "SsigPtHatErrors_%i", i );
        sigSPtHatErrors[i] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
        sprintf( line, "PsigPtHatErrors_%i", i );
        sigPPtHatErrors[i] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
        sprintf( line, "MsigPtHatErrors_%i", i );
        sigMPtHatErrors[i] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
    }

    // pt dependent variances and errors.
    for (int i=0;i<NPTSCENTBINS;i++) {
        for (int j=0;j<NPTBINS;j++) {
            sprintf( line, "ptNSig_%i_%i", i, j );
            ptNSig[i][j] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
            sprintf( line, "ptNDel_%i_%i", i, j );
            ptNDel[i][j] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
            sprintf( line, "ptNPlus_%i_%i", i, j );
            ptNPlus[i][j] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
            sprintf( line, "ptNMinus_%i_%i", i, j );
            ptNMinus[i][j] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
            sprintf( line, "ptNPlusMinus_%i_%i", i, j );
            ptNPlusMinus[i][j] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
            sprintf( line, "ptPSig_%i_%i", i, j );
            ptPSig[i][j] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
            sprintf( line, "ptPDel_%i_%i", i, j );
            ptPDel[i][j] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
            sprintf( line, "ptPPlus_%i_%i", i, j );
            ptPPlus[i][j] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
            sprintf( line, "ptPMinus_%i_%i", i, j );
            ptPMinus[i][j] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
            sprintf( line, "ptPPlusMinus_%i_%i", i, j );
            ptPPlusMinus[i][j] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
            sprintf( line, "ptPNSig_%i_%i", i, j );
            ptPNSig[i][j] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
            sprintf( line, "ptPNDel_%i_%i", i, j );
            ptPNDel[i][j] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
            sprintf( line, "ptPNPlus_%i_%i", i, j );
            ptPNPlus[i][j] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
            sprintf( line, "ptPNMinus_%i_%i", i, j );
            ptPNMinus[i][j] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
            sprintf( line, "ptPNPlusMinus_%i_%i", i, j );
            ptPNPlusMinus[i][j] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
            sprintf( line, "ptPNMinusPlus_%i_%i", i, j );
            ptPNMinusPlus[i][j] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);

            sprintf( line, "ptPtSHat_%i_%i", i, j );
            ptSPtHat[i][j]    = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
            sprintf( line, "ptPtPHat_%i_%i", i, j );
            ptPPtHat[i][j]    = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
            sprintf( line, "ptPtMHat_%i_%i", i, j );
            ptMPtHat[i][j]    = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
            sprintf( line, "ptsigPtSHat_%i_%i", i, j );
            ptsigSPtHat[i][j] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
            sprintf( line, "ptsigPtPHat_%i_%i", i, j );
            ptsigPPtHat[i][j] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
            sprintf( line, "ptsigPtMHat_%i_%i", i, j );
            ptsigMPtHat[i][j] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);

            sprintf( line, "ptNSigErrors_%i_%i", i, j );
            ptNSigErrors[i][j] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
            sprintf( line, "ptNDelErrors_%i_%i", i, j );
            ptNDelErrors[i][j] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
            sprintf( line, "ptNPlusErrors_%i_%i", i, j );
            ptNPlusErrors[i][j] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
            sprintf( line, "ptNMinusErrors_%i_%i", i, j );
            ptNMinusErrors[i][j] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
            sprintf( line, "ptNPlusMinusErrors_%i_%i", i, j );
            ptNPlusMinusErrors[i][j] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
            sprintf( line, "ptPSigErrors_%i_%i", i, j );
            ptPSigErrors[i][j] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
            sprintf( line, "ptPDelErrors_%i_%i", i, j );
            ptPDelErrors[i][j] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
            sprintf( line, "ptPPlusErrors_%i_%i", i, j );
            ptPPlusErrors[i][j] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
            sprintf( line, "ptPMinusErrors_%i_%i", i, j );
            ptPMinusErrors[i][j] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
            sprintf( line, "ptPPlusMinusErrors_%i_%i", i, j );
            ptPPlusMinusErrors[i][j] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
            sprintf( line, "ptPNSigErrors_%i_%i", i, j );
            ptPNSigErrors[i][j] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
            sprintf( line, "ptPNDelErrors_%i_%i", i, j );
            ptPNDelErrors[i][j] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
            sprintf( line, "ptPNPlusErrors_%i_%i", i, j );
            ptPNPlusErrors[i][j] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
            sprintf( line, "ptPNMinusErrors_%i_%i", i, j );
            ptPNMinusErrors[i][j] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
            sprintf( line, "ptPNPlusMinusErrors_%i_%i", i, j );
            ptPNPlusMinusErrors[i][j] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
            sprintf( line, "ptPNMinusPlusErrors_%i_%i", i, j );
            ptPNMinusPlusErrors[i][j] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);

            sprintf( line, "ptsigPtSHatErrors_%i_%i", i, j );
            ptsigSPtHatErrors[i][j] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
            sprintf( line, "ptsigPtPHatErrors_%i_%i", i, j );
            ptsigPPtHatErrors[i][j] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
            sprintf( line, "ptsigPtMHatErrors_%i_%i", i, j );
            ptsigMPtHatErrors[i][j] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
        }
    }
    for (int j=0;j<NPTBINS;j++) {
    }
}

//--------------------------------------------------------------------------
void StEStructSigmas::deleteArraysAndHistograms() {

cout << "Deleting bin***Var2, bin***Errors histograms." << endl;
    for (int i=0;i<NSCENTBINS;i++) {
        delete NSig[i];
        delete NDel[i];
        delete NPlus[i];
        delete NMinus[i];
        delete NPlusMinus[i];
        delete PSig[i];
        delete PDel[i];
        delete PPlus[i];
        delete PMinus[i];
        delete PPlusMinus[i];
        delete PNSig[i];
        delete PNDel[i];
        delete PNPlus[i];
        delete PNMinus[i];
        delete PNPlusMinus[i];
        delete PNMinusPlus[i];

        delete SPtHat[i];
        delete PPtHat[i];
        delete MPtHat[i];
        delete sigSPtHat[i];
        delete sigPPtHat[i];
        delete sigMPtHat[i];

        delete NSigErrors[i];
        delete NDelErrors[i];
        delete NPlusErrors[i];
        delete NMinusErrors[i];
        delete NPlusMinusErrors[i];
        delete PSigErrors[i];
        delete PDelErrors[i];
        delete PPlusErrors[i];
        delete PMinusErrors[i];
        delete PPlusMinusErrors[i];
        delete PNSigErrors[i];
        delete PNDelErrors[i];
        delete PNPlusErrors[i];
        delete PNMinusErrors[i];
        delete PNPlusMinusErrors[i];
        delete PNMinusPlusErrors[i];

        delete sigSPtHatErrors[i];
        delete sigPPtHatErrors[i];
        delete sigMPtHatErrors[i];
    }

cout << "Deleting pt***Var2, bin***Errors histograms." << endl;
    for (int i=0;i<NPTSCENTBINS;i++) {
        for (int j=0;j<NPTBINS;j++) {
            delete ptNSig[i][j];
            delete ptNDel[i][j];
            delete ptNPlus[i][j];
            delete ptNMinus[i][j];
            delete ptNPlusMinus[i][j];
            delete ptPSig[i][j];
            delete ptPDel[i][j];
            delete ptPPlus[i][j];
            delete ptPMinus[i][j];
            delete ptPPlusMinus[i][j];
            delete ptPNSig[i][j];
            delete ptPNDel[i][j];
            delete ptPNPlus[i][j];
            delete ptPNMinus[i][j];
            delete ptPNPlusMinus[i][j];
            delete ptPNMinusPlus[i][j];

            delete ptSPtHat[i][j];
            delete ptPPtHat[i][j];
            delete ptMPtHat[i][j];
            delete ptsigSPtHat[i][j];
            delete ptsigPPtHat[i][j];
            delete ptsigMPtHat[i][j];

            delete ptNSigErrors[i][j];
            delete ptNDelErrors[i][j];
            delete ptNPlusErrors[i][j];
            delete ptNMinusErrors[i][j];
            delete ptNPlusMinusErrors[i][j];
            delete ptPSigErrors[i][j];
            delete ptPDelErrors[i][j];
            delete ptPPlusErrors[i][j];
            delete ptPMinusErrors[i][j];
            delete ptPPlusMinusErrors[i][j];
            delete ptPNSigErrors[i][j];
            delete ptPNDelErrors[i][j];
            delete ptPNPlusErrors[i][j];
            delete ptPNMinusErrors[i][j];
            delete ptPNPlusMinusErrors[i][j];
            delete ptPNMinusPlusErrors[i][j];

            delete ptsigSPtHatErrors[i][j];
            delete ptsigPPtHatErrors[i][j];
            delete ptsigMPtHatErrors[i][j];
        }
    }
}
