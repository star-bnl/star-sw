
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
    TH1D *hNDel[2];
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

            sprintf(buffer,"NDel%i_%i",jCent,jStat);
            hNDel[jStat] = (TH1D *) gDirectory->Get(buffer);

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
                        NS1  = hNSum[0]->GetBinContent(iBin);
                        NS2  = hNSum[1]->GetBinContent(iBin);
                        if (NS1 > 0) {
                            nSig    = (NS2 - NS1*NS1/sumEvents) / NS1;
                            SSig   += nSig - 1;
                            NSSig  += 1;
                            DSSig  += (4 + nSig/(NS1/sumEvents)) * nSig / sumEvents;
                            NDSSig += sqrt(hfUnique->GetBinContent(iPhi,iEta));

                            ND1     = hNDel[0]->GetBinContent(iBin);
                            ND2     = hNDel[1]->GetBinContent(iBin);
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
        PPlus[jCent]->Reset();
        PMinus[jCent]->Reset();
        PPlusMinus[jCent]->Reset();

        PSigErrors[jCent]->Reset();
        PPlusErrors[jCent]->Reset();
        PMinusErrors[jCent]->Reset();
        PPlusMinusErrors[jCent]->Reset();

        SPtHat[jCent]->Reset();
        PPtHat[jCent]->Reset();
        MPtHat[jCent]->Reset();
        sigSPtHat[jCent]->Reset();
        sigPPtHat[jCent]->Reset();
        sigMPtHat[jCent]->Reset();
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
            sprintf(buffer,"PPlusMinus%i_%i",jCent,jStat);
            hPPlusMinus[jStat] = (TH1D *) gDirectory->Get(buffer);
        }

        int mBin, oBin;

        for (int iPhi=1;iPhi<=NPhiBins;iPhi++) {
            for (int iEta=1;iEta<=NEtaBins;iEta++) {
                double NS1, PS1, PS2, PS3, PS4, PS5, DSsig = 0, NDSsig = 0;
                double ssig, Ssig = 0, NSsig = 0, ptSHat = 0, sigptSHat = 0;
                double NP1, PP1, PP2, PP3, PP4, PP5, DPsig = 0, NDPsig = 0;
                double Psig = 0, NPsig = 0, ptPHat = 0, sigptPHat = 0;
                double NM1, PM1, PM2, PM3, PM4, PM5, DMsig = 0, NDMsig = 0;
                double Msig = 0, NMsig = 0, ptMHat = 0, sigptMHat = 0;
                double PPM1, PPM2, PPM3,  PPM4, PMsig = 0, NPMsig = 0;
                double sigPtHat, PHat, MHat, DsigptHat;
                double sumEvents, plusEvents, minusEvents, plusMinusEvents;

                mBin = (int) hnBins->GetBinContent(iPhi,iEta);
                oBin = (int) hoffset->GetBinContent(iPhi,iEta);


                for (int iBin=oBin+1;iBin<=oBin+mBin;iBin++) {
                    sumEvents = hTotEvents[1]->GetBinContent(iBin);
                    if (sumEvents > 0) {
                        NS1 = hNSum[0]->GetBinContent(iBin) / sumEvents;
                        PS1 = hPSum[0]->GetBinContent(iBin) / sumEvents;
                        PS2 = hPSum[1]->GetBinContent(iBin) / sumEvents;
                        PS3 = hPSum[2]->GetBinContent(iBin) / sumEvents;
                        PS4 = hPSum[3]->GetBinContent(iBin) / sumEvents;
                        PS5 = hPSum[4]->GetBinContent(iBin) / sumEvents;
                        if (NS1 > 0) {
                            double R1  = PS1 / NS1;
                            double R2  = R1*R1;
                            ptSHat    += R1;
                            sigPtHat   = PS5/NS1 - R1*R1;
                            sigptSHat += sigPtHat;
                            DsigptHat  = sigPtHat*sigPtHat / (NS1*sumEvents);
                            ssig       = PS2 - PS1*PS1/NS1;
                            Ssig      += ssig - sigPtHat;
                            NSsig     += 1;
// Didn't accumulate Sum(p/n) !!!
// Omit first term from error for now.
//                            DSsig     += (4*R1*(PS3-2*PS1*R1+R1*R1) +
                           DSsig      += (NS1*(PS4-2*PS3*R2+R2*R2)) / sumEvents
                                           + DsigptHat;
                            NDSsig    += sqrt(hfUnique->GetBinContent(iPhi,iEta));
                        }
                    }

                    plusEvents = hTotEvents[2]->GetBinContent(iBin);
                    PHat = 0;
                    if (plusEvents > 0) {
                        NP1 = hNPlus[0]->GetBinContent(iBin) / plusEvents;
                        PP1 = hPPlus[0]->GetBinContent(iBin) / plusEvents;
                        PP2 = hPPlus[1]->GetBinContent(iBin) / plusEvents;
                        PP3 = hPPlus[2]->GetBinContent(iBin) / plusEvents;
                        PP4 = hPPlus[3]->GetBinContent(iBin) / plusEvents;
                        PP5 = hPPlus[4]->GetBinContent(iBin) / plusEvents;
                        if (NP1 > 0) {
                            double R1  = PP1 / NP1;
                            double R2  = R1*R1;
                            ptPHat    += R1;
                            sigPtHat   = PP5/NP1 - R1*R1;
                            sigptPHat += sigPtHat;
                            DsigptHat  = pow(sigPtHat+R1*R1,2) / plusEvents;
                            Psig      += PP2 - PP1*PP1/NP1 - sigPtHat;
// Didn't accumulate Sum(p/n) !!!
// Omit first term from error for now.
//                            DPsig     += (4*R1*(PP3-2*PP1*R1+R1*R1) +
                            DPsig     += (NP1*(PP4-2*PP3*R2+R2*R2)) / plusEvents
                                           + DsigptHat;
                            NPsig     += 1;
                            NDPsig    += sqrt(hfUnique->GetBinContent(iPhi,iEta));
                        }
                    }

                    minusEvents = hTotEvents[3]->GetBinContent(iBin);
                    MHat = 0;
                    if (minusEvents > 0) {
                        NM1 = hNMinus[0]->GetBinContent(iBin) / minusEvents;
                        PM1 = hPMinus[0]->GetBinContent(iBin) / minusEvents;
                        PM2 = hPMinus[1]->GetBinContent(iBin) / minusEvents;
                        PM3 = hPMinus[2]->GetBinContent(iBin) / minusEvents;
                        PM4 = hPMinus[3]->GetBinContent(iBin) / minusEvents;
                        PM5 = hPMinus[4]->GetBinContent(iBin) / minusEvents;
                        if (NM1 > 0) {
                            double R1  = PM1 / NM1;
                            double R2  = R1*R1;
                            ptMHat    += R1;
                            sigPtHat   = PM5/NM1 - R1*R1;
                            sigptMHat += sigPtHat;
                            DsigptHat  = pow(sigPtHat+R1*R1,2) / minusEvents;
                            Msig      += PM2 - PM1*PM1/NM1 - sigPtHat;
// Didn't accumulate Sum(p/n) !!!
// Omit first term from error for now.
//                            DMsig     += (4*R1*(PM3-2*PM1*R1+R1*R1) +
                            DMsig     += (NM1*(PM4-2*PM3*R2+R2*R2)) / minusEvents
                                           + DsigptHat;
                            NMsig     += 1;
                            NDMsig    += sqrt(hfUnique->GetBinContent(iPhi,iEta));
                        }
                    }

                    plusMinusEvents = hTotEvents[4]->GetBinContent(iBin);
                    if (plusMinusEvents > 0) {
                        NP1  = hPPlusMinus[4]->GetBinContent(iBin);
                        PP1  = hPPlusMinus[5]->GetBinContent(iBin);
                        PHat = PP1 / NP1;
                        NM1  = hPPlusMinus[6]->GetBinContent(iBin);
                        PM1  = hPPlusMinus[7]->GetBinContent(iBin);
                        MHat = PM1 / NM1;
                        PPM1 = PHat*MHat*hPPlusMinus[0]->GetBinContent(iBin);
                        PPM2 = MHat*hPPlusMinus[1]->GetBinContent(iBin);
                        PPM3 = PHat*hPPlusMinus[2]->GetBinContent(iBin);
                        PPM4 = hPPlusMinus[3]->GetBinContent(iBin);
                        PMsig  += (PPM1 - PPM2 - PPM3 + PPM4) / plusMinusEvents;
                        NPMsig += 1;
                    }
                }

                if (NSsig > 0) {
                    PSig[jCent]->SetBinContent(iPhi,iEta,Ssig/NSsig);
                    PSigErrors[jCent]->SetBinContent(iPhi,iEta,sqrt(DSsig)/NDSsig);
                    SPtHat[jCent]->SetBinContent(iPhi,iEta,ptSHat/NSsig);
                    sigSPtHat[jCent]->SetBinContent(iPhi,iEta,sigptSHat/NSsig);
                }

                if (NPsig > 0) {
                    PPlus[jCent]->SetBinContent(iPhi,iEta,Psig/NPsig);
                    PPlusErrors[jCent]->SetBinContent(iPhi,iEta,sqrt(DPsig)/NDPsig);
                    PPtHat[jCent]->SetBinContent(iPhi,iEta,ptPHat/NPsig);
                    sigPPtHat[jCent]->SetBinContent(iPhi,iEta,sigptPHat/NPsig);
                }

                if (NMsig > 0) {
                    PMinus[jCent]->SetBinContent(iPhi,iEta,Msig/NMsig);
                    PMinusErrors[jCent]->SetBinContent(iPhi,iEta,sqrt(DMsig)/NDMsig);
                    MPtHat[jCent]->SetBinContent(iPhi,iEta,ptMHat/NMsig);
                    sigMPtHat[jCent]->SetBinContent(iPhi,iEta,sigptMHat/NMsig);
                }

                if (NPMsig > 0) {
                    PMsig = PMsig / NPMsig;
                    PPlusMinus[jCent]->SetBinContent(iPhi,iEta,PMsig);
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
        PNPlus[jCent]->Reset();
        PNMinus[jCent]->Reset();
        PNPlusMinus[jCent]->Reset();
        PNMinusPlus[jCent]->Reset();

        PNSigErrors[jCent]->Reset();
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
    TH1D *hPSum[4];
    TH1D *hPNSum[4];
    TH1D *hNPlus[2];
    TH1D *hPPlus[4];
    TH1D *hPNPlus[4];
    TH1D *hNMinus[2];
    TH1D *hPMinus[4];
    TH1D *hPNMinus[4];
    TH1D *hPNPlusMinus[12];

    for (int jCent=0;jCent<NSCENTBINS;jCent++) {
        // Note that NTot, NTotPlus and NTotMinus are summed over events.
        // In my notes I use event averaged numbers and get an extra term
        // of NEvent.

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

        for (int jStat=0;jStat<4;jStat++) {
            sprintf(buffer,"PSum%i_%i",jCent,jStat);
            hPSum[jStat] = (TH1D *) gDirectory->Get(buffer);

            sprintf(buffer,"PPlus%i_%i",jCent,jStat);
            hPPlus[jStat] = (TH1D *) gDirectory->Get(buffer);

            sprintf(buffer,"PMinus%i_%i",jCent,jStat);
            hPMinus[jStat] = (TH1D *) gDirectory->Get(buffer);
        }
        for (int jStat=0;jStat<4;jStat++) {
            sprintf(buffer,"PNSum%i_%i",jCent,jStat);
            hPNSum[jStat] = (TH1D *) gDirectory->Get(buffer);

            sprintf(buffer,"PNPlus%i_%i",jCent,jStat);
            hPNPlus[jStat] = (TH1D *) gDirectory->Get(buffer);

            sprintf(buffer,"PNMinus%i_%i",jCent,jStat);
            hPNMinus[jStat] = (TH1D *) gDirectory->Get(buffer);
        }
        for (int jStat=0;jStat<12;jStat++) {
            sprintf(buffer,"PNPlusMinus%i_%i",jCent,jStat);
            hPNPlusMinus[jStat] = (TH1D *) gDirectory->Get(buffer);
        }

        int mBin, oBin;

        for (int iPhi=1;iPhi<=NPhiBins;iPhi++) {
            for (int iEta=1;iEta<=NEtaBins;iEta++) {
                double sumEvents, plusEvents, minusEvents, plusMinusEvents;
                double SHat, PHat, MHat;
                double NS1, PS1, PNS1, PNS2, PNS3, PNS4, Ssig = 0, NSsig = 0;
                double NP1, PP1, PNP1, PNP2, PNP3, PNP4, Psig = 0, NPsig = 0;
                double NM1, PM1, PNM1, PNM2, PNM3, PNM4, Msig = 0, NMsig = 0;
                double PNPM1, PNPM2, PNPM3, PNPM4, PMsig = 0, NPMsig = 0;
                double PNMP1, PNMP2, PNMP3, PNMP4, MPsig = 0, NMPsig = 0;

                mBin = (int) hnBins->GetBinContent(iPhi,iEta);
                oBin = (int) hoffset->GetBinContent(iPhi,iEta);

                for (int iBin=oBin+1;iBin<=oBin+mBin;iBin++) {
                    sumEvents = hTotEvents[1]->GetBinContent(iBin);
                    if (sumEvents > 0) {
                        NS1 = hNSum[0]->GetBinContent(iBin) / sumEvents;
                        PS1 = hPSum[0]->GetBinContent(iBin) / sumEvents;
                        SHat = PS1 / NS1;
                        PNS1 = SHat*NS1*hPNSum[0]->GetBinContent(iBin) / sumEvents;
                        PNS2 = SHat*hPNSum[1]->GetBinContent(iBin) / sumEvents;
                        PNS3 = NS1*hPNSum[2]->GetBinContent(iBin) / sumEvents;
                        PNS4 = hPNSum[3]->GetBinContent(iBin) / sumEvents;
                        Ssig  += (PNS1 - PNS2 - PNS3 + PNS4) / sqrt(NS1);
                        NSsig +=1;
                    }

                    plusEvents = hTotEvents[2]->GetBinContent(iBin);
                    if (plusEvents > 0) {
                        NP1 = hNPlus[0]->GetBinContent(iBin) / plusEvents;
                        PP1 = hPPlus[0]->GetBinContent(iBin) / plusEvents;
                        PHat = PP1 / NP1;
                        PNP1 = PHat* NP1*hPNPlus[0]->GetBinContent(iBin) / plusEvents;
                        PNP2 = PHat*hPNPlus[1]->GetBinContent(iBin) / plusEvents;
                        PNP3 = NP1*hPNPlus[2]->GetBinContent(iBin) / plusEvents;
                        PNP4 = hPNPlus[3]->GetBinContent(iBin) / plusEvents;
                        Psig  += (PNP1 - PNP2 - PNP3 + PNP4) / sqrt(NP1);
                        NPsig += 1;
                    }

                    minusEvents = hTotEvents[3]->GetBinContent(iBin);
                    if (minusEvents > 0) {
                        NM1 = hNMinus[0]->GetBinContent(iBin) / minusEvents;
                        PM1 = hPMinus[0]->GetBinContent(iBin) / minusEvents;
                        MHat = PM1 / NM1;
                        PNM1 = MHat*NM1*hPNMinus[0]->GetBinContent(iBin);
                        PNM2 = MHat*hPNMinus[1]->GetBinContent(iBin);
                        PNM3 = NM1*hPNMinus[2]->GetBinContent(iBin);
                        PNM4 = hPNMinus[3]->GetBinContent(iBin);
                        Msig  += (PNM1 - PNM2 - PNM3 + PNM4) / (minusEvents*sqrt(NM1));
                        NMsig += 1;
                    }

                    plusMinusEvents = hTotEvents[4]->GetBinContent(iBin);
                    if (plusMinusEvents > 0) {
                        NP1   = hPNPlusMinus[4]->GetBinContent(iBin) / plusMinusEvents;
                        PP1   = hPNPlusMinus[5]->GetBinContent(iBin) / plusMinusEvents;
                        PHat  = PP1 / NP1;
                        NM1   = hPNPlusMinus[8]->GetBinContent(iBin) / plusMinusEvents;
                        PM1   = hPNPlusMinus[9]->GetBinContent(iBin) / plusMinusEvents;
                        MHat  = PM1 / NM1;
                        PNPM1 = hPNPlusMinus[0]->GetBinContent(iBin) / plusMinusEvents;
                        PNPM2 = PHat*hPNPlusMinus[1]->GetBinContent(iBin) / plusMinusEvents;
                        PNPM3 = NM1*hPNPlusMinus[7]->GetBinContent(iBin) / plusMinusEvents;
                        PNPM4 = PHat*NM1*hPNPlusMinus[6]->GetBinContent(iBin) / plusMinusEvents;
                        PMsig  += (PNPM1 - PNPM2 - PNPM3 + PNPM4) / sqrt(NM1);
                        NPMsig += 1;

                        PNMP1 = hPNPlusMinus[2]->GetBinContent(iBin) / plusMinusEvents;
                        PNMP2 = MHat*hPNPlusMinus[3]->GetBinContent(iBin) / plusMinusEvents;
                        PNMP3 = NP1*hPNPlusMinus[11]->GetBinContent(iBin) / plusMinusEvents;
                        PNMP4 = MHat*NP1*hPNPlusMinus[10]->GetBinContent(iBin) / plusMinusEvents;
                        MPsig  += (PNMP1 - PNMP2 - PNMP3 + PNMP4) / sqrt(NP1);
                        NMPsig += 1;
                    }
                }

                if (NSsig > 0) {
                    Ssig = Ssig / NSsig;
                    PNSig[jCent]->SetBinContent(iPhi,iEta,Ssig);
                }
                if (NPsig > 0) {
                    Psig = Psig / NPsig;
                    PNPlus[jCent]->SetBinContent(iPhi,iEta,Psig);
                }
                if (NMsig > 0) {
                    Msig = Msig / NMsig;
                    PNMinus[jCent]->SetBinContent(iPhi,iEta,Msig);
                }
                if (NPMsig) {
                    PMsig = PMsig / NPMsig;
                    PNPlusMinus[jCent]->SetBinContent(iPhi,iEta,PMsig);
                }
                if (NMPsig) {
                    MPsig = MPsig / NMPsig;
                    PNMinusPlus[jCent]->SetBinContent(iPhi,iEta,MPsig);
                }
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
    TH1D *hptNDel[2];
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

                sprintf(buffer,"ptNDel%i_%i_%i",jPtCent,jPt,jStat);
                hptNDel[jStat] = (TH1D *) gDirectory->Get(buffer);

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
                            if (NS1 > 0) {
                                sSig    = (NS2 - NS1*NS1/sumEvents) / NS1;
                                SSig   += sSig - 1;
                                NSSig  += 1;
                                DSSig  += (4 + sSig/(NS1/sumEvents)) * sSig / sumEvents;
                                NDSSig += sqrt(hfUnique->GetBinContent(iPhi,iEta));

                                ND1     = hptNDel[0]->GetBinContent(iBin);
                                ND2     = hptNDel[1]->GetBinContent(iBin);
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
            ptPPlus[jPtCent][jPt]->Reset();
            ptPMinus[jPtCent][jPt]->Reset();
            ptPPlusMinus[jPtCent][jPt]->Reset();

            ptPSigErrors[jPtCent][jPt]->Reset();
            ptPPlusErrors[jPtCent][jPt]->Reset();
            ptPMinusErrors[jPtCent][jPt]->Reset();
            ptPPlusMinusErrors[jPtCent][jPt]->Reset();

            ptSPtHat[jPtCent][jPt]->Reset();
            ptPPtHat[jPtCent][jPt]->Reset();
            ptMPtHat[jPtCent][jPt]->Reset();
            ptsigSPtHat[jPtCent][jPt]->Reset();
            ptsigPPtHat[jPtCent][jPt]->Reset();
            ptsigMPtHat[jPtCent][jPt]->Reset();
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
                sprintf(buffer,"ptPPlusMinus%i_%i_%i",jPtCent,jPt,jStat);
                hptPPlusMinus[jStat] = (TH1D *) gDirectory->Get(buffer);
            }

            int mBin, oBin;
            int iBin = 1 + (int) hoffset->GetBinContent(NPhiBins,NEtaBins);
            double NTot      = hptNSum[0]->GetBinContent(iBin);
            double NTotPlus  = hptNPlus[0]->GetBinContent(iBin);
            double NTotMinus = hptNMinus[0]->GetBinContent(iBin);

            for (int iPhi=1;iPhi<=NPhiBins;iPhi++) {
                for (int iEta=1;iEta<=NEtaBins;iEta++) {
                    double NSe1, NSb1 = 0, PSe1, PSe2, PSe3, PSe4;
                    double PSNbSig = 0, PSPbSig = 0;
                    double NPe1, NPb1 = 0, PPe1, PPe2, PPe3, PPe4;
                    double PPNbSig = 0, PPPbSig = 0;
                    double NMe1, NMb1 = 0, PMe1, PMe2, PMe3, PMe4;
                    double PMNbSig = 0, PMPbSig = 0;
                    double PPMe1, PPMe2, PPMe3, PPMe4;
                    double totBins = 0, sumBins = 0, plusBins = 0;
                    double minusBins = 0, plusMinusBins = 0;
                    double ptSHat = 0, sigptSHat = 0;
                    double ptPHat = 0, sigptPHat = 0;
                    double ptMHat = 0, sigptMHat = 0;
                    double sigPtHat, PHat, MHat;
                    double sumEvents, plusEvents, minusEvents, plusMinusEvents;
                    int NSum = 0, NPlus = 0, NMinus = 0, NPlusMinus = 0;

                    mBin = (int) hnBins->GetBinContent(iPhi,iEta);
                    oBin = (int) hoffset->GetBinContent(iPhi,iEta);

                    double SumSig = 0, PlusSig = 0, MinusSig = 0, PMSig = 0;
                    for (int iBin=oBin+1;iBin<=oBin+mBin;iBin++) {
                        totBins       += hptTotEvents[0]->GetBinContent(iBin);
                        sumBins       += hptTotEvents[1]->GetBinContent(iBin);
                        plusBins      += hptTotEvents[2]->GetBinContent(iBin);
                        minusBins     += hptTotEvents[3]->GetBinContent(iBin);
                        plusMinusBins += hptTotEvents[4]->GetBinContent(iBin);

                        sumEvents = hptTotEvents[1]->GetBinContent(iBin);
                        if (sumEvents > 0) {
                            NSe1  = hptNSum[0]->GetBinContent(iBin);
                            NSb1 += NSe1;
                            PSe1  = hptPSum[0]->GetBinContent(iBin);
                            PSe2  = hptPSum[1]->GetBinContent(iBin);
                            PSe3  = hptPSum[2]->GetBinContent(iBin);
                            PSe4  = hptPSum[3]->GetBinContent(iBin);
                            double SHat = 0;
                            if (NSe1 > 0) {
                                SHat       = PSe1 / NSe1;
                                SumSig    += (PSe2 - PSe1*PSe1/NSe1) / sumEvents;
                                ptSHat    += PSe1/NSe1;
                                sigPtHat   = hptPSum[4]->GetBinContent(iBin)/NSe1 - SHat*SHat;
                                sigptSHat += sigPtHat;
                                NSum++;
                            }
                            PSNbSig += PSe4 - 2*PSe3*pow(SHat,2) + pow(SHat,4);
                            PSPbSig += PSe3 - 2*PSe1*SHat + pow(SHat,2);
                        }

                        plusEvents = hptTotEvents[2]->GetBinContent(iBin);
                        if (plusEvents > 0) {
                            NPe1  = hptNPlus[0]->GetBinContent(iBin);
                            NPb1 += NPe1;
                            PPe1  = hptPPlus[0]->GetBinContent(iBin);
                            PPe2  = hptPPlus[1]->GetBinContent(iBin);
                            PPe3  = hptPPlus[2]->GetBinContent(iBin);
                            PPe4  = hptPPlus[3]->GetBinContent(iBin);
                            double PHat = 0;
                            if (NPe1 > 0) {
                                PHat       = PPe1 / NPe1;
                                PlusSig   += (PPe2 - PPe1*PPe1/NPe1) / plusEvents;
                                ptPHat    += PPe1/NPe1;
                                sigPtHat   = hptPPlus[4]->GetBinContent(iBin)/NPe1 - PHat*PHat;
                                sigptPHat += sigPtHat;
                                NPlus++;
                            }
                            PPNbSig += PPe4 - 2*PPe3*pow(PHat,2) + pow(PHat,4);
                            PPPbSig += PPe3 - 2*PPe1*PHat + pow(PHat,2);
                        }

                        minusEvents = hptTotEvents[3]->GetBinContent(iBin);
                        if (minusEvents > 0) {
                            NMe1  = hptNMinus[0]->GetBinContent(iBin);
                            NMb1 += NMe1;
                            PMe1  = hptPMinus[0]->GetBinContent(iBin);
                            PMe2  = hptPMinus[1]->GetBinContent(iBin);
                            PMe3  = hptPMinus[2]->GetBinContent(iBin);
                            PMe4  = hptPMinus[3]->GetBinContent(iBin);
                            double MHat = 0;
                            if (NMe1 > 0) {
                                MHat       = PMe1 / NMe1;
                                MinusSig  += (PMe2 - PMe1*PMe1/NMe1) / minusEvents;
                                ptMHat    += PMe1/NMe1;
                                sigPtHat   = hptPMinus[4]->GetBinContent(iBin)/NMe1 - MHat*MHat;
                                sigptMHat += sigPtHat;
                                NMinus++;
                            }
                            PMNbSig += PMe4 - 2*PMe3*pow(MHat,2) + pow(MHat,4);
                            PMPbSig += PMe3 - 2*PMe1*MHat + pow(MHat,2);
                        }

                        plusMinusEvents = hptTotEvents[4]->GetBinContent(iBin);
                        if (plusMinusEvents > 0) {
                            NPe1  = hptPPlusMinus[4]->GetBinContent(iBin);
                            PPe1  = hptPPlusMinus[5]->GetBinContent(iBin);
                            PHat = PPe1 / NPe1;
                            NMe1  = hptPPlusMinus[6]->GetBinContent(iBin);
                            PMe1  = hptPPlusMinus[7]->GetBinContent(iBin);
                            MHat = PMe1 / NMe1;
                            PPMe1 = PHat*MHat*hptPPlusMinus[0]->GetBinContent(iBin);
                            PPMe2 = MHat*hptPPlusMinus[1]->GetBinContent(iBin);
                            PPMe3 = PHat*hptPPlusMinus[2]->GetBinContent(iBin);
                            PPMe4 = hptPPlusMinus[3]->GetBinContent(iBin);
                            PMSig += (PPMe1 - PPMe2 - PPMe3 + PPMe4) / plusMinusEvents;
                            NPlusMinus++;
                        }
                    }

                    if (NSum > 0) {
                        SumSig    = SumSig / NSum;
                        ptSHat    = ptSHat / NSum;
                        sigptSHat = sigptSHat / NSum;
                        ptPSig[jPtCent][jPt]->SetBinContent(iPhi,iEta,SumSig-sigptSHat);
                        ptSPtHat[jPtCent][jPt]->SetBinContent(iPhi,iEta,ptSHat);
                        ptsigSPtHat[jPtCent][jPt]->SetBinContent(iPhi,iEta,sigptSHat);
                    } else {
                        ptPSig[jPtCent][jPt]->SetBinContent(iPhi,iEta,0);
                        ptSPtHat[jPtCent][jPt]->SetBinContent(iPhi,iEta,0);
                        ptsigSPtHat[jPtCent][jPt]->SetBinContent(iPhi,iEta,0);
                    }

                    if (NPlus > 0) {
                        PlusSig   = PlusSig / NPlus;
                        ptPHat    = ptPHat / NPlus;
                        sigptPHat = sigptPHat / NPlus;
                        ptPPlus[jPtCent][jPt]->SetBinContent(iPhi,iEta,PlusSig-sigptPHat);
                        ptPPtHat[jPtCent][jPt]->SetBinContent(iPhi,iEta,ptPHat);
                        ptsigPPtHat[jPtCent][jPt]->SetBinContent(iPhi,iEta,sigptPHat);
                    } else {
                        ptPPlus[jPtCent][jPt]->SetBinContent(iPhi,iEta,0);
                        ptPPtHat[jPtCent][jPt]->SetBinContent(iPhi,iEta,0);
                        ptsigPPtHat[jPtCent][jPt]->SetBinContent(iPhi,iEta,0);
                    }

                    if (NMinus > 0) {
                        MinusSig  = MinusSig / NMinus;
                        ptMHat    = ptMHat / NMinus;
                        sigptMHat = sigptMHat / NMinus;
                        ptPMinus[jPtCent][jPt]->SetBinContent(iPhi,iEta,MinusSig-sigptMHat);
                        ptMPtHat[jPtCent][jPt]->SetBinContent(iPhi,iEta,ptMHat);
                        ptsigMPtHat[jPtCent][jPt]->SetBinContent(iPhi,iEta,sigptMHat);
                    } else {
                        ptPMinus[jPtCent][jPt]->SetBinContent(iPhi,iEta,0);
                        ptMPtHat[jPtCent][jPt]->SetBinContent(iPhi,iEta,0);
                        ptsigMPtHat[jPtCent][jPt]->SetBinContent(iPhi,iEta,0);
                    }

                    if (NPlusMinus > 0) {
                        PMSig = PMSig / NPlusMinus;
                        ptPPlusMinus[jPtCent][jPt]->SetBinContent(iPhi,iEta,PMSig);
                    } else {
                        ptPPlusMinus[jPtCent][jPt]->SetBinContent(iPhi,iEta,0);
                    }

                    if (NSum > 0) {
                        double nSbar = NSb1 / NSum;
                        double SErr = nSbar*(1-nSbar/NTot)*PSNbSig +
                                      4*SumSig*PSPbSig / sumBins;
                        SErr = sqrt(SErr) / sumBins;
                        ptPSigErrors[jPtCent][jPt]->SetBinContent(iPhi,iEta,SErr);
                    } else {
                        ptPSigErrors[jPtCent][jPt]->SetBinContent(iPhi,iEta,0.001);
                    }

                    if (NPlus > 0) {
                        double nPbar = NPb1 / NPlus;
                        double PErr = nPbar*(1-nPbar/NTotPlus)*PPNbSig +
                                      4*PlusSig*PPPbSig / plusBins;
                        PErr = sqrt(PErr) / plusBins;
                        ptPPlusErrors[jPtCent][jPt]->SetBinContent(iPhi,iEta,PErr);
                    } else {
                        ptPPlusErrors[jPtCent][jPt]->SetBinContent(iPhi,iEta,0.001);
                    }

                    if (NMinus > 0) {
                        double nMbar = NMb1 / NMinus;
                        double MErr = nMbar*(1-nMbar/NTotMinus)*PMNbSig +
                                      4*MinusSig*PMPbSig / minusBins;
                        MErr = sqrt(MErr) / minusBins;
                        ptPMinusErrors[jPtCent][jPt]->SetBinContent(iPhi,iEta,MErr);
                    } else {
                        ptPMinusErrors[jPtCent][jPt]->SetBinContent(iPhi,iEta,0.001);
                    }
                    
                    // Haven't worked out error on plus-minus covariance.
                    double PMErr = PMSig / mBin;
                    if (NPlusMinus > 0) {
                        PMErr = sqrt(PMErr) / NPlusMinus;
                        ptNPlusMinusErrors[jPtCent][jPt]->SetBinContent(iPhi,iEta,1/sqrt(plusMinusBins));
                    } else {
                        ptNPlusMinusErrors[jPtCent][jPt]->SetBinContent(iPhi,iEta,0.001);
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
            ptPNPlus[jPtCent][j]->Reset();
            ptPNMinus[jPtCent][j]->Reset();
            ptPNPlusMinus[jPtCent][j]->Reset();

            ptPNSigErrors[jPtCent][j]->Reset();
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
    TH1D *hptNPlus[2];
    TH1D *hptNMinus[2];
    TH1D *hptPSum[2];
    TH1D *hptPPlus[2];
    TH1D *hptPMinus[2];
    TH1D *hptPNSum[4];
    TH1D *hptPNPlus[4];
    TH1D *hptPNMinus[4];
    TH1D *hptPNPlusMinus[12];

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
            for (int jStat=0;jStat<2;jStat++) {
                sprintf(buffer,"ptPSum%i_%i_%i",jPtCent,jPt,jStat);
                hptPSum[jStat] = (TH1D *) gDirectory->Get(buffer);

                sprintf(buffer,"ptPPlus%i_%i_%i",jPtCent,jPt,jStat);
                hptPPlus[jStat] = (TH1D *) gDirectory->Get(buffer);

                sprintf(buffer,"ptPMinus%i_%i_%i",jPtCent,jPt,jStat);
                hptPMinus[jStat] = (TH1D *) gDirectory->Get(buffer);
            }
            for (int jStat=0;jStat<4;jStat++) {
                sprintf(buffer,"ptPNSum%i_%i_%i",jPtCent,jPt,jStat);
                hptPNSum[jStat] = (TH1D *) gDirectory->Get(buffer);

                sprintf(buffer,"ptPNPlus%i_%i_%i",jPtCent,jPt,jStat);
                hptPNPlus[jStat] = (TH1D *) gDirectory->Get(buffer);

                sprintf(buffer,"ptPNMinus%i_%i_%i",jPtCent,jPt,jStat);
                hptPNMinus[jStat] = (TH1D *) gDirectory->Get(buffer);
            }
            for (int jStat=0;jStat<12;jStat++) {
                sprintf(buffer,"ptPNPlusMinus%i_%i_%i",jPtCent,jPt,jStat);
                hptPNPlusMinus[jStat] = (TH1D *) gDirectory->Get(buffer);
            }

            int mBin, oBin;

            for (int iPhi=1;iPhi<=NPhiBins;iPhi++) {
                for (int iEta=1;iEta<=NEtaBins;iEta++) {
                    double SHat, PHat, MHat;
                    double NSe1, PSe1, PNSe1, PNSe2, PNSe3, PNSe4;
                    double NPe1, PPe1, PNPe1, PNPe2, PNPe3, PNPe4;
                    double NMe1, PMe1, PNMe1, PNMe2, PNMe3, PNMe4;
                    double PNPMe1, PNPMe2, PNPMe3, PNPMe4;
                    double PNMPe1, PNMPe2, PNMPe3, PNMPe4;
                    double totBins = 0, totEvents;
                    double totSumBins = 0, totPlusBins = 0;
                    double totMinusBins = 0, totPlusMinusBins = 0;
                    double sumEvents, plusEvents, minusEvents, plusMinusEvents;

                    mBin = (int) hnBins->GetBinContent(iPhi,iEta);
                    oBin = (int) hoffset->GetBinContent(iPhi,iEta);

                    double SSig = 0, PSig = 0, MSig = 0, PMSig = 0, MPSig = 0;
                    double NSSig = 0, NPSig = 0, NMSig = 0, NPMSig = 0, NMPSig = 0;

                    for (int iBin=oBin+1;iBin<=oBin+mBin;iBin++) {
                        totEvents = hptTotEvents[0]->GetBinContent(iBin);
                        totBins += totEvents;
                        totSumBins       += hptTotEvents[1]->GetBinContent(iBin);
                        totPlusBins      += hptTotEvents[2]->GetBinContent(iBin);
                        totMinusBins     += hptTotEvents[3]->GetBinContent(iBin);
                        totPlusMinusBins += hptTotEvents[4]->GetBinContent(iBin);

                        sumEvents = hptTotEvents[1]->GetBinContent(iBin);
                        if (sumEvents > 0) {
                            NSe1  = hptNSum[0]->GetBinContent(iBin) / sumEvents;
                            PSe1  = hptPSum[0]->GetBinContent(iBin) / sumEvents;
                            SHat  = PSe1 / NSe1;
                            PNSe1 = SHat*NSe1*hptPNSum[0]->GetBinContent(iBin) / sumEvents;
                            PNSe2 = SHat*hptPNSum[1]->GetBinContent(iBin) / sumEvents;
                            PNSe3 = NSe1*hptPNSum[2]->GetBinContent(iBin) / sumEvents;
                            PNSe4 = hptPNSum[3]->GetBinContent(iBin) / sumEvents;
                            SSig += (PNSe1 - PNSe2 - PNSe3 + PNSe4) / sqrt(NSe1);
                            NSSig  += 1;
                        }

                        plusEvents = hptTotEvents[2]->GetBinContent(iBin);
                        if (plusEvents > 0) {
                            NPe1  = hptNPlus[0]->GetBinContent(iBin) / plusEvents;
                            PPe1  = hptPPlus[0]->GetBinContent(iBin) / plusEvents;
                            PHat = PPe1 / NPe1;
                            PNPe1 = PHat*NPe1*hptPNPlus[0]->GetBinContent(iBin) / plusEvents;
                            PNPe2 = PHat*hptPNPlus[1]->GetBinContent(iBin) / plusEvents;
                            PNPe3 = NPe1*hptPNPlus[2]->GetBinContent(iBin) / plusEvents;
                            PNPe4 = hptPNPlus[3]->GetBinContent(iBin) / plusEvents;
                            PSig += (PNPe1 - PNPe2 - PNPe3 + PNPe4) / sqrt(NPe1);
                            NPSig   += 1;
                        }

                        minusEvents = hptTotEvents[3]->GetBinContent(iBin);
                        MHat = 0;
                        if (minusEvents > 0) {
                            NMe1  = hptNMinus[0]->GetBinContent(iBin) / minusEvents;
                            PMe1  = hptPMinus[0]->GetBinContent(iBin) / minusEvents;
                            MHat = PMe1 / NMe1;
                            PNMe1 = MHat*NMe1*hptPNMinus[0]->GetBinContent(iBin) / minusEvents;
                            PNMe2 = MHat*hptPNMinus[1]->GetBinContent(iBin) / minusEvents;
                            PNMe3 = NMe1*hptPNMinus[2]->GetBinContent(iBin) / minusEvents;
                            PNMe4 = hptPNMinus[3]->GetBinContent(iBin) / minusEvents;
                            MSig += (PNMe1 - PNMe2 - PNMe3 + PNMe4) / sqrt(NMe1);
                            NMSig    += 1;
                        }

                        plusMinusEvents = hptTotEvents[4]->GetBinContent(iBin);
                        if (plusMinusEvents > 0) {
                            NPe1   = hptPNPlusMinus[4]->GetBinContent(iBin) / plusMinusEvents;
                            PPe1   = hptPNPlusMinus[5]->GetBinContent(iBin) / plusMinusEvents;
                            PHat  = PPe1 / NPe1;
                            NMe1   = hptPNPlusMinus[8]->GetBinContent(iBin) / plusMinusEvents;
                            PMe1   = hptPNPlusMinus[9]->GetBinContent(iBin) / plusMinusEvents;
                            MHat  = PMe1 / NMe1;
                            PNPMe1 = hptPNPlusMinus[0]->GetBinContent(iBin) / plusMinusEvents;
                            PNPMe2 = PHat*hptPNPlusMinus[1]->GetBinContent(iBin) / plusMinusEvents;
                            PNPMe3 = NMe1*hptPNPlusMinus[7]->GetBinContent(iBin) / plusMinusEvents;
                            PNPMe4 = PHat*NMe1*hptPNPlusMinus[6]->GetBinContent(iBin) / plusMinusEvents;
                            PMSig  += (PNPMe1 - PNPMe2 - PNPMe3 + PNPMe4) / sqrt(NMe1);
                            NPMSig += 1;

                            PNMPe1 = hptPNPlusMinus[2]->GetBinContent(iBin) / plusMinusEvents;
                            PNMPe2 = MHat*hptPNPlusMinus[3]->GetBinContent(iBin) / plusMinusEvents;
                            PNMPe3 = NPe1*hptPNPlusMinus[11]->GetBinContent(iBin) / plusMinusEvents;
                            PNMPe4 = MHat*NPe1*hptPNPlusMinus[10]->GetBinContent(iBin) / plusMinusEvents;
                            MPSig  += (PNMPe1 - PNMPe2 - PNMPe3 + PNMPe4) / sqrt(NPe1);
                            NMPSig += 1;
                        }
                    }

                    if (NSSig > 0) {
                        SSig = SSig / NSSig;
                        ptPNSig[jPtCent][jPt]->SetBinContent(iPhi,iEta,SSig);
                    }

                    if (NPSig > 0) {
                        PSig  = PSig / NPSig;
                        ptPNPlus[jPtCent][jPt]->SetBinContent(iPhi,iEta,PSig);
                    }

                    if (NMSig > 0) {
                        MSig = MSig / NMSig;
                        ptPNMinus[jPtCent][jPt]->SetBinContent(iPhi,iEta,MSig);
                    }

                    if (NPMSig > 0) {
                        PMSig = PMSig / NPMSig;
                        MPSig = MPSig / NMPSig;
                        ptPNPlusMinus[jPtCent][jPt]->SetBinContent(iPhi,iEta,PMSig);
                    }
                    if (NMPSig > 0) {
                        MPSig = MPSig / NMPSig;
                        ptPNMinusPlus[jPtCent][jPt]->SetBinContent(iPhi,iEta,MPSig);
                    }


                    // I haven't worked out the propogation of errors.
                    // Looks difficult.
                    // For now just put something in so fitting doesn't crash.
                    if (totSumBins > 0) {
                        ptPNSigErrors[jPtCent][jPt]->SetBinContent(iPhi,iEta,1/sqrt(totSumBins));
                    } else {
                        ptPNSigErrors[jPtCent][jPt]->SetBinContent(iPhi,iEta,0.001);
                    }
                    if (totPlusBins > 0) {
                        ptPNPlusErrors[jPtCent][jPt]->SetBinContent(iPhi,iEta,1/sqrt(totPlusBins));
                    } else {
                        ptPNPlusErrors[jPtCent][jPt]->SetBinContent(iPhi,iEta,0.001);
                    }
                    if (totMinusBins > 0) {
                        ptPNMinusErrors[jPtCent][jPt]->SetBinContent(iPhi,iEta,1/sqrt(totMinusBins));
                    } else {
                        ptPNMinusErrors[jPtCent][jPt]->SetBinContent(iPhi,iEta,0.001);
                    }
                    if (totPlusMinusBins > 0) {
                        ptPNPlusMinusErrors[jPtCent][jPt]->SetBinContent(iPhi,iEta,1/sqrt(totPlusMinusBins));
                        ptPNMinusPlusErrors[jPtCent][jPt]->SetBinContent(iPhi,iEta,1/sqrt(totPlusMinusBins));
                    } else {
                        ptPNPlusMinusErrors[jPtCent][jPt]->SetBinContent(iPhi,iEta,0.001);
                        ptPNMinusPlusErrors[jPtCent][jPt]->SetBinContent(iPhi,iEta,0.001);
                    }
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
        PPlus[i]->Write();
        PMinus[i]->Write();
        PPlusMinus[i]->Write();
        PNSig[i]->Write();
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
        PPlusErrors[i]->Write();
        PMinusErrors[i]->Write();
        PPlusMinusErrors[i]->Write();
        PNSigErrors[i]->Write();
        PNPlusErrors[i]->Write();
        PNMinusErrors[i]->Write();
        PNPlusMinusErrors[i]->Write();
        PNMinusPlusErrors[i]->Write();
    }

    for (int i=0;i<NPTSCENTBINS;i++) {
        for (int j=0;j<NPTBINS;j++) {
            ptNSig[i][j]->Write();
            ptNDel[i][j]->Write();
            ptNPlus[i][j]->Write();
            ptNMinus[i][j]->Write();
            ptNPlusMinus[i][j]->Write();
            ptPSig[i][j]->Write();
            ptPPlus[i][j]->Write();
            ptPMinus[i][j]->Write();
            ptPPlusMinus[i][j]->Write();
            ptPNSig[i][j]->Write();
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
            ptNDel[i][j]->Write();
            ptNPlusErrors[i][j]->Write();
            ptNMinusErrors[i][j]->Write();
            ptNPlusMinusErrors[i][j]->Write();
            ptPSigErrors[i][j]->Write();
            ptPPlusErrors[i][j]->Write();
            ptPMinusErrors[i][j]->Write();
            ptPPlusMinusErrors[i][j]->Write();
            ptPNSigErrors[i][j]->Write();
            ptPNPlusErrors[i][j]->Write();
            ptPNMinusErrors[i][j]->Write();
            ptPNPlusMinusErrors[i][j]->Write();
            ptPNMinusPlusErrors[i][j]->Write();
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
        sprintf( line, "PPlus_%i", i );
        PPlus[i] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
        sprintf( line, "PMinus_%i", i );
        PMinus[i] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
        sprintf( line, "PPlusMinus_%i", i );
        PPlusMinus[i] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
        sprintf( line, "PNSig_%i", i );
        PNSig[i] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
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
        sprintf( line, "PPlusErrors_%i", i );
        PPlusErrors[i] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
        sprintf( line, "PMinusErrors_%i", i );
        PMinusErrors[i] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
        sprintf( line, "PPlusMinusErrors_%i", i );
        PPlusMinusErrors[i] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
        sprintf( line, "PNSigErrors_%i", i );
        PNSigErrors[i] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
        sprintf( line, "PNPlusErrors_%i", i );
        PNPlusErrors[i] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
        sprintf( line, "PNMinusErrors_%i", i );
        PNMinusErrors[i] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
        sprintf( line, "PNPlusMinusErrors_%i", i );
        PNPlusMinusErrors[i] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
        sprintf( line, "PNMinusPlusErrors_%i", i );
        PNMinusPlusErrors[i] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
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
            sprintf( line, "ptPPlus_%i_%i", i, j );
            ptPPlus[i][j] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
            sprintf( line, "ptPMinus_%i_%i", i, j );
            ptPMinus[i][j] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
            sprintf( line, "ptPPlusMinus_%i_%i", i, j );
            ptPPlusMinus[i][j] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
            sprintf( line, "ptPNSig_%i_%i", i, j );
            ptPNSig[i][j] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
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
            sprintf( line, "ptPPlusErrors_%i_%i", i, j );
            ptPPlusErrors[i][j] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
            sprintf( line, "ptPMinusErrors_%i_%i", i, j );
            ptPMinusErrors[i][j] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
            sprintf( line, "ptPPlusMinusErrors_%i_%i", i, j );
            ptPPlusMinusErrors[i][j] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
            sprintf( line, "ptPNSigErrors_%i_%i", i, j );
            ptPNSigErrors[i][j] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
            sprintf( line, "ptPNPlusErrors_%i_%i", i, j );
            ptPNPlusErrors[i][j] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
            sprintf( line, "ptPNMinusErrors_%i_%i", i, j );
            ptPNMinusErrors[i][j] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
            sprintf( line, "ptPNPlusMinusErrors_%i_%i", i, j );
            ptPNPlusMinusErrors[i][j] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
            sprintf( line, "ptPNMinusPlusErrors_%i_%i", i, j );
            ptPNMinusPlusErrors[i][j] = new TH2D(line,line,NPhiBins,0.0,6.2831852,NEtaBins,0.0,SETAMAX-SETAMIN);
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
        delete PPlus[i];
        delete PMinus[i];
        delete PPlusMinus[i];
        delete PNSig[i];
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
        delete PPlusErrors[i];
        delete PMinusErrors[i];
        delete PPlusMinusErrors[i];
        delete PNSigErrors[i];
        delete PNPlusErrors[i];
        delete PNMinusErrors[i];
        delete PNPlusMinusErrors[i];
        delete PNMinusPlusErrors[i];
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
            delete ptPPlus[i][j];
            delete ptPMinus[i][j];
            delete ptPPlusMinus[i][j];
            delete ptPNSig[i][j];
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
            delete ptPPlusErrors[i][j];
            delete ptPMinusErrors[i][j];
            delete ptPPlusMinusErrors[i][j];
            delete ptPNSigErrors[i][j];
            delete ptPNPlusErrors[i][j];
            delete ptPNMinusErrors[i][j];
            delete ptPNPlusMinusErrors[i][j];
            delete ptPNMinusPlusErrors[i][j];
        }
    }
}
