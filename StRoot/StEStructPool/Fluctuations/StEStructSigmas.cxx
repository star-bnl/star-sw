
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
                double NS1, NS2,  NSSig  = 0, SSig  = 0;
                double ND1, ND2,  NDSig  = 0, DSig  = 0;
                double NP1, NP2,  NPSig  = 0, PSig  = 0;
                double NM1, NM2,  NMSig  = 0, MSig  = 0;
                double      NPM2, NPMSig = 0, PMSig = 0;
                double sumEvents, plusEvents, minusEvents, plusMinusEvents;

                mBin = (int) hnBins->GetBinContent(iPhi,iEta);
                oBin = (int) hoffset->GetBinContent(iPhi,iEta);

                for (int iBin=oBin+1;iBin<=oBin+mBin;iBin++) {
                    sumEvents = hTotEvents[0]->GetBinContent(iBin);
                    NS1 = 0;
                    if (sumEvents > 0) {
                        NS1  = hNSum[0]->GetBinContent(iBin) / sumEvents;
                        NS2  = hNSum[1]->GetBinContent(iBin) / sumEvents;
                        if (NS1 > 0) {
                            SSig  += (NS2 - NS1*NS1) / NS1 - 1;
                            NSSig += 1;
                        }
                        ND1  = hNDel[0]->GetBinContent(iBin) / sumEvents;
                        ND2  = hNDel[1]->GetBinContent(iBin) / sumEvents;
                        if (NS1 > 0) {
                            DSig  += (ND2 - ND1*ND1) / NS1 - 1;
                            NDSig += 1;
                        }
                    }

                    plusEvents = hTotEvents[0]->GetBinContent(iBin);
                    NP1 = 0;
                    if (plusEvents > 0) {
                        NP1  = hNPlus[0]->GetBinContent(iBin) / plusEvents;
                        NP2  = hNPlus[1]->GetBinContent(iBin) / plusEvents;
                        if (NP1 > 0) {
                            PSig  += (NP2 - NP1*NP1) / NP1 - 1;
                            NPSig += 1;
                        }
                    }

                    minusEvents = hTotEvents[0]->GetBinContent(iBin);
                    NM1 = 0;
                    if (minusEvents > 0) {
                        NM1  = hNMinus[0]->GetBinContent(iBin) / minusEvents;
                        NM2  = hNMinus[1]->GetBinContent(iBin) / minusEvents;
                        if (NM1 > 0) {
                            MSig  += (NM2 - NM1*NM1) / NM1 - 1;
                            NMSig += 1;
                        }
                    }

                    plusMinusEvents = hTotEvents[0]->GetBinContent(iBin);
                    if (NP1*NM1 > 0) {
                        NPM2    = hNPlusMinus[0]->GetBinContent(iBin) / plusMinusEvents;
                        PMSig  += (NPM2 - NP1*NM1) / sqrt(NP1*NM1);
                        NPMSig += 1;
                    }
                }

                if (NSSig > 0) {
                    NSig[jCent]->SetBinContent(iPhi,iEta,SSig/NSSig);
                }
                if (NDSig > 0) {
                    NDel[jCent]->SetBinContent(iPhi,iEta,DSig/NSSig);
                }
                if (NPSig > 0) {
                    NPlus[jCent]->SetBinContent(iPhi,iEta,PSig/NPSig);
                }
                if (NMSig > 0) {
                    NMinus[jCent]->SetBinContent(iPhi,iEta,MSig/NMSig);
                }
                if (NPMSig > 0) {
                    NPlusMinus[jCent]->SetBinContent(iPhi,iEta,PMSig/NPMSig);
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
    }
    PtSHat->Reset();
    PtPHat->Reset();
    PtMHat->Reset();
    sigPtSHat->Reset();
    sigPtPHat->Reset();
    sigPtMHat->Reset();

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
    TH1D *hPSum[4];
    TH1D *hPPlus[4];
    TH1D *hPMinus[4];
    TH1D *hPPlusMinus[8];
    TH1D *hPtSumSq[3];

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
        for (int jStat=0;jStat<8;jStat++) {
            sprintf(buffer,"PPlusMinus%i_%i",jCent,jStat);
            hPPlusMinus[jStat] = (TH1D *) gDirectory->Get(buffer);
        }
        hPtSumSq[0] = (TH1D *) gDirectory->Get("PtSumSq");
        hPtSumSq[1] = (TH1D *) gDirectory->Get("PtPlusSq");
        hPtSumSq[2] = (TH1D *) gDirectory->Get("PtMinusSq");

        int mBin, oBin;
        int iBin = 1 + (int) hoffset->GetBinContent(NPhiBins,NEtaBins);
        double NTot      = hNSum[0]->GetBinContent(iBin);
        double NTotPlus  = hNPlus[0]->GetBinContent(iBin);
        double NTotMinus = hNMinus[0]->GetBinContent(iBin);

        double StHat, PtHat, MtHat;
        StHat = hPSum[0]->GetBinContent(iBin) / NTot;
        PtHat = hPPlus[0]->GetBinContent(iBin) / NTotPlus;
        MtHat = hPMinus[0]->GetBinContent(iBin) / NTotMinus;
        int iCent = 1 + jCent;
        PtSHat->Fill(iCent,StHat);
        PtPHat->Fill(iCent,PtHat);
        PtMHat->Fill(iCent,MtHat);
        double sigSHat, sigPHat, sigMHat;
        sigSHat = hPtSumSq[0]->GetBinContent(iCent)/NTot - StHat*StHat;
        sigPHat = hPtSumSq[1]->GetBinContent(iCent)/NTotPlus - PtHat*PtHat;
        sigMHat = hPtSumSq[2]->GetBinContent(iCent)/NTotMinus - MtHat*MtHat;
        sigPtSHat->Fill(iCent,sigSHat);
        sigPtPHat->Fill(iCent,sigPHat);
        sigPtMHat->Fill(iCent,sigMHat);

        for (int iPhi=1;iPhi<=NPhiBins;iPhi++) {
            for (int iEta=1;iEta<=NEtaBins;iEta++) {
                double NS1, PS1, PS2, Ssig = 0, NSsig = 0;
                double NP1, PP1, PP2, Psig = 0, NPsig = 0;
                double NM1, PM1, PM2, Msig = 0, NMsig = 0;
                double PPM1, PPM2, PPM3,  PPM4, PMsig = 0, NPMsig = 0;
                double PHat, MHat;
                double totEvents, sumEvents, plusEvents, minusEvents, plusMinusEvents;

                mBin = (int) hnBins->GetBinContent(iPhi,iEta);
                oBin = (int) hoffset->GetBinContent(iPhi,iEta);

                for (int iBin=oBin+1;iBin<=oBin+mBin;iBin++) {
                    totEvents = hTotEvents[0]->GetBinContent(iBin);
                    sumEvents = hTotEvents[1]->GetBinContent(iBin);
                    if (sumEvents > 0) {
                        NS1 = hNSum[0]->GetBinContent(iBin) / sumEvents;
                        PS1 = hPSum[0]->GetBinContent(iBin) / sumEvents;
                        PS2 = hPSum[1]->GetBinContent(iBin) / sumEvents;
                        if (NS1 > 0) {
                            Ssig  += (PS2 - PS1*PS1/NS1) - sigSHat;
                            NSsig += 1;
                        }
                    }

                    plusEvents = hTotEvents[2]->GetBinContent(iBin);
                    PHat = 0;
                    if (plusEvents > 0) {
                        NP1 = hNPlus[0]->GetBinContent(iBin) / plusEvents;
                        PP1 = hPPlus[0]->GetBinContent(iBin) / plusEvents;
                        PP2 = hPPlus[1]->GetBinContent(iBin) / plusEvents;
                        if (NP1 > 0) {
                            PHat = PP1 / NP1;
                            Psig  += (PP2 - PP1*PP1/NP1) - sigPHat;
                            NPsig += 1;
                        }
                    }

                    minusEvents = hTotEvents[3]->GetBinContent(iBin);
                    MHat = 0;
                    if (minusEvents > 0) {
                        NM1 = hNMinus[0]->GetBinContent(iBin) / minusEvents;
                        PM1 = hPMinus[0]->GetBinContent(iBin) / minusEvents;
                        PM2 = hPMinus[1]->GetBinContent(iBin) / minusEvents;
                        if (NM1 > 0) {
                            MHat = PM1 / NM1;
                            Msig  += (PM2 - PM1*PM1/NM1) - sigMHat;
                            NMsig += 1;
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

                if (StHat == 0) {
                    return;
                }
                if (NSsig > 0) {
                    Ssig = Ssig / NSsig;
                    PSig[jCent]->SetBinContent(iPhi,iEta,Ssig/(StHat*StHat));
                }

                if (NPsig > 0) {
                    Psig = Psig / NPsig;
                    PPlus[jCent]->SetBinContent(iPhi,iEta,Psig/(StHat*StHat));
                }

                if (NMsig > 0) {
                    Msig = Msig / NMsig;
                    PMinus[jCent]->SetBinContent(iPhi,iEta,Msig/(StHat*StHat));
                }

                if (NPMsig > 0) {
                    PMsig = PMsig / NPMsig;
                    PPlusMinus[jCent]->SetBinContent(iPhi,iEta,PMsig/(StHat*StHat));
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
    TH1D *hNPlus[2];
    TH1D *hNMinus[2];
    TH1D *hPSum[4];
    TH1D *hPPlus[4];
    TH1D *hPMinus[4];
    TH1D *hPNSum[4];
    TH1D *hPNPlus[4];
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
        int iBin = 1 + (int) hoffset->GetBinContent(NPhiBins,NEtaBins);
        double NTot      = hNSum[0]->GetBinContent(iBin);
        double NTotPlus  = hNPlus[0]->GetBinContent(iBin);
        double NTotMinus = hNMinus[0]->GetBinContent(iBin);
        double StHat, PtHat, MtHat;
        StHat = hPSum[0]->GetBinContent(iBin) / NTot;
        PtHat = hPPlus[0]->GetBinContent(iBin) / NTotPlus;
        MtHat = hPMinus[0]->GetBinContent(iBin) / NTotMinus;

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
//>>>>>I need to check that I am calculating correct means and dividing
//>>>>>by the correct number of events!!!!!
                for (int iBin=oBin+1;iBin<=oBin+mBin;iBin++) {
                    sumEvents = hTotEvents[1]->GetBinContent(iBin);
                    if (sumEvents > 0) {
                        NS1 = hNSum[0]->GetBinContent(iBin) / sumEvents;
                        PS1 = hPSum[0]->GetBinContent(iBin) / sumEvents;
                        if (NS1 > 0) {
                            SHat = PS1 / NS1;
                            PNS1 = SHat*NS1*hPNSum[0]->GetBinContent(iBin);
                            PNS2 = SHat*hPNSum[1]->GetBinContent(iBin);
                            PNS3 = NS1*hPNSum[2]->GetBinContent(iBin);
                            PNS4 = hPNSum[3]->GetBinContent(iBin);
                            Ssig  += (PNS1 - PNS2 - PNS3 + PNS4) / (sumEvents*sqrt(NS1));
                            NSsig +=1;
                        }
                    }

                    plusEvents = hTotEvents[2]->GetBinContent(iBin);
                    NP1  = 0;
                    PHat = 0;
                    if (plusEvents > 0) {
                        NP1 = hNPlus[0]->GetBinContent(iBin) / plusEvents;
                        PP1 = hPPlus[0]->GetBinContent(iBin) / plusEvents;
                        if (NP1 > 0) {
                            PHat = PP1 / NP1;
                            PNP1 = PHat* NP1*hPNPlus[0]->GetBinContent(iBin);
                            PNP2 = PHat*hPNPlus[1]->GetBinContent(iBin);
                            PNP3 = NP1*hPNPlus[2]->GetBinContent(iBin);
                            PNP4 = hPNPlus[3]->GetBinContent(iBin);
                            Psig  += (PNP1 - PNP2 - PNP3 + PNP4) / (plusEvents*sqrt(NP1));
                            NPsig += 1;
                        }
                    }

                    minusEvents = hTotEvents[3]->GetBinContent(iBin);
                    NM1 = 0;
                    MHat = 0;
                    if (minusEvents > 0) {
                        NM1 = hNMinus[0]->GetBinContent(iBin) / minusEvents;
                        PM1 = hPMinus[0]->GetBinContent(iBin) / minusEvents;
                        if (NM1 > 0) {
                            MHat = PM1 / NM1;
                            PNM1 = MHat*NM1*hPNMinus[0]->GetBinContent(iBin);
                            PNM2 = MHat*hPNMinus[1]->GetBinContent(iBin);
                            PNM3 = NM1*hPNMinus[2]->GetBinContent(iBin);
                            PNM4 = hPNMinus[3]->GetBinContent(iBin);
                            Msig  += (PNM1 - PNM2 - PNM3 + PNM4) / (minusEvents*sqrt(NM1));
                            NMsig += 1;
                        }
                    }

                    plusMinusEvents = hTotEvents[4]->GetBinContent(iBin);
                    if (plusMinusEvents > 0) {
                        NP1   = hPNPlusMinus[4]->GetBinContent(iBin) / plusMinusEvents;
                        PP1   = hPNPlusMinus[5]->GetBinContent(iBin) / plusMinusEvents;
                        PHat  = PP1 / NP1;
                        NM1   = hPNPlusMinus[8]->GetBinContent(iBin) / plusMinusEvents;
                        PM1   = hPNPlusMinus[9]->GetBinContent(iBin) / plusMinusEvents;
                        MHat  = PM1 / NM1;
                        PNPM1 = hPNPlusMinus[0]->GetBinContent(iBin);
                        PNPM2 = PHat*hPNPlusMinus[1]->GetBinContent(iBin);
                        PNPM3 = NM1*hPNPlusMinus[7]->GetBinContent(iBin);
                        PNPM4 = PHat*NM1*hPNPlusMinus[6]->GetBinContent(iBin);
                        if (NM1 > 0) {
                            PMsig  += (PNPM1 - PNPM2 - PNPM3 + PNPM4) / (plusMinusEvents*sqrt(NM1));
                            NPMsig += 1;
                        }

                        PNMP1 = hPNPlusMinus[2]->GetBinContent(iBin);
                        PNMP2 = MHat*hPNPlusMinus[3]->GetBinContent(iBin);
                        PNMP3 = NP1*hPNPlusMinus[11]->GetBinContent(iBin);
                        PNMP4 = MHat*NP1*hPNPlusMinus[10]->GetBinContent(iBin);
                        if (NP1 > 0) {
                            MPsig  += (PNMP1 - PNMP2 - PNMP3 + PNMP4) / (plusMinusEvents*sqrt(NP1));
                            NMPsig += 1;
                        }
                    }
                }

                if (StHat <= 0) {
                    return;
                }
                if (NSsig > 0) {
                    Ssig = Ssig / NSsig;
                    PNSig[jCent]->SetBinContent(iPhi,iEta,Ssig/StHat);
                }
                if (NPsig > 0) {
                    Psig = Psig / NPsig;
                    PNPlus[jCent]->SetBinContent(iPhi,iEta,Psig/StHat);
                }
                if (NMsig > 0) {
                    Msig = Msig / NMsig;
                    PNMinus[jCent]->SetBinContent(iPhi,iEta,Msig/StHat);
                }
                if (NPMsig) {
                    PMsig = PMsig / NPMsig;
                    PNPlusMinus[jCent]->SetBinContent(iPhi,iEta,PMsig/StHat);
                }
                if (NMPsig) {
                    MPsig = MPsig / NMPsig;
                    PNMinusPlus[jCent]->SetBinContent(iPhi,iEta,MPsig/StHat);
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
            int iBin = 1 + (int) hoffset->GetBinContent(NPhiBins,NEtaBins);
            double NTot      = hptNSum[0]->GetBinContent(iBin);
            double NTotPlus  = hptNPlus[0]->GetBinContent(iBin);
            double NTotMinus = hptNMinus[0]->GetBinContent(iBin);

            for (int iPhi=1;iPhi<=NPhiBins;iPhi++) {
                for (int iEta=1;iEta<=NEtaBins;iEta++) {
                    double NSe1 = 0, NSb1 = 0, NSb1Sq = 0, NSb2 = 0;
                    double NDe1, NDb1 = 0, NDb1Sq = 0, NDb2 = 0;
                    double NPe1, NPb1 = 0, NPb1Sq = 0, NPb2 = 0;
                    double NMe1, NMb1 = 0, NMb1Sq = 0, NMb2 = 0;
                    double NPMb1 = 0, NPMb1Sq = 0, NPMb2 = 0;
                    double totBins = 0, sumBins = 0, plusBins = 0;
                    double minusBins = 0;

                    mBin = (int) hnBins->GetBinContent(iPhi,iEta);
                    oBin = (int) hoffset->GetBinContent(iPhi,iEta);

                    for (int iBin=oBin+1;iBin<=oBin+mBin;iBin++) {
                        totBins  += hptTotEvents[0]->GetBinContent(iBin);
                        sumBins   = hptTotEvents[0]->GetBinContent(iBin);
                        plusBins  = hptTotEvents[0]->GetBinContent(iBin);
                        minusBins = hptTotEvents[0]->GetBinContent(iBin);

                        if (sumBins > 0) {
                            NSe1  = hptNSum[0]->GetBinContent(iBin);
                            NSb1 += NSe1;
                            NSb2 += hptNSum[1]->GetBinContent(iBin);
                            NSb1Sq += NSe1*NSe1 / sumBins;
                        }

                        if (sumBins > 0) {
                            NDe1  = hptNDel[0]->GetBinContent(iBin);
                            NDb1 += NDe1;
                            NDb2 += hptNDel[1]->GetBinContent(iBin);
                            NDb1Sq += NDe1*NDe1 / sumBins;
                        }

                        NPe1 = 0;
                        if (plusBins > 0) {
                            NPe1  = hptNPlus[0]->GetBinContent(iBin);
                            NPb1 += NPe1;
                            NPb2 += hptNPlus[1]->GetBinContent(iBin);
                            NPb1Sq += NPe1*NPe1 / plusBins;
                        }

                        NMe1 = 0;
                        if (minusBins > 0) {
                            NMe1  = hptNMinus[0]->GetBinContent(iBin);
                            NMb1 += NMe1;
                            NMb2 += hptNMinus[1]->GetBinContent(iBin);
                            NMb1Sq += NMe1*NMe1 / minusBins;
                        }

                        if (sumBins > 0) {
                            NPMb1 += sqrt(NPe1*NMe1);
                            NPMb2 += hptNPlusMinus[0]->GetBinContent(iBin);
                            NPMb1Sq += NPe1*NMe1 / sumBins;
                        }
                    }

                    double nEvents = totBins / mBin;
                    if (0 == nEvents) {
                        return;
                    }
                    double SumSig = 0, DiffSig = 0, PlusSig = 0;
                    double MinusSig = 0, PMSig = 0;

                    if (NSb1 > 0) {
                        SumSig   = (NSb2 - NSb1Sq) / NSb1;
                        ptNSig[jPtCent][jPt]->SetBinContent(iPhi,iEta,SumSig-1);
                    }
                    if (NSb1 > 0) {
                        DiffSig  = (NDb2 - NDb1Sq) / NSb1;
                        ptNDel[jPtCent][jPt]->SetBinContent(iPhi,iEta,DiffSig-1);
                    }
                    if (NPb1 > 0) {
                        PlusSig  = (NPb2 - NPb1Sq) / NPb1;
                        ptNPlus[jPtCent][jPt]->SetBinContent(iPhi,iEta,PlusSig-1);
                    }
                    if (NMb1 > 0) {
                        MinusSig = (NMb2 - NMb1Sq) / NMb1;
                        ptNMinus[jPtCent][jPt]->SetBinContent(iPhi,iEta,MinusSig-1);
                    }
                    if (NPMb1 > 0) {
                        PMSig    = (NPMb2 - NPMb1Sq) / NPMb1;
                        ptNPlusMinus[jPtCent][jPt]->SetBinContent(iPhi,iEta,PMSig);
                    }

                    double SbNum = 0, SbDen = NSb1;
                    double DbNum = 0, DbDen = NSb1;
                    double PbNum = 0, PbDen = NPb1;
                    double MbNum = 0, MbDen = NMb1;
                    double PMbNum = 0, PMbDen = 0;
                    double NSe2, NDe2, NPe2 = 0, NMe2 = 0;
                    for (int iBin=oBin+1;iBin<=oBin+mBin;iBin++) {
                        if (NTot > 0) {
                            NSe1 = hptNSum[0]->GetBinContent(iBin);
                            NSe2 = hptNSum[1]->GetBinContent(iBin);
                            SbNum += (NSe1/nEvents) * (1 - NSe1/NTot) *
                                    (4*NSe2 - 4*NSe1*NSe1/nEvents + SumSig*nEvents);
                        }

                        NPe1 = 0;
                        if (NTotPlus > 0) {
                            NPe1 = hptNPlus[0]->GetBinContent(iBin);
                            NPe2 = hptNPlus[1]->GetBinContent(iBin);
                            PbNum += (NPe1/nEvents) * (1 - NPe1/NTotPlus) *
                                    (4*NPe2 - 4*NPe1*NPe1/nEvents + PlusSig*nEvents);
                        }

                        NMe1 = 0;
                        if (NTotMinus > 0) {
                            NMe1 = hptNMinus[0]->GetBinContent(iBin);
                            NMe2 = hptNMinus[1]->GetBinContent(iBin);
                            MbNum += (NMe1/nEvents) * (1 - NMe1/NTotMinus) *
                                    (4*NMe2 - 4*NMe1*NMe1/nEvents + MinusSig*nEvents);
                        }

                        if ((NTotPlus > 0) && (NTotMinus > 0)) {
                            NDe1 = hptNDel[0]->GetBinContent(iBin);
                            NDe2 = hptNDel[1]->GetBinContent(iBin);
                            DbNum += (NSe1 - NPe1*NPe1/NTotPlus - NMe1*NMe1/NTotMinus) / nEvents*
                                    (4*NDe2 - 4*NDe1*NDe1/nEvents + DiffSig*nEvents);

                            PMbNum += (1 - NPe1/NTotPlus) / nEvents *
                                     (NPe1*NMe2 - NPe1*NMe1*NMe1/nEvents + PMSig*NMe1/4)
                                    + (1 - NMe1/NTotMinus) / nEvents *
                                     (NMe1*NPe2 - NMe1*NPe1*NPe1/nEvents + PMSig*NPe1/4);
                            PMbDen += sqrt( NPe1 * NMe1 );
                       }
                    }
                    double f = hfUnique->GetBinContent(iPhi,iEta);
                    if ((SbNum > 0) && (SbDen > 0)) {
                        ptNSigErrors[jPtCent][jPt]->SetBinContent(iPhi,iEta,sqrt(SbNum/f)/SbDen);
                    }
                    if ((DbNum > 0) && (DbDen > 0)) {
                        ptNDelErrors[jPtCent][jPt]->SetBinContent(iPhi,iEta,sqrt(DbNum/f)/DbDen);
                    }
                    if ((PbNum > 0) && (PbDen > 0)) {
                        ptNPlusErrors[jPtCent][jPt]->SetBinContent(iPhi,iEta,sqrt(PbNum/f)/PbDen);
                    }
                    if ((MbNum > 0) && (MbDen > 0)) {
                        ptNMinusErrors[jPtCent][jPt]->SetBinContent(iPhi,iEta,sqrt(MbNum/f)/MbDen);
                    }
                    if ((PMbNum > 0) && (PMbDen > 0)) {
                        ptNPlusMinusErrors[jPtCent][jPt]->SetBinContent(iPhi,iEta,sqrt(PMbNum/f)/PMbDen);
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
        }
    }
    for (int jPt=0;jPt<NPTBINS;jPt++) {
        ptPtSHat[jPt]->Reset();
        ptPtPHat[jPt]->Reset();
        ptPtMHat[jPt]->Reset();
        ptsigPtSHat[jPt]->Reset();
        ptsigPtPHat[jPt]->Reset();
        ptsigPtMHat[jPt]->Reset();
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
    TH1D *hptPSum[4];
    TH1D *hptPPlus[4];
    TH1D *hptPMinus[4];
    TH1D *hptPPlusMinus[4];
    TH1D *hptPtSumSq[4][3];

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

            for (int jStat=0;jStat<4;jStat++) {
                sprintf(buffer,"ptPSum%i_%i_%i",jPtCent,jPt,jStat);
                hptPSum[jStat] = (TH1D *) gDirectory->Get(buffer);

                sprintf(buffer,"ptPPlus%i_%i_%i",jPtCent,jPt,jStat);
                hptPPlus[jStat] = (TH1D *) gDirectory->Get(buffer);

                sprintf(buffer,"ptPMinus%i_%i_%i",jPtCent,jPt,jStat);
                hptPMinus[jStat] = (TH1D *) gDirectory->Get(buffer);
            }
            for (int jStat=0;jStat<4;jStat++) {
                sprintf(buffer,"ptPPlusMinus%i_%i_%i",jPtCent,jPt,jStat);
                hptPPlusMinus[jStat] = (TH1D *) gDirectory->Get(buffer);
            }
            sprintf( buffer, "ptPtSumSq_%i", jPt );
            hptPtSumSq[jPt][0] = (TH1D *) gDirectory->Get(buffer);
            sprintf( buffer, "ptPtPlusSq_%i", jPt );
            hptPtSumSq[jPt][1] = (TH1D *) gDirectory->Get(buffer);
            sprintf( buffer, "ptPtMinusSq_%i", jPt );
            hptPtSumSq[jPt][2] = (TH1D *) gDirectory->Get(buffer);


            int mBin, oBin;
            int iBin = 1 + (int) hoffset->GetBinContent(NPhiBins,NEtaBins);
            double NTot      = hptNSum[0]->GetBinContent(iBin);
            double NTotPlus  = hptNPlus[0]->GetBinContent(iBin);
            double NTotMinus = hptNMinus[0]->GetBinContent(iBin);

            double SHat, PHat = 0, MHat = 0, StHat, PtHat, MtHat;
            StHat = hptPSum[0]->GetBinContent(iBin) / NTot;
            PtHat = hptPPlus[0]->GetBinContent(iBin) / NTotPlus;
            MtHat = hptPMinus[0]->GetBinContent(iBin) / NTotMinus;
            int iCent = 1 + jPtCent;
            ptPtSHat[jPt]->Fill(iCent,StHat);
            ptPtPHat[jPt]->Fill(iCent,PtHat);
            ptPtMHat[jPt]->Fill(iCent,MtHat);
            double sigSHat, sigPHat, sigMHat;
            sigSHat = hptPtSumSq[jPt][0]->GetBinContent(iCent)/NTot - StHat*StHat;
            sigPHat = hptPtSumSq[jPt][1]->GetBinContent(iCent)/NTotPlus - PtHat*PtHat;
            sigMHat = hptPtSumSq[jPt][2]->GetBinContent(iCent)/NTotMinus - MtHat*MtHat;
            ptsigPtSHat[jPt]->Fill(iCent,sigSHat);
            ptsigPtPHat[jPt]->Fill(iCent,sigPHat);
            ptsigPtMHat[jPt]->Fill(iCent,sigMHat);

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
                    double sumEvents, plusEvents, minusEvents, plusMinusEvents;

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
                            SHat = 0;
                            if (NSe1 > 0) {
                                SumSig += (PSe2 - PSe1*PSe1 / NSe1) / sumEvents;
                                SHat  = PSe1 / NSe1;
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
                            PHat = 0;
                            if (NPe1 > 0) {
                                PlusSig += (PPe2 - PPe1*PPe1 / NPe1) / plusEvents;
                                PHat  = PPe1 / NPe1;
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
                            MHat = 0;
                            if (NMe1 > 0) {
                                MinusSig += (PMe2 - PMe1*PMe1 / NMe1) / minusEvents;
                                MHat  = PMe1 / NMe1;
                            }
                            PMNbSig += PMe4 - 2*PMe3*pow(MHat,2) + pow(MHat,4);
                            PMPbSig += PMe3 - 2*PMe1*MHat + pow(MHat,2);
                        }

                        plusMinusEvents = hptTotEvents[4]->GetBinContent(iBin);
                        if (plusMinusEvents > 0) {
                            PPMe1 = PHat*MHat*hptPPlusMinus[0]->GetBinContent(iBin);
                            PPMe2 = MHat*hptPPlusMinus[1]->GetBinContent(iBin);
                            PPMe3 = PHat*hptPPlusMinus[2]->GetBinContent(iBin);
                            PPMe4 = hptPPlusMinus[3]->GetBinContent(iBin);
                            PMSig += (PPMe1 - PPMe2 - PPMe3 + PPMe4) / plusMinusEvents;
                        }
                    }

                    if (0 == totBins) {
                        return;
                    }
                    SumSig = SumSig / mBin;
                    if (StHat > 0) {
                        double sSig = (SumSig - sigSHat) / (StHat*StHat);
                        ptPSig[jPtCent][jPt]->SetBinContent(iPhi,iEta,sSig);
                    } else {
                        ptPSig[jPtCent][jPt]->SetBinContent(iPhi,iEta,0);
                    }

                    PlusSig = PlusSig / mBin;
                    if (PtHat > 0) {
                        double pSig = (PlusSig - sigPHat) / (PtHat*PtHat);
                        ptPPlus[jPtCent][jPt]->SetBinContent(iPhi,iEta,pSig);
                    } else {
                        ptPPlus[jPtCent][jPt]->SetBinContent(iPhi,iEta,0);
                    }

                    MinusSig = MinusSig / mBin;
                    if (MtHat > 0) {
                        double mSig = (MinusSig = sigMHat) / (MtHat*MtHat);
                        ptPMinus[jPtCent][jPt]->SetBinContent(iPhi,iEta,mSig);
                    } else {
                        ptPMinus[jPtCent][jPt]->SetBinContent(iPhi,iEta,0);
                    }

                    PMSig = PMSig / mBin;
                    if ((PtHat > 0) && (MtHat > 0)) {
                        double pmSig = PMSig / sqrt(PtHat*MtHat);
                        ptPPlusMinus[jPtCent][jPt]->SetBinContent(iPhi,iEta,pmSig);
                    } else {
                        ptPPlusMinus[jPtCent][jPt]->SetBinContent(iPhi,iEta,0);
                    }

                    double nSbar = NSb1 / mBin;
                    double SErr = nSbar*(1-nSbar/NTot)*PSNbSig + 4*SumSig*PSPbSig;
                    if (StHat > 0) {
                        SErr = sqrt(SErr) / (sumBins*StHat);
                        ptPSigErrors[jPtCent][jPt]->SetBinContent(iPhi,iEta,SErr);
                    } else {
                        ptPSigErrors[jPtCent][jPt]->SetBinContent(iPhi,iEta,0.001);
                    }
                    double nPbar = NPb1 / mBin;
                    double PErr = nPbar*(1-nPbar/NTotPlus)*PPNbSig + 4*PlusSig*PPPbSig;
                    if (PtHat > 0) {
                        PErr = sqrt(PErr) / (plusBins*PtHat);
                        ptPPlusErrors[jPtCent][jPt]->SetBinContent(iPhi,iEta,PErr);
                    } else {
                        ptPPlusErrors[jPtCent][jPt]->SetBinContent(iPhi,iEta,0.001);
                    }
                    double nMbar = NMb1 / mBin;
                    double MErr = nMbar*(1-nMbar/NTotMinus)*PMNbSig + 4*MinusSig*PMPbSig;
                    if (MtHat > 0) {
                        MErr = sqrt(MErr) / (minusBins*MtHat);
                        ptPMinusErrors[jPtCent][jPt]->SetBinContent(iPhi,iEta,MErr);
                    } else {
                        ptPMinusErrors[jPtCent][jPt]->SetBinContent(iPhi,iEta,0.001);
                    }
                    
                    // Haven't worked out error on plus-minus covariance.
                    double PMErr = PMSig / mBin;
                    if ((plusMinusBins > 0) && (PtHat > 0) && (MtHat > 0)) {
                        PMErr = sqrt(PMErr / (PtHat*MtHat)) / plusMinusBins;
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
    TH1D *hptPNPlusMinus[4];

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
            for (int jStat=0;jStat<4;jStat++) {
                sprintf(buffer,"ptPNPlusMinus%i_%i_%i",jPtCent,jPt,jStat);
                hptPNPlusMinus[jStat] = (TH1D *) gDirectory->Get(buffer);
            }

            int mBin, oBin;
            int iBin = 1 + (int) hoffset->GetBinContent(NPhiBins,NEtaBins);
            double NTot      = hptNSum[0]->GetBinContent(iBin);
            double NTotPlus  = hptNPlus[0]->GetBinContent(iBin);
            double NTotMinus = hptNMinus[0]->GetBinContent(iBin);
            double StHat, PtHat, MtHat;
            StHat = hptPSum[0]->GetBinContent(iBin) / NTot;
            PtHat = hptPPlus[0]->GetBinContent(iBin) / NTotPlus;
            MtHat = hptPMinus[0]->GetBinContent(iBin) / NTotMinus;

            for (int iPhi=1;iPhi<=NPhiBins;iPhi++) {
                for (int iEta=1;iEta<=NEtaBins;iEta++) {
                    double nSBar, SHat;
                    double nPBar = 0, PHat = 0;
                    double nMBar = 0, MHat = 0;
                    double NSe1, PSe1, PNSe1, PNSe2, PNSe3, PNSe4;
                    double NPe1, PPe1, PNPe1, PNPe2, PNPe3, PNPe4;
                    double NMe1, PMe1, PNMe1, PNMe2, PNMe3, PNMe4;
                    double PNPMe1, PNPMe2, PNPMe3, PNPMe4;
                    double PNPMe5, PNPMe6, PNPMe7, PNPMe8;
                    double totBins = 0, totEvents;
                    double totSumBins = 0, totPlusBins = 0;
                    double totMinusBins = 0, totPlusMinusBins = 0;
                    double sumEvents, plusEvents, minusEvents, plusMinusEvents;

                    mBin = (int) hnBins->GetBinContent(iPhi,iEta);
                    oBin = (int) hoffset->GetBinContent(iPhi,iEta);

                    double SumSig = 0, PlusSig = 0, MinusSig = 0;
                    double PMSig = 0, MPSig = 0;

                    for (int iBin=oBin+1;iBin<=oBin+mBin;iBin++) {
                        totEvents = hptTotEvents[0]->GetBinContent(iBin);
                        totBins += totEvents;
                        totSumBins       += hptTotEvents[1]->GetBinContent(iBin);
                        totPlusBins      += hptTotEvents[2]->GetBinContent(iBin);
                        totMinusBins     += hptTotEvents[3]->GetBinContent(iBin);
                        totPlusMinusBins += hptTotEvents[4]->GetBinContent(iBin);

                        sumEvents = hptTotEvents[1]->GetBinContent(iBin);
                        SHat = 0;
                        if (sumEvents > 0) {
                            NSe1  = hptNSum[0]->GetBinContent(iBin);
                            nSBar = NSe1 / totEvents;
                            PSe1  = hptPSum[0]->GetBinContent(iBin);
                            if (NSe1 > 0) {
                                SHat = PSe1 / NSe1;
                                PNSe1 = SHat*sqrt(nSBar)*hptPNSum[0]->GetBinContent(iBin);
                                PNSe2 = SHat*hptPNSum[1]->GetBinContent(iBin)/sqrt(nSBar);
                                PNSe3 = sqrt(nSBar)*hptPNSum[2]->GetBinContent(iBin);
                                PNSe4 = hptPNSum[3]->GetBinContent(iBin)/sqrt(nSBar);
                                SumSig += (PNSe1 - PNSe2 - PNSe3 + PNSe4) / sqrt(sumEvents*totEvents);
                            }
                        }

                        plusEvents = hptTotEvents[2]->GetBinContent(iBin);
                        PHat = 0;
                        if (plusEvents > 0) {
                            NPe1  = hptNPlus[0]->GetBinContent(iBin);
                            nPBar = NPe1 / totEvents;
                            PPe1  = hptPPlus[0]->GetBinContent(iBin);
                            if (NPe1 > 0) {
                                PHat = PPe1 / NPe1;
                                PNPe1 = PHat*sqrt(nPBar)*hptPNPlus[0]->GetBinContent(iBin);
                                PNPe2 = PHat*hptPNPlus[1]->GetBinContent(iBin)/sqrt(nPBar);
                                PNPe3 = sqrt(nPBar)*hptPNPlus[2]->GetBinContent(iBin);
                                PNPe4 = hptPNPlus[3]->GetBinContent(iBin)/sqrt(nPBar);
                                PlusSig += (PNPe1 - PNPe2 - PNPe3 + PNPe4) / sqrt(plusEvents*totEvents);
                            }
                        }

                        minusEvents = hptTotEvents[3]->GetBinContent(iBin);
                        MHat = 0;
                        if (minusEvents > 0) {
                            NMe1  = hptNMinus[0]->GetBinContent(iBin);
                            nMBar = NMe1 / totEvents;
                            PMe1  = hptPMinus[0]->GetBinContent(iBin);
                            if (NMe1 > 0) {
                                MHat = PMe1 / NMe1;
                                PNMe1 = MHat*sqrt(nMBar)*hptPNMinus[0]->GetBinContent(iBin);
                                PNMe2 = MHat*hptPNMinus[1]->GetBinContent(iBin)/sqrt(nMBar);
                                PNMe3 = sqrt(nMBar)*hptPNMinus[2]->GetBinContent(iBin);
                                PNMe4 = hptPNMinus[3]->GetBinContent(iBin)/sqrt(nMBar);
                                MinusSig += (PNMe1 - PNMe2 - PNMe3 + PNMe4) / sqrt(minusEvents*totEvents);
                            }
                        }

                        plusMinusEvents = hptTotEvents[4]->GetBinContent(iBin);
                        if (plusMinusEvents > 0) {
                            if (nMBar > 0) {
                                PNPMe1 = hptPNPlusMinus[0]->GetBinContent(iBin)/sqrt(nMBar);
                                PNPMe2 = hptPNPlusMinus[1]->GetBinContent(iBin)*PHat/sqrt(nMBar);
                                PNPMe3 = hptPNPlus[2]->GetBinContent(iBin)*sqrt(nMBar);
                                PNPMe4 = hptPNPlus[0]->GetBinContent(iBin)*PHat*sqrt(nMBar);
                                PMSig += (PNPMe1 - PNPMe2 - PNPMe3 + PNPMe4) / sqrt(plusMinusEvents*totEvents);
                            }

                            if (nPBar > 0) {
                                PNPMe5 = hptPNPlusMinus[2]->GetBinContent(iBin)/sqrt(nPBar);
                                PNPMe6 = hptPNPlusMinus[3]->GetBinContent(iBin)*MHat/sqrt(nPBar);
                                PNPMe7 = hptPNMinus[2]->GetBinContent(iBin)*sqrt(nPBar);
                                PNPMe8 = hptPNMinus[0]->GetBinContent(iBin)*MHat*sqrt(nPBar);
                                MPSig += (PNPMe5 - PNPMe6 - PNPMe7 + PNPMe8) / sqrt(plusMinusEvents*totEvents);
                            }
                        }
                    }

                    if (0 == totBins) {
                        return;
                    }
                    SumSig = SumSig / mBin;
                    if (StHat > 0) {
                        ptPNSig[jPtCent][jPt]->SetBinContent(iPhi,iEta,SumSig/StHat);
                    } else {
                        ptPNSig[jPtCent][jPt]->SetBinContent(iPhi,iEta,0);
                    }

                    PlusSig  = PlusSig / mBin;
                    if (PtHat > 0) {
                        ptPNPlus[jPtCent][jPt]->SetBinContent(iPhi,iEta,PlusSig/PtHat);
                    } else {
                        ptPNPlus[jPtCent][jPt]->SetBinContent(iPhi,iEta,0);
                    }

                    MinusSig = MinusSig / mBin;
                    if (MtHat > 0) {
                        ptPNMinus[jPtCent][jPt]->SetBinContent(iPhi,iEta,MinusSig/MtHat);
                    } else {
                        ptPNMinus[jPtCent][jPt]->SetBinContent(iPhi,iEta,0);
                    }

                    PMSig = PMSig / mBin;
                    MPSig = MPSig / mBin;
                    if ((PtHat > 0) && (MtHat > 0)) {
                        ptPNPlusMinus[jPtCent][jPt]->SetBinContent(iPhi,iEta,PMSig/sqrt(PtHat*MtHat));
                        ptPNMinusPlus[jPtCent][jPt]->SetBinContent(iPhi,iEta,MPSig/sqrt(PtHat*MtHat));
                    } else {
                        ptPNPlusMinus[jPtCent][jPt]->SetBinContent(iPhi,iEta,0);
                        ptPNMinusPlus[jPtCent][jPt]->SetBinContent(iPhi,iEta,0);
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
    PtSHat->Write();
    PtPHat->Write();
    PtMHat->Write();
    sigPtSHat->Write();
    sigPtPHat->Write();
    sigPtMHat->Write();

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
    for (int j=0;j<NPTBINS;j++) {
        ptPtSHat[j]->Write();
        ptPtPHat[j]->Write();
        ptPtMHat[j]->Write();
        ptsigPtSHat[j]->Write();
        ptsigPtPHat[j]->Write();
        ptsigPtMHat[j]->Write();
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
    PtSHat    = new TH1D("PtSHat",   "PtSHat",   NSCENTBINS,0.5,NSCENTBINS+0.5);
    PtPHat    = new TH1D("PtPHat",   "PtPHat",   NSCENTBINS,0.5,NSCENTBINS+0.5);
    PtMHat    = new TH1D("PtMHat",   "PtMHat",   NSCENTBINS,0.5,NSCENTBINS+0.5);
    sigPtSHat = new TH1D("sigPtSHat","sigPtSHat",NSCENTBINS,0.5,NSCENTBINS+0.5);
    sigPtPHat = new TH1D("sigPtPHat","sigPtPHat",NSCENTBINS,0.5,NSCENTBINS+0.5);
    sigPtMHat = new TH1D("sigPtMHat","sigPtMHat",NSCENTBINS,0.5,NSCENTBINS+0.5);

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
        sprintf( line, "ptPtSHat_%i", j );
        ptPtSHat[j]    = new TH1D(line,line,NPTSCENTBINS,0.5,NPTSCENTBINS+0.5);
        sprintf( line, "ptPtPHat_%i", j );
        ptPtPHat[j]    = new TH1D(line,line,NPTSCENTBINS,0.5,NPTSCENTBINS+0.5);
        sprintf( line, "ptPtMHat_%i", j );
        ptPtMHat[j]    = new TH1D(line,line,NPTSCENTBINS,0.5,NPTSCENTBINS+0.5);
        sprintf( line, "ptsigPtSHat_%i", j );
        ptsigPtSHat[j] = new TH1D(line,line,NPTSCENTBINS,0.5,NPTSCENTBINS+0.5);
        sprintf( line, "ptsigPtPHat_%i", j );
        ptsigPtPHat[j] = new TH1D(line,line,NPTSCENTBINS,0.5,NPTSCENTBINS+0.5);
        sprintf( line, "ptsigPtMHat_%i", j );
        ptsigPtMHat[j] = new TH1D(line,line,NPTSCENTBINS,0.5,NPTSCENTBINS+0.5);
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
    delete PtSHat;
    delete PtPHat;
    delete PtMHat;
    delete sigPtSHat;
    delete sigPtPHat;
    delete sigPtMHat;

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
    for (int j=0;j<NPTBINS;j++) {
        delete ptPtSHat[j];
        delete ptPtPHat[j];
        delete ptPtMHat[j];
        delete ptsigPtSHat[j];
        delete ptsigPtPHat[j];
        delete ptsigPtMHat[j];
    }
}
