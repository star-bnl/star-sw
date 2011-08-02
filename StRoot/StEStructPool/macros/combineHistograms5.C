void combineHistograms5(const char *dirName, const char **inNames, const char *outName, int nCent) {

    // -- Calculate \Delta\rho/\sqrt{\rho_{ref}} for all quantities, i.e. Phi-Phi, Eta-Eta, DPhi-DEta etc.
    //
    // root.exe -q -b combineHistograms5.C("dir","inFileList","outFileName",numFiles)
    // 
    //    inFileList contains centrality information, so I can have a list
    //    like {"Data1", "Data2",...}  or {"Sum0_1", "Sum2_3",...}
    //    Before getting here we use hadd to add histograms from different jobs
    //    creating files we call Data{n}.root (where n is a centrality bin)
    //    then we may combine centralities be merging the Data{n} files tagging
    //    histograms with z-vertex index and creating files Sum{n1}_{n2}.root where
    //    n1 and n2 are the limits of the centrality bin range.

    gROOT->LoadMacro("load2ptLibs.C");
    load2ptLibs();
    gSystem->Load("StEStructPoolSupport.so");

    TFile *tf;
    StEStructSupport *ehelp;
    TH2D **ptdedp;
    TH2D **ptdedpC;
    TH2D **dedp;
    TH2D **dedpC;
    TH2D **ptsedp;
    TH2D **ptsedpC;
    TH2D **sedp;
    TH2D **sedpC;
    TH2D **ptetaeta;
    TH2D **ptetaetaC;
    TH2D **etaeta;
    TH2D **etaetaC;
    TH2D **ptphiphi;
    TH2D **ptphiphiC;
    TH2D **phiphi;
    TH2D **phiphiC;

    const char* pidName[] = {"all", "pipi", "piK", "pip", "KK", "Kp", "pp", "oo"};
    const char* chargeName[] = {"_LS_", "_US_", "_CD_", "_CI_"};
    const char* chargeType[] = {"_PP_", "_PM_", "_MP_", "_MM_"};
    char outFileName[1024];
    sprintf(outFileName,"%s/%s.root",dirName,outName);
    TFile *out = new TFile(outFileName,"RECREATE");

    const char* oname[]={"all","pi_pi","pi_K","pi_p","K_K","K_p","p_p","o_o"};
    char inFileName[1024];

    for (int ic=0;ic<nCent;ic++) {
        printf("Centrality %2i:  d2N/dEdP    pHat_{A+}   pHat_{A-}   pHat_{B+}   pHat_{B-}\n",ic);
        for (int ipid=0;ipid<8;ipid++) {
            sprintf(inFileName,"%s/%s%s.root",dirName,inNames[ic],oname[ipid]);
            tf        = new TFile(inFileName);
            tf->cd();
            ehelp     = new StEStructSupport(tf,0);
            ehelp->msilent = true;
            ehelp->mapplyDEtaFix = false;
            if ((0 ==ipid) || (1 == ipid) || (4 == ipid) || (6 == ipid) || (7 == ipid)) {
                ehelp->mIdenticalPair = true;
            } else {
                ehelp->mIdenticalPair = false;
            }
            ehelp->setBGMode(0);
            int subtract = 1;
            ehelp->mPairNormalization = false;
            ptdedpC   = ehelp->buildPtCommon("DEtaDPhi",2,subtract);
            ptsedpC   = ehelp->buildPtCommon("SEtaDPhi",2,subtract);
            ptetaetaC = ehelp->buildPtCommon("EtaEta",2,subtract);
            ptphiphiC = ehelp->buildPtCommon("PhiPhi",2,subtract);

            ptdedp   = ehelp->buildPtChargeTypes("DEtaDPhi",2,subtract);
            ptsedp   = ehelp->buildPtChargeTypes("SEtaDPhi",2,subtract);
            ptetaeta = ehelp->buildPtChargeTypes("EtaEta",2,subtract);
            ptphiphi = ehelp->buildPtChargeTypes("PhiPhi",2,subtract);

            ehelp->mPairNormalization = true;
            dedpC     = ehelp->buildCommon("DEtaDPhi",2);
            sedpC     = ehelp->buildCommon("SEtaDPhi",2);
            etaetaC   = ehelp->buildCommon("EtaEta",2);
            phiphiC   = ehelp->buildCommon("PhiPhi",2);

            dedp     = ehelp->buildChargeTypes("DEtaDPhi",2);
            sedp     = ehelp->buildChargeTypes("SEtaDPhi",2);
            etaeta   = ehelp->buildChargeTypes("EtaEta",2);
            phiphi   = ehelp->buildChargeTypes("PhiPhi",2);

            out->cd();
            for (int icharge=0;icharge<4;icharge++) {
                TString name(pidName[ipid]);
                name += "_NDEtaDPhi"; name += chargeName[icharge];  name += ic;
                if (dedp) {
                    dedp[icharge]->SetName(name.Data());
                    dedp[icharge]->SetTitle(name.Data());
                    dedp[icharge]->Write();
                }
                TString name(pidName[ipid]);
                name += "_PtDEtaDPhi"; name += chargeName[icharge];  name += ic;
                if (ptdedp) {
                    ptdedp[icharge]->SetName(name.Data());
                    ptdedp[icharge]->SetTitle(name.Data());
                    ptdedp[icharge]->Write();
                }
                TString name(pidName[ipid]);
                name += "_NSEtaDPhi"; name += chargeName[icharge];  name += ic;
                if (sedp) {
                    sedp[icharge]->SetName(name.Data());
                    sedp[icharge]->SetTitle(name.Data());
                    sedp[icharge]->Write();
                }
                TString name(pidName[ipid]);
                name += "_PtSEtaDPhi"; name += chargeName[icharge];  name += ic;
                if (ptsedp) {
                    ptsedp[icharge]->SetName(name.Data());
                    ptsedp[icharge]->SetTitle(name.Data());
                    ptsedp[icharge]->Write();
                }

                TString name(pidName[ipid]);
                name += "_NDEtaDPhi"; name += chargeType[icharge];  name += ic;
                if (dedpC) {
                    dedpC[icharge]->SetName(name.Data());
                    dedpC[icharge]->SetTitle(name.Data());
                    dedpC[icharge]->Write();
                }
                TString name(pidName[ipid]);
                name += "_PtDEtaDPhi"; name += chargeType[icharge];  name += ic;
                if (ptdedpC) {
                    ptdedpC[icharge]->SetName(name.Data());
                    ptdedpC[icharge]->SetTitle(name.Data());
                    ptdedpC[icharge]->Write();
                }
                TString name(pidName[ipid]);
                name += "_NSEtaDPhi"; name += chargeType[icharge];  name += ic;
                if (sedpC) {
                    sedpC[icharge]->SetName(name.Data());
                    sedpC[icharge]->SetTitle(name.Data());
                    sedpC[icharge]->Write();
                }
                TString name(pidName[ipid]);
                name += "_PtSEtaDPhi"; name += chargeType[icharge];  name += ic;
                if (ptsedpC) {
                    ptsedpC[icharge]->SetName(name.Data());
                    ptsedpC[icharge]->SetTitle(name.Data());
                    ptsedpC[icharge]->Write();
                }


                TString name(pidName[ipid]);
                name += "_NEtaEta"; name += chargeName[icharge];  name += ic;
                if (etaeta) {
                    etaeta[icharge]->SetName(name.Data());
                    etaeta[icharge]->SetTitle(name.Data());
                    etaeta[icharge]->Write();
                }
                TString name(pidName[ipid]);
                name += "_PtEtaEta"; name += chargeName[icharge];  name += ic;
                if (ptetaeta) {
                    ptetaeta[icharge]->SetName(name.Data());
                    ptetaeta[icharge]->SetTitle(name.Data());
                    ptetaeta[icharge]->Write();
                }

                TString name(pidName[ipid]);
                name += "_NEtaEta"; name += chargeType[icharge];  name += ic;
                if (etaetaC) {
                    etaetaC[icharge]->SetName(name.Data());
                    etaetaC[icharge]->SetTitle(name.Data());
                    etaetaC[icharge]->Write();
                }
                TString name(pidName[ipid]);
                name += "_PtEtaEta"; name += chargeType[icharge];  name += ic;
                if (ptetaetaC) {
                    ptetaetaC[icharge]->SetName(name.Data());
                    ptetaetaC[icharge]->SetTitle(name.Data());
                    ptetaetaC[icharge]->Write();
                }


                TString name(pidName[ipid]);
                name += "_NPhiPhi"; name += chargeName[icharge];  name += ic;
                if (phiphi) {
                    phiphi[icharge]->SetName(name.Data());
                    phiphi[icharge]->SetTitle(name.Data());
                    phiphi[icharge]->Write();
                }
                TString name(pidName[ipid]);
                name += "_PtPhiPhi"; name += chargeName[icharge];  name += ic;
                if (ptphiphi) {
                    ptphiphi[icharge]->SetName(name.Data());
                    ptphiphi[icharge]->SetTitle(name.Data());
                    ptphiphi[icharge]->Write();
                }

                TString name(pidName[ipid]);
                name += "_NPhiPhi"; name += chargeType[icharge];  name += ic;
                if (phiphiC) {
                    phiphiC[icharge]->SetName(name.Data());
                    phiphiC[icharge]->SetTitle(name.Data());
                    phiphiC[icharge]->Write();
                }
                TString name(pidName[ipid]);
                name += "_PtPhiPhi"; name += chargeType[icharge];  name += ic;
                if (ptphiphiC) {
                    ptphiphiC[icharge]->SetName(name.Data());
                    ptphiphiC[icharge]->SetTitle(name.Data());
                    ptphiphiC[icharge]->Write();
                }
            }
            // Calculate and print scale factors
            double scale = ehelp->getCIdNdEtadPhi();
            double *ptHat = ehelp->getptHat();
            printf("  bin %8s  %7.3f   %7.3f     %7.3f     %7.3f     %7.3f\n",pidName[ipid],scale,ptHat[0],ptHat[1],ptHat[2],ptHat[3]);
            delete [] ptHat;

            delete tf;
            delete ehelp;
        }
    }

    TH2D **ytyt;
    TH2D **ytytC;
    TH2D **sytdyt;
    TH2D **sytdytC;
    gROOT->LoadMacro("minimizeNegative2.C");
    double *sFactor[2][8];
    for (int it=0;it<2;it++) {
        for (int ipid=0;ipid<8;ipid++) {
            sFactor[it][ipid] = new double[nCent];
        }
    }
    float  sf[2];
    bool scaleYt = false;
    double min = 0.85;
    double max = 1.05;
    double D = max - min;
    int    nBins = 51;
    double d = D/(nBins-1.0);
    int npar = 1;
    double gin = 10;
    double f;
    double par;
    int iflag = 1;
    double aH, bH, aL, bL;
    TH1F *hFunc = new TH1F("hFunc","hFunc",nBins,min-d/2,max+d/2);
    TF1 *fHigh = new TF1("HighFit","[0]+[1]*x",1.01,1.05);
    TF1 *fLow = new TF1("LowFit","[0]+[1]*x",0.85,0.95);

    minData.mCorrType = 1;
    minData.mLambda   = 10;

    // For ytyt space pid is not yet useful because of limited range due to dE/dx.
    // Now we have ToF. Check if yt range is big enough that yty space is interesting.
    for (int ic=0;ic<nCent;ic++) {
        for (int ipid=0;ipid<8;ipid++) {
            sprintf(inFileName,"%s/%s%s.root",dirName,inNames[ic],oname[ipid]);
            tf        = new TFile(inFileName);
            tf->cd();
            ehelp     = new StEStructSupport(tf,0);
            ehelp->msilent            = true;
            ehelp->mapplyDEtaFix      = false;
            ehelp->mPairNormalization = false;
            ehelp->mYtYtNormalization = true;
            if ((0 ==ipid) || (1 == ipid) || (4 == ipid) || (6 == ipid) || (7 == ipid)) {
                ehelp->mIdenticalPair = true;
            } else {
                ehelp->mIdenticalPair = false;
            }

            // Make sure LS histogram for zBin 0 is in file.
            if (0 == ehelp->histogramExists("YtYt", 0)) {
                continue;
            }

            if (!scaleYt) {
                ehelp->setBGMode(0);
                ehelp->mYtYtVolumeNormalization = true;
                ehelp->mYtYtNormalization = false;
                ehelp->mPairNormalization = false;
                sf[0] = 1;
                sf[1] = 1;
                cout << ">>>>>Not fitting scale factors for centrality " << ic << " pid bin " << ipid << ", Using YtYtNormalization = " << ehelp->mYtYtNormalization << endl;
                ytyt  = ehelp->buildChargeTypes("YtYt",5,sf);
                ytytC = ehelp->buildCommon("YtYt",5,sf);
                sytdyt  = ehelp->buildChargeTypes("SYtDYt",5,sf);
                sytdytC = ehelp->buildCommon("SYtDYt",5,sf);
            } else {
                // Here we try minimizing volume of \Delta\rho/\sqrt(\rho_{ref})
                // constraining the quantity to be mostly positive.
                ehelp->setBGMode(1);
                minData.mSupport    = ehelp;
                minData.mChargeType = 0;
                for (int ia=1;ia<=nBins;ia++) {
                    par = min + float(ia-1)*d;
                    minimizeNegative2(npar,&gin,f,&par,iflag);
                    hFunc->SetBinContent(ia,f);
                }
                hFunc->Fit("HighFit","NORQ");
                aH = fHigh->GetParameter(0);
                bH = fHigh->GetParameter(1);
                hFunc->Fit("LowFit","NORQ");
                aL = fLow->GetParameter(0);
                bL = fLow->GetParameter(1);
                if (bL > 0  || bH < 0) {
                    cout << "Suspect fit for ic = " << ic << ", ipid = " << ipid << ", LS " << endl;
                }
                sFactor[0][ipid][ic] = -(aH-aL)/(bH-bL);

                minData.mChargeType = 1;
                for (int ia=1;ia<=nBins;ia++) {
                    par = min + float(ia-1)*d;
                    minimizeNegative2(npar,&gin,f,&par,iflag);
                    hFunc->SetBinContent(ia,f);
                }
                hFunc->Fit("HighFit","NORQ");
                aH = fHigh->GetParameter(0);
                bH = fHigh->GetParameter(1);
                hFunc->Fit("LowFit","NORQ");
                aL = fLow->GetParameter(0);
                bL = fLow->GetParameter(1);
                if (bL > 0  || bH < 0) {
                    cout << "Suspect fit for ic = " << ic << ", ipid = " << ipid << ", US " << endl;
                }
                sFactor[1][ipid][ic] = -(aH-aL)/(bH-bL);


                sf[0] = sFactor[0][ipid][ic];
                sf[1] = sFactor[1][ipid][ic];
                ytyt  = ehelp->buildChargeTypes("YtYt",5,sf);
                ytytC = ehelp->buildCommon("YtYt",5,sf);
                sytdyt  = ehelp->buildChargeTypes("SYtDYt",5,sf);
                sytdytC = ehelp->buildCommon("SYtDYt",5,sf);
            }

            out->cd();
            for (int icharge=0;icharge<4;icharge++) {
                TString name(pidName[ipid]);
                name += "_YtYt"; name += chargeName[icharge];  name += ic;
                ytyt[icharge]->SetName(name.Data());
                ytyt[icharge]->SetTitle(name.Data());
                ytyt[icharge]->Write();
                TString name(pidName[ipid]);
                name += "_YtYt"; name += chargeType[icharge];  name += ic;
                ytytC[icharge]->SetName(name.Data());
                ytytC[icharge]->SetTitle(name.Data());
                ytytC[icharge]->Write();

                TString name(pidName[ipid]);
                name += "_SYtDYt"; name += chargeName[icharge];  name += ic;
                sytdyt[icharge]->SetName(name.Data());
                sytdyt[icharge]->SetTitle(name.Data());
                sytdyt[icharge]->Write();
                TString name(pidName[ipid]);
                name += "_SYtDYt"; name += chargeType[icharge];  name += ic;
                sytdytC[icharge]->SetName(name.Data());
                sytdytC[icharge]->SetTitle(name.Data());
                sytdytC[icharge]->Write();
            }
            delete tf;
            delete ehelp;
        }
    }
    if (scaleYt) {
        cout << "<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<" << endl;
        cout << " Here are Yt scale factors for rho_{ref}" << endl;
        printf(" Centrality   all     pi_pi   pi_K    pi_p    K_K     K_p     p_p     o_o\n");
        for (int ic=0;ic<nCent;ic++) {
            printf("%4i US     %7.3f %7.3f %7.3f %7.3f %7.3f %7.3f %7.3f %7.3f\n",ic,
                    sFactor[0][0][ic],sFactor[0][1][ic],
                    sFactor[0][2][ic],sFactor[0][3][ic],
                    sFactor[0][4][ic],sFactor[0][5][ic],
                    sFactor[0][6][ic],sFactor[0][7][ic]);
            printf("%4i LS     %7.3f %7.3f %7.3f %7.3f %7.3f %7.3f %7.3f %7.3f\n",ic,
                    sFactor[1][0][ic],sFactor[1][1][ic],
                    sFactor[1][2][ic],sFactor[1][3][ic],
                    sFactor[1][4][ic],sFactor[1][5][ic],
                    sFactor[1][6][ic],sFactor[1][7][ic]);
        }
    }
    out->Close();
//    delete out;
    for (int it=0;it<2;it++) {
        for (int ipid=0;ipid<8;ipid++) {
            delete sFactor[it][ipid];
        }
    }
}
