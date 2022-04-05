void combineHistograms8(const char *dirName, const char **inNames, const char *outName, int nCent) {

    // -- Calculate \Delta\rho/\sqrt{\rho_{ref}} for all quantities, i.e. Phi-Phi, Eta-Eta, DPhi-DEta etc.
    //
    // root.exe -q -b combineHistograms3.C("dir","inFileList","outFileName",numFiles)
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
    TH2F **ptsedp;
    TH2F **ptsedpC;
    TH2F **ptdedp;
    TH2F **ptdedpC;
    TH2F **sedp;
    TH2F **sedpC;
    TH2F **dedp;
    TH2F **dedpC;
    TH2F **ptetaeta;
    TH2F **ptetaetaC;
    TH2F **etaeta;
    TH2F **etaetaC;
    TH2F **ptphiphi;
    TH2F **ptphiphiC;
    TH2F **phiphi;
    TH2F **phiphiC;

    const char* binName[]={"all","soft","neck","hard","softNeck","softHard","neckHard"};
    const char* chargeName[] = {"_LS_", "_US_", "_CD_", "_CI_"};
    const char* chargeType[] = {"_PP_", "_PM_", "_MP_", "_MM_"};
    char outFileName[1024];
    sprintf(outFileName,"%s/%s.root",dirName,outName);
    TFile *out = new TFile(outFileName,"RECREATE");

    char inFileName[1024];

    for (int ic=0;ic<nCent;ic++) {
        printf("Scale factors for centrality %2i,    ++       +-       -+       --       CI   \n",ic);
        for (int ibin=0;ibin<7;ibin++) {
            sprintf(inFileName,"%s/%s%s.root",dirName,inNames[ic],binName[ibin]);
            tf        = new TFile(inFileName);
            tf->cd();
            ehelp     = new StEStructSupport(tf,0);
            ehelp->msilent            = true;
            ehelp->mapplyDEtaFix      = false;
            ehelp->mPairNormalization = true;
            ehelp->mIdenticalPair     = true;
            ehelp->setBGMode(0);
            int subtract = 1;
            ptsedpC   = (TH2F**) ehelp->buildPtCommon("SEtaDPhi",2,subtract);
            ptdedpC   = (TH2F**) ehelp->buildPtCommon("DEtaDPhi",2,subtract);
            ptetaetaC = (TH2F**) ehelp->buildPtCommon("EtaEta",2,subtract);
            ptphiphiC = (TH2F**) ehelp->buildPtCommon("PhiPhi",2,subtract);

            ptsedp   = (TH2F**) ehelp->buildPtChargeTypes("SEtaDPhi",2,subtract);
            ptdedp   = (TH2F**) ehelp->buildPtChargeTypes("DEtaDPhi",2,subtract);
            ptetaeta = (TH2F**) ehelp->buildPtChargeTypes("EtaEta",2,subtract);
            ptphiphi = (TH2F**) ehelp->buildPtChargeTypes("PhiPhi",2,subtract);

            sedpC     = (TH2F**) ehelp->buildCommon("SEtaDPhi",2);
            dedpC     = (TH2F**) ehelp->buildCommon("DEtaDPhi",2);
            etaetaC   = (TH2F**) ehelp->buildCommon("EtaEta",2);
            phiphiC   = (TH2F**) ehelp->buildCommon("PhiPhi",2);

            sedp     = (TH2F**) ehelp->buildChargeTypes("SEtaDPhi",2);
            dedp     = (TH2F**) ehelp->buildChargeTypes("DEtaDPhi",2);
            etaeta   = (TH2F**) ehelp->buildChargeTypes("EtaEta",2);
            phiphi   = (TH2F**) ehelp->buildChargeTypes("PhiPhi",2);

            out->cd();
            for (int icharge=0;icharge<4;icharge++) {
                TString name(binName[ibin]);
                name += "_NDEtaDPhi"; name += chargeName[icharge];  name += ic;
                if (dedp) {
                    dedp[icharge]->SetName(name.Data());
                    dedp[icharge]->SetTitle(name.Data());
                    dedp[icharge]->Write();
                }
                TString name(binName[ibin]);
                name += "_NSEtaDPhi"; name += chargeName[icharge];  name += ic;
                if (sedp) {
                    sedp[icharge]->SetName(name.Data());
                    sedp[icharge]->SetTitle(name.Data());
                    sedp[icharge]->Write();
                }
                TString name(binName[ibin]);
                name += "_PtDEtaDPhi"; name += chargeName[icharge];  name += ic;
                if (ptdedp) {
                    ptdedp[icharge]->SetName(name.Data());
                    ptdedp[icharge]->SetTitle(name.Data());
                    ptdedp[icharge]->Write();
                }
                TString name(binName[ibin]);
                name += "_PtSEtaDPhi"; name += chargeName[icharge];  name += ic;
                if (ptsedp) {
                    ptsedp[icharge]->SetName(name.Data());
                    ptsedp[icharge]->SetTitle(name.Data());
                    ptsedp[icharge]->Write();
                }

                TString name(binName[ibin]);
                name += "_NDEtaDPhi"; name += chargeType[icharge];  name += ic;
                if (dedpC) {
                    dedpC[icharge]->SetName(name.Data());
                    dedpC[icharge]->SetTitle(name.Data());
                    dedpC[icharge]->Write();
                }
                TString name(binName[ibin]);
                name += "_NSEtaDPhi"; name += chargeType[icharge];  name += ic;
                if (sedpC) {
                    sedpC[icharge]->SetName(name.Data());
                    sedpC[icharge]->SetTitle(name.Data());
                    sedpC[icharge]->Write();
                }
                TString name(binName[ibin]);
                name += "_PtEtaDPhi"; name += chargeType[icharge];  name += ic;
                if (ptdedpC) {
                    ptdedpC[icharge]->SetName(name.Data());
                    ptdedpC[icharge]->SetTitle(name.Data());
                    ptdedpC[icharge]->Write();
                }
                TString name(binName[ibin]);
                if (name += ) {
                    name += "_PtEtaSPhi"; name += chargeType[icharge];  name += ic;
                    ptsedpC[icharge]->SetName(name.Data());
                    ptsedpC[icharge]->SetTitle(name.Data());
                }
                ptsedpC[icharge]->Write();


                TString name(binName[ibin]);
                name += "_NEtaEta"; name += chargeName[icharge];  name += ic;
                if (etaeta) {
                    etaeta[icharge]->SetName(name.Data());
                    etaeta[icharge]->SetTitle(name.Data());
                    etaeta[icharge]->Write();
                }
                TString name(binName[ibin]);
                name += "_PtEtaEta"; name += chargeName[icharge];  name += ic;
                if (ptetaeta) {
                    ptetaeta[icharge]->SetName(name.Data());
                    ptetaeta[icharge]->SetTitle(name.Data());
                    ptetaeta[icharge]->Write();
                }

                TString name(binName[ibin]);
                name += "_NEtaEta"; name += chargeType[icharge];  name += ic;
                if (etaetaC) {
                    etaetaC[icharge]->SetName(name.Data());
                    etaetaC[icharge]->SetTitle(name.Data());
                    etaetaC[icharge]->Write();
                }
                TString name(binName[ibin]);
                name += "_PtEtaEta"; name += chargeType[icharge];  name += ic;
                if (ptetaetaC) {
                    ptetaetaC[icharge]->SetName(name.Data());
                    ptetaetaC[icharge]->SetTitle(name.Data());
                    ptetaetaC[icharge]->Write();
                }


                TString name(binName[ibin]);
                name += "_NPhiPhi"; name += chargeName[icharge];  name += ic;
                if (phiphi) {
                    phiphi[icharge]->SetName(name.Data());
                    phiphi[icharge]->SetTitle(name.Data());
                    phiphi[icharge]->Write();
                }
                TString name(binName[ibin]);
                name += "_PtPhiPhi"; name += chargeName[icharge];  name += ic;
                if (ptphiphi) {
                    ptphiphi[icharge]->SetName(name.Data());
                    ptphiphi[icharge]->SetTitle(name.Data());
                    ptphiphi[icharge]->Write();
                }

                TString name(binName[ibin]);
                name += "_NPhiPhi"; name += chargeType[icharge];  name += ic;
                if (phiphiC) {
                    phiphiC[icharge]->SetName(name.Data());
                    phiphiC[icharge]->SetTitle(name.Data());
                    phiphiC[icharge]->Write();
                }
                TString name(binName[ibin]);
                name += "_PtPhiPhi"; name += chargeType[icharge];  name += ic;
                if (ptphiphiC) {
                    ptphiphiC[icharge]->SetName(name.Data());
                    ptphiphiC[icharge]->SetTitle(name.Data());
                    ptphiphiC[icharge]->Write();
                }
            }
            // Calculate and print scale factors
            double *scale = ehelp->getScaleFactors();
            printf("  bin %8s                    %7.2f  %7.2f  %7.2f  %7.2f  %7.2f\n",binName[ibin],scale[0],scale[1],scale[2],scale[3],scale[4]);
            delete [] scale;
            delete tf;
            delete ehelp;
        }
    }

    TH2D **ytyt;
    TH2D **ytytC;
    TH2D **sytdyt;
    TH2D **sytdytC;
    gROOT->LoadMacro("minimizeNegative.C");
     double *sFactor[2][3], *eSFactor[2][3];
    for (int it=0;it<2;it++) {
        for (int ibin=0;ibin<3;ibin++) {
            sFactor[it][ibin] = new double[nCent];
            eSFactor[it][ibin] = new double[nCent];
        }
    }
   float  sf[2];
    double argList[10];
    double start = 0.95;
    double step  = 0.001;
    double bmin  = 0.9;
    double bmax  = 1.1;
    int errFlag = 0;
    minData.mCorrType =  1;
    minData.mLambda   = 10;

    // For ytyt space soft/neck/hard is not useful.
    int ytytBins[] = {0};
    for (int ic=0;ic<nCent;ic++) {
        for (int ibin=0;ibin<1;ibin++) {
            sprintf(inFileName,"%s/%s%s.root",dirName,inNames[ic],binName[ytytBins[ibin]]);
            tf        = new TFile(inFileName);
            tf->cd();
            ehelp     = new StEStructSupport(tf,0);
            ehelp->msilent            = true;
            ehelp->mapplyDEtaFix      = false;
            ehelp->mPairNormalization = false;
            ehelp->mIdenticalPair     = true;
            ehelp->setBGMode(1);

            // Make sure LS histogram for zBin 0 is in file.
            if (0 == ehelp->histogramExists("YtYt", 0)) {
                continue;
            }

            // A lot of stuff so we can find a scaling factor for \rho_{ref}
            // such that \Delta\rho is almost always positive.
            minData.mSupport    = ehelp;
            minData.mChargeType = 0;

            minuit = new TMinuit(1);
            minuit->SetFCN(minimizeNegative);
            minuit->mnparm( 0, "rho_ref scale factor", start, step, bmin, bmax, errFlag );
            minuit->SetErrorDef(1);
            minuit->SetPrintLevel(-1);
            argList[0] = 1;
            minuit->mnexcm("SET STR",argList,1,errFlag);
            argList[0] = 500;
            cout << ">>>>>Starting scale factor 0 fit for centrality " << ic << " yt bin " << ibin << endl;
            minuit->mnexcm("MIGRAD",argList,1,errFlag);
            minuit->GetParameter(0,sFactor[0][ibin][ic],eSFactor[0][ibin][ic]);
            if (0 != errFlag) {
                cout << "++++Out of chesse error; Fit failed. Using scale factor = 1" << endl;
                sFactor[0][ibin][ic]  =   1;
                eSFactor[0][ibin][ic] = 100;
            }
            delete minuit;

            // Seems like I should be able to reset the TMinuit object to do a
            // different fit. Didn't work on my first attempts, so I just create
            // a new one.
            minData.mChargeType = 1;
            minuit = new TMinuit(1);
            minuit->SetFCN(minimizeNegative);
            minuit->mnparm( 0, "rho_ref scale factor", start, step, bmin, bmax, errFlag );
            minuit->SetErrorDef(1);
            minuit->SetPrintLevel(-1);
            argList[0] = 1;
            minuit->mnexcm("SET STR",argList,1,errFlag);
            argList[0] = 500;
            cout << ">>>>>Starting scale factor 1 fit for centrality " << ic << " yt bin " << ibin << endl;
            minuit->mnexcm("MIGRAD",argList,1,errFlag);
            minuit->GetParameter(0,sFactor[1][ibin][ic],eSFactor[1][ibin][ic]);
            if (0 != errFlag) {
                cout << "++++Out of chesse error; Fit failed. Using scale factor = 1" << endl;
                sFactor[1][ibin][ic]  =   1;
                eSFactor[1][ibin][ic] = 100;
            }
            delete minuit;

            sf[0] = sFactor[0][ibin][ic];
            sf[1] = sFactor[1][ibin][ic];
            ytyt  = ehelp->buildChargeTypes("YtYt",5,sf);
            ytytC = ehelp->buildCommon("YtYt",5,sf);
            sytdyt  = ehelp->buildChargeTypes("SYtDYt",5,sf);
            sytdytC = ehelp->buildCommon("SYtDYt",5,sf);

            out->cd();
            for (int icharge=0;icharge<4;icharge++) {
                TString name(binName[ytytBins[ibin]]);
                name += "_YtYt"; name += chargeName[icharge];  name += ic;
                ytyt[icharge]->SetName(name.Data());
                ytyt[icharge]->SetTitle(name.Data());
                ytyt[icharge]->Write();
                TString name(binName[ytytBins[ibin]]);
                name += "_YtYt"; name += chargeType[icharge];  name += ic;
                ytytC[icharge]->SetName(name.Data());
                ytytC[icharge]->SetTitle(name.Data());
                ytytC[icharge]->Write();

                TString name(binName[ytytBins[ibin]]);
                name += "_SYtDYt"; name += chargeName[icharge];  name += ic;
                sytdyt[icharge]->SetName(name.Data());
                sytdyt[icharge]->SetTitle(name.Data());
                sytdyt[icharge]->Write();
                TString name(binName[ytytBins[ibin]]);
                name += "_SYtDYt"; name += chargeType[icharge];  name += ic;
                sytdytC[icharge]->SetName(name.Data());
                sytdytC[icharge]->SetTitle(name.Data());
                sytdytC[icharge]->Write();
            }
            delete tf;
            delete ehelp;
        }
    }
    cout << "<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<" << endl;
    cout << " Here are Yt scale factors for rho_{ref}" << endl;
    printf(" Centrality   all LS       SS LS        AS LS        all US       SS US        AS US    \n");
    for (int ic=0;ic<nCent;ic++) {
        printf("%6i    %9.3f(%3.0f) %7.3f(%3.0f) %7.3f(%3.0f) %7.3f(%3.0f) %7.3f(%3.0f) %7.3f(%3.0f)\n",ic,
            sFactor[0][0][ic],1000*eSFactor[0][0][ic],sFactor[0][1][ic],1000*eSFactor[0][1][ic],sFactor[0][2][ic],1000*eSFactor[0][2][ic],
            sFactor[1][0][ic],1000*eSFactor[1][0][ic],sFactor[1][1][ic],1000*eSFactor[1][1][ic],sFactor[1][2][ic],1000*eSFactor[1][2][ic]);
    }
    out->Close();
//    delete out;
    for (int it=0;it<2;it++) {
        for (int ibin=0;ibin<3;ibin++) {
            delete sFactor[it][ibin];
            delete eSFactor[it][ibin];
        }
    }
}
