void combineHistograms9(const char *dirName, const char **inNames, const char *outName, int nCent) {

    // -- Calculate \Delta\rho/\sqrt{\rho_{ref}} for all quantities, i.e. Phi-Phi, Eta-Eta, DPhi-DEta etc.
    //
    // root.exe -q -b combineHistograms9.C("dir","inFileList","outFileName",numFiles)
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
    TH2D **ptsedp;
    TH2D **ptsedpC;
    TH2D **ptdedp;
    TH2D **ptdedpC;
    TH2D **sedp;
    TH2D **sedpC;
    TH2D **dedp;
    TH2D **dedpC;
    TH2D **ptetaeta;
    TH2D **ptetaetaC;
    TH2D **etaeta;
    TH2D **etaetaC;
    TH2D **ptphiphi;
    TH2D **ptphiphiC;
    TH2D **phiphi;
    TH2D **phiphiC;

    const char* binName[]={"all","soft","hard","below4","below3","below2","below1","above1","above2","above3","above4","one","onetwo","onethree","two","twothree","three"};
    const char* chargeName[] = {"_LS_", "_US_", "_CD_", "_CI_"};
    const char* chargeType[] = {"_PP_", "_PM_", "_MP_", "_MM_"};
    char outFileName[1024];
    sprintf(outFileName,"%s/%s.root",dirName,outName);
    TFile *out = new TFile(outFileName,"RECREATE");

    char inFileName[1024];

    // For axial space near/away side is not useful.
    for (int ic=0;ic<nCent;ic++) {
        printf("Centrality %2i:  d2N/dEdP    pHat_{A+}   pHat_{A-}   pHat_{B+}   pHat_{B-}\n",ic);
        for (int ibin=0;ibin<17;ibin++) {
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

            ptsedpC   = ehelp->buildPtCommon("SEtaDPhi",2,subtract);
            ptdedpC   = ehelp->buildPtCommon("DEtaDPhi",2,subtract);
            ptetaetaC = ehelp->buildPtCommon("EtaEta",2,subtract);
            ptphiphiC = ehelp->buildPtCommon("PhiPhi",2,subtract);

            ptsedp   = ehelp->buildPtChargeTypes("SEtaDPhi",2,subtract);
            ptdedp   = ehelp->buildPtChargeTypes("DEtaDPhi",2,subtract);
            ptetaeta = ehelp->buildPtChargeTypes("EtaEta",2,subtract);
            ptphiphi = ehelp->buildPtChargeTypes("PhiPhi",2,subtract);

            sedpC     = ehelp->buildCommon("SEtaDPhi",2);
            dedpC     = ehelp->buildCommon("DEtaDPhi",2);
            etaetaC   = ehelp->buildCommon("EtaEta",2);
            phiphiC   = ehelp->buildCommon("PhiPhi",2);

            sedp     = ehelp->buildChargeTypes("SEtaDPhi",2);
            dedp     = ehelp->buildChargeTypes("DEtaDPhi",2);
            etaeta   = ehelp->buildChargeTypes("EtaEta",2);
            phiphi   = ehelp->buildChargeTypes("PhiPhi",2);

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
                name += "_PtDEtaDPhi"; name += chargeType[icharge];  name += ic;
                if (ptdedpC) {
                    ptdedpC[icharge]->SetName(name.Data());
                    ptdedpC[icharge]->SetTitle(name.Data());
                    ptdedpC[icharge]->Write();
                }
                TString name(binName[ibin]);
                name += "_PtSEtaDPhi"; name += chargeType[icharge];  name += ic;
                if (ptsedpC) {
                    ptsedpC[icharge]->SetName(name.Data());
                    ptsedpC[icharge]->SetTitle(name.Data());
                    ptsedpC[icharge]->Write();
                }


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
            double scale = ehelp->getCIdNdEtadPhi();
            double *ptHat = ehelp->getptHat();
            printf("  bin %8s  %7.3f   %7.3f     %7.3f     %7.3f     %7.3f\n",binName[ibin],scale,ptHat[0],ptHat[1],ptHat[2],ptHat[3]);
            delete [] ptHat;

            delete tf;
            delete ehelp;
        }
    }

    out->Close();
//    delete out;
}
