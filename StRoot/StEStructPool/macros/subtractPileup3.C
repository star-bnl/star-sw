void subtractPileup3(const char *pileupCutFile, const char *noPileupCutFile, const char *subFile, double eff=0.75) {

    // -- Look for correlation histograms in both input files solve for pileup and
    //    noPileup (assuming 75% rejection efficiency for now) and write to outputfile.
    //
    // root.exe -q -b subtractPileup.C("dir","cutFile","noCutFile","subFile")
    // 
    //    cutFile is root histogram file with pileup cuts invoked
    //    noCutFile is root histogram file without pileup cuts invoked
    //    Solve for extrapolation to no pileup and to only pileup.
    //    Write histograms to subFile
    //    Filenames include paths.

    TFile *tfCut, *tfNoCut, *tfOut;
    TH2D *cut, *noCut, *noPileup, *pileup;

    const char* binName[]={"all","awayside","nearside","soft","softAS","softNS","neck","neckAS","neckNS","hard","hardAS","hardNS","softHard","softHardAS","softHardNS"};
    const char* type[] = {"PP", "PM", "MP", "MM", "LS", "US", "CD", "CI"};
    //>>>>> I may have forgotten things that should be in knd.
    const char* knd[]={"YtYt", "SYtDYt", "PtPt",
                       "NEtaEta", "PtEtaEta", "NPhiPhi", "PtPhiPhi",
                       "NDEtaDPhi", "PtDEtaDPhi", "NSEtaDPhi", "PtSEtaDPhi"};

    char buffer[1024];
    TFile *tfCut = new TFile(pileupCutFile);
    TFile *tfNoCut = new TFile(noPileupCutFile);
    TFile *tfOut = new TFile(subFile,"RECREATE");

    // Don't know how to ask root for list of histograms.
    // Look for names we believe are possible
    for (int it=0;it<8;it++) {
        for (int ik=0;ik<11;ik++) {
            for (int ibin=0;ibin<15;ibin++) {
                int ic=0;
                sprintf(buffer,"%s_%s_%s_%i",binName[ibin],knd[ik],type[it],ic);
                tfCut->GetObject(buffer,cut);
                tfNoCut->GetObject(buffer,noCut);
                while (cut && noCut) {
                    // Found something.
                    sprintf(buffer,"%s_%s_%s_%i_pileup",binName[ibin],knd[ik],type[it],ic);
                    noPileup = (TH2D *) cut->Clone();
                    pileup   = (TH2D *) cut->Clone(buffer);
                    noPileup->Reset();
                    pileup->Reset();

                    noPileup->Add(cut,noCut,1,-(1-eff));
                    noPileup->Scale(1/eff);
                    pileup->Add(noCut,cut,1,-1);
                    pileup->Scale(1/eff);
                    // Adjust errors to be only errors of cut.
                    for (int ix=1;ix<=noCut->GetNbinsX();ix++) {
                        for (int iy=1;iy<=noCut->GetNbinsY();iy++) {
                            double err = cut->GetBinError(ix,iy);
                            noPileup->SetBinError(ix,iy,err);
                            pileup->SetBinError(ix,iy,err);
                        }
                    }
                    // Write to subFile
                    tfOut->cd();
                    noPileup->Write();
                    pileup->Write();
                    // Clean up histogram space.
                    delete pileup;
                    delete noPileup;

                    // Check for next centrality
                    ic++;
                    sprintf(buffer,"%s_%s_%s_%i",binName[ibin],knd[ik],type[it],ic);
                    tfCut->GetObject(buffer,cut);
                    tfNoCut->GetObject(buffer,noCut);
               }
            }
        }
    }
    tfOut->Close();    
}
