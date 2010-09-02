#include <iostream>
#include "TFile.h"
#include "TString.h"
#include "TH1D.h"
#include "TH2D.h"


void addJobs(const char* outFile, const char** inFile, int numFiles) {

  // -- See addCentralities.C for description.
  //    This is the same code as addCentralities.C except we are summing from parallel directories.
  //    Need to keep each z-buffer, multiplicity sub-bin and job separate so we can add ratios
  //    in selectAll.
  //    Had I been more clever I would have made the loop over input files flexible enough
  //    to handle adding jobs or summing centralities within a job.
  //

  // Need to determine how many cutBins, parentBins and zBuffers in each file.
  // I think we need the same numbers of cutBins and parentBins, but we can
  // allow a different number of zBuffers
  // Add copying of Density histograms if they are in the file.
    int nZBins, nzBin[20], ozBin[20], nTotZBins = 0;
    int nCuts, nCutBins;
    int nDens, nDenBins;
    int nParents, nParentBins;
    for (int jf=0;jf<numFiles;jf++) {
cout << "Input file " << inFile[jf] << endl;
        TFile *in = new TFile(inFile[jf]);

        nZBins = 0;
        TString name("NEventsSib_zBuf_");  name += nZBins;
        while (gDirectory->Get(name.Data())) {
            name = "NEventsSib_zBuf_";  name += nZBins;
            nZBins++;
        }
        nZBins--;
        nParents = 0;
        name = "meanPtP_parentBin"; name += nParents; name += "_zBuf_0";
        while (gDirectory->Get(name.Data())) {
            name ="meanPtP_parentBin"; name += nParents; name += "_zBuf_0";
            nParents++;
        }
        nParents--;

        // Had been looking for number of YtYt histograms to define number of cut bins.
        // YtYt is now an optional set of histograms.
        // Look for DEtaDPhi now. So far we always require those.
        nCuts = 0;
        name = "SibppDEtaDPhiArr_cutBin_"; name += nCuts; name += "_zBuf_0";
        while (gDirectory->Get(name.Data())) {
            name ="SibppDEtaDPhiArr_cutBin_"; name += nCuts; name += "_zBuf_0";
            nCuts++;
        }
        nCuts--;

        nDens = 0;
        name = "SibppTPCAvgTSep_cutBin_"; name += nDens; name += "_zBuf_0";
        while (gDirectory->Get(name.Data())) {
            name ="SibppTPCAvgTSep_cutBin_"; name += nDens; name += "_zBuf_0";
            nDens++;
        }
        nDens--;

        nzBin[jf]  = nZBins;
        ozBin[jf]  = nTotZBins;
        nTotZBins += nZBins;
        if (0 == jf) {
            nParentBins = nParents;
            nCutBins = nCuts;
            nDenBins = nDens;
        } else {
            if (nParents != nParentBins) {
                cout<<"Error in ParentBins. First file had "<< nParentBins << " while file " << jf << " has " << nParents << endl;
                return;
            }
            if (nCuts != nCutBins) {
                cout<<"Error in CutBins. First file had "<< nCutBins << " while file " << jf << " has " << nCuts << endl;
                return;
            }
            if (nDens != nDenBins) {
                cout<<"Error in DensityBins. First file had "<< nDenBins << " while file " << jf << " has " << nDens << endl;
                return;
            }
        }
        in->Close();
        delete in;
    }

    // Declare histograms
    TH1D *tmp;
    TH1D *mHmix = 0;
    TH1D *mHcb = 0;
    TH1D *mHptAll = 0;
    TH2D *mHMixedDistance = 0;
    TH2D *mHMixedMults = 0;
    TH2D *tmp2;

    // Hack: Should do memory allocation, but since these are only arrays of pointers
    // I will just declare them to be bigger than I need.
    if (nTotZBins > 100) {
        cout << "Oops... I declared total number of Z bins as less than 100. You have " << nTotZBins << endl;
        return;
    }
    TH1D *mHNEventsSib[100];
    TH1D *mHNEventsMix[100];
    TH1D *mHNEventsPosSib[100];
    TH1D *mHNEventsPosMix[100];
    TH1D *mHNEventsNegSib[100];
    TH1D *mHNEventsNegMix[100];

    if (nParentBins > 20) {
        cout << "Oops... I declared number of Parent bins as less than 20. You have " << nParentBins << endl;
        return;
    }
    TH1D *mHMeanPtP[20][100];
    TH1D *mHMeanPtM[20][100];
    TH1D *mHEtaP[20][100];
    TH1D *mHEtaM[20][100];

    if (nCutBins > 40) {
        cout << "Oops... I declared number of Cut bins as less than 40. You have " << nCutBins << endl;
        return;
    }
    TH2D *mHYtYt[40][100][8];
    TH2D *mHNYtYt[40][100][8];
    TH2D *mHPtPt[40][100][8];

    TH2D *mHPhiPhi[40][100][8];
    TH2D *mHNPhiPhi[40][100][8];
    TH2D *mHEtaEta[40][100][8];
    TH2D *mHPrPhiPhi[40][100][8];
    TH2D *mHPrEtaEta[40][100][8];
    TH2D *mHPaPhiPhi[40][100][8];
    TH2D *mHPaEtaEta[40][100][8];
    TH2D *mHPbPhiPhi[40][100][8];
    TH2D *mHPbEtaEta[40][100][8];

    TH2D *mHAtSYtDYt[40][100][8];
    TH2D *mHAtNSYtDYt[40][100][8];
    TH2D *mHJtDEtaDPhi[40][100][8];
    TH2D *mHJtNDEtaDPhi[40][100][8];
    TH2D *mHPrJtDEtaDPhi[40][100][8];
    TH2D *mHPaJtDEtaDPhi[40][100][8];
    TH2D *mHPbJtDEtaDPhi[40][100][8];

    TH2D *mHJtSEtaDPhi[40][100][8];
    TH2D *mHJtNSEtaDPhi[40][100][8];
    TH2D *mHPrJtSEtaDPhi[40][100][8];
    TH2D *mHPaJtSEtaDPhi[40][100][8];
    TH2D *mHPbJtSEtaDPhi[40][100][8];

    TH1D *mHQinv[40][100][8];
    TH1D *mHNQinv[40][100][8];

    // Density histograms. TPC separation
    if (nDenBins > 40) {
        cout << "Oops... I declared number of Density bins as less than 40. You have " << nDenBins << endl;
        return;
    }
    TH1D *mHTPCAvgTSep[40][100][8];
    TH1D *mHTPCAvgZSep[40][100][8];
    TH1D *mHTPCEntTSep[40][100][8];
    TH1D *mHTPCEntZSep[40][100][8];
    TH1D *mHTPCMidTSep[40][100][8];
    TH1D *mHTPCMidZSep[40][100][8];
    TH1D *mHTPCExitTSep[40][100][8];
    TH1D *mHTPCExitZSep[40][100][8];

    TH1D *mHTPCMidTdptP[40][100][8];
    TH1D *mHTPCMidTdptN[40][100][8];
    TH1D *mHTPCMidZdptP[40][100][8];
    TH1D *mHTPCMidZdptN[40][100][8];

    TH2D *mHTPCAvgTZ[40][100][8];
    TH2D *mHTPCEntTZ[40][100][8];
    TH2D *mHTPCMidTZ[40][100][8];
    TH2D *mHTPCMidTZC[40][100][8];
    TH2D *mHTPCMidTZNC[40][100][8];
    TH2D *mHTPCExitTZ[40][100][8];
    TH2D *mHTPCEntTdpt[40][100][8];
    TH2D *mHTPCMidTdpt[40][100][8];
    TH2D *mHTPCExitTdpt[40][100][8];

    TFile *out = new TFile(outFile,"RECREATE");

    // With histograms declared we can loop over files making clones.
    for (int jf=0;jf<numFiles;jf++) {
cout << "Opening input file " << jf << endl;
        TFile *in = new TFile(inFile[jf]);

      // Couple of histograms with no cutBin, parentBin or zBuf information.
      // For first file we make clones, rest of the files we add to existing histograms.
        in->GetObject("EventMixing",tmp);
        if (tmp) {
            if (mHmix) {
                mHmix->Add(tmp);
            } else {
                out->cd();
                mHmix = (TH1D *) tmp->Clone();
            }
        }
        in->GetObject("hcb",tmp);
        if (tmp) {
            if (mHcb) {
                mHcb->Add(tmp);
            } else {
                out->cd();
                mHcb = (TH1D *) tmp->Clone();
            }
        }
        in->GetObject("ptAll",tmp);
        if (tmp) {
            if (mHptAll) {
                mHptAll->Add(tmp);
            } else {
                out->cd();
                mHptAll = (TH1D *) tmp->Clone();
            }
        }
        in->GetObject("MixedDistance",tmp2);
        if (tmp2) {
            if (mHMixedDistance) {
                mHMixedDistance->Add(tmp2);
            } else {
                out->cd();
                mHMixedDistance = (TH2D *) tmp2->Clone();
            }
        }
        in->GetObject("MixedMults",tmp2);
        if (tmp2) {
            if (mHMixedMults) {
                mHMixedMults->Add(tmp2);
            } else {
                out->cd();
                mHMixedMults = (TH2D *) tmp2->Clone();
            }
        }


        // First set integrated over cutBins (and parentBins)
        TString histName, outName;
        for (int iz=0;iz<nzBin[jf];iz++) {
            histName = "NEventsSib_zBuf_"; histName += iz;
            in->GetObject(histName.Data(),tmp);
            out->cd();
            mHNEventsSib[iz+ozBin[jf]] = (TH1D *) tmp->Clone();
            outName = "NEventsSib_zBuf_"; outName += iz+ozBin[jf];
            mHNEventsSib[iz+ozBin[jf]]->SetName(outName.Data());

            histName = "NEventsMix_zBuf_"; histName += iz;
            in->GetObject(histName.Data(),tmp);
            out->cd();
            mHNEventsMix[iz+ozBin[jf]] = (TH1D *) tmp->Clone();
            outName = "NEventsMix_zBuf_";  outName += iz+ozBin[jf];
            mHNEventsMix[iz+ozBin[jf]]->SetName(outName.Data());

            histName = "NEventsPosSib_zBuf_"; histName += iz;
            in->GetObject(histName.Data(),tmp);
            out->cd();
            mHNEventsPosSib[iz+ozBin[jf]] = (TH1D *) tmp->Clone();
            outName = "NEventsPosSib_zBuf_";  outName += iz+ozBin[jf];
            mHNEventsPosSib[iz+ozBin[jf]]->SetName(outName.Data());

            histName = "NEventsPosMix_zBuf_"; histName += iz;
            in->GetObject(histName.Data(),tmp);
            out->cd();
            mHNEventsPosMix[iz+ozBin[jf]] = (TH1D *) tmp->Clone();
            outName = "NEventsPosMix_zBuf_";  outName += iz+ozBin[jf];
            mHNEventsPosMix[iz+ozBin[jf]]->SetName(outName.Data());

            histName = "NEventsNegSib_zBuf_"; histName += iz;
            in->GetObject(histName.Data(),tmp);
            out->cd();
            mHNEventsNegSib[iz+ozBin[jf]] = (TH1D *) tmp->Clone();
            outName = "NEventsNegSib_zBuf_";  outName += iz+ozBin[jf];
            mHNEventsNegSib[iz+ozBin[jf]]->SetName(outName.Data());

            histName = "NEventsNegMix_zBuf_"; histName += iz;
            in->GetObject(histName.Data(),tmp);
            out->cd();
            mHNEventsNegMix[iz+ozBin[jf]] = (TH1D *) tmp->Clone();
            outName = "NEventsNegMix_zBuf_";  outName += iz+ozBin[jf];
            mHNEventsNegMix[iz+ozBin[jf]]->SetName(outName.Data());
        }

        // Second set is per parent bin (and zBuf)
        for (int iz=0;iz<nzBin[jf];iz++) {
            for (int ip=0;ip<nParentBins;ip++) {
                histName = "meanPtP_parentBin"; histName += ip; histName += "_zBuf_"; histName += iz;
                in->GetObject(histName.Data(),tmp);
                out->cd();
                mHMeanPtP[ip][iz+ozBin[jf]] = (TH1D *) tmp->Clone();
                outName = "meanPtP_parentBin";  outName += ip;  outName += "_zBuf_";  outName += iz+ozBin[jf];
                mHMeanPtP[ip][iz+ozBin[jf]]->SetName(outName.Data());

                histName = "meanPtM_parentBin"; histName += ip; histName += "_zBuf_"; histName += iz;
                in->GetObject(histName.Data(),tmp);
                out->cd();
                mHMeanPtM[ip][iz+ozBin[jf]] = (TH1D *) tmp->Clone();
                outName = "meanPtM_parentBin";  outName += ip;  outName += "_zBuf_";  outName += iz+ozBin[jf];
                mHMeanPtM[ip][iz+ozBin[jf]]->SetName(outName.Data());

                histName = "etaP_parentBin"; histName += ip; histName += "_zBuf_"; histName += iz;
                in->GetObject(histName.Data(),tmp);
                out->cd();
                mHEtaP[ip][iz+ozBin[jf]] = (TH1D *) tmp->Clone();
                outName = "etaP_parentBin";  outName += ip;  outName += "_zBuf_";  outName += iz+ozBin[jf];
                mHEtaP[ip][iz+ozBin[jf]]->SetName(outName.Data());

                histName = "etaM_parentBin"; histName += ip; histName += "_zBuf_"; histName += iz;
                in->GetObject(histName.Data(),tmp);
                out->cd();
                mHEtaM[ip][iz+ozBin[jf]] = (TH1D *) tmp->Clone();
                outName = "etaM_parentBin";  outName += ip;  outName += "_zBuf_";  outName += iz+ozBin[jf];
                mHEtaM[ip][iz+ozBin[jf]]->SetName(outName.Data());
            }
        }



        // Largest set is per cut bin.
        char *type[] = {"Sibpp", "Sibpm", "Sibmp", "Sibmm", "Mixpp", "Mixpm", "Mixmp", "Mixmm"};
        for (int iz=0;iz<nzBin[jf];iz++) {
cout << "Loop over main histograms:  zBin = " << iz << endl;
            for (int ic=0;ic<nCutBins;ic++) {
                for (int it=0;it<8;it++) {
                    histName = type[it]; histName += "YtYt_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    in->GetObject(histName.Data(),tmp2);
                    if (tmp2) {
                        out->cd();
                        mHYtYt[ic][iz+ozBin[jf]][it] = (TH2D *) tmp2->Clone();
                        outName = type[it];  outName += "YtYt_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                        mHYtYt[ic][iz+ozBin[jf]][it]->SetName(outName.Data());
                    } else {
                        mHYtYt[ic][iz+ozBin[jf]][it] = 0;
                    }

                    histName = type[it]; histName += "NYtYt_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    in->GetObject(histName.Data(),tmp2);
                    if (tmp2) {
                        out->cd();
                        mHNYtYt[ic][iz+ozBin[jf]][it] = (TH2D *) tmp2->Clone();
                        outName = type[it];  outName += "NYtYt_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                        mHNYtYt[ic][iz+ozBin[jf]][it]->SetName(outName.Data());
                    } else {
                        mHNYtYt[ic][iz+ozBin[jf]][it] = 0;
                    }
                    histName = type[it]; histName += "PtPt_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    in->GetObject(histName.Data(),tmp2);
                    if (tmp2) {
                        out->cd();
                        mHPtPt[ic][iz+ozBin[jf]][it] = (TH2D *) tmp2->Clone();
                        outName = type[it];  outName += "PtPt_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                        mHPtPt[ic][iz+ozBin[jf]][it]->SetName(outName.Data());
                    } else {
                        mHPtPt[ic][iz+ozBin[jf]][it] = 0;
                    }


                    histName = type[it]; histName += "PhiPhi_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    in->GetObject(histName.Data(),tmp2);
                    if (tmp2) {
                        out->cd();
                        mHPhiPhi[ic][iz+ozBin[jf]][it] = (TH2D *) tmp2->Clone();
                        outName = type[it];  outName += "PhiPhi_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                        mHPhiPhi[ic][iz+ozBin[jf]][it]->SetName(outName.Data());
                    } else {
                        mHPhiPhi[ic][iz+ozBin[jf]][it] = 0;
                    }

                    histName = type[it]; histName += "NPhiPhi_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    in->GetObject(histName.Data(),tmp2);
                    if (tmp2) {
                        out->cd();
                        mHNPhiPhi[ic][iz+ozBin[jf]][it] = (TH2D *) tmp2->Clone();
                        outName = type[it];  outName += "NPhiPhi_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                        mHNPhiPhi[ic][iz+ozBin[jf]][it]->SetName(outName.Data());
                    } else {
                        mHNPhiPhi[ic][iz+ozBin[jf]][it] = 0;
                    }

                    histName = type[it]; histName += "EtaEta_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    in->GetObject(histName.Data(),tmp2);
                    if (tmp2) {
                        out->cd();
                        mHEtaEta[ic][iz+ozBin[jf]][it] = (TH2D *) tmp2->Clone();
                        outName = type[it];  outName += "EtaEta_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                        mHEtaEta[ic][iz+ozBin[jf]][it]->SetName(outName.Data());
                    } else {
                        mHEtaEta[ic][iz+ozBin[jf]][it] = 0;
                    }

                    histName = type[it]; histName += "PrPhiPhi_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    in->GetObject(histName.Data(),tmp2);
                    if (tmp2) {
                        out->cd();
                        mHPrPhiPhi[ic][iz+ozBin[jf]][it] = (TH2D *) tmp2->Clone();
                        outName = type[it];  outName += "PrPhiPhi_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                        mHPrPhiPhi[ic][iz+ozBin[jf]][it]->SetName(outName.Data());
                    } else {
                        mHPrPhiPhi[ic][iz+ozBin[jf]][it] = 0;
                    }

                    histName = type[it]; histName += "PrEtaEta_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    in->GetObject(histName.Data(),tmp2);
                    if (tmp2) {
                        out->cd();
                        mHPrEtaEta[ic][iz+ozBin[jf]][it] = (TH2D *) tmp2->Clone();
                        outName = type[it];  outName += "PrEtaEta_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                        mHPrEtaEta[ic][iz+ozBin[jf]][it]->SetName(outName.Data());
                    } else {
                        mHPrEtaEta[ic][iz+ozBin[jf]][it] = 0;
                    }

                    histName = type[it]; histName += "PaPhiPhi_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    in->GetObject(histName.Data(),tmp2);
                    if (tmp2) {
                        out->cd();
                        mHPaPhiPhi[ic][iz+ozBin[jf]][it] = (TH2D *) tmp2->Clone();
                        outName = type[it];  outName += "PaPhiPhi_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                        mHPaPhiPhi[ic][iz+ozBin[jf]][it]->SetName(outName.Data());
                    } else {
                        mHPaPhiPhi[ic][iz+ozBin[jf]][it] = 0;
                    }

                    histName = type[it]; histName += "PaEtaEta_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    in->GetObject(histName.Data(),tmp2);
                    if (tmp2) {
                        out->cd();
                        mHPaEtaEta[ic][iz+ozBin[jf]][it] = (TH2D *) tmp2->Clone();
                        outName = type[it];  outName += "PaEtaEta_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                        mHPaEtaEta[ic][iz+ozBin[jf]][it]->SetName(outName.Data());
                    } else {
                        mHPaEtaEta[ic][iz+ozBin[jf]][it] = 0;
                    }

                    histName = type[it]; histName += "PbPhiPhi_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    in->GetObject(histName.Data(),tmp2);
                    if (tmp2) {
                        out->cd();
                        mHPbPhiPhi[ic][iz+ozBin[jf]][it] = (TH2D *) tmp2->Clone();
                        outName = type[it];  outName += "PbPhiPhi_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                        mHPbPhiPhi[ic][iz+ozBin[jf]][it]->SetName(outName.Data());
                    } else {
                        mHPbPhiPhi[ic][iz+ozBin[jf]][it] = 0;
                    }

                    histName = type[it]; histName += "PbEtaEta_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    in->GetObject(histName.Data(),tmp2);
                    if (tmp2) {
                        out->cd();
                        mHPbEtaEta[ic][iz+ozBin[jf]][it] = (TH2D *) tmp2->Clone();
                        outName = type[it];  outName += "PbEtaEta_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                        mHPbEtaEta[ic][iz+ozBin[jf]][it]->SetName(outName.Data());
                    } else {
                        mHPbEtaEta[ic][iz+ozBin[jf]][it] = 0;
                    }


                    histName = type[it]; histName += "SYtDYt_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    in->GetObject(histName.Data(),tmp2);
                    if (tmp2) {
                        out->cd();
                        mHAtSYtDYt[ic][iz+ozBin[jf]][it] = (TH2D *) tmp2->Clone();
                        outName = type[it];  outName += "SYtDYt_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                        mHAtSYtDYt[ic][iz+ozBin[jf]][it]->SetName(outName.Data());
                    } else {
                        mHAtSYtDYt[ic][iz+ozBin[jf]][it] = 0;
                    }

                    histName = type[it]; histName += "NSYtDYt_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    in->GetObject(histName.Data(),tmp2);
                    if (tmp2) {
                        out->cd();
                        mHAtNSYtDYt[ic][iz+ozBin[jf]][it] = (TH2D *) tmp2->Clone();
                        outName = type[it];  outName += "NSYtDYt_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                        mHAtNSYtDYt[ic][iz+ozBin[jf]][it]->SetName(outName.Data());
                    } else {
                        mHAtNSYtDYt[ic][iz+ozBin[jf]][it] = 0;
                    }

                    histName = type[it]; histName += "DEtaDPhiArr_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    in->GetObject(histName.Data(),tmp2);
                    if (tmp2) {
                        out->cd();
                        mHJtDEtaDPhi[ic][iz+ozBin[jf]][it] = (TH2D *) tmp2->Clone();
                        outName = type[it];  outName += "DEtaDPhiArr_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                        mHJtDEtaDPhi[ic][iz+ozBin[jf]][it]->SetName(outName.Data());
                    } else {
                        mHJtDEtaDPhi[ic][iz+ozBin[jf]][it] = 0;
                    }

                    histName = type[it]; histName += "NDEtaDPhiArr_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    in->GetObject(histName.Data(),tmp2);
                    if (tmp2) {
                        out->cd();
                        mHJtNDEtaDPhi[ic][iz+ozBin[jf]][it] = (TH2D *) tmp2->Clone();
                        outName = type[it];  outName += "NDEtaDPhiArr_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                        mHJtNDEtaDPhi[ic][iz+ozBin[jf]][it]->SetName(outName.Data());
                    } else {
                        mHJtNDEtaDPhi[ic][iz+ozBin[jf]][it] = 0;
                    }

                    histName = type[it]; histName += "PrDEtaDPhiArr_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    in->GetObject(histName.Data(),tmp2);
                    if (tmp2) {
                        out->cd();
                        mHPrJtDEtaDPhi[ic][iz+ozBin[jf]][it] = (TH2D *) tmp2->Clone();
                        outName = type[it];  outName += "PrDEtaDPhiArr_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                        mHPrJtDEtaDPhi[ic][iz+ozBin[jf]][it]->SetName(outName.Data());
                    } else {
                        mHPrJtDEtaDPhi[ic][iz+ozBin[jf]][it] = 0;
                    }

                    histName = type[it]; histName += "PaDEtaDPhiArr_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    in->GetObject(histName.Data(),tmp2);
                    if (tmp2) {
                        out->cd();
                        mHPaJtDEtaDPhi[ic][iz+ozBin[jf]][it] = (TH2D *) tmp2->Clone();
                        outName = type[it];  outName += "PaDEtaDPhiArr_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                        mHPaJtDEtaDPhi[ic][iz+ozBin[jf]][it]->SetName(outName.Data());
                    } else {
                        mHPaJtDEtaDPhi[ic][iz+ozBin[jf]][it] = 0;
                    }

                    histName = type[it]; histName += "PbDEtaDPhiArr_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    in->GetObject(histName.Data(),tmp2);
                    if (tmp2) {
                        out->cd();
                        mHPbJtDEtaDPhi[ic][iz+ozBin[jf]][it] = (TH2D *) tmp2->Clone();
                        outName = type[it];  outName += "PbDEtaDPhiArr_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                        mHPbJtDEtaDPhi[ic][iz+ozBin[jf]][it]->SetName(outName.Data());
                    } else {
                        mHPbJtDEtaDPhi[ic][iz+ozBin[jf]][it] = 0;
                    }


                    histName = type[it]; histName += "SEtaDPhiArr_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    in->GetObject(histName.Data(),tmp2);
                    if (tmp2) {
                        out->cd();
                        mHJtSEtaDPhi[ic][iz+ozBin[jf]][it] = (TH2D *) tmp2->Clone();
                        outName = type[it];  outName += "SEtaDPhiArr_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                        mHJtSEtaDPhi[ic][iz+ozBin[jf]][it]->SetName(outName.Data());
                    } else {
                        mHJtSEtaDPhi[ic][iz+ozBin[jf]][it] = 0;
                    }

                    histName = type[it]; histName += "NSEtaDPhiArr_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    in->GetObject(histName.Data(),tmp2);
                    if (tmp2) {
                        out->cd();
                        mHJtNSEtaDPhi[ic][iz+ozBin[jf]][it] = (TH2D *) tmp2->Clone();
                        outName = type[it];  outName += "NSEtaDPhiArr_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                        mHJtNSEtaDPhi[ic][iz+ozBin[jf]][it]->SetName(outName.Data());
                    } else {
                        mHJtNSEtaDPhi[ic][iz+ozBin[jf]][it] = 0;
                    }

                    histName = type[it]; histName += "PrSEtaDPhiArr_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    in->GetObject(histName.Data(),tmp2);
                    if (tmp2) {
                        out->cd();
                        mHPrJtSEtaDPhi[ic][iz+ozBin[jf]][it] = (TH2D *) tmp2->Clone();
                        outName = type[it];  outName += "PrSEtaDPhiArr_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                        mHPrJtSEtaDPhi[ic][iz+ozBin[jf]][it]->SetName(outName.Data());
                    } else {
                        mHPrJtSEtaDPhi[ic][iz+ozBin[jf]][it] = 0;
                    }

                    histName = type[it]; histName += "PaSEtaDPhiArr_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    in->GetObject(histName.Data(),tmp2);
                    if (tmp2) {
                        out->cd();
                        mHPaJtSEtaDPhi[ic][iz+ozBin[jf]][it] = (TH2D *) tmp2->Clone();
                        outName = type[it];  outName += "PaSEtaDPhiArr_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                        mHPaJtSEtaDPhi[ic][iz+ozBin[jf]][it]->SetName(outName.Data());
                    } else {
                        mHPaJtSEtaDPhi[ic][iz+ozBin[jf]][it] = 0;
                    }

                    histName = type[it]; histName += "PbSEtaDPhiArr_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    in->GetObject(histName.Data(),tmp2);
                    if (tmp2) {
                        out->cd();
                        mHPbJtSEtaDPhi[ic][iz+ozBin[jf]][it] = (TH2D *) tmp2->Clone();
                        outName = type[it];  outName += "PbSEtaDPhiArr_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                        mHPbJtSEtaDPhi[ic][iz+ozBin[jf]][it]->SetName(outName.Data());
                    } else {
                        mHPbJtSEtaDPhi[ic][iz+ozBin[jf]][it] = 0;
                    }


                    histName = type[it]; histName += "Qinv_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    in->GetObject(histName.Data(),tmp);
                    if (tmp) {
                        out->cd();
                        mHQinv[ic][iz+ozBin[jf]][it] = (TH1D *) tmp->Clone();
                        outName = type[it];  outName += "Qinv_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                        mHQinv[ic][iz+ozBin[jf]][it]->SetName(outName.Data());
                    } else {
                        mHQinv[ic][iz+ozBin[jf]][it] = 0;
                    }

                    histName = type[it]; histName += "NQinv_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    in->GetObject(histName.Data(),tmp);
                    if (tmp) {
                        out->cd();
                        mHNQinv[ic][iz+ozBin[jf]][it] = (TH1D *) tmp->Clone();
                        outName = type[it];  outName += "NQinv_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                        mHNQinv[ic][iz+ozBin[jf]][it]->SetName(outName.Data());
                    } else {
                        mHNQinv[ic][iz+ozBin[jf]][it] = 0;
                    }
                }
            }
        }

        // Density histograms are per cutbin for most cutbinning modes.
        char *type[] = {"Sibpp", "Sibpm", "Sibmp", "Sibmm", "Mixpp", "Mixpm", "Mixmp", "Mixmm"};


        for (int iz=0;iz<nzBin[jf];iz++) {
cout << "Loop over density histograms:  zBin = " << iz << endl;
            for (int ic=0;ic<nDenBins;ic++) {
                for (int it=0;it<8;it++) {
                    histName = type[it]; histName += "TPCAvgTSep_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    in->GetObject(histName.Data(),tmp2);
                    out->cd();
                    mHTPCAvgTSep[ic][iz+ozBin[jf]][it] = (TH1D *) tmp2->Clone();
                    outName = type[it];  outName += "TPCAvgTSep_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                    mHTPCAvgTSep[ic][iz+ozBin[jf]][it]->SetName(outName.Data());

                    histName = type[it]; histName += "TPCAvgZSep_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    in->GetObject(histName.Data(),tmp2);
                    out->cd();
                    mHTPCAvgZSep[ic][iz+ozBin[jf]][it] = (TH1D *) tmp2->Clone();
                    outName = type[it];  outName += "TPCAvgZSep_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                    mHTPCAvgZSep[ic][iz+ozBin[jf]][it]->SetName(outName.Data());

                    histName = type[it]; histName += "TPCEntTSep_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    in->GetObject(histName.Data(),tmp2);
                    out->cd();
                    mHTPCEntTSep[ic][iz+ozBin[jf]][it] = (TH1D *) tmp2->Clone();
                    outName = type[it];  outName += "TPCEntTSep_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                    mHTPCEntTSep[ic][iz+ozBin[jf]][it]->SetName(outName.Data());

                    histName = type[it]; histName += "TPCEntZSep_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    in->GetObject(histName.Data(),tmp2);
                    out->cd();
                    mHTPCEntZSep[ic][iz+ozBin[jf]][it] = (TH1D *) tmp2->Clone();
                    outName = type[it];  outName += "TPCEntZSep_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                    mHTPCEntZSep[ic][iz+ozBin[jf]][it]->SetName(outName.Data());

                    histName = type[it]; histName += "TPCMidTSep_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    in->GetObject(histName.Data(),tmp2);
                    out->cd();
                    mHTPCMidTSep[ic][iz+ozBin[jf]][it] = (TH1D *) tmp2->Clone();
                    outName = type[it];  outName += "TPCMidTSep_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                    mHTPCMidTSep[ic][iz+ozBin[jf]][it]->SetName(outName.Data());

                    histName = type[it]; histName += "TPCMidZSep_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    in->GetObject(histName.Data(),tmp2);
                    out->cd();
                    mHTPCMidZSep[ic][iz+ozBin[jf]][it] = (TH1D *) tmp2->Clone();
                    outName = type[it];  outName += "TPCMidZSep_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                    mHTPCMidZSep[ic][iz+ozBin[jf]][it]->SetName(outName.Data());

                    histName = type[it]; histName += "TPCExitTSep_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    in->GetObject(histName.Data(),tmp2);
                    out->cd();
                    mHTPCExitTSep[ic][iz+ozBin[jf]][it] = (TH1D *) tmp2->Clone();
                    outName = type[it];  outName += "TPCExitTSep_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                    mHTPCExitTSep[ic][iz+ozBin[jf]][it]->SetName(outName.Data());

                    histName = type[it]; histName += "TPCExitZSep_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    in->GetObject(histName.Data(),tmp2);
                    out->cd();
                    mHTPCExitZSep[ic][iz+ozBin[jf]][it] = (TH1D *) tmp2->Clone();
                    outName = type[it];  outName += "TPCExitZSep_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                    mHTPCExitZSep[ic][iz+ozBin[jf]][it]->SetName(outName.Data());


                    histName = type[it]; histName += "TPCMidTdptP_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    in->GetObject(histName.Data(),tmp2);
                    out->cd();
                    mHTPCMidTdptP[ic][iz+ozBin[jf]][it] = (TH1D *) tmp2->Clone();
                    outName = type[it];  outName += "TPCMidTdptP_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                    mHTPCMidTdptP[ic][iz+ozBin[jf]][it]->SetName(outName.Data());

                    histName = type[it]; histName += "TPCMidTdptN_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    in->GetObject(histName.Data(),tmp2);
                    out->cd();
                    mHTPCMidTdptN[ic][iz+ozBin[jf]][it] = (TH1D *) tmp2->Clone();
                    outName = type[it];  outName += "TPCMidTdptN_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                    mHTPCMidTdptN[ic][iz+ozBin[jf]][it]->SetName(outName.Data());

                    histName = type[it]; histName += "TPCMidZdptP_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    in->GetObject(histName.Data(),tmp2);
                    out->cd();
                    mHTPCMidZdptP[ic][iz+ozBin[jf]][it] = (TH1D *) tmp2->Clone();
                    outName = type[it];  outName += "TPCMidZdptP_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                    mHTPCMidZdptP[ic][iz+ozBin[jf]][it]->SetName(outName.Data());

                    histName = type[it]; histName += "TPCMidZdptN_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    in->GetObject(histName.Data(),tmp2);
                    out->cd();
                    mHTPCMidZdptN[ic][iz+ozBin[jf]][it] = (TH1D *) tmp2->Clone();
                    outName = type[it];  outName += "TPCMidZdptN_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                    mHTPCMidZdptN[ic][iz+ozBin[jf]][it]->SetName(outName.Data());

                    histName = type[it]; histName += "TPCAvgTZ_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    in->GetObject(histName.Data(),tmp2);
                    out->cd();
                    mHTPCAvgTZ[ic][iz+ozBin[jf]][it] = (TH2D *) tmp2->Clone();
                    outName = type[it];  outName += "TPCAvgTZ_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                    mHTPCAvgTZ[ic][iz+ozBin[jf]][it]->SetName(outName.Data());

                    histName = type[it]; histName += "TPCEntTZ_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    in->GetObject(histName.Data(),tmp2);
                    out->cd();
                    mHTPCEntTZ[ic][iz+ozBin[jf]][it] = (TH2D *) tmp2->Clone();
                    outName = type[it];  outName += "TPCEntTZ_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                    mHTPCEntTZ[ic][iz+ozBin[jf]][it]->SetName(outName.Data());

                    histName = type[it]; histName += "TPCMidTZ_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    in->GetObject(histName.Data(),tmp2);
                    out->cd();
                    mHTPCMidTZ[ic][iz+ozBin[jf]][it] = (TH2D *) tmp2->Clone();
                    outName = type[it];  outName += "TPCMidTZ_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                    mHTPCMidTZ[ic][iz+ozBin[jf]][it]->SetName(outName.Data());

                    histName = type[it]; histName += "TPCMidTZC_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    in->GetObject(histName.Data(),tmp2);
                    out->cd();
                    mHTPCMidTZC[ic][iz+ozBin[jf]][it] = (TH2D *) tmp2->Clone();
                    outName = type[it];  outName += "TPCMidTZC_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                    mHTPCMidTZC[ic][iz+ozBin[jf]][it]->SetName(outName.Data());

                    histName = type[it]; histName += "TPCMidTZNC_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    in->GetObject(histName.Data(),tmp2);
                    out->cd();
                    mHTPCMidTZNC[ic][iz+ozBin[jf]][it] = (TH2D *) tmp2->Clone();
                    outName = type[it];  outName += "TPCMidTZNC_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                    mHTPCMidTZNC[ic][iz+ozBin[jf]][it]->SetName(outName.Data());

                    histName = type[it]; histName += "TPCExitTZ_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    in->GetObject(histName.Data(),tmp2);
                    out->cd();
                    mHTPCExitTZ[ic][iz+ozBin[jf]][it] = (TH2D *) tmp2->Clone();
                    outName = type[it];  outName += "TPCExitTZ_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                    mHTPCExitTZ[ic][iz+ozBin[jf]][it]->SetName(outName.Data());

                    histName = type[it]; histName += "TPCEntTdpt_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    in->GetObject(histName.Data(),tmp2);
                    out->cd();
                    mHTPCEntTdpt[ic][iz+ozBin[jf]][it] = (TH2D *) tmp2->Clone();
                    outName = type[it];  outName += "TPCEntTdpt_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                    mHTPCEntTdpt[ic][iz+ozBin[jf]][it]->SetName(outName.Data());

                    histName = type[it]; histName += "TPCMidTdpt_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    in->GetObject(histName.Data(),tmp2);
                    out->cd();
                    mHTPCMidTdpt[ic][iz+ozBin[jf]][it] = (TH2D *) tmp2->Clone();
                    outName = type[it];  outName += "TPCMidTdpt_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                    mHTPCMidTdpt[ic][iz+ozBin[jf]][it]->SetName(outName.Data());

                    histName = type[it]; histName += "TPCExitTdpt_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    in->GetObject(histName.Data(),tmp2);
                    out->cd();
                    mHTPCExitTdpt[ic][iz+ozBin[jf]][it] = (TH2D *) tmp2->Clone();
                    outName = type[it];  outName += "TPCExitTdpt_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                    mHTPCExitTdpt[ic][iz+ozBin[jf]][it]->SetName(outName.Data());
                }
            }
        }
cout << "Closing input file " << jf << endl;
// Close seems to take an awfully long time. Try leaving file hanging and hope system cleans up quickly.
//        in->Close();
//        delete in;
    }

    // Now write out all histograms.

    out->cd();
    if (mHmix) {
        mHmix->Write();
    }
    if (mHcb) {
        mHcb->Write();
    }
    if (mHptAll) {
        mHptAll->Write();
    }
    if (mHMixedDistance) {
        mHMixedDistance->Write();
    }
    if (mHMixedMults) {
        mHMixedMults->Write();
    }

    for (int iz=0;iz<nTotZBins;iz++) {
        mHNEventsSib[iz]->Write();
        mHNEventsMix[iz]->Write();
        mHNEventsPosSib[iz]->Write();
        mHNEventsPosMix[iz]->Write();
        mHNEventsNegSib[iz]->Write();
        mHNEventsNegMix[iz]->Write();

        for (int ip=0;ip<nParentBins;ip++) {
            mHMeanPtP[ip][iz]->Write();
            mHMeanPtM[ip][iz]->Write();
            mHEtaP[ip][iz]->Write();
            mHEtaM[ip][iz]->Write();
        }

        for (int ic=0;ic<nCutBins;ic++) {
            for (int it=0;it<8;it++) {
                if (mHYtYt[ic][iz][it]) mHYtYt[ic][iz][it]->Write();
                if (mHNYtYt[ic][iz][it]) mHNYtYt[ic][iz][it]->Write();
                if (mHPtPt[ic][iz][it]) mHPtPt[ic][iz][it]->Write();

                if (mHPhiPhi[ic][iz][it]) mHPhiPhi[ic][iz][it]->Write();
                if (mHNPhiPhi[ic][iz][it]) mHNPhiPhi[ic][iz][it]->Write();
                if (mHEtaEta[ic][iz][it]) mHEtaEta[ic][iz][it]->Write();
                if (mHPrPhiPhi[ic][iz][it]) mHPrPhiPhi[ic][iz][it]->Write();
                if (mHPrEtaEta[ic][iz][it]) mHPrEtaEta[ic][iz][it]->Write();
                if (mHPaPhiPhi[ic][iz][it]) mHPaPhiPhi[ic][iz][it]->Write();
                if (mHPaEtaEta[ic][iz][it]) mHPaEtaEta[ic][iz][it]->Write();
                if (mHPbPhiPhi[ic][iz][it]) mHPbPhiPhi[ic][iz][it]->Write();
                if (mHPbEtaEta[ic][iz][it]) mHPbEtaEta[ic][iz][it]->Write();

                if (mHAtSYtDYt[ic][iz][it]) mHAtSYtDYt[ic][iz][it]->Write();
                if (mHAtNSYtDYt[ic][iz][it]) mHAtNSYtDYt[ic][iz][it]->Write();
                if (mHJtDEtaDPhi[ic][iz][it]) mHJtDEtaDPhi[ic][iz][it]->Write();
                if (mHJtNDEtaDPhi[ic][iz][it]) mHJtNDEtaDPhi[ic][iz][it]->Write();
                if (mHPrJtDEtaDPhi[ic][iz][it]) mHPrJtDEtaDPhi[ic][iz][it]->Write();
                if (mHPaJtDEtaDPhi[ic][iz][it]) mHPaJtDEtaDPhi[ic][iz][it]->Write();
                if (mHPbJtDEtaDPhi[ic][iz][it]) mHPbJtDEtaDPhi[ic][iz][it]->Write();

                if (mHJtSEtaDPhi[ic][iz][it]) mHJtSEtaDPhi[ic][iz][it]->Write();
                if (mHJtNSEtaDPhi[ic][iz][it]) mHJtNSEtaDPhi[ic][iz][it]->Write();
                if (mHPrJtSEtaDPhi[ic][iz][it]) mHPrJtSEtaDPhi[ic][iz][it]->Write();
                if (mHPaJtSEtaDPhi[ic][iz][it]) mHPaJtSEtaDPhi[ic][iz][it]->Write();
                if (mHPbJtSEtaDPhi[ic][iz][it]) mHPbJtSEtaDPhi[ic][iz][it]->Write();

                if (mHQinv[ic][iz][it]) mHQinv[ic][iz][it]->Write();
                if (mHNQinv[ic][iz][it]) mHNQinv[ic][iz][it]->Write();
            }
        }

        for (int ic=0;ic<nDenBins;ic++) {
            for (int it=0;it<8;it++) {
                mHTPCAvgTSep[ic][iz][it]->Write();
                mHTPCAvgZSep[ic][iz][it]->Write();
                mHTPCEntTSep[ic][iz][it]->Write();
                mHTPCEntZSep[ic][iz][it]->Write();
                mHTPCMidTSep[ic][iz][it]->Write();
                mHTPCMidZSep[ic][iz][it]->Write();
                mHTPCExitTSep[ic][iz][it]->Write();
                mHTPCExitZSep[ic][iz][it]->Write();

                mHTPCMidTdptP[ic][iz][it]->Write();
                mHTPCMidTdptN[ic][iz][it]->Write();
                mHTPCMidZdptP[ic][iz][it]->Write();
                mHTPCMidZdptN[ic][iz][it]->Write();

                mHTPCAvgTZ[ic][iz][it]->Write();
                mHTPCEntTZ[ic][iz][it]->Write();
                mHTPCMidTZ[ic][iz][it]->Write();
                mHTPCMidTZC[ic][iz][it]->Write();
                mHTPCMidTZNC[ic][iz][it]->Write();
                mHTPCExitTZ[ic][iz][it]->Write();
                mHTPCEntTdpt[ic][iz][it]->Write();
                mHTPCMidTdpt[ic][iz][it]->Write();
                mHTPCExitTdpt[ic][iz][it]->Write();
            }
        }
    }
// Actually closing file seems to take an infinite amount of time.
// I think data is already on disk.
//    out->Close();
//    delete out;
    // That's all (I hope).
}
