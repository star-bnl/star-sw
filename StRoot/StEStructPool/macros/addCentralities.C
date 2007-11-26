#include <iostream>
#include "TFile.h"
#include "TString.h"
#include "TH1D.h"
#include "TH2D.h"


void addCentralities(const char* dirName, const char* inFile, const char* outFile, int *nFile, int numFiles) {

  // -- Combine files by renaming z-buffer indices. This allows the Support code to
  //    calculate the ratios for all centralitles and z-buffers.
  //
  // root.exe -q -b addCentralities.C("dir","inFileBase","outFile", nFile, numFiles)
  // 
  //    where dir is directory to look for histogram files,
  //    inFileBase will be something like "Data" (where we append 0, 1, 2 etc to get actual name)
  //    outFile is name of out file (such as Sum5_6, dir will be pre-pended and .root appended)
  //    nFile will be an array like int numbers[] = {5, 6, 7}
  //    and numFiles is the number of entries in nFile (3 in this case.)
  //

  // Need to determine how many cutBins, parentBins and zBuffers in each file.
  // I think we need the same numbers of cutBins and parentBins, but we can
  // allow a different number of zBuffers
    int nZBins, nzBin[20], ozBin[20], nTotZBins = 0;
    int nCuts, nCutBins;
    int nParents, nParentBins;
    for (int jf=0;jf<numFiles;jf++) {
        TString fileName(dirName); fileName += "/"; fileName += inFile; fileName += nFile[jf]; fileName += ".root";
cout << "Input file " << fileName.Data() << endl;
        TFile *in = new TFile(fileName.Data());

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

        nCuts = 0;
        name = "SibppYtYt_cutBin_"; name += nCuts; name += "_zBuf_0";
        while (gDirectory->Get(name.Data())) {
            name ="SibppYtYt_cutBin_"; name += nCuts; name += "_zBuf_0";
            nCuts++;
        }
        nCuts--;

        nzBin[jf]  = nZBins;
        ozBin[jf]  = nTotZBins;
        nTotZBins += nZBins;
        if (0 == jf) {
            nParentBins = nParents;
            nCutBins = nCuts;
        } else {
            if (nParents != nParentBins) {
                cout<<"Error in ParentBins. First file had "<< nParentBins << " while file " << jf << " has " << nParents << endl;
                return;
            }
            if (nCuts != nCutBins) {
                cout<<"Error in CutBins. First file had "<< nCutBins << " while file " << jf << " has " << nCuts << endl;
                return;
            }
        }
        in->Close();
        delete in;
    }

    // Declare histograms
    TH1D *tmp;
    TH1D *mHmix;
    TH1D *mHcb;
    TH1D *mHptAll;

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

    if (nCutBins > 100) {
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


    TString fileName(dirName); fileName += "/"; fileName += outFile; fileName += ".root";
    TFile *out = new TFile(fileName.Data(),"RECREATE");

    // With histograms declared we can loop over files making clones.
    for (int jf=0;jf<numFiles;jf++) {
cout << "Opening input file " << jf << endl;
        TString fileName(dirName); fileName += "/"; fileName += inFile; fileName += nFile[jf]; fileName += ".root";
        TFile *in = new TFile(fileName.Data());

      // Couple of histograms with no cutBin, parentBin or zBuf information.
      // For first file we make clones, rest of the files we add to existing histograms.
        in->cd();
        tmp = (TH1D *) gDirectory->Get("EventMixing");
        out->cd();
        if (0 == jf) {
            mHmix   = (TH1D *) tmp->Clone();
        } else {
            mHmix->Add(tmp);
        }
        in->cd();
        tmp = (TH1D *) gDirectory->Get("hcb");
        out->cd();
        if (0 == jf) {
            mHcb    = (TH1D *) tmp->Clone();
        } else {
            mHcb->Add(tmp);
        }
        in->cd();
        tmp = (TH1D *) gDirectory->Get("ptAll");
        out->cd();
        if (0 == jf) {
            mHptAll = (TH1D *) tmp->Clone();
        } else {
            mHptAll->Add(tmp);
        }


        // First set integrated over cutBins (and parentBins)
        TString histName, outName;
        for (int iz=0;iz<nzBin[jf];iz++) {
            in->cd();
            histName = "NEventsSib_zBuf_"; histName += iz;
            tmp = (TH1D *) gDirectory->Get(histName.Data());
            out->cd();
            mHNEventsSib[iz+ozBin[jf]] = (TH1D *) tmp->Clone();
            outName = "NEventsSib_zBuf_"; outName += iz+ozBin[jf];
            mHNEventsSib[iz+ozBin[jf]]->SetName(outName.Data());

            in->cd();
            histName = "NEventsMix_zBuf_"; histName += iz;
            tmp = (TH1D *) gDirectory->Get(histName.Data());
            out->cd();
            mHNEventsMix[iz+ozBin[jf]] = (TH1D *) tmp->Clone();
            outName = "NEventsMix_zBuf_";  outName += iz+ozBin[jf];
            mHNEventsMix[iz+ozBin[jf]]->SetName(outName.Data());

            in->cd();
            histName = "NEventsPosSib_zBuf_"; histName += iz;
            tmp = (TH1D *) gDirectory->Get(histName.Data());
            out->cd();
            mHNEventsPosSib[iz+ozBin[jf]] = (TH1D *) tmp->Clone();
            outName = "NEventsPosSib_zBuf_";  outName += iz+ozBin[jf];
            mHNEventsPosSib[iz+ozBin[jf]]->SetName(outName.Data());

            in->cd();
            histName = "NEventsPosMix_zBuf_"; histName += iz;
            tmp = (TH1D *) gDirectory->Get(histName.Data());
            out->cd();
            mHNEventsPosMix[iz+ozBin[jf]] = (TH1D *) tmp->Clone();
            outName = "NEventsPosMix_zBuf_";  outName += iz+ozBin[jf];
            mHNEventsPosMix[iz+ozBin[jf]]->SetName(outName.Data());

            in->cd();
            histName = "NEventsNegSib_zBuf_"; histName += iz;
            tmp = (TH1D *) gDirectory->Get(histName.Data());
            out->cd();
            mHNEventsNegSib[iz+ozBin[jf]] = (TH1D *) tmp->Clone();
            outName = "NEventsNegSib_zBuf_";  outName += iz+ozBin[jf];
            mHNEventsNegSib[iz+ozBin[jf]]->SetName(outName.Data());

            in->cd();
            histName = "NEventsNegMix_zBuf_"; histName += iz;
            tmp = (TH1D *) gDirectory->Get(histName.Data());
            out->cd();
            mHNEventsNegMix[iz+ozBin[jf]] = (TH1D *) tmp->Clone();
            outName = "NEventsNegMix_zBuf_";  outName += iz+ozBin[jf];
            mHNEventsNegMix[iz+ozBin[jf]]->SetName(outName.Data());
        }

        // Second set is per parent bin (and zBuf)
        for (int iz=0;iz<nzBin[jf];iz++) {
            for (int ip=0;ip<nParentBins;ip++) {
                in->cd();
                histName = "meanPtP_parentBin"; histName += ip; histName += "_zBuf_"; histName += iz;
                tmp = (TH1D *) gDirectory->Get(histName.Data());
                out->cd();
                mHMeanPtP[ip][iz+ozBin[jf]] = (TH1D *) tmp->Clone();
                outName = "meanPtP_parentBin";  outName += ip;  outName += "_zBuf_";  outName += iz+ozBin[jf];
                mHMeanPtP[ip][iz+ozBin[jf]]->SetName(outName.Data());

                in->cd();
                histName = "meanPtM_parentBin"; histName += ip; histName += "_zBuf_"; histName += iz;
                tmp = (TH1D *) gDirectory->Get(histName.Data());
                out->cd();
                mHMeanPtM[ip][iz+ozBin[jf]] = (TH1D *) tmp->Clone();
                outName = "meanPtM_parentBin";  outName += ip;  outName += "_zBuf_";  outName += iz+ozBin[jf];
                mHMeanPtM[ip][iz+ozBin[jf]]->SetName(outName.Data());

                in->cd();
                histName = "etaP_parentBin"; histName += ip; histName += "_zBuf_"; histName += iz;
                tmp = (TH1D *) gDirectory->Get(histName.Data());
                out->cd();
                mHEtaP[ip][iz+ozBin[jf]] = (TH1D *) tmp->Clone();
                outName = "etaP_parentBin";  outName += ip;  outName += "_zBuf_";  outName += iz+ozBin[jf];
                mHEtaP[ip][iz+ozBin[jf]]->SetName(outName.Data());

                in->cd();
                histName = "etaM_parentBin"; histName += ip; histName += "_zBuf_"; histName += iz;
                tmp = (TH1D *) gDirectory->Get(histName.Data());
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
                    in->cd();
                    histName = type[it]; histName += "YtYt_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    tmp = (TH1D *) gDirectory->Get(histName.Data());
                    out->cd();
                    mHYtYt[ic][iz+ozBin[jf]][it] = (TH2D *) tmp->Clone();
                    outName = type[it];  outName += "YtYt_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                    mHYtYt[ic][iz+ozBin[jf]][it]->SetName(outName.Data());

                    in->cd();
                    histName = type[it]; histName += "NYtYt_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    tmp = (TH1D *) gDirectory->Get(histName.Data());
                    out->cd();
                    mHNYtYt[ic][iz+ozBin[jf]][it] = (TH2D *) tmp->Clone();
                    outName = type[it];  outName += "NYtYt_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                    mHNYtYt[ic][iz+ozBin[jf]][it]->SetName(outName.Data());

                    in->cd();
                    histName = type[it]; histName += "PtPt_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    tmp = (TH1D *) gDirectory->Get(histName.Data());
                    out->cd();
                    mHPtPt[ic][iz+ozBin[jf]][it] = (TH2D *) tmp->Clone();
                    outName = type[it];  outName += "PtPt_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                    mHPtPt[ic][iz+ozBin[jf]][it]->SetName(outName.Data());


                    in->cd();
                    histName = type[it]; histName += "PhiPhi_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    tmp = (TH1D *) gDirectory->Get(histName.Data());
                    out->cd();
                    mHPhiPhi[ic][iz+ozBin[jf]][it] = (TH2D *) tmp->Clone();
                    outName = type[it];  outName += "PhiPhi_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                    mHPhiPhi[ic][iz+ozBin[jf]][it]->SetName(outName.Data());

                    in->cd();
                    histName = type[it]; histName += "NPhiPhi_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    tmp = (TH1D *) gDirectory->Get(histName.Data());
                    out->cd();
                    mHNPhiPhi[ic][iz+ozBin[jf]][it] = (TH2D *) tmp->Clone();
                    outName = type[it];  outName += "NPhiPhi_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                    mHNPhiPhi[ic][iz+ozBin[jf]][it]->SetName(outName.Data());

                    in->cd();
                    histName = type[it]; histName += "EtaEta_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    tmp = (TH1D *) gDirectory->Get(histName.Data());
                    out->cd();
                    mHEtaEta[ic][iz+ozBin[jf]][it] = (TH2D *) tmp->Clone();
                    outName = type[it];  outName += "EtaEta_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                    mHEtaEta[ic][iz+ozBin[jf]][it]->SetName(outName.Data());

                    in->cd();
                    histName = type[it]; histName += "PrPhiPhi_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    tmp = (TH1D *) gDirectory->Get(histName.Data());
                    out->cd();
                    mHPrPhiPhi[ic][iz+ozBin[jf]][it] = (TH2D *) tmp->Clone();
                    outName = type[it];  outName += "PrPhiPhi_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                    mHPrPhiPhi[ic][iz+ozBin[jf]][it]->SetName(outName.Data());

                    in->cd();
                    histName = type[it]; histName += "PrEtaEta_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    tmp = (TH1D *) gDirectory->Get(histName.Data());
                    out->cd();
                    mHPrEtaEta[ic][iz+ozBin[jf]][it] = (TH2D *) tmp->Clone();
                    outName = type[it];  outName += "PrEtaEta_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                    mHPrEtaEta[ic][iz+ozBin[jf]][it]->SetName(outName.Data());

                    in->cd();
                    histName = type[it]; histName += "PaPhiPhi_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    tmp = (TH1D *) gDirectory->Get(histName.Data());
                    out->cd();
                    mHPaPhiPhi[ic][iz+ozBin[jf]][it] = (TH2D *) tmp->Clone();
                    outName = type[it];  outName += "PaPhiPhi_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                    mHPaPhiPhi[ic][iz+ozBin[jf]][it]->SetName(outName.Data());

                    in->cd();
                    histName = type[it]; histName += "PaEtaEta_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    tmp = (TH1D *) gDirectory->Get(histName.Data());
                    out->cd();
                    mHPaEtaEta[ic][iz+ozBin[jf]][it] = (TH2D *) tmp->Clone();
                    outName = type[it];  outName += "PaEtaEta_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                    mHPaEtaEta[ic][iz+ozBin[jf]][it]->SetName(outName.Data());

                    in->cd();
                    histName = type[it]; histName += "PbPhiPhi_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    tmp = (TH1D *) gDirectory->Get(histName.Data());
                    out->cd();
                    mHPbPhiPhi[ic][iz+ozBin[jf]][it] = (TH2D *) tmp->Clone();
                    outName = type[it];  outName += "PbPhiPhi_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                    mHPbPhiPhi[ic][iz+ozBin[jf]][it]->SetName(outName.Data());

                    in->cd();
                    histName = type[it]; histName += "PbEtaEta_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    tmp = (TH1D *) gDirectory->Get(histName.Data());
                    out->cd();
                    mHPbEtaEta[ic][iz+ozBin[jf]][it] = (TH2D *) tmp->Clone();
                    outName = type[it];  outName += "PbEtaEta_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                    mHPbEtaEta[ic][iz+ozBin[jf]][it]->SetName(outName.Data());


                    in->cd();
                    histName = type[it]; histName += "SYtDYt_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    tmp = (TH1D *) gDirectory->Get(histName.Data());
                    out->cd();
                    mHAtSYtDYt[ic][iz+ozBin[jf]][it] = (TH2D *) tmp->Clone();
                    outName = type[it];  outName += "SYtDYt_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                    mHAtSYtDYt[ic][iz+ozBin[jf]][it]->SetName(outName.Data());

                    in->cd();
                    histName = type[it]; histName += "NSYtDYt_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    tmp = (TH1D *) gDirectory->Get(histName.Data());
                    out->cd();
                    mHAtNSYtDYt[ic][iz+ozBin[jf]][it] = (TH2D *) tmp->Clone();
                    outName = type[it];  outName += "NSYtDYt_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                    mHAtNSYtDYt[ic][iz+ozBin[jf]][it]->SetName(outName.Data());

                    in->cd();
                    histName = type[it]; histName += "DEtaDPhiArr_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    tmp = (TH1D *) gDirectory->Get(histName.Data());
                    out->cd();
                    mHJtDEtaDPhi[ic][iz+ozBin[jf]][it] = (TH2D *) tmp->Clone();
                    outName = type[it];  outName += "DEtaDPhiArr_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                    mHJtDEtaDPhi[ic][iz+ozBin[jf]][it]->SetName(outName.Data());

                    in->cd();
                    histName = type[it]; histName += "NDEtaDPhiArr_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    tmp = (TH1D *) gDirectory->Get(histName.Data());
                    out->cd();
                    mHJtNDEtaDPhi[ic][iz+ozBin[jf]][it] = (TH2D *) tmp->Clone();
                    outName = type[it];  outName += "NDEtaDPhiArr_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                    mHJtNDEtaDPhi[ic][iz+ozBin[jf]][it]->SetName(outName.Data());

                    in->cd();
                    histName = type[it]; histName += "PrDEtaDPhiArr_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    tmp = (TH1D *) gDirectory->Get(histName.Data());
                    out->cd();
                    mHPrJtDEtaDPhi[ic][iz+ozBin[jf]][it] = (TH2D *) tmp->Clone();
                    outName = type[it];  outName += "PrDEtaDPhiArr_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                    mHPrJtDEtaDPhi[ic][iz+ozBin[jf]][it]->SetName(outName.Data());

                    in->cd();
                    histName = type[it]; histName += "PaDEtaDPhiArr_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    tmp = (TH1D *) gDirectory->Get(histName.Data());
                    out->cd();
                    mHPaJtDEtaDPhi[ic][iz+ozBin[jf]][it] = (TH2D *) tmp->Clone();
                    outName = type[it];  outName += "PaDEtaDPhiArr_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                    mHPaJtDEtaDPhi[ic][iz+ozBin[jf]][it]->SetName(outName.Data());

                    in->cd();
                    histName = type[it]; histName += "PbDEtaDPhiArr_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    tmp = (TH1D *) gDirectory->Get(histName.Data());
                    out->cd();
                    mHPbJtDEtaDPhi[ic][iz+ozBin[jf]][it] = (TH2D *) tmp->Clone();
                    outName = type[it];  outName += "PbDEtaDPhiArr_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                    mHPbJtDEtaDPhi[ic][iz+ozBin[jf]][it]->SetName(outName.Data());


                    in->cd();
                    histName = type[it]; histName += "SEtaDPhiArr_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    tmp = (TH1D *) gDirectory->Get(histName.Data());
                    out->cd();
                    mHJtSEtaDPhi[ic][iz+ozBin[jf]][it] = (TH2D *) tmp->Clone();
                    outName = type[it];  outName += "SEtaDPhiArr_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                    mHJtSEtaDPhi[ic][iz+ozBin[jf]][it]->SetName(outName.Data());

                    in->cd();
                    histName = type[it]; histName += "NSEtaDPhiArr_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    tmp = (TH1D *) gDirectory->Get(histName.Data());
                    out->cd();
                    mHJtNSEtaDPhi[ic][iz+ozBin[jf]][it] = (TH2D *) tmp->Clone();
                    outName = type[it];  outName += "NSEtaDPhiArr_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                    mHJtNSEtaDPhi[ic][iz+ozBin[jf]][it]->SetName(outName.Data());

                    in->cd();
                    histName = type[it]; histName += "PrSEtaDPhiArr_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    tmp = (TH1D *) gDirectory->Get(histName.Data());
                    out->cd();
                    mHPrJtSEtaDPhi[ic][iz+ozBin[jf]][it] = (TH2D *) tmp->Clone();
                    outName = type[it];  outName += "PrSEtaDPhiArr_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                    mHPrJtSEtaDPhi[ic][iz+ozBin[jf]][it]->SetName(outName.Data());

                    in->cd();
                    histName = type[it]; histName += "PaSEtaDPhiArr_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    tmp = (TH1D *) gDirectory->Get(histName.Data());
                    out->cd();
                    mHPaJtSEtaDPhi[ic][iz+ozBin[jf]][it] = (TH2D *) tmp->Clone();
                    outName = type[it];  outName += "PaSEtaDPhiArr_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                    mHPaJtSEtaDPhi[ic][iz+ozBin[jf]][it]->SetName(outName.Data());

                    in->cd();
                    histName = type[it]; histName += "PbSEtaDPhiArr_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    tmp = (TH1D *) gDirectory->Get(histName.Data());
                    out->cd();
                    mHPbJtSEtaDPhi[ic][iz+ozBin[jf]][it] = (TH2D *) tmp->Clone();
                    outName = type[it];  outName += "PbSEtaDPhiArr_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                    mHPbJtSEtaDPhi[ic][iz+ozBin[jf]][it]->SetName(outName.Data());


                    in->cd();
                    histName = type[it]; histName += "Qinv_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    tmp = (TH1D *) gDirectory->Get(histName.Data());
                    out->cd();
                    mHQinv[ic][iz+ozBin[jf]][it] = (TH1D *) tmp->Clone();
                    outName = type[it];  outName += "Qinv_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                    mHQinv[ic][iz+ozBin[jf]][it]->SetName(outName.Data());

                    in->cd();
                    histName = type[it]; histName += "NQinv_cutBin_"; histName += ic; histName += "_zBuf_"; histName += iz;
                    tmp = (TH1D *) gDirectory->Get(histName.Data());
                    out->cd();
                    mHNQinv[ic][iz+ozBin[jf]][it] = (TH1D *) tmp->Clone();
                    outName = type[it];  outName += "NQinv_cutBin_"; outName += ic; outName += "_zBuf_"; outName += iz+ozBin[jf];
                    mHNQinv[ic][iz+ozBin[jf]][it]->SetName(outName.Data());
                }
            }
        }
cout << "Closing input file " << jf << endl;
// Close seems to take an awfully long time. Try leaving file hanging and hope system cleans up quickly.
//        in->Close();
//        delete in;
    }

    // Now write out all histograms.

    mHmix->Write();
    mHcb->Write();
    mHptAll->Write();

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
                mHYtYt[ic][iz][it]->Write();
                mHNYtYt[ic][iz][it]->Write();
                mHPtPt[ic][iz][it]->Write();

                mHPhiPhi[ic][iz][it]->Write();
                mHNPhiPhi[ic][iz][it]->Write();
                mHEtaEta[ic][iz][it]->Write();
                mHPrPhiPhi[ic][iz][it]->Write();
                mHPrEtaEta[ic][iz][it]->Write();
                mHPaPhiPhi[ic][iz][it]->Write();
                mHPaEtaEta[ic][iz][it]->Write();
                mHPbPhiPhi[ic][iz][it]->Write();
                mHPbEtaEta[ic][iz][it]->Write();

                mHAtSYtDYt[ic][iz][it]->Write();
                mHAtNSYtDYt[ic][iz][it]->Write();
                mHJtDEtaDPhi[ic][iz][it]->Write();
                mHJtNDEtaDPhi[ic][iz][it]->Write();
                mHPrJtDEtaDPhi[ic][iz][it]->Write();
                mHPaJtDEtaDPhi[ic][iz][it]->Write();
                mHPbJtDEtaDPhi[ic][iz][it]->Write();

                mHJtSEtaDPhi[ic][iz][it]->Write();
                mHJtNSEtaDPhi[ic][iz][it]->Write();
                mHPrJtSEtaDPhi[ic][iz][it]->Write();
                mHPaJtSEtaDPhi[ic][iz][it]->Write();
                mHPbJtSEtaDPhi[ic][iz][it]->Write();

                mHQinv[ic][iz][it]->Write();
                mHNQinv[ic][iz][it]->Write();
            }
        }
    }
// Actually closing file seems to take an infinite amount of time.
// I think data is already on disk.
//    out->Close();
//    delete out;
    // That's all (I hope).
}
