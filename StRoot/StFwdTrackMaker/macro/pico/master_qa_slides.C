#include "TFile.h"
#include "TH1.h"
#include "TH2.h"
#include "TH1F.h"
#include "TH2F.h"
#include "TCanvas.h"
#include "TStyle.h"
#include "TString.h"
#include "TKey.h"
#include "TClass.h"
#include "TPaveText.h"
#include "TSystemDirectory.h"
#include "TList.h"
#include "TSystemFile.h"
#include "TGraphErrors.h"
#include "TLegend.h"
#include "TMultiGraph.h"
#include "TObjArray.h"
#include "TObjString.h"
#include "TAxis.h"
#include "TROOT.h"
#include "TMath.h"
#include <iostream>
#include <vector>
#include <map>
#include <algorithm>

void master_qa_slides() {
    
    // ==========================================
    // --- PRE-SCAN DIRECTORY FOR RUN RANGE ---
    // ==========================================
    std::map<int, std::map<TString, std::vector<TString>>> fileMap;
    TString inDir = "./batch_outputs_full"; //output directory from running qa jobs
    TSystemDirectory dir("batchDir", inDir.Data());
    TList *files = dir.GetListOfFiles();

    std::vector<int> orderedRuns;
    std::vector<TString> trackTypes = {"Global", "Primary", "BLC", "Other"};

    if (files) {
        std::cout << "Scanning directory: " << inDir << " to determine run range..." << std::endl;
        for (auto obj : *files) {
            TSystemFile *sysFile = (TSystemFile*)obj;
            TString fname = sysFile->GetName();
            
            if (!sysFile->IsDirectory() && fname.BeginsWith("fwdQA_") && fname.EndsWith(".root")) {
                TObjArray *tokens = fname.Tokenize("_");
                if (tokens->GetEntries() >= 5) {
                    TString tType = ((TObjString*)tokens->At(1))->GetString();
                    TString runStr = ((TObjString*)tokens->At(4))->GetString();
                    int runNum = runStr.Atoi();
                    if (runNum > 0) {
                        TString fullPath = Form("%s/%s", inDir.Data(), fname.Data());
                        fileMap[runNum][tType].push_back(fullPath);
                    }
                }
                delete tokens;
            }
        }
        for (auto const& [runNum, _] : fileMap) { orderedRuns.push_back(runNum); }
    }

    TString outPDF = "Master_QA_Deck.pdf";
    TCanvas *c1 = new TCanvas("c1", "QA Slides", 1600, 900); 
    c1->Print(outPDF + "["); // Open the PDF

    // ==========================================
    // --- SLIDE 1: TITLE SLIDE ---
    // ==========================================
    c1->Clear();
    TPaveText *titleText = new TPaveText(0.2, 0.4, 0.8, 0.6, "NDC");
    titleText->SetFillColor(kWhite);
    titleText->SetBorderSize(0);
    titleText->SetTextSize(0.06);
    titleText->AddText("Forward Tracking QA Summary");
    titleText->AddText("Run 22 pp500 GeV");
    titleText->Draw();
    c1->Print(outPDF); 

    // ==========================================
    // --- SLIDE 2: ANALYSIS INFO & CUTS ---
    // ==========================================
    c1->Clear();
    TPaveText *infoText = new TPaveText(0.1, 0.1, 0.9, 0.9, "NDC");
    infoText->SetFillColor(kWhite);
    infoText->SetBorderSize(0);
    infoText->SetTextAlign(12);
    infoText->SetTextSize(0.035);

    infoText->AddText("Dataset: Run 22 pp @ #sqrt{s} = 500 GeV");
    if (!orderedRuns.empty()) {
        infoText->AddText(Form("Run Range Processed: %d - %d", orderedRuns.front(), orderedRuns.back()));
        infoText->AddText(Form("Total Unique Runs: %zu", orderedRuns.size()));
    }
    infoText->AddText(" "); 
    infoText->AddText("#bf{General Cuts:}");
    infoText->AddText("  #bullet Track status > 0 (Kalman fit converged)");
    infoText->AddText(" ");
    infoText->AddText("#bf{Cuts applied for kinematic plots (p_{T}, #eta, #phi):}");
    infoText->AddText("  #bullet Primary Vertex |Z| < 50 cm");
    infoText->AddText("  #bullet nBTOFMatch #geq 2");
    infoText->Draw();
    c1->Print(outPDF);

    // ==========================================
    // --- SLIDE 3: LIST OF RUNS PROCESSED ---
    // ==========================================
    if (!orderedRuns.empty()) {
        c1->Clear();
        TPaveText *runsText = new TPaveText(0.05, 0.05, 0.95, 0.95, "NDC");
        runsText->SetFillColor(kWhite);
        runsText->SetBorderSize(0);
        runsText->SetTextAlign(13); // Align top-left
        runsText->SetTextSize(0.025); // Small font to fit many runs
        
        runsText->AddText(Form("#bf{List of Processed Runs (%zu total):}", orderedRuns.size()));
        runsText->AddText(" ");
        
        TString runLine = "";
        int runsPerLine = 12; // Fits well horizontally with size 0.025
        int maxLinesPerSlide = 30; // Prevents vertical spill-off
        int currentLineCount = 0;

        for(size_t i = 0; i < orderedRuns.size(); ++i) {
            runLine += Form("%d, ", orderedRuns[i]);
            
            // Print the line if we hit the limit or the very last run
            if ((i + 1) % runsPerLine == 0 || i == orderedRuns.size() - 1) {
                // Clean up trailing comma for the absolute last run
                if (i == orderedRuns.size() - 1) {
                    runLine.Remove(TString::kTrailing, ' ');
                    runLine.Remove(TString::kTrailing, ',');
                }
                runsText->AddText(runLine.Data());
                runLine = "";
                currentLineCount++;
                
                // If we run out of vertical room, draw the slide and start a new one
                if (currentLineCount >= maxLinesPerSlide && i != orderedRuns.size() - 1) {
                    runsText->Draw();
                    c1->Print(outPDF);
                    runsText->Clear();
                    runsText->AddText(Form("#bf{List of Processed Runs (%zu total) - Continued:}", orderedRuns.size()));
                    runsText->AddText(" ");
                    currentLineCount = 0;
                }
            }
        }
        runsText->Draw();
        c1->Print(outPDF);
    }

    // ==========================================
    // --- PART 1: OVERALL DISTRIBUTIONS (2x2) ---
    // ==========================================
    {
        gStyle->SetOptStat(1111); 
        
        std::vector<TString> histFiles = {
            "fwdQA_Global_full.root", "fwdQA_Primary_full.root",
            "fwdQA_BLC_full.root", "fwdQA_fwdvtx_full.root"
        };
        int colors[4] = {kBlack, kRed, kBlue, kGreen+2};

        // Custom Text Box Explanations for Physics Constraints
        std::map<TString, TString> slideExplanations;
        slideExplanations["h_mDCAZ"] = "Note: DCAz reference depends on track type constraint. (Global/Primary: PV. BLC: Beamline z-intercept. FwdVtx: Secondary vertex).";
        slideExplanations["h_mDCAXY"] = "Note: DCAxy is the transverse radial distance from the track to its specific reference vertex or 1D constraint line.";
        slideExplanations["hPt"] = "Note: Transverse momentum distributions are zero-suppressed below 0.1 GeV/c.";

        std::vector<TFile*> fList;
        for (const auto& fName : histFiles) {
            TFile *f = TFile::Open(fName, "READ");
            if (f && !f->IsZombie()) fList.push_back(f);
        }

        if (!fList.empty()) {
            TIter next(fList[0]->GetListOfKeys());
            TKey *key;
            
            while ((key = (TKey*)next())) {
                TClass *cl = gROOT->GetClass(key->GetClassName());
                if (!cl->InheritsFrom("TH1")) continue; 
                
                TString hName = key->GetName();
                TString lowerName = hName;
                lowerName.ToLower();

                // FILTER: Skip Mass and MatchIndex plots
                if (lowerName.Contains("mass") || lowerName.Contains("matchindex")) continue;

                c1->Clear();
                c1->Divide(2, 2); 

                bool needsLogY = false;
                if (lowerName.Contains("chi2") || lowerName.Contains("momentum") || 
                    lowerName.Contains("dca")  || lowerName.Contains("hcal") || 
                    lowerName.Contains("ecal") || lowerName.Contains("mult") || 
                    lowerName.Contains("pt")   || lowerName.Contains("eta") || 
                    lowerName.Contains("phi")  || lowerName.Contains("hfwdm")) {
                    needsLogY = true;
                }

                for (size_t i = 0; i < fList.size(); ++i) {
                    c1->cd(i + 1);
                    gPad->SetGridx(); gPad->SetGridy();
                    
                    TH1 *hOriginal = (TH1*)fList[i]->Get(hName);
                    if (!hOriginal) continue;

                    TH1 *hDraw = (TH1*)hOriginal->Clone(Form("%s_draw_%zu", hName.Data(), i));
                    hDraw->SetStats(1); 

                    // ==========================================
                    // --- 2D SPECIFIC AXES LIMITS ---
                    // ==========================================
                    if (hName == "hMult_vs_nBTOFMatch") {
                        hDraw->GetXaxis()->SetRangeUser(1, 50);
                        hDraw->GetYaxis()->SetRangeUser(1, 50); 
                    } else if (hName == "hMult_vs_refMult") {
                        hDraw->GetXaxis()->SetRangeUser(1, 80);
                        hDraw->GetYaxis()->SetRangeUser(1, 50); 
                    } else if (hName == "hMult_vs_nEcal") {
                        hDraw->GetXaxis()->SetRangeUser(1, 35);
                        hDraw->GetYaxis()->SetRangeUser(1, 50); 
                    } else if (hName == "hMult_vs_nHcal") {
                        hDraw->GetXaxis()->SetRangeUser(1, 20);
                        hDraw->GetYaxis()->SetRangeUser(1, 50); 
                    }

                    // ==========================================
                    // --- CUSTOM REBINNING & AXIS RANGES ---
                    // ==========================================
                    
                    if (hName == "hMult") {
                        if (i == 0) { hDraw->Rebin(1); hDraw->GetXaxis()->SetRangeUser(0, 80); } 
                        else if (i == 1) { hDraw->Rebin(1); hDraw->GetXaxis()->SetRangeUser(0, 80); } 
                        else if (i == 2) { hDraw->Rebin(1); hDraw->GetXaxis()->SetRangeUser(0, 100); } 
                        else if (i == 3) { hDraw->Rebin(2); hDraw->GetXaxis()->SetRangeUser(0, 30); }
                    }
                    else if (hName.Contains("Pt")) {
                        hDraw->Rebin(4); hDraw->GetXaxis()->SetRangeUser(0, 50.0);
                    }
                    else if (hName.Contains("Eta")) {
                        hDraw->Rebin(4); hDraw->GetXaxis()->SetRangeUser(0, 10);
                    }
                    else if (hName == "h_mChi2") {
                        if (i == 0) hDraw->Rebin(8);
                        else hDraw->Rebin(2);
                    }
                    else if (hName.Contains("MomentumX") || hName.Contains("MomentumY")) {
                        if (i == 0 || i == 2) hDraw->Rebin(2);
                        if (i == 3) { hDraw->Rebin(4); hDraw->GetXaxis()->SetRangeUser(-10, 10); } 
                    }
                    else if(hName.Contains("MomentumZ")) {
                        if (i == 0 || i == 2) hDraw->Rebin(2);  
                        if (i == 3) { hDraw->Rebin(4); hDraw->GetXaxis()->SetRangeUser(0, 100); }
                    }
                    else if (hName.Contains("DCAXY")) {
                        if (i == 0 || i == 3) { hDraw->Rebin(4); hDraw->GetXaxis()->SetRangeUser(0, 50); } 
                        else if (i == 1) { hDraw->Rebin(4); hDraw->GetXaxis()->SetRangeUser(0, 12); } 
                        else if (i == 2) { hDraw->Rebin(2); hDraw->GetXaxis()->SetRangeUser(0, 8); } 
                    }
                    else if (hName.Contains("DCAZ")) {
                        if (i == 0) { hDraw->Rebin(64); hDraw->GetXaxis()->SetRangeUser(-2500, 2500); } 
                        else if (i == 1) { hDraw->Rebin(4); hDraw->GetXaxis()->SetRangeUser(-500, 500); } 
                        else if (i == 2) { hDraw->Rebin(16); hDraw->GetXaxis()->SetRangeUser(-1000, 1000); } 
                        else if (i == 3) { hDraw->Rebin(8); hDraw->GetXaxis()->SetRangeUser(-500, 500); } 
                    }
                    else if (hName.Contains("mGlobalTrackIndex")) {
                        hDraw->GetXaxis()->SetRangeUser(0, 50);
                    }
                    else if(hName.Contains("mIdTruth")){
                        hDraw->Rebin(1); hDraw->GetXaxis()->SetRangeUser(0, 2);
                    }
                    else if(hName.Contains("mQATruth")){
                        hDraw->Rebin(1); hDraw->GetXaxis()->SetRangeUser(50, 110);
                    }
                    else if(hName.Contains("mPVal")){
                        hDraw->Rebin(4); hDraw->GetXaxis()->SetRangeUser(0, 1);
                    }
                    else if (hName.Contains("ECalX") || hName.Contains("ECalY") || hName.Contains("HCalX") || hName.Contains("HCalY")) {
                        hDraw->GetXaxis()->SetRangeUser(-320, 320);
                    }
                    else if(hName.Contains("mECalZ")) {
                        hDraw->Rebin(32); hDraw->GetXaxis()->SetRangeUser(640, 740);
                    }
                    else if(hName.Contains("mHCalZ")) {
                        hDraw->Rebin(32); hDraw->GetXaxis()->SetRangeUser(720, 820);
                    }
                    else if (hName.Contains("mId")) {
                        if (i == 0 || i == 1) { hDraw->Rebin(2); hDraw->GetXaxis()->SetRangeUser(0, 100); } 
                        else { hDraw->Rebin(4); hDraw->GetXaxis()->SetRangeUser(0, 150); } 
                    }
                    
                    // ==========================================
                    // --- DRAWING ---
                    // ==========================================
                    if (cl->InheritsFrom("TH2")) {
                        int bin00 = hDraw->FindBin(0, 0);
                        hDraw->SetBinContent(bin00, 0);
                        gPad->SetRightMargin(0.12); 
                        hDraw->Draw("COLZ"); 
                    } else {
                        hDraw->SetLineColor(colors[i]);
                        hDraw->SetLineWidth(2);
                        if (needsLogY) {
                            gPad->SetLogy(1);
                            hDraw->SetMinimum(0.5); 
                        }
                        hDraw->Draw("HIST"); 
                    }
                }

                if (slideExplanations.count(hName)) {
                    c1->cd(0); 
                    TPaveText *note = new TPaveText(0.1, 0.01, 0.9, 0.05, "NDC");
                    note->SetBorderSize(1);
                    note->SetFillColor(kWhite);
                    note->SetTextColor(kRed+2);
                    note->SetTextSize(0.025);
                    note->AddText(slideExplanations[hName].Data());
                    note->Draw();
                }

                c1->Print(outPDF); 
            }
            for (auto f : fList) f->Close();
        }
    }

    // ==========================================
    // --- PART 2: RUN-BY-RUN QA TRENDS       ---
    // ==========================================
    {
        int xAxisLabelStep = 30; 
        gStyle->SetOptStat(0); 
        c1->Clear();
        c1->Divide(1, 1); 

        std::vector<TString> histNames = {
            "hMult", "h_mChi2", "h_mNumberOfFitPoints", 
            "h_mMomentumX", "h_mMomentumY", "h_mMomentumZ",
            "h_mDCAXY", "h_mDCAZ", "h_mPVal",
            "hPt", "hEta", "hPhi",
            "hPt_pos", "hPt_neg", "hEta_pos", "hEta_neg", "hPhi_pos", "hPhi_neg"
        };
        std::vector<TString> derivedHistNames = {"h_mMomentumZ_noZero"};
        std::vector<TString> hist2DNames = {"hMult_vs_nEcal", "hMult_vs_nHcal", "hMult_vs_nBTOFMatch", "hMult_vs_refMult"};

        std::map<TString, TString> yUnits = {
            {"hMult", "Multiplicity"}, {"h_mChi2", "#chi^{2}"}, {"h_mNumberOfFitPoints", "Fit Points"},
            {"h_mMomentumX", "p_{x} [GeV/c]"}, {"h_mMomentumY", "p_{y} [GeV/c]"}, {"h_mMomentumZ", "p_{z} [GeV/c]"},
            {"h_mMomentumZ_noZero", "p_{z} (zeros suppressed) [GeV/c]"}, 
            {"h_mDCAXY", "DCA_{xy} [cm]"}, {"h_mDCAZ", "DCA_{z} [cm]"}, {"h_mPVal", "p-value"},
            {"hPt", "p_{T} [GeV/c]"}, {"hPt_pos", "p_{T} [GeV/c]"}, {"hPt_neg", "p_{T} [GeV/c]"},
            {"hEta", "#eta"}, {"hEta_pos", "#eta"}, {"hEta_neg", "#eta"},
            {"hPhi", "#phi [rad]"}, {"hPhi_pos", "#phi [rad]"}, {"hPhi_neg", "#phi [rad]"},
            {"hMult_vs_nEcal", "Pearson Correlation (r)"},
            {"hMult_vs_nHcal", "Pearson Correlation (r)"},
            {"hMult_vs_nBTOFMatch", "Pearson Correlation (r)"},
            {"hMult_vs_refMult", "Pearson Correlation (r)"}
        };

        if (!orderedRuns.empty()) {
            std::map<TString, std::map<TString, TGraphErrors*>> graphs;
            for(const auto& t : trackTypes) {
                for(const auto& h : histNames) {
                    graphs[t][h] = new TGraphErrors();
                    graphs[t][h]->SetName(Form("g_%s_%s", t.Data(), h.Data()));
                }
                for(const auto& hd : derivedHistNames) {
                    graphs[t][hd] = new TGraphErrors();
                    graphs[t][hd]->SetName(Form("g_%s_%s", t.Data(), hd.Data()));
                }
                for(const auto& h2 : hist2DNames) {
                    graphs[t][h2] = new TGraphErrors();
                    graphs[t][h2]->SetName(Form("g_%s_%s_corr", t.Data(), h2.Data()));
                }
            }

            int runCounter = 1;
            for (size_t i = 0; i < orderedRuns.size(); ++i) {
                int runNum = orderedRuns[i];
                int xIndex = i + 1;
                
                std::cout << "Processing RBR Data for Run " << runNum << " (" << runCounter++ << "/" << orderedRuns.size() << ")\r" << std::flush;
                auto const& typeMap = fileMap[runNum];
                
                for (auto const& [tType, fileList] : typeMap) {
                    std::map<TString, TH1F*> mergedHists1D;
                    std::map<TString, TH2F*> mergedHists2D;

                    for (const auto& fname : fileList) {
                        TFile *f = TFile::Open(fname, "READ");
                        if (!f || f->IsZombie()) continue;

                        for (const auto& hName : histNames) {
                            TH1F *h = (TH1F*)f->Get(hName);
                            if (h) {
                                if (mergedHists1D.find(hName) == mergedHists1D.end()) {
                                    mergedHists1D[hName] = (TH1F*)h->Clone(Form("m1d_%d_%s_%s", runNum, tType.Data(), hName.Data()));
                                    mergedHists1D[hName]->SetDirectory(0); 
                                } else { mergedHists1D[hName]->Add(h); }
                            }
                        }
                        
                        for (const auto& h2Name : hist2DNames) {
                            TH2F *h2 = (TH2F*)f->Get(h2Name);
                            if (h2) {
                                if (mergedHists2D.find(h2Name) == mergedHists2D.end()) {
                                    mergedHists2D[h2Name] = (TH2F*)h2->Clone(Form("m2d_%d_%s_%s", runNum, tType.Data(), h2Name.Data()));
                                    mergedHists2D[h2Name]->SetDirectory(0); 
                                } else { mergedHists2D[h2Name]->Add(h2); }
                            }
                        }
                        f->Close();
                    }

                    for (auto& [hName, h] : mergedHists1D) {
                        if (h->GetEntries() > 0) {
                            TGraphErrors *g = graphs[tType][hName];
                            int nPoints = g->GetN();
                            g->SetPoint(nPoints, xIndex, h->GetMean());
                            g->SetPointError(nPoints, 0, h->GetMeanError());
                            
                            if (hName == "h_mMomentumZ") {
                                TH1F *hClone = (TH1F*)h->Clone(Form("%s_noZero", h->GetName()));
                                int zeroBin = hClone->FindBin(0.0);
                                hClone->SetBinContent(zeroBin, 0.0);
                                hClone->SetBinError(zeroBin, 0.0);
                                hClone->ResetStats();
                                
                                TGraphErrors *gNZ = graphs[tType]["h_mMomentumZ_noZero"];
                                int nPointsNZ = gNZ->GetN();
                                gNZ->SetPoint(nPointsNZ, xIndex, hClone->GetMean());
                                gNZ->SetPointError(nPointsNZ, 0, hClone->GetMeanError());
                                delete hClone;
                            }
                        }
                        delete h; 
                    }
                    
                    for (auto& [h2Name, h2] : mergedHists2D) {
                        if (h2->GetEntries() > 0) {
                            int bin00 = h2->FindBin(0, 0);
                            double tempContent = h2->GetBinContent(bin00);
                            h2->SetBinContent(bin00, 0);
                            
                            double corrFactor = h2->GetCorrelationFactor();
                            h2->SetBinContent(bin00, tempContent); 
                            
                            TGraphErrors *g = graphs[tType][h2Name];
                            int nPoints = g->GetN();
                            g->SetPoint(nPoints, xIndex, corrFactor);
                            g->SetPointError(nPoints, 0, 0); 
                        }
                        delete h2; 
                    }
                }
            }
            std::cout << std::endl << "RBR Data aggregation complete. Plotting trends..." << std::endl;

            int colors[4] = {kBlack, kRed, kBlue, kGreen+2};
            std::vector<TString> allPlotsToDraw = histNames;
            allPlotsToDraw.insert(allPlotsToDraw.end(), derivedHistNames.begin(), derivedHistNames.end());
            allPlotsToDraw.insert(allPlotsToDraw.end(), hist2DNames.begin(), hist2DNames.end());

            c1->SetBottomMargin(0.15); 

            for (const auto& hName : allPlotsToDraw) {
                
                TString lowerName = hName;
                lowerName.ToLower();
                if (lowerName.Contains("mass") || lowerName.Contains("matchindex")) continue;

                c1->Clear();
                TMultiGraph *mg = new TMultiGraph();
                
                TString yLabel = yUnits.count(hName) ? yUnits[hName] : "Value";
                
                if (std::find(hist2DNames.begin(), hist2DNames.end(), hName) != hist2DNames.end()) {
                    mg->SetTitle(Form("%s vs Run Number; Run Number; %s", hName.Data(), yLabel.Data()));
                } else {
                    mg->SetTitle(Form("Mean %s vs Run Number; Run Number; Mean %s", hName.Data(), yLabel.Data()));
                }
                
                TLegend *leg = new TLegend(0.85, 0.7, 0.98, 0.9);
                leg->SetBorderSize(1);

                int typeIdx = 0;
                bool hasData = false;
                std::vector<double> allYValues;
                
                for (const auto& tType : trackTypes) {
                    TGraphErrors *g = graphs[tType][hName];
                    g->Sort(); 
                    
                    if (g->GetN() > 0) {
                        g->SetMarkerStyle(20);
                        g->SetMarkerColor(colors[typeIdx]);
                        g->SetLineColor(colors[typeIdx]);
                        mg->Add(g, "PL"); 
                        leg->AddEntry(g, tType.Data(), "lep");
                        hasData = true;
                        
                        // Collect Y values for robust axis scaling
                        for (int k = 0; k < g->GetN(); k++) {
                            allYValues.push_back(g->GetY()[k]);
                        }
                    }
                    typeIdx++;
                }

                if (hasData) {
                    mg->Draw("A");
                    
                    // --- RELAXED ROBUST Y-AXIS SCALING ---
                    // Changed from 10th/90th to 5th/95th with massive 2.0x padding
                    if (std::find(hist2DNames.begin(), hist2DNames.end(), hName) != hist2DNames.end()) {
                        mg->GetHistogram()->SetMinimum(-0.2); 
                        mg->GetHistogram()->SetMaximum(1.0);
                    } else if (allYValues.size() > 10) {
                        std::sort(allYValues.begin(), allYValues.end());
                        int nVals = allYValues.size();
                        double p05 = allYValues[nVals * 0.05]; 
                        double p95 = allYValues[nVals * 0.95]; 
                        double spread = p95 - p05;
                        if (spread < 1e-5) spread = 0.1; // Prevent flattening on uniform data
                        
                        // The 2.0 multiplier ensures statistical noise doesn't look like massive spikes
                        mg->GetHistogram()->SetMinimum(p05 - 2.0 * spread);
                        mg->GetHistogram()->SetMaximum(p95 + 2.0 * spread);
                    }
                    
                    gPad->Update(); 
                    
                    TAxis *ax = mg->GetHistogram()->GetXaxis();
                    ax->Set(orderedRuns.size(), 0.5, orderedRuns.size() + 0.5);
                    
                    for (size_t i = 0; i < orderedRuns.size(); ++i) {
                        if (i % xAxisLabelStep == 0 || i == orderedRuns.size() - 1) {
                            ax->SetBinLabel(i + 1, Form("%d", orderedRuns[i]));
                        } else {
                            ax->SetBinLabel(i + 1, ""); 
                        }
                    }
                    ax->LabelsOption("v"); 
                    
                    leg->Draw();
                    c1->Modified();
                    c1->Update();
                    c1->Print(outPDF); 
                }
                delete mg;
                delete leg;
            }
        }
    }

    // ==========================================
    // --- SLIDE LAST: NEXT STEPS & TO-DO LIST ---
    // ==========================================
    c1->Clear();
    TPaveText *todoText = new TPaveText(0.1, 0.1, 0.9, 0.9, "NDC");
    todoText->SetFillColor(kWhite);
    todoText->SetBorderSize(0);
    todoText->SetTextAlign(12); // Left-aligned
    todoText->SetTextSize(0.035); // Reduced size to prevent bounding box cutoff

    /*todoText->AddText("#bf{Next Steps & To-Do List}");
    todoText->AddText(" ");
    todoText->AddText("  #bullet Scale up execution to run over the full Run 22 production dataset.");
    todoText->AddText("  #bullet Establish quantitative rejection criteria for anomalous/bad runs");
    todoText->AddText("     based on RBR QA trends.");
    todoText->AddText("  #bullet Finalize tight DCA_{z} and DCA_{xy} cut limits to optimize");
    todoText->AddText("     secondary vertex finding for lambda search.");

    todoText->Draw();*/
    c1->Print(outPDF);

    // ==========================================
    // --- CLOSING ---
    // ==========================================
    c1->Print(outPDF + "]"); 
    std::cout << "Successfully generated " << outPDF << "!" << std::endl;
}