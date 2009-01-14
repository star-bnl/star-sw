#include "BEMCPlotsPresenter.h"
#include <iostream>
using namespace std;
#include <TH1F.h>
#include <TH2F.h>
#include <TCanvas.h>
#include <TLine.h>
#include <TArrow.h>
#include <TLatex.h>
#include <TString.h>
#include <TBox.h>

#include <GenericFile.h>

#include <StDaqLib/EMC/StEmcDecoder.h>

#include "BEMCPlotsNames.h"

#define min(A, B) (((A) < (B)) ? (A) : (B))
#define max(A, B) (((A) > (B)) ? (A) : (B))

TList *BEMCPlotsCleanUpHistoList = 0;
StEmcDecoder *BEMCDecoderPresenter = 0;
Bool_t useDecoderForBoundaries = false; // Use StEmcDecoder for showing ranges (the tower map bug fix mixes softIds between crates)

//-------------------------------------------------------------------
// Taken from EEMC EEqaPresenter
TH1 *GetHisto(FileType &fd, const char *name) {
    if (!BEMCPlotsCleanUpHistoList) BEMCPlotsCleanUpHistoList = new TList();
    TH1 *hist = 0;
    // this is very silly trick to avoid memory leak in the online version
    hist = fd.file() ? (TH1 *)fd.Get(name, 0) : 0;
    if (getenv("ONLINBEPLOTDIR")&&
       BEMCPlotsCleanUpHistoList && hist) {
       BEMCPlotsCleanUpHistoList->Add(hist);
    }
    return hist;
}
//-------------------------------------------------------------------
void BEMCPlotsPresenter::displayRawAdc(FileType file, TPad *pad, bool psd, Int_t mDebug) {
    if (mDebug >= 10) cout << __FILE__ << ":" << __LINE__ << endl;

    TH2F *HistRawAdc1 = (TH2F*)GetHisto(file, psd ? HistRawAdcPsd1Name : HistRawAdc1Name);
    if (!HistRawAdc1 || (mDebug >= 2)) cout << "HistRawAdc1 = " << HistRawAdc1 << endl;
    TH2F *HistRawAdc2 = (TH2F*)GetHisto(file, psd ? HistRawAdcPsd2Name : HistRawAdc2Name);
    if (!HistRawAdc2 || (mDebug >= 2)) cout << "HistRawAdc2 = " << HistRawAdc2 << endl;
    TH2F *HistRawAdc3 = (TH2F*)GetHisto(file, psd ? HistRawAdcPsd3Name : HistRawAdc3Name);
    if (!HistRawAdc3 || (mDebug >= 2)) cout << "HistRawAdc3 = " << HistRawAdc3 << endl;
    TH2F *HistRawAdc4 = (TH2F*)GetHisto(file, psd ? HistRawAdcPsd4Name : HistRawAdc4Name);
    if (!HistRawAdc4 || (mDebug >= 2)) cout << "HistRawAdc4 = " << HistRawAdc4 << endl;

    if (!pad) return;
    pad->Clear();
    pad->cd(0);
    
    TPad* c = new TPad("pad2", "apd2",0.0,0.1,1.,1.);
    c->Draw();
    c->cd(0);
    c->Divide(1, 4, 0.001, 0.001);
    
    // 12-19 are the grey shades dark-light
    Int_t linesColor = 16;
    Int_t crateMinSoftId[] = {4661, 2421, 2581, 2741, 2901, 3061, 3221, 3381, 3541, 3701, 3861, 4021, 4181, 4341, 4501, 2181, 2021, 1861, 1701, 1541, 1381, 1221, 1061,  901,  741,  581,  421,  261,  101,    1};
    Int_t crateMaxSoftId[] = {4800, 2580, 2740, 2900, 3060, 3220, 3380, 3540, 3700, 3860, 4020, 4180, 4340, 4500, 4660, 2340, 2180, 2020, 1860, 1700, 1540, 1380, 1220, 1060,  900,  740,  580,  420,  260,  100};
    Int_t pmtbxMinSoftId[] = {2261, 2181, 2101, 2021, 1941, 1861, 1781, 1701, 1621, 1541, 1461, 1381, 1301, 1221, 1141, 1061,  981,  901,  821,  741,  661,  581,  501,  421,  341,  261,  181,  101,   21, 2341/*and from 1*/,
			     4661, 4741, 2421, 2501, 2581, 2661, 2741, 2821, 2901, 2981, 3061, 3141, 3221, 3301, 3381, 3461, 3541, 3621, 3701, 3781, 3861, 3941, 4021, 4101, 4181, 4261, 4341, 4421, 4501, 4581
			 	   /*and from 2401*/
			     };
    Int_t pmtbxMaxSoftId[] = {2340, 2260, 2180, 2100, 2020, 1940, 1860, 1780, 1700, 1620, 1540, 1460, 1380, 1300, 1220, 1140, 1060,  980,  900,  820,  740,  660,  580,  500,  420,  340,  260,  180,  100, 2400/*to 20*/,
			     4740, 4800, 2500, 2580, 2660, 2740, 2820, 2900, 2980, 3060, 3140, 3220, 3300, 3380, 3460, 3540, 3620, 3700, 3780, 3860, 3940, 4020, 4100, 4180, 4260, 4340, 4420, 4500, 4580, 4660
				   /*to 2420*/
			     };
    if (useDecoderForBoundaries && BEMCDecoderPresenter) {
	for (Int_t crate = 0;crate < 30;crate++) {
	    crateMinSoftId[crate] = 4800;
	    crateMaxSoftId[crate] = 1;
	}
	for (Int_t crate = 0;crate < 30;crate++) {
    	    if (BEMCDecoderPresenter) {
		for (Int_t crateSeq = 0;crateSeq < 160;crateSeq++) {
		    Int_t softId;
		    if (BEMCDecoderPresenter->GetTowerIdFromCrate(crate + 1, crateSeq, softId)) {
			if (softId < crateMinSoftId[crate]) crateMinSoftId[crate] = softId;
			if (softId > crateMaxSoftId[crate]) crateMaxSoftId[crate] = softId;
		    }
		}
	    }
	}
	//for (Int_t crate = 0;crate < 30;crate++) cout << "Crate " << (crate+1) << ": SoftId " << crateMinSoftId[crate] << " - " << crateMaxSoftId[crate] << endl;
	// no PMT boxes numeration scheme in decoder
    }

    Int_t *boxMinSoftId = psd ? pmtbxMinSoftId : crateMinSoftId;
    Int_t *boxMaxSoftId = psd ? pmtbxMaxSoftId : crateMaxSoftId;
    const Char_t *boxLabelFormat = psd ? "%i" : "0x%.2X";
    Int_t numBoxes = psd ? (sizeof(pmtbxMinSoftId)/sizeof(pmtbxMinSoftId[0])) : (sizeof(crateMinSoftId)/sizeof(crateMinSoftId[0]));

    c->cd(1);
    if (HistRawAdc1) {
	HistRawAdc1->SetStats(0);
	HistRawAdc1->Draw("COLZ");
	for (Int_t crate = 0;crate < numBoxes;crate++) {
	    if (!((boxMaxSoftId[crate] < HistRawAdc1->GetXaxis()->GetBinLowEdge(1))
		|| (boxMinSoftId[crate] > HistRawAdc1->GetXaxis()->GetBinUpEdge(HistRawAdc1->GetXaxis()->GetNbins())))
		) {
		Float_t left = max(boxMinSoftId[crate], HistRawAdc1->GetXaxis()->GetBinCenter(1)) - 0.5;
		Float_t right = 0.5 + min(boxMaxSoftId[crate], HistRawAdc1->GetXaxis()->GetBinCenter(HistRawAdc1->GetXaxis()->GetNbins()));
		TLine *lCrates = new TLine(left, HistRawAdc1->GetYaxis()->GetBinLowEdge(1), left, HistRawAdc1->GetYaxis()->GetBinUpEdge(HistRawAdc1->GetYaxis()->GetNbins()));
		if (lCrates) {
    		    lCrates->SetLineColor(linesColor); 
		    lCrates->SetLineWidth(1);
		    lCrates->Draw();
		}
		TLine *rCrates = new TLine(right, HistRawAdc1->GetYaxis()->GetBinLowEdge(1), right, HistRawAdc1->GetYaxis()->GetBinUpEdge(HistRawAdc1->GetYaxis()->GetNbins()));
		if (rCrates) {
    		    rCrates->SetLineColor(linesColor); 
		    rCrates->SetLineWidth(1);
		    rCrates->Draw();
		}
    		TString crateLabel;
    		crateLabel = Form(boxLabelFormat, crate + 1);
		TLatex *textCrate = new TLatex(left + 10, HistRawAdc1->GetYaxis()->GetBinUpEdge(HistRawAdc1->GetYaxis()->GetNbins()) * 0.8, crateLabel.Data());
		if (textCrate) {
		    textCrate->SetTextColor(linesColor);
    		    textCrate->SetTextSize(0.14);
		    textCrate->Draw();
		}
	    }
	}
    }
    c->cd(2);
    if (HistRawAdc2) {
	HistRawAdc2->SetStats(0);
	HistRawAdc2->Draw("COLZ");
	for (Int_t crate = 0;crate < numBoxes;crate++) {
	    if (!((boxMaxSoftId[crate] < HistRawAdc2->GetXaxis()->GetBinLowEdge(1))
		|| (boxMinSoftId[crate] > HistRawAdc2->GetXaxis()->GetBinUpEdge(HistRawAdc2->GetXaxis()->GetNbins())))
		) {
		Float_t left = max(boxMinSoftId[crate], HistRawAdc2->GetXaxis()->GetBinCenter(1)) - 0.5;
		Float_t right = 0.5 + min(boxMaxSoftId[crate], HistRawAdc2->GetXaxis()->GetBinCenter(HistRawAdc2->GetXaxis()->GetNbins()));
		TLine *lCrates = new TLine(left, HistRawAdc2->GetYaxis()->GetBinLowEdge(1), left, HistRawAdc2->GetYaxis()->GetBinUpEdge(HistRawAdc2->GetYaxis()->GetNbins()));
		if (lCrates) {
    		    lCrates->SetLineColor(linesColor); 
		    lCrates->SetLineWidth(1);
		    lCrates->Draw();
		}
		TLine *rCrates = new TLine(right, HistRawAdc2->GetYaxis()->GetBinLowEdge(1), right, HistRawAdc2->GetYaxis()->GetBinUpEdge(HistRawAdc2->GetYaxis()->GetNbins()));
		if (rCrates) {
    		    rCrates->SetLineColor(linesColor); 
		    rCrates->SetLineWidth(1);
		    rCrates->Draw();
		}
    		TString crateLabel;
    		crateLabel = Form(boxLabelFormat, crate + 1);
		TLatex *textCrate = new TLatex(left + 10, HistRawAdc2->GetYaxis()->GetBinUpEdge(HistRawAdc2->GetYaxis()->GetNbins()) * 0.8, crateLabel.Data());
		if (textCrate) {
		    textCrate->SetTextColor(linesColor);
    		    textCrate->SetTextSize(0.14);
		    textCrate->Draw();
		}
	    }
	}
    }
    c->cd(3);
    if (HistRawAdc3) {
	HistRawAdc3->SetStats(0);
	HistRawAdc3->Draw("COLZ");
	for (Int_t crate = 0;crate < numBoxes;crate++) {
	    if (!((boxMaxSoftId[crate] < HistRawAdc3->GetXaxis()->GetBinLowEdge(1))
		|| (boxMinSoftId[crate] > HistRawAdc3->GetXaxis()->GetBinUpEdge(HistRawAdc3->GetXaxis()->GetNbins())))
		) {
		Float_t left = max(boxMinSoftId[crate], HistRawAdc3->GetXaxis()->GetBinCenter(1)) - 0.5;
		Float_t right = 0.5 + min(boxMaxSoftId[crate], HistRawAdc3->GetXaxis()->GetBinCenter(HistRawAdc3->GetXaxis()->GetNbins()));
		TLine *lCrates = new TLine(left, HistRawAdc3->GetYaxis()->GetBinLowEdge(1), left, HistRawAdc3->GetYaxis()->GetBinUpEdge(HistRawAdc3->GetYaxis()->GetNbins()));
		if (lCrates) {
    		    lCrates->SetLineColor(linesColor); 
		    lCrates->SetLineWidth(1);
		    lCrates->Draw();
		}
		TLine *rCrates = new TLine(right, HistRawAdc3->GetYaxis()->GetBinLowEdge(1), right, HistRawAdc3->GetYaxis()->GetBinUpEdge(HistRawAdc3->GetYaxis()->GetNbins()));
		if (rCrates) {
    		    rCrates->SetLineColor(linesColor); 
		    rCrates->SetLineWidth(1);
		    rCrates->Draw();
		}
    		TString crateLabel;
    		crateLabel = Form(boxLabelFormat, crate + 1);
		TLatex *textCrate = new TLatex(left + 10, HistRawAdc3->GetYaxis()->GetBinUpEdge(HistRawAdc3->GetYaxis()->GetNbins()) * 0.8, crateLabel.Data());
		if (textCrate) {
		    textCrate->SetTextColor(linesColor);
    		    textCrate->SetTextSize(0.14);
		    textCrate->Draw();
		}
	    }
	}
    }
    c->cd(4);
    if (HistRawAdc4) {
	HistRawAdc4->SetStats(0);
	HistRawAdc4->Draw("COLZ");
	for (Int_t crate = 0;crate < numBoxes;crate++) {
	    if (!((boxMaxSoftId[crate] < HistRawAdc4->GetXaxis()->GetBinLowEdge(1))
		|| (boxMinSoftId[crate] > HistRawAdc4->GetXaxis()->GetBinUpEdge(HistRawAdc4->GetXaxis()->GetNbins())))
		) {
		Float_t left = max(boxMinSoftId[crate], HistRawAdc4->GetXaxis()->GetBinCenter(1)) - 0.5;
		Float_t right = 0.5 + min(boxMaxSoftId[crate], HistRawAdc4->GetXaxis()->GetBinCenter(HistRawAdc4->GetXaxis()->GetNbins()));
		TLine *lCrates = new TLine(left, HistRawAdc4->GetYaxis()->GetBinLowEdge(1), left, HistRawAdc4->GetYaxis()->GetBinUpEdge(HistRawAdc4->GetYaxis()->GetNbins()));
		if (lCrates) {
    		    lCrates->SetLineColor(linesColor); 
		    lCrates->SetLineWidth(1);
		    lCrates->Draw();
		}
		TLine *rCrates = new TLine(right, HistRawAdc4->GetYaxis()->GetBinLowEdge(1), right, HistRawAdc4->GetYaxis()->GetBinUpEdge(HistRawAdc4->GetYaxis()->GetNbins()));
		if (rCrates) {
    		    rCrates->SetLineColor(linesColor); 
		    rCrates->SetLineWidth(1);
		    rCrates->Draw();
		}
    		TString crateLabel;
    		crateLabel = Form(boxLabelFormat, crate + 1);
		TLatex *textCrate = new TLatex(left + 10, HistRawAdc4->GetYaxis()->GetBinUpEdge(HistRawAdc4->GetYaxis()->GetNbins()) * 0.8, crateLabel.Data());
		if (textCrate) {
		    textCrate->SetTextColor(linesColor);
    		    textCrate->SetTextSize(0.14);
		    textCrate->Draw();
		}
	    }
	}
    }
}
//-------------------------------------------------------------------
void BEMCPlotsPresenter::displayJetPatchHT(FileType file, TPad *pad, Int_t mDebug) {
    if (mDebug >= 10) cout << __FILE__ << ":" << __LINE__ << endl;

    TH1F *HistHighTowerSpectrum[12];
    for (Int_t i = 0;i < 12;i++) {
        TString name;
        name = Form(HistHighTowerSpectrumName "_%u", i);
        HistHighTowerSpectrum[i] = (TH1F*)GetHisto(file, (const char *)name);
	if (!HistHighTowerSpectrum[i] || (mDebug >= 2)) cout << "HistHighTowerSpectrum[" << i << "] = " << HistHighTowerSpectrum[i] << endl;
    }
    if (!pad) return;
    pad->Clear();
    pad->cd(0);
    
    TPad* c = new TPad("pad2", "apd2",0.0,0.1,1.,1.);
    c->Draw();
    c->cd(0);
    c->Divide(3, 4, 0.001, 0.001);
    
    for (Int_t jetPatch = 0;jetPatch < 12;jetPatch++) {
	c->cd(jetPatch+1);
	if (HistHighTowerSpectrum[jetPatch]) {
	    HistHighTowerSpectrum[jetPatch]->SetStats(0);
	    HistHighTowerSpectrum[jetPatch]->Draw();
	}
	gPad->SetLogy();
    }
}
//-------------------------------------------------------------------
void BEMCPlotsPresenter::displayJetPatchSum(FileType file, TPad *pad, Int_t mDebug) {
    if (mDebug >= 10) cout << __FILE__ << ":" << __LINE__ << endl;

    TH1F *HistPatchSumSpectrum[12];
    for (Int_t i = 0;i < 12;i++) {
        TString name;
        name = Form(HistPatchSumSpectrumName "_%u", i);
	HistPatchSumSpectrum[i] = (TH1F*)GetHisto(file, (const char*)name);
	if (!HistPatchSumSpectrum[i] || (mDebug >= 2)) cout << "HistPatchSumSpectrum[" << i << "] = " << HistPatchSumSpectrum[i] << endl;
    }
    if (!pad) return;
    pad->Clear();
    pad->cd(0);
    
    TPad* c = new TPad("pad2", "apd2",0.0,0.1,1.,1.);
    c->Draw();
    c->cd(0);
    c->Divide(3, 4, 0.001, 0.001);
    
    for (Int_t jetPatch = 0;jetPatch < 12;jetPatch++) {
	c->cd(jetPatch+1);
	if (HistPatchSumSpectrum[jetPatch]) {
	    HistPatchSumSpectrum[jetPatch]->SetStats(0);
	    HistPatchSumSpectrum[jetPatch]->Draw();
	}
	gPad->SetLogy();
    }
}
//-------------------------------------------------------------------
void BEMCPlotsPresenter::displayL0Input(FileType file, TPad *pad, Int_t mDebug) {
    if (mDebug >= 10) cout << __FILE__ << ":" << __LINE__ << endl;

    TH2F *HistDsmL0InputHighTower = (TH2F*)GetHisto(file, HistDsmL0InputHighTowerName);
    if (!HistDsmL0InputHighTower || (mDebug >= 2)) cout << "HistDsmL0InputHighTower = " << HistDsmL0InputHighTower << endl;
    TH2F *HistDsmL0InputPatchSum = (TH2F*)GetHisto(file, HistDsmL0InputPatchSumName);
    if (!HistDsmL0InputPatchSum || (mDebug >= 2)) cout << "HistDsmL0InputPatchSum = " << HistDsmL0InputPatchSum << endl;

    if (!pad) return;
    pad->Clear();
    pad->cd(0);
    
    TPad* c = new TPad("pad2", "apd2",0.0,0.1,1.,1.);
    c->Draw();
    c->cd(0);
    c->Divide(1, 2, 0.001, 0.001);
    
    // 12-19 are the grey shades dark-light
    Int_t linesColor = 16;
    c->cd(1);
    if (HistDsmL0InputHighTower && BEMCDecoderPresenter) {
	HistDsmL0InputHighTower->SetStats(0);
	HistDsmL0InputHighTower->Draw("COLZ");
	TLine *lCrates = new TLine(0, 50, 300, 50);
	if (lCrates) {
    	    lCrates->SetLineColor(linesColor); 
	    lCrates->SetLineWidth(1);
	    lCrates->Draw();
	}
	TLatex *textCrates = new TLatex(1, 50, "BTOW Crates:");
	if (textCrates) {
	    textCrates->SetTextColor(linesColor);
    	    textCrates->SetTextSize(0.0333);
	    textCrates->Draw();
	}
	for (Int_t icrate = 1;icrate <= 30;icrate++) {
	    Int_t triggerPatchBegin, triggerPatchEnd;
	    if (BEMCDecoderPresenter->GetTriggerPatchFromCrate(icrate, 0, triggerPatchBegin) && BEMCDecoderPresenter->GetTriggerPatchFromCrate(icrate, 159, triggerPatchEnd)) {
    		TLine *lCrateBegin = new TLine(triggerPatchBegin-0.5, 0, triggerPatchBegin-0.5, 50);
    		if (lCrateBegin) {
		    lCrateBegin->SetLineColor(linesColor);
    		    lCrateBegin->SetLineWidth(1);
		    lCrateBegin->Draw();
		}
    		TString crateLabel;
    		crateLabel = Form("%.2X", icrate);
		TLatex *textCrate = new TLatex(triggerPatchBegin + 1, 45, crateLabel.Data());
		if (textCrate) {
		    textCrate->SetTextColor(linesColor);
    		    textCrate->SetTextSize(0.0333);
		    textCrate->Draw();
		}
	    }
	}
	for (Int_t ijp = 0;ijp < 12;ijp++) {
	    Int_t maxTriggerPatch = 0;
	    Int_t minTriggerPatch = 300;
    	    for (Int_t j = 0;j < 25;j++) {
		Int_t triggerPatch;
		if (BEMCDecoderPresenter->GetTriggerPatchFromJetPatch(ijp, j, triggerPatch)) {
		    if (maxTriggerPatch < triggerPatch) maxTriggerPatch = triggerPatch;
		    if (minTriggerPatch > triggerPatch) minTriggerPatch = triggerPatch;
		}
	    }
	    maxTriggerPatch++;
	    Int_t height = (ijp%2) ? 55 : 60;
	    TArrow *arrow = new TArrow(minTriggerPatch-0.5, height, maxTriggerPatch-0.5, height, 0.02, "<>");
	    if (arrow) {
		arrow->SetLineColor(linesColor);
		arrow->SetFillColor(linesColor);
		arrow->Draw();
	    }
    	    TString jpLabel;
    	    jpLabel = Form("JetPatch %u", ijp);
	    TLatex *text = new TLatex(minTriggerPatch + 1, height + 1, jpLabel.Data());
	    if (text) {
		text->SetTextColor(linesColor);
    		text->SetTextSize(0.0333);
		text->Draw();
	    }
	}
    }
    c->cd(2);
    if (HistDsmL0InputPatchSum) {
	HistDsmL0InputPatchSum->SetStats(0);
	HistDsmL0InputPatchSum->Draw("COLZ");
	for (Int_t icrate = 1;icrate <= 30;icrate++) {
	    Int_t triggerPatchBegin, triggerPatchEnd;
	    if (BEMCDecoderPresenter->GetTriggerPatchFromCrate(icrate, 0, triggerPatchBegin) && BEMCDecoderPresenter->GetTriggerPatchFromCrate(icrate, 159, triggerPatchEnd)) {
    		TLine *lCrateBegin = new TLine(triggerPatchBegin-0.5, 0, triggerPatchBegin-0.5, 64);
    		if (lCrateBegin) {
		    lCrateBegin->SetLineColor(linesColor);
    		    lCrateBegin->SetLineWidth(1);
		    lCrateBegin->Draw();
		}
    		TString crateLabel;
    		crateLabel = Form("%.2X", icrate);
		TLatex *textCrate = new TLatex(triggerPatchBegin + 1, 60, crateLabel.Data());
		if (textCrate) {
		    textCrate->SetTextColor(linesColor);
    		    textCrate->SetTextSize(0.0333);
		    textCrate->Draw();
		}
	    }
	}
    }
    if (mDebug >= 10) cout << __FILE__ << ":" << __LINE__ << endl;
}
//-------------------------------------------------------------------
void BEMCPlotsPresenter::displayL1Input(FileType file, TPad *pad, Int_t mDebug) {
    if (mDebug >= 10) cout << __FILE__ << ":" << __LINE__ << endl;

    TH2F *HistDsmL1InputHighTowerBits = (TH2F*)GetHisto(file, HistDsmL1InputHighTowerBitsName);
    if (!HistDsmL1InputHighTowerBits || (mDebug >= 2)) cout << "HistDsmL1InputHighTowerBits = " << HistDsmL1InputHighTowerBits << endl;
    TH2F *HistDsmL1InputPatchSum = (TH2F*)GetHisto(file, HistDsmL1InputPatchSumName);
    if (!HistDsmL1InputPatchSum || (mDebug >= 2)) cout << "HistDsmL1InputPatchSum = " << HistDsmL1InputPatchSum << endl;

    if (!pad) return;
    pad->Clear();
    pad->cd(0);
    
    TPad* c = new TPad("pad2", "apd2",0.0,0.1,1.,1.);
    c->Draw();
    c->cd(0);
    c->Divide(1, 2, 0.001, 0.001);
    
    // 12-19 are the grey shades dark-light
    Int_t linesColor = 16;
    c->cd(1);
    if (HistDsmL1InputHighTowerBits) {
	HistDsmL1InputHighTowerBits->SetStats(0);
	HistDsmL1InputHighTowerBits->Draw("COLZ");
	HistDsmL1InputHighTowerBits->GetXaxis()->SetNdivisions(0, false);
	HistDsmL1InputHighTowerBits->GetYaxis()->SetNdivisions(0, false);
	TLatex *textHT0 = new TLatex(-3.5, 0.5, "HT < th0");
	if (textHT0) {
	    textHT0->SetTextColor(linesColor);
    	    textHT0->SetTextSize(0.0333);
	    textHT0->Draw();
	}
	TLatex *textHT1 = new TLatex(-3.5, 1.5, "th0 < HT < th1");
	if (textHT1) {
	    textHT1->SetTextColor(linesColor);
    	    textHT1->SetTextSize(0.0333);
	    textHT1->Draw();
	}
	TLatex *textHT2 = new TLatex(-3.5, 2.5, "th1 < HT < th2");
	if (textHT2) {
	    textHT2->SetTextColor(linesColor);
    	    textHT2->SetTextSize(0.0333);
	    textHT2->Draw();
	}
	TLatex *textHT3 = new TLatex(-3.5, 3.5, "th2 < HT");
	if (textHT3) {
	    textHT3->SetTextColor(linesColor);
    	    textHT3->SetTextSize(0.0333);
	    textHT3->Draw();
	}
	TLatex *textDsmsL0 = new TLatex(-3.5, 4.0, "#splitline{#splitline{DSM Level-0}{Boards}}{BTOW Crates}");
	if (textDsmsL0) {
	    textDsmsL0->SetTextColor(linesColor);
    	    textDsmsL0->SetTextSize(0.0333);
	    textDsmsL0->Draw();
	}
	/*TLatex *textCrates = new TLatex(-3.5, 4.45, "BTOW Crates");
	if (textCrates) {
	    textCrates->SetTextColor(linesColor);
    	    textCrates->SetTextSize(0.0333);
	    textCrates->Draw();
	}*/
	/*TLine *lDsms = new TLine(0, 4.4, 36, 4.4);
	if (lDsms) {
    	    lDsms->SetLineColor(linesColor); 
	    lDsms->SetLineWidth(1);
	    lDsms->Draw();
	}*/
	Int_t dsmL0 = 0;
	Int_t ch = 0;
	for (Int_t idsmL1 = 0;idsmL1 < 6;idsmL1++) {
	    for (Int_t idsmL1ch = 0;idsmL1ch < 6;idsmL1ch++) {		
    		TLine *lDsmL0Begin = new TLine(ch-0.5, 0, ch-0.5, 4.4);
    		if (lDsmL0Begin) {
		    lDsmL0Begin->SetLineColor(linesColor);
    		    lDsmL0Begin->SetLineWidth(1);
		    lDsmL0Begin->Draw();
		}
        	TString label;
		Int_t crate = 0, crateSeq = 0;
		if (BEMCDecoderPresenter) {
		    Int_t triggerPatch = dsmL0 * 10;
		    BEMCDecoderPresenter->GetCrateAndSequenceFromTriggerPatch(triggerPatch, crate, crateSeq);
		}
		if (idsmL1 < 3) {
    		    label = Form("#splitline{#splitline{BW}{0%.2u}}{%.2X}", (dsmL0 % 15) + 1, crate);
		} else {
    		    label = Form("#splitline{#splitline{BE}{0%.2u}}{%.2X}", (dsmL0 % 15) + 1, crate);
		}
		TLatex *text = new TLatex(ch + 0.1-0.5, 4.0, label.Data());
		if (text) {
		    text->SetTextColor(linesColor);
    		    text->SetTextSize(0.0333);
		    text->Draw();
		}
        	/*TString labelCrate;
		if (BEMCDecoderPresenter) {
		    Int_t triggerPatch = dsmL0 * 10;
		    Int_t crate, crateSeq;
		    if (BEMCDecoderPresenter->GetCrateAndSequenceFromTriggerPatch(triggerPatch, crate, crateSeq)) {
			labelCrate = Form("%.2X", crate);
		    }
		}
		TLatex *textCrate = new TLatex(ch + 0.1, 4.45, labelCrate.Data());
		if (textCrate) {
		    textCrate->SetTextColor(linesColor);
    		    textCrate->SetTextSize(0.0333);
		    textCrate->Draw();
		}*/
		ch++;	
		if (idsmL1ch != 2) dsmL0++;
	    }
	}
	for (Int_t ijp = 0;ijp < 12;ijp++) {
	    Int_t maxCh = (ijp * 3) + 3;
	    Int_t minCh = (ijp * 3) + 0;
	    Float_t height = (ijp%2) ? 4.7 : 4.7;
	    TArrow *arrow = new TArrow(minCh-0.5, height, maxCh-0.5, height, 0.02, "<>");
	    if (arrow) {
		arrow->SetLineColor(linesColor);
		arrow->SetFillColor(linesColor);
		arrow->Draw();
	    }
    	    TString jpLabel;
    	    jpLabel = Form("JetPatch %u", ijp);
	    TLatex *text = new TLatex(minCh + 0.1-0.5, height + 0.1, jpLabel.Data());
	    if (text) {
		text->SetTextColor(linesColor);
    		text->SetTextSize(0.0333);
		text->Draw();
	    }
	}
	TLatex *textDsmsL1 = new TLatex(-3.5, -0.4, "#splitline{DSM Level-1}{Boards}");
	if (textDsmsL1) {
	    textDsmsL1->SetTextColor(linesColor);
    	    textDsmsL1->SetTextSize(0.0333);
	    textDsmsL1->Draw();
	}
	for (Int_t idsmL1 = 0;idsmL1 < 6;idsmL1++) {
	    TArrow *arrow = new TArrow((idsmL1 * 6)-0.5, -0.4, (idsmL1 * 6) + 6-0.5, -0.4, 0.02, "<>");
	    if (arrow) {
		arrow->SetLineColor(linesColor);
		arrow->SetFillColor(linesColor);
		arrow->Draw();
	    }
    	    TString label;
	    if (idsmL1 < 3) {
    		label = Form("BW1%.2u", (idsmL1 % 3) + 1);
	    } else {
    		label = Form("BE1%.2u", (idsmL1 % 3) + 1);	    
	    }
	    TLatex *text = new TLatex((idsmL1 * 6) + 1-0.5, -0.35, label.Data());
	    if (text) {
		text->SetTextColor(linesColor);
    		text->SetTextSize(0.0333);
		text->Draw();
	    }
	}
    }

    c->cd(2);
    if (HistDsmL1InputPatchSum) {
	HistDsmL1InputPatchSum->SetStats(0);
	HistDsmL1InputPatchSum->Draw("COLZ");
	Int_t dsmL0 = 0;
	Int_t ch = 0;
	for (Int_t idsmL1 = 0;idsmL1 < 6;idsmL1++) {
	    for (Int_t idsmL1ch = 0;idsmL1ch < 6;idsmL1ch++) {		
    		TLine *lDsmL0Begin = new TLine(ch-0.5, 0, ch-0.5, 1024);
    		if (lDsmL0Begin) {
		    lDsmL0Begin->SetLineColor(linesColor);
    		    lDsmL0Begin->SetLineWidth(1);
		    lDsmL0Begin->Draw();
		}
		Int_t crate = 0, crateSeq = 0;
		if (BEMCDecoderPresenter) {
		    Int_t triggerPatch = dsmL0 * 10;
		    BEMCDecoderPresenter->GetCrateAndSequenceFromTriggerPatch(triggerPatch, crate, crateSeq);
		}
        	TString label;
    		label = Form("%.2X", crate);
		TLatex *text = new TLatex(ch + 0.1-0.5, 900, label.Data());
		if (text) {
		    text->SetTextColor(linesColor);
    		    text->SetTextSize(0.0333);
		    text->Draw();
		}
		ch++;	
		if (idsmL1ch != 2) dsmL0++;
	    }
	}
    }
    if (mDebug >= 10) cout << __FILE__ << ":" << __LINE__ << endl;
}
//-------------------------------------------------------------------
void BEMCPlotsPresenter::displayL2Input(FileType file, TPad *pad, Int_t mDebug) {
    if (mDebug >= 10) cout << __FILE__ << ":" << __LINE__ << endl;

    TH2F *HistDsmL2InputHighTowerBits = (TH2F*)GetHisto(file, HistDsmL2InputHighTowerBitsName);
    if (!HistDsmL2InputHighTowerBits || (mDebug >= 2)) cout << "HistDsmL2InputHighTowerBits = " << HistDsmL2InputHighTowerBits << endl;
    TH2F *HistDsmL2InputPatchSumBits = (TH2F*)GetHisto(file, HistDsmL2InputPatchSumBitsName);
    if (!HistDsmL2InputPatchSumBits || (mDebug >= 2)) cout << "HistDsmL2InputPatchSumBits = " << HistDsmL2InputPatchSumBits << endl;
    TH2F *HistDsmL2InputPatchSum = (TH2F*)GetHisto(file, HistDsmL2InputPatchSumName);
    if (!HistDsmL2InputPatchSum || (mDebug >= 2)) cout << "HistDsmL2InputPatchSum = " << HistDsmL2InputPatchSum << endl;

    if (!pad) return;
    pad->Clear();
    pad->cd(0);
    
    TPad* c = new TPad("pad2", "apd2",0.0,0.1,1.,1.);
    c->Draw();
    c->cd(0);
    c->Divide(1, 3, 0.001, 0.001);
    
    // 12-19 are the grey shades dark-light
    Int_t linesColor = 16;
    c->cd(1);

    if (HistDsmL2InputHighTowerBits) {
	HistDsmL2InputHighTowerBits->SetStats(0);
	HistDsmL2InputHighTowerBits->Draw("COLZ");
	HistDsmL2InputHighTowerBits->GetXaxis()->SetNdivisions(12, false);
	HistDsmL2InputHighTowerBits->GetYaxis()->SetNdivisions(0, false);
	TLatex *textHT0 = new TLatex(-1.4, 0.5, "HT < th0");
	if (textHT0) {
	    textHT0->SetTextColor(linesColor);
    	    textHT0->SetTextSize(0.05);
	    textHT0->Draw();
	}
	TLatex *textHT1 = new TLatex(-1.4, 1.5, "th0 < HT < th1");
	if (textHT1) {
	    textHT1->SetTextColor(linesColor);
    	    textHT1->SetTextSize(0.05);
	    textHT1->Draw();
	}
	TLatex *textHT2 = new TLatex(-1.4, 2.5, "th1 < HT < th2");
	if (textHT2) {
	    textHT2->SetTextColor(linesColor);
    	    textHT2->SetTextSize(0.05);
	    textHT2->Draw();
	}
	TLatex *textHT3 = new TLatex(-1.4, 3.5, "th2 < HT");
	if (textHT3) {
	    textHT3->SetTextColor(linesColor);
    	    textHT3->SetTextSize(0.05);
	    textHT3->Draw();
	}
	TLatex *textDsmsL1 = new TLatex(-1.4, 4.5, "#splitline{DSM Level-1}{Boards}");
	if (textDsmsL1) {
	    textDsmsL1->SetTextColor(linesColor);
    	    textDsmsL1->SetTextSize(0.05);
	    textDsmsL1->Draw();
	}
	for (Int_t ch = 0;ch < 12;ch += 2) {
    	    TLine *lDsmL1Begin = new TLine(ch-0.5, 0, ch-0.5, 5);
    	    if (lDsmL1Begin) {
	        lDsmL1Begin->SetLineColor(linesColor);
    		lDsmL1Begin->SetLineWidth(1);
		lDsmL1Begin->Draw();
	    }
    	    TString label;
	    if (ch < 6) {
    		label = Form("BW1%.2u", ((ch / 2) % 3) + 1);
	    } else {
    	        label = Form("BE1%.2u", ((ch / 2) % 3) + 1);
	    }
	    TLatex *text = new TLatex(ch + 0.1-0.5, 4.5, label.Data());
	    if (text) {
		text->SetTextColor(linesColor);
    		text->SetTextSize(0.05);
		text->Draw();
	    }
	}
    }

    c->cd(2);
    if (HistDsmL2InputPatchSumBits) {
	HistDsmL2InputPatchSumBits->SetStats(0);
	HistDsmL2InputPatchSumBits->Draw("COLZ");
	HistDsmL2InputPatchSumBits->GetXaxis()->SetNdivisions(12, false);
	HistDsmL2InputPatchSumBits->GetYaxis()->SetNdivisions(0, false);
	TLatex *textHT0 = new TLatex(-1.4, 0.5, "Sum < th0");
	if (textHT0) {
	    textHT0->SetTextColor(linesColor);
    	    textHT0->SetTextSize(0.05);
	    textHT0->Draw();
	}
	TLatex *textHT1 = new TLatex(-1.4, 1.5, "th0 < Sum < th1");
	if (textHT1) {
	    textHT1->SetTextColor(linesColor);
    	    textHT1->SetTextSize(0.05);
	    textHT1->Draw();
	}
	TLatex *textHT2 = new TLatex(-1.4, 2.5, "th1 < Sum < th2");
	if (textHT2) {
	    textHT2->SetTextColor(linesColor);
    	    textHT2->SetTextSize(0.05);
	    textHT2->Draw();
	}
	TLatex *textHT3 = new TLatex(-1.4, 3.5, "th2 < Sum");
	if (textHT3) {
	    textHT3->SetTextColor(linesColor);
    	    textHT3->SetTextSize(0.05);
	    textHT3->Draw();
	}
	for (Int_t ch = 0;ch < 12;ch += 2) {
    	    TLine *lDsmL1Begin = new TLine(ch-0.5, 0, ch-0.5, 4);
    	    if (lDsmL1Begin) {
	        lDsmL1Begin->SetLineColor(linesColor);
    		lDsmL1Begin->SetLineWidth(1);
		lDsmL1Begin->Draw();
	    }
	}
    }

    c->cd(3);
    if (HistDsmL2InputPatchSum) {
	HistDsmL2InputPatchSum->SetStats(0);
	//HistDsmL2InputPatchSum->GetXaxis()->SetNdivisions(0, false);
	//HistDsmL2InputPatchSum->GetYaxis()->SetNdivisions(0, false);
	HistDsmL2InputPatchSum->Draw("COLZ");
	for (Int_t ch = 0;ch < 6;ch++) {
    	    TLine *lDsmL1Begin = new TLine(ch-0.5, 0, ch-0.5, 256);
    	    if (lDsmL1Begin) {
	        lDsmL1Begin->SetLineColor(linesColor);
    		lDsmL1Begin->SetLineWidth(1);
		lDsmL1Begin->Draw();
	    }
	}
    }

    if (mDebug >= 10) cout << __FILE__ << ":" << __LINE__ << endl;
}
//-------------------------------------------------------------------
void BEMCPlotsPresenter::displayL3Input(FileType file, TPad *pad, Int_t mDebug) {
    if (mDebug >= 10) cout << __FILE__ << ":" << __LINE__ << endl;

    TH1F *HistDsmL3InputHighTowerBits = (TH1F*)GetHisto(file, HistDsmL3InputHighTowerBitsName);
    if (!HistDsmL3InputHighTowerBits || (mDebug >= 2)) cout << "HistDsmL3InputHighTowerBits = " << HistDsmL3InputHighTowerBits << endl;
    TH1F *HistDsmL3InputPatchSumBits = (TH1F*)GetHisto(file, HistDsmL3InputPatchSumBitsName);
    if (!HistDsmL3InputPatchSumBits || (mDebug >= 2)) cout << "HistDsmL3InputPatchSumBits = " << HistDsmL3InputPatchSumBits << endl;
    TH1F *HistDsmL3InputBackToBackBit = (TH1F*)GetHisto(file, HistDsmL3InputBackToBackBitName);
    if (!HistDsmL3InputBackToBackBit || (mDebug >= 2)) cout << "HistDsmL3InputBackToBackBit = " << HistDsmL3InputBackToBackBit << endl;
    TH1F *HistDsmL3InputJPsiTopoBit = (TH1F*)GetHisto(file, HistDsmL3InputJPsiTopoBitName);
    if (!HistDsmL3InputJPsiTopoBit || (mDebug >= 2)) cout << "HistDsmL3InputJPsiTopoBit = " << HistDsmL3InputJPsiTopoBit << endl;
    TH1F *HistDsmL3InputJetPatchTopoBit = (TH1F*)GetHisto(file, HistDsmL3InputJetPatchTopoBitName);
    if (!HistDsmL3InputJetPatchTopoBit || (mDebug >= 2)) cout << "HistDsmL3InputJetPatchTopoBit = " << HistDsmL3InputJetPatchTopoBit << endl;

    if (!pad) return;
    pad->Clear();
    pad->cd(0);
    
    TPad* c = new TPad("pad2", "apd2",0.0,0.1,1.,1.);
    c->Draw();
    c->cd(0);
    c->Divide(2, 1, 0.001, 0.001);
    
    c->cd(1);

    if (HistDsmL3InputHighTowerBits) {
	//HistDsmL3InputHighTowerBits->SetStats(0);
	HistDsmL3InputHighTowerBits->Draw();
    }

    c->cd(2);

    if (HistDsmL3InputPatchSumBits) {
	//HistDsmL3InputPatchSumBits->SetStats(0);
	HistDsmL3InputPatchSumBits->Draw();
    }
	
}
//-------------------------------------------------------------------
void BEMCPlotsPresenter::displaySmdFeeSum(FileType file, TPad *pad, Int_t mDebug) {
    if (mDebug >= 10) cout << __FILE__ << ":" << __LINE__ << endl;

    TH2F *HistSmdFeeSum = (TH2F*)GetHisto(file, HistSmdFeeSumName);
    if (!HistSmdFeeSum || (mDebug >= 2)) cout << "HistSmdFeeSum = " << HistSmdFeeSum << endl;
    if (!pad) return;
    pad->Clear();
    pad->cd(0);
    
    // 12-19 are the grey shades dark-light
    Int_t linesColor = 16;
    TPad* c = new TPad("pad2", "apd2",0.0,0.1,1.,1.);
    c->Draw();
    c->cd();
    if (HistSmdFeeSum) {
	HistSmdFeeSum->SetStats(0);
	HistSmdFeeSum->Draw("COLZ");
	Int_t moduleRdo[120];
        if(BEMCDecoderPresenter) {
	    Int_t RDO, index;
            for (Int_t module = 1;module <= 120;module++) {
		moduleRdo[module - 1] = -1;
	        if (BEMCDecoderPresenter->GetSmdRDO(3, module, 1, 1, RDO, index)) {
		    moduleRdo[module - 1] = RDO;
	        }
//cout << "module " << module << ": RDO " << moduleRdo[module - 1] << endl;
	    }
	}
	Int_t curmod = 1;
	Int_t currdo = -1;
	Int_t beginrdo = -1;
	Int_t endrdo = -1;
	while (curmod <= 121) {
//cout << "curmod = " << curmod << ", currdo = " << currdo << ", beginrdo = " << beginrdo << ", endrdo = " << endrdo << endl;
	    if (((curmod <= 120) && (moduleRdo[curmod - 1] != currdo)) || (curmod == 121)) {
		if ((currdo == -1) && (curmod <= 120)) {
		    beginrdo = curmod;
		    currdo = moduleRdo[curmod - 1];
		    curmod++;
		} else {
		    endrdo = curmod - 1;
    		    TLine *lRdoBegin = new TLine(beginrdo - 0.5, HistSmdFeeSum->GetYaxis()->GetBinLowEdge(1), beginrdo - 0.5, HistSmdFeeSum->GetYaxis()->GetBinUpEdge(HistSmdFeeSum->GetYaxis()->GetNbins()));
    		    if (lRdoBegin) {
	    		lRdoBegin->SetLineColor(linesColor);
    		        lRdoBegin->SetLineWidth(1);
			lRdoBegin->Draw();
		    }
    		    TLine *lRdoEnd = new TLine(endrdo + 0.5, HistSmdFeeSum->GetYaxis()->GetBinLowEdge(1), endrdo + 0.5, HistSmdFeeSum->GetYaxis()->GetBinUpEdge(HistSmdFeeSum->GetYaxis()->GetNbins()));
    		    if (lRdoEnd) {
	    		lRdoEnd->SetLineColor(linesColor);
    		        lRdoEnd->SetLineWidth(1);
			lRdoEnd->Draw();
		    }
    		    TString label;
    		    label = Form("RDO %i", currdo);
		    TLatex *text = new TLatex(beginrdo + 1 -0.5, 0.8 * HistSmdFeeSum->GetYaxis()->GetBinUpEdge(HistSmdFeeSum->GetYaxis()->GetNbins()), label.Data());
		    if (text) {
			text->SetTextColor(linesColor);
    			text->SetTextSize(0.03);
			text->Draw();
		    }
		    currdo = -1;
		    if (curmod > 120) curmod++;
		}
	    } else {
		curmod++;
	    }
	}
    }
}	
//-------------------------------------------------------------------
void BEMCPlotsPresenter::displayPsdFeeSum(FileType file, TPad *pad, Int_t mDebug) {
    if (mDebug >= 10) cout << __FILE__ << ":" << __LINE__ << endl;

    TH2F *HistPsdFeeSum = (TH2F*)GetHisto(file, HistPsdFeeSumName);
    if (!HistPsdFeeSum || (mDebug >= 2)) cout << "HistPsdFeeSum = " << HistPsdFeeSum << endl;
    if (!pad) return;
    pad->Clear();
    pad->cd(0);
    
    // 12-19 are the grey shades dark-light
    Int_t linesColor = 16;
    TPad* c = new TPad("pad2", "apd2",0.0,0.1,1.,1.);
    c->Draw();
    c->cd();
    if (HistPsdFeeSum) {
	HistPsdFeeSum->SetStats(0);
	HistPsdFeeSum->Draw("COLZ");
	Int_t pmtRdo[60];
	for (Int_t box = 0;box < 60;box++) pmtRdo[box] = -1;
        if(BEMCDecoderPresenter) {
            for (Int_t rdo = 0;rdo < 4;rdo++) {
        	for (Int_t index = 0;index < 4800;index++) {
	    	    Int_t id, box, wire, Avalue;
		    if (BEMCDecoderPresenter->GetPsdId(rdo, index, id, box, wire, Avalue)) {
			if ((box >= 1) && (box <= 60)) {
			    pmtRdo[box - 1] = rdo + 8;
			}
		    }
	        }
	    }
	}
	Int_t curmod = 1;
	Int_t currdo = -1;
	Int_t beginrdo = -1;
	Int_t endrdo = -1;
	while (curmod <= 61) {
//cout << "curmod = " << curmod << ", currdo = " << currdo << ", beginrdo = " << beginrdo << ", endrdo = " << endrdo << endl;
	    if (((curmod <= 60) && (pmtRdo[curmod - 1] != currdo)) || (curmod == 61)) {
		if ((currdo == -1) && (curmod <= 60)) {
		    beginrdo = curmod;
		    currdo = pmtRdo[curmod - 1];
		    curmod++;
		} else {
		    endrdo = curmod - 1;
    		    TLine *lRdoBegin = new TLine(beginrdo - 0.5, HistPsdFeeSum->GetYaxis()->GetBinLowEdge(1), beginrdo - 0.5, HistPsdFeeSum->GetYaxis()->GetBinUpEdge(HistPsdFeeSum->GetYaxis()->GetNbins()));
    		    if (lRdoBegin) {
	    		lRdoBegin->SetLineColor(linesColor);
    		        lRdoBegin->SetLineWidth(1);
			lRdoBegin->Draw();
		    }
    		    TLine *lRdoEnd = new TLine(endrdo + 0.5, HistPsdFeeSum->GetYaxis()->GetBinLowEdge(1), endrdo + 0.5, HistPsdFeeSum->GetYaxis()->GetBinUpEdge(HistPsdFeeSum->GetYaxis()->GetNbins()));
    		    if (lRdoEnd) {
	    		lRdoEnd->SetLineColor(linesColor);
    		        lRdoEnd->SetLineWidth(1);
			lRdoEnd->Draw();
		    }
    		    TString label;
    		    label = Form("RDO %i", currdo);
		    TLatex *text = new TLatex(beginrdo + 0.5 -0.5, 0.8 * HistPsdFeeSum->GetYaxis()->GetBinUpEdge(HistPsdFeeSum->GetYaxis()->GetNbins()), label.Data());
		    if (text) {
			text->SetTextColor(linesColor);
    			text->SetTextSize(0.03);
			text->Draw();
		    }
		    currdo = -1;
		    if (curmod > 60) curmod++;
		}
	    } else {
		curmod++;
	    }
	}
    }
}	
//-------------------------------------------------------------------
void BEMCPlotsPresenter::displayTriggerCorruption(FileType file, TPad *pad, Int_t mDebug) {
    if (mDebug >= 10) cout << __FILE__ << ":" << __LINE__ << endl;

    TH1F *HistTriggerCorruptionHighTower = (TH1F*)GetHisto(file, HistTriggerCorruptionHighTowerName);
    if (!HistTriggerCorruptionHighTower || (mDebug >= 2)) cout << "HistTriggerCorruptionHighTower = " << HistTriggerCorruptionHighTower << endl;
    TH1F *HistTriggerCorruptionPatchSum = (TH1F*)GetHisto(file, HistTriggerCorruptionPatchSumName);
    if (!HistTriggerCorruptionPatchSum || (mDebug >= 2)) cout << "HistTriggerCorruptionPatchSum = " << HistTriggerCorruptionPatchSum << endl;
    TH2F *HistTriggerCorruptionHighTowerCorr = (TH2F*)GetHisto(file, HistTriggerCorruptionHighTowerCorrName);
    if (!HistTriggerCorruptionHighTowerCorr || (mDebug >= 2)) cout << "HistTriggerCorruptionHighTowerCorr = " << HistTriggerCorruptionHighTowerCorr << endl;
    TH2F *HistTriggerCorruptionPatchSumCorr = (TH2F*)GetHisto(file, HistTriggerCorruptionPatchSumCorrName);
    if (!HistTriggerCorruptionPatchSumCorr || (mDebug >= 2)) cout << "HistTriggerCorruptionPatchSumCorr = " << HistTriggerCorruptionPatchSumCorr << endl;

    if (!pad) return;
    pad->Clear();
    pad->cd(0);
    
    // 12-19 are the grey shades dark-light
    //Int_t linesColor = 16;
    TPad* c = new TPad("pad2", "apd2",0.0,0.1,1.,1.);
    c->Draw();
    c->cd(0);
    c->Divide(2, 2, 0.001, 0.001);
    
    c->cd(1);
    if (HistTriggerCorruptionHighTower) {
	HistTriggerCorruptionHighTower->SetStats(0);
	HistTriggerCorruptionHighTower->Draw();
    }
    c->cd(2);
    if (HistTriggerCorruptionHighTowerCorr) {
	HistTriggerCorruptionHighTowerCorr->SetStats(0);
	HistTriggerCorruptionHighTowerCorr->Draw("COLZ");
    }
    c->cd(3);
    if (HistTriggerCorruptionPatchSum) {
	HistTriggerCorruptionPatchSum->SetStats(0);
	HistTriggerCorruptionPatchSum->Draw();
    }
    c->cd(4);
    if (HistTriggerCorruptionPatchSumCorr) {
	HistTriggerCorruptionPatchSumCorr->SetStats(0);
	HistTriggerCorruptionPatchSumCorr->Draw("COLZ");
    }
}
//-------------------------------------------------------------------
void BEMCPlotsPresenter::displayTab(Int_t tab, Int_t panel, FileType file, TPad *pad, Int_t mDebug) {
//mDebug = 10;
    if (mDebug >= 10) cout << __FILE__ << ":" << __LINE__ << endl;
    if (BEMCPlotsCleanUpHistoList) {
	BEMCPlotsCleanUpHistoList->Delete();
    }
    if (!BEMCDecoderPresenter) BEMCDecoderPresenter = new StEmcDecoder();
    if (mDebug >= 2) cout << "tab = " << tab << endl;
    if (mDebug >= 2) cout << "panel = " << panel << endl;
    if (!file.file() || (mDebug >= 2)) cout << "file = " << file.file() << endl;
    if (!pad || (mDebug >= 2)) cout << "pad = " << pad << endl;
    if (!BEMCDecoderPresenter || (mDebug >= 2)) cout << "BEMCDecoderPresenter = " << BEMCDecoderPresenter << endl;

    if (tab == 0) {
	if (panel == 0) {
	    displayRawAdc(file, pad, false, mDebug);
	} else if (panel == 1) {
	    displayJetPatchHT(file, pad, mDebug);
	} else if (panel == 2) {
	    displayJetPatchSum(file, pad, mDebug);
	} else if (panel == 3) {
	    displayL0Input(file, pad, mDebug);
	} else if (panel == 4) {
	    displayL1Input(file, pad, mDebug);
	} else if (panel == 5) {
	    displayL2Input(file, pad, mDebug);
	} else if (panel == 6) {
	    displaySmdFeeSum(file, pad, mDebug);
	} else if (panel == 7) {
	    displayTriggerCorruption(file, pad, mDebug);
	} else if (panel == 8) {
	    displayPsdFeeSum(file, pad, mDebug);
	} else if (panel == 9) {
	    displayRawAdc(file, pad, true, mDebug);
	}
    }
    if (mDebug >= 10) cout << __FILE__ << ":" << __LINE__ << endl;
}
//-------------------------------------------------------------------
