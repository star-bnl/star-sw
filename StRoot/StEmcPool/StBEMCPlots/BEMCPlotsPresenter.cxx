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
#include <TEnv.h>
#include <TObjArray.h>

#include <StMessMgr.h>
#include <GenericFile.h>

#include <StEmcUtil/database/StEmcDecoder.h>

#include "BEMCPlotsNames.h"
#include "StRoot/StEmcPool/StBEMCPlots/BemcTwMask.h"

#define min(A, B) (((A) < (B)) ? (A) : (B))
#define max(A, B) (((A) > (B)) ? (A) : (B))

TObjArray *BEMCPlotsCleanUp = new TObjArray();
StEmcDecoder *BEMCDecoderPresenter = 0;
Bool_t useDecoderForBoundaries = false; // Use StEmcDecoder for showing ranges (the tower map bug fix mixes softIds between crates)

//-------------------------------------------------------------------
TH1 *GetHisto(FileType &fd, const char *name) {
    TH1 *hist = 0;
    hist = fd.file() ? (TH1 *)fd.Get(name, 0) : 0;
    if (hist) {
	hist->SetDirectory(0);
        if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(hist);
    }
    return hist;
}

#define FIND_HISTO(TYPE, NAME, FILE, TITLE) \
TYPE *NAME = (TYPE*)GetHisto((FILE), (TITLE)); \
if (!NAME) { \
    LOG_ERROR << "Histogram " << #NAME << " not found: " << (TITLE) << endm; \
}

//-------------------------------------------------------------------
void BEMCPlotsPresenter::displayStatus(FileType file, TPad *pad, int mDebug) {
    if (mDebug >= 10) cout << __FILE__ << ":" << __LINE__ << endl;

    if (!pad) return;
    pad->Clear();
    pad->cd(0);

    FIND_HISTO(TH2F, Hist_TDC_status, file, Hist_TDC_statusName);
    FIND_HISTO(TH2F, Hist_SMD_status, file, Hist_SMD_statusName);
    FIND_HISTO(TH2F, Hist_PSD_status, file, Hist_PSD_statusName);
    FIND_HISTO(TH1F, Hist_BTOW_Corruption, file, Hist_BTOW_CorruptionName);
    
    TPad* c = new TPad("pad2", "apd2",0.0,0.1,1.,1.);
    c->Draw();
    c->cd(0);
    c->Divide(2, 2, 0.001, 0.001);

    c->cd(1);
    if (Hist_TDC_status) {
        Hist_TDC_status->Draw("H COLZ");
    }
    c->cd(2);
    if (Hist_SMD_status) {
        Hist_SMD_status->Draw("H COLZ");
    }
    c->cd(3);
    if (Hist_PSD_status) {
        Hist_PSD_status->Draw("H COLZ");
    }
    c->cd(4);
    if (Hist_BTOW_Corruption) {
        Hist_BTOW_Corruption->Draw("H COLZ");
    }
    
    if (mDebug >= 10) cout << __FILE__ << ":" << __LINE__ << endl;
}

//-------------------------------------------------------------------
void BEMCPlotsPresenter::displayTowers(FileType file, TPad *pad, int mDebug) {
    if (mDebug >= 10) cout << __FILE__ << ":" << __LINE__ << endl;

    if (!pad) return;
    pad->Clear();
    pad->cd(0);

    FIND_HISTO(TH2F, Hist_btow_spectra_1, file, Hist_btow_spectra_1Name);
    FIND_HISTO(TH2F, Hist_btow_spectra_2, file, Hist_btow_spectra_2Name);
    FIND_HISTO(TH2F, Hist_btow_spectra_3, file, Hist_btow_spectra_3Name);
    
    TPad* c = new TPad("pad2", "apd2",0.0,0.1,1.,1.);
    c->Draw();
    c->cd(0);
    c->Divide(1, 3, 0.001, 0.001);

    c->cd(1);
    if (Hist_btow_spectra_1) {
        Hist_btow_spectra_1->Draw("H COLZ");
    }
    c->cd(2);
    if (Hist_btow_spectra_2) {
        Hist_btow_spectra_2->Draw("H COLZ");
    }
    c->cd(3);
    if (Hist_btow_spectra_3) {
        Hist_btow_spectra_3->Draw("H COLZ");
    }
    
    if (mDebug >= 10) cout << __FILE__ << ":" << __LINE__ << endl;
}

//-------------------------------------------------------------------
void BEMCPlotsPresenter::displaySMDPSD(FileType file, TPad *pad, int mDebug) {
    if (mDebug >= 10) cout << __FILE__ << ":" << __LINE__ << endl;

    if (!pad) return;
    pad->Clear();
    pad->cd(0);

    FIND_HISTO(TH1F, Hist_smd_spectra, file, Hist_smd_spectraName);
    FIND_HISTO(TH1F, Hist_smd_spectraNonZS, file, Hist_smd_spectraNonZSName);
    FIND_HISTO(TH2F, Hist_smd_capacitor, file, Hist_smd_capacitorName);
    FIND_HISTO(TH2F, Hist_smd_sum, file, Hist_smd_sumName);
    FIND_HISTO(TH1F, Hist_psd_spectra, file, Hist_psd_spectraName);
    FIND_HISTO(TH1F, Hist_psd_spectraNonZS, file, Hist_psd_spectraNonZSName);
    FIND_HISTO(TH2F, Hist_psd_capacitor, file, Hist_psd_capacitorName);
    FIND_HISTO(TH2F, Hist_psd_sum, file, Hist_psd_sumName);
    
    TPad* c = new TPad("pad2", "apd2",0.0,0.1,1.,1.);
    c->Draw();
    c->cd(0);
    c->Divide(2, 3, 0.001, 0.001);

    c->cd(1);
    if (Hist_smd_spectra) {
        Hist_smd_spectra->Draw("H COLZ");
    }
    if (Hist_smd_spectraNonZS) {
        Hist_smd_spectraNonZS->SetLineColor(kRed);
        Hist_smd_spectraNonZS->Draw("SAME H COLZ");
    }
    c->cd(2);
    if (Hist_psd_spectra) {
        Hist_psd_spectra->Draw("H COLZ");
    }
    if (Hist_psd_spectraNonZS) {
        Hist_psd_spectraNonZS->SetLineColor(kRed);
        Hist_psd_spectraNonZS->Draw("SAME H COLZ");
    }
    c->cd(3);
    if (Hist_smd_capacitor) {
        Hist_smd_capacitor->Draw("H COLZ");
    }
    c->cd(4);
    if (Hist_psd_capacitor) {
        Hist_psd_capacitor->Draw("H COLZ");
    }
    c->cd(5);
    if (Hist_smd_sum) {
	Hist_smd_sum->Draw("H COLZ");
    }
    c->cd(6);
    if (Hist_psd_sum) {
        Hist_psd_sum->Draw("H COLZ");
    }
    
    if (mDebug >= 10) cout << __FILE__ << ":" << __LINE__ << endl;
}

//-------------------------------------------------------------------
void BEMCPlotsPresenter::displayTrigger(FileType file, TPad *pad, int mDebug) {
    if (mDebug >= 10) cout << __FILE__ << ":" << __LINE__ << endl;

    if (!pad) return;
    pad->Clear();
    pad->cd(0);

    FIND_HISTO(TH2F, Hist_HTMAX_spectra, file, Hist_HTMAX_spectraName);
    FIND_HISTO(TH2F, Hist_PAMAX_spectra, file, Hist_PAMAX_spectraName);
    FIND_HISTO(TH1F, Hist_HTMAX_dist, file, Hist_HTMAX_distName);
    FIND_HISTO(TH1F, Hist_PAMAX_dist, file, Hist_PAMAX_distName);
    
    TPad* c = new TPad("pad2", "apd2",0.0,0.1,1.,1.);
    c->Draw();
    c->cd(0);
    c->Divide(2, 2, 0.001, 0.001);

    c->cd(1);
    if (Hist_HTMAX_spectra) {
        Hist_HTMAX_spectra->Draw("H COLZ");
    }
    c->cd(2);
    if (Hist_PAMAX_spectra) {
        Hist_PAMAX_spectra->Draw("H COLZ");
    }
    c->cd(3);
    if (Hist_HTMAX_dist) {
        Hist_HTMAX_dist->Draw("H COLZ");
    }
    c->cd(4);
    if (Hist_PAMAX_dist) {
        Hist_PAMAX_dist->Draw("H COLZ");
    }
    
    if (mDebug >= 10) cout << __FILE__ << ":" << __LINE__ << endl;
}

//-------------------------------------------------------------------
void BEMCPlotsPresenter::displayJet(FileType file, TPad *pad, int mDebug) {
    if (mDebug >= 10) cout << __FILE__ << ":" << __LINE__ << endl;

    if (!pad) return;
    pad->Clear();
    pad->cd(0);

    FIND_HISTO(TH2F, Hist_JET_ped, file, Hist_JET_pedName);
    FIND_HISTO(TH2F, Hist_JET_spectra, file, Hist_JET_spectraName);
    FIND_HISTO(TH2F, Hist_JETMAX_spectra, file, Hist_JETMAX_spectraName);
    FIND_HISTO(TH1F, Hist_JETMAX_dist, file, Hist_JETMAX_distName);
    
    TPad* c = new TPad("pad2", "apd2",0.0,0.1,1.,1.);
    c->Draw();
    c->cd(0);
    c->Divide(2, 2, 0.001, 0.001);

    c->cd(1);
    if (Hist_JET_ped) {
        Hist_JET_ped->Draw("H COLZ");
    }
    c->cd(2);
    if (Hist_JET_spectra) {
        Hist_JET_spectra->Draw("H COLZ");
    }
    c->cd(3);
    if (Hist_JETMAX_spectra) {
        Hist_JETMAX_spectra->Draw("H COLZ");
    }
    c->cd(4);
    if (Hist_JETMAX_dist) {
        Hist_JETMAX_dist->Draw("H COLZ");
    }
    
    if (mDebug >= 10) cout << __FILE__ << ":" << __LINE__ << endl;
}

//-------------------------------------------------------------------
void BEMCPlotsPresenter::displayRawAdc(FileType file, TPad *pad, bool psd, bool zoom, BemcTwMask *twMask, Int_t mDebug) {
    if (mDebug >= 10) cout << __FILE__ << ":" << __LINE__ << endl;

    FIND_HISTO(TH2F, HistRawAdc1, file, psd ? HistRawAdcPsd1Name : (zoom ? HistRawAdc1NameZoom : HistRawAdc1Name));
    FIND_HISTO(TH2F, HistRawAdc2, file, psd ? HistRawAdcPsd2Name : (zoom ? HistRawAdc2NameZoom : HistRawAdc2Name));
    FIND_HISTO(TH2F, HistRawAdc3, file, psd ? HistRawAdcPsd3Name : (zoom ? HistRawAdc3NameZoom : HistRawAdc3Name));
    FIND_HISTO(TH2F, HistRawAdc4, file, psd ? HistRawAdcPsd4Name : (zoom ? HistRawAdc4NameZoom : HistRawAdc4Name));

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
	if (twMask) {
	  TGraph &g1 = twMask->crG[0];
	  if (g1.GetN() > 0) {
	    g1.Draw("L");
	    //g1.SetLineWidth(1);
	    g1.SetLineColor(2);
	  }
	  /*TLine *testline1=new TLine(0,0,0,10000);
	  if (testline1) {
	    if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(testline1);
	    testline1->SetLineColor(1);
	    testline1->SetLineWidth(2);
	    testline1->Draw();
	  }*/
	}
	for (Int_t crate = 0;crate < numBoxes;crate++) {
	    if (!((boxMaxSoftId[crate] < HistRawAdc1->GetXaxis()->GetBinLowEdge(1))
		|| (boxMinSoftId[crate] > HistRawAdc1->GetXaxis()->GetBinUpEdge(HistRawAdc1->GetXaxis()->GetNbins())))
		) {
		Float_t left = max(boxMinSoftId[crate], HistRawAdc1->GetXaxis()->GetBinCenter(1)) - 0.5;
		Float_t right = 0.5 + min(boxMaxSoftId[crate], HistRawAdc1->GetXaxis()->GetBinCenter(HistRawAdc1->GetXaxis()->GetNbins()));
		TLine *lCrates = new TLine(left, HistRawAdc1->GetYaxis()->GetBinLowEdge(1), left, HistRawAdc1->GetYaxis()->GetBinUpEdge(HistRawAdc1->GetYaxis()->GetNbins()));
		if (lCrates) {
		    if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(lCrates);
    		    lCrates->SetLineColor(linesColor); 
		    lCrates->SetLineWidth(1);
		    lCrates->Draw();
		}
		TLine *rCrates = new TLine(right, HistRawAdc1->GetYaxis()->GetBinLowEdge(1), right, HistRawAdc1->GetYaxis()->GetBinUpEdge(HistRawAdc1->GetYaxis()->GetNbins()));
		if (rCrates) {
		    if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(rCrates);
    		    rCrates->SetLineColor(linesColor); 
		    rCrates->SetLineWidth(1);
		    rCrates->Draw();
		}
    		TString crateLabel;
    		crateLabel = Form(boxLabelFormat, crate + 1);
		TLatex *textCrate = new TLatex(left + 10, HistRawAdc1->GetYaxis()->GetBinUpEdge(HistRawAdc1->GetYaxis()->GetNbins()) * 0.8, crateLabel.Data());
		if (textCrate) {
		    if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(textCrate);
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
	if (twMask) {
	  TGraph &g2 = twMask->crG[1];
	  if (g2.GetN() > 0) {
	    g2.Draw("L");
	    g2.SetLineColor(2);
	  }
	}
	for (Int_t crate = 0;crate < numBoxes;crate++) {
	  if (!((boxMaxSoftId[crate] < HistRawAdc2->GetXaxis()->GetBinLowEdge(1))
		|| (boxMinSoftId[crate] > HistRawAdc2->GetXaxis()->GetBinUpEdge(HistRawAdc2->GetXaxis()->GetNbins())))
	      ) {
	    Float_t left = max(boxMinSoftId[crate], HistRawAdc2->GetXaxis()->GetBinCenter(1)) - 0.5;
	    Float_t right = 0.5 + min(boxMaxSoftId[crate], HistRawAdc2->GetXaxis()->GetBinCenter(HistRawAdc2->GetXaxis()->GetNbins()));
	    TLine *lCrates = new TLine(left, HistRawAdc2->GetYaxis()->GetBinLowEdge(1), left, HistRawAdc2->GetYaxis()->GetBinUpEdge(HistRawAdc2->GetYaxis()->GetNbins()));
	    if (lCrates) {
	      if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(lCrates);
	      lCrates->SetLineColor(linesColor); 
	      lCrates->SetLineWidth(1);
	      lCrates->Draw();
	    }
	    TLine *rCrates = new TLine(right, HistRawAdc2->GetYaxis()->GetBinLowEdge(1), right, HistRawAdc2->GetYaxis()->GetBinUpEdge(HistRawAdc2->GetYaxis()->GetNbins()));
	    if (rCrates) {
	      if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(rCrates);
	      rCrates->SetLineColor(linesColor); 
	      rCrates->SetLineWidth(1);
	      rCrates->Draw();
	    }
	    TString crateLabel;
	    crateLabel = Form(boxLabelFormat, crate + 1);
	    TLatex *textCrate = new TLatex(left + 10, HistRawAdc2->GetYaxis()->GetBinUpEdge(HistRawAdc2->GetYaxis()->GetNbins()) * 0.8, crateLabel.Data());
	    if (textCrate) {
	      if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(textCrate);
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
	if (twMask) {
	  TGraph &g3 = twMask->crG[2];
	  if (g3.GetN() > 0) {
	    g3.Draw("L");
	    g3.SetLineColor(2);
	  }
	}
	for (Int_t crate = 0;crate < numBoxes;crate++) {
	    if (!((boxMaxSoftId[crate] < HistRawAdc3->GetXaxis()->GetBinLowEdge(1))
		|| (boxMinSoftId[crate] > HistRawAdc3->GetXaxis()->GetBinUpEdge(HistRawAdc3->GetXaxis()->GetNbins())))
		) {
		Float_t left = max(boxMinSoftId[crate], HistRawAdc3->GetXaxis()->GetBinCenter(1)) - 0.5;
		Float_t right = 0.5 + min(boxMaxSoftId[crate], HistRawAdc3->GetXaxis()->GetBinCenter(HistRawAdc3->GetXaxis()->GetNbins()));
		TLine *lCrates = new TLine(left, HistRawAdc3->GetYaxis()->GetBinLowEdge(1), left, HistRawAdc3->GetYaxis()->GetBinUpEdge(HistRawAdc3->GetYaxis()->GetNbins()));
		if (lCrates) {
		    if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(lCrates);
    		    lCrates->SetLineColor(linesColor); 
		    lCrates->SetLineWidth(1);
		    lCrates->Draw();
		}
		TLine *rCrates = new TLine(right, HistRawAdc3->GetYaxis()->GetBinLowEdge(1), right, HistRawAdc3->GetYaxis()->GetBinUpEdge(HistRawAdc3->GetYaxis()->GetNbins()));
		if (rCrates) {
		    if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(rCrates);
    		    rCrates->SetLineColor(linesColor); 
		    rCrates->SetLineWidth(1);
		    rCrates->Draw();
		}
    		TString crateLabel;
    		crateLabel = Form(boxLabelFormat, crate + 1);
		TLatex *textCrate = new TLatex(left + 10, HistRawAdc3->GetYaxis()->GetBinUpEdge(HistRawAdc3->GetYaxis()->GetNbins()) * 0.8, crateLabel.Data());
		if (textCrate) {
		    if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(textCrate);
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
	if (twMask) {
	  TGraph &g4 = twMask->crG[3];
	  if (g4.GetN() > 0) {
	    g4.Draw("L");
	    //g4.SetLineWidth(2);
	    g4.SetLineColor(2);
	  }
	}
	for (Int_t crate = 0;crate < numBoxes;crate++) {
	    if (!((boxMaxSoftId[crate] < HistRawAdc4->GetXaxis()->GetBinLowEdge(1))
		|| (boxMinSoftId[crate] > HistRawAdc4->GetXaxis()->GetBinUpEdge(HistRawAdc4->GetXaxis()->GetNbins())))
		) {
		Float_t left = max(boxMinSoftId[crate], HistRawAdc4->GetXaxis()->GetBinCenter(1)) - 0.5;
		Float_t right = 0.5 + min(boxMaxSoftId[crate], HistRawAdc4->GetXaxis()->GetBinCenter(HistRawAdc4->GetXaxis()->GetNbins()));
		TLine *lCrates = new TLine(left, HistRawAdc4->GetYaxis()->GetBinLowEdge(1), left, HistRawAdc4->GetYaxis()->GetBinUpEdge(HistRawAdc4->GetYaxis()->GetNbins()));
		if (lCrates) {
		    if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(lCrates);
    		    lCrates->SetLineColor(linesColor); 
		    lCrates->SetLineWidth(1);
		    lCrates->Draw();
		}
		TLine *rCrates = new TLine(right, HistRawAdc4->GetYaxis()->GetBinLowEdge(1), right, HistRawAdc4->GetYaxis()->GetBinUpEdge(HistRawAdc4->GetYaxis()->GetNbins()));
		if (rCrates) {
		    if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(rCrates);
    		    rCrates->SetLineColor(linesColor); 
		    rCrates->SetLineWidth(1);
		    rCrates->Draw();
		}
    		TString crateLabel;
    		crateLabel = Form(boxLabelFormat, crate + 1);
		TLatex *textCrate = new TLatex(left + 10, HistRawAdc4->GetYaxis()->GetBinUpEdge(HistRawAdc4->GetYaxis()->GetNbins()) * 0.8, crateLabel.Data());
		if (textCrate) {
		    if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(textCrate);
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

    if (!pad) return;
    pad->Clear();
    pad->cd(0);
    
    TPad* c = new TPad("pad2", "apd2",0.0,0.1,1.,1.);
    c->Draw();
    c->cd(0);
    c->Divide(3, 4, 0.001, 0.001);
    
    for (Int_t jetPatch = 0;jetPatch < 12;jetPatch++) {
	c->cd(jetPatch+1);
        FIND_HISTO(TH1F, HistHighTowerSpectrum, file, Form(HistHighTowerSpectrumName "_%u", jetPatch));
	if (HistHighTowerSpectrum) {
	    HistHighTowerSpectrum->SetStats(0);
	    HistHighTowerSpectrum->Draw();
	    if (HistHighTowerSpectrum->GetEntries()) gPad->SetLogy();
	}
    }
}
//-------------------------------------------------------------------
void BEMCPlotsPresenter::displayJetPatchSum(FileType file, TPad *pad, Int_t mDebug) {
    if (mDebug >= 10) cout << __FILE__ << ":" << __LINE__ << endl;

    if (!pad) return;
    pad->Clear();
    pad->cd(0);
    
    TPad* c = new TPad("pad2", "apd2",0.0,0.1,1.,1.);
    c->Draw();
    c->cd(0);
    c->Divide(3, 4, 0.001, 0.001);
    
    for (Int_t jetPatch = 0;jetPatch < 12;jetPatch++) {
	c->cd(jetPatch+1);
	FIND_HISTO(TH1F, HistPatchSumSpectrum, file, Form(HistPatchSumSpectrumName "_%u", jetPatch));
	if (HistPatchSumSpectrum) {
	    HistPatchSumSpectrum->SetStats(0);
	    HistPatchSumSpectrum->Draw();
	    if (HistPatchSumSpectrum->GetEntries()) gPad->SetLogy();
	}
    }
}
//-------------------------------------------------------------------
void BEMCPlotsPresenter::displayL0Input(FileType file, TPad *pad, Int_t mDebug) {
    if (mDebug >= 10) cout << __FILE__ << ":" << __LINE__ << endl;

    FIND_HISTO(TH2F, HistDsmL0InputHighTower, file, HistDsmL0InputHighTowerName);
    FIND_HISTO(TH2F, HistDsmL0InputPatchSum, file, HistDsmL0InputPatchSumName);

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
	    if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(lCrates);
    	    lCrates->SetLineColor(linesColor); 
	    lCrates->SetLineWidth(1);
	    lCrates->Draw();
	}
	TLatex *textCrates = new TLatex(1, 50, "BTOW Crates:");
	if (textCrates) {
	    if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(textCrates);
	    textCrates->SetTextColor(linesColor);
    	    textCrates->SetTextSize(0.0333);
	    textCrates->Draw();
	}
	for (Int_t icrate = 1;icrate <= 30;icrate++) {
	    Int_t triggerPatchBegin, triggerPatchEnd;
	    if (BEMCDecoderPresenter->GetTriggerPatchFromCrate(icrate, 0, triggerPatchBegin) && BEMCDecoderPresenter->GetTriggerPatchFromCrate(icrate, 159, triggerPatchEnd)) {
    		TLine *lCrateBegin = new TLine(triggerPatchBegin-0.5, 0, triggerPatchBegin-0.5, 50);
    		if (lCrateBegin) {
		    if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(lCrateBegin);
		    lCrateBegin->SetLineColor(linesColor);
    		    lCrateBegin->SetLineWidth(1);
		    lCrateBegin->Draw();
		}
    		TString crateLabel;
    		crateLabel = Form("%.2X", icrate);
		TLatex *textCrate = new TLatex(triggerPatchBegin + 1, 45, crateLabel.Data());
		if (textCrate) {
		    if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(textCrate);
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
		if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(arrow);
		arrow->SetLineColor(linesColor);
		arrow->SetFillColor(linesColor);
		arrow->Draw();
	    }
    	    TString jpLabel;
    	    jpLabel = Form("JetPatch %u", ijp);
	    TLatex *text = new TLatex(minTriggerPatch + 1, height + 1, jpLabel.Data());
	    if (text) {
		if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(text);
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
		    if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(lCrateBegin);
		    lCrateBegin->SetLineColor(linesColor);
    		    lCrateBegin->SetLineWidth(1);
		    lCrateBegin->Draw();
		}
    		TString crateLabel;
    		crateLabel = Form("%.2X", icrate);
		TLatex *textCrate = new TLatex(triggerPatchBegin + 1, 60, crateLabel.Data());
		if (textCrate) {
		    if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(textCrate);
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

    FIND_HISTO(TH2F, HistDsmL1InputHighTowerBits, file, HistDsmL1InputHighTowerBitsName);
    FIND_HISTO(TH2F, HistDsmL1InputPatchSum, file, HistDsmL1InputPatchSumName);

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
	    if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(textHT0);
	    textHT0->SetTextColor(linesColor);
    	    textHT0->SetTextSize(0.0333);
	    textHT0->Draw();
	}
	TLatex *textHT1 = new TLatex(-3.5, 1.5, "th0 < HT < th1");
	if (textHT1) {
	    if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(textHT1);
	    textHT1->SetTextColor(linesColor);
    	    textHT1->SetTextSize(0.0333);
	    textHT1->Draw();
	}
	TLatex *textHT2 = new TLatex(-3.5, 2.5, "th1 < HT < th2");
	if (textHT2) {
	    if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(textHT2);
	    textHT2->SetTextColor(linesColor);
    	    textHT2->SetTextSize(0.0333);
	    textHT2->Draw();
	}
	TLatex *textHT3 = new TLatex(-3.5, 3.5, "th2 < HT");
	if (textHT3) {
	    if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(textHT3);
	    textHT3->SetTextColor(linesColor);
    	    textHT3->SetTextSize(0.0333);
	    textHT3->Draw();
	}
	TLatex *textDsmsL0 = new TLatex(-3.5, 4.0, "#splitline{#splitline{DSM Level-0}{Boards}}{BTOW Crates}");
	if (textDsmsL0) {
	    if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(textDsmsL0);
	    textDsmsL0->SetTextColor(linesColor);
    	    textDsmsL0->SetTextSize(0.0333);
	    textDsmsL0->Draw();
	}
	/*TLatex *textCrates = new TLatex(-3.5, 4.45, "BTOW Crates");
	if (textCrates) {
	    if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(textCrates);
	    textCrates->SetTextColor(linesColor);
    	    textCrates->SetTextSize(0.0333);
	    textCrates->Draw();
	}*/
	/*TLine *lDsms = new TLine(0, 4.4, 36, 4.4);
	if (lDsms) {
	    if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(lDsms);
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
		    if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(lDsmL0Begin);
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
		    if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(text);
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
		    if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(textCrate);
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
		if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(arrow);
		arrow->SetLineColor(linesColor);
		arrow->SetFillColor(linesColor);
		arrow->Draw();
	    }
    	    TString jpLabel;
    	    jpLabel = Form("JetPatch %u", ijp);
	    TLatex *text = new TLatex(minCh + 0.1-0.5, height + 0.1, jpLabel.Data());
	    if (text) {
		if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(text);
		text->SetTextColor(linesColor);
    		text->SetTextSize(0.0333);
		text->Draw();
	    }
	}
	TLatex *textDsmsL1 = new TLatex(-3.5, -0.4, "#splitline{DSM Level-1}{Boards}");
	if (textDsmsL1) {
	    if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(textDsmsL1);
	    textDsmsL1->SetTextColor(linesColor);
    	    textDsmsL1->SetTextSize(0.0333);
	    textDsmsL1->Draw();
	}
	for (Int_t idsmL1 = 0;idsmL1 < 6;idsmL1++) {
	    TArrow *arrow = new TArrow((idsmL1 * 6)-0.5, -0.4, (idsmL1 * 6) + 6-0.5, -0.4, 0.02, "<>");
	    if (arrow) {
		if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(arrow);
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
		if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(text);
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
		    if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(lDsmL0Begin);
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
		    if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(text);
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

    FIND_HISTO(TH2F, HistDsmL2InputHighTowerBits, file, HistDsmL2InputHighTowerBitsName);
    FIND_HISTO(TH2F, HistDsmL2InputPatchSumBits, file, HistDsmL2InputPatchSumBitsName);
    FIND_HISTO(TH2F, HistDsmL2InputPatchSum, file, HistDsmL2InputPatchSumName);

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
	    if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(textHT0);
	    textHT0->SetTextColor(linesColor);
    	    textHT0->SetTextSize(0.05);
	    textHT0->Draw();
	}
	TLatex *textHT1 = new TLatex(-1.4, 1.5, "th0 < HT < th1");
	if (textHT1) {
	    if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(textHT1);
	    textHT1->SetTextColor(linesColor);
    	    textHT1->SetTextSize(0.05);
	    textHT1->Draw();
	}
	TLatex *textHT2 = new TLatex(-1.4, 2.5, "th1 < HT < th2");
	if (textHT2) {
	    if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(textHT2);
	    textHT2->SetTextColor(linesColor);
    	    textHT2->SetTextSize(0.05);
	    textHT2->Draw();
	}
	TLatex *textHT3 = new TLatex(-1.4, 3.5, "th2 < HT");
	if (textHT3) {
	    if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(textHT3);
	    textHT3->SetTextColor(linesColor);
    	    textHT3->SetTextSize(0.05);
	    textHT3->Draw();
	}
	TLatex *textDsmsL1 = new TLatex(-1.4, 4.5, "#splitline{DSM Level-1}{Boards}");
	if (textDsmsL1) {
	    if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(textDsmsL1);
	    textDsmsL1->SetTextColor(linesColor);
    	    textDsmsL1->SetTextSize(0.05);
	    textDsmsL1->Draw();
	}
	for (Int_t ch = 0;ch < 12;ch += 2) {
    	    TLine *lDsmL1Begin = new TLine(ch-0.5, 0, ch-0.5, 5);
    	    if (lDsmL1Begin) {
		if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(lDsmL1Begin);
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
		if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(text);
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
	    if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(textHT0);
	    textHT0->SetTextColor(linesColor);
    	    textHT0->SetTextSize(0.05);
	    textHT0->Draw();
	}
	TLatex *textHT1 = new TLatex(-1.4, 1.5, "th0 < Sum < th1");
	if (textHT1) {
	    if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(textHT1);
	    textHT1->SetTextColor(linesColor);
    	    textHT1->SetTextSize(0.05);
	    textHT1->Draw();
	}
	TLatex *textHT2 = new TLatex(-1.4, 2.5, "th1 < Sum < th2");
	if (textHT2) {
	    if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(textHT2);
	    textHT2->SetTextColor(linesColor);
    	    textHT2->SetTextSize(0.05);
	    textHT2->Draw();
	}
	TLatex *textHT3 = new TLatex(-1.4, 3.5, "th2 < Sum");
	if (textHT3) {
	    if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(textHT3);
	    textHT3->SetTextColor(linesColor);
    	    textHT3->SetTextSize(0.05);
	    textHT3->Draw();
	}
	for (Int_t ch = 0;ch < 12;ch += 2) {
    	    TLine *lDsmL1Begin = new TLine(ch-0.5, 0, ch-0.5, 4);
    	    if (lDsmL1Begin) {
		if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(lDsmL1Begin);
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
		if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(lDsmL1Begin);
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

    FIND_HISTO(TH1F, HistDsmL3InputHighTowerBits, file, HistDsmL3InputHighTowerBitsName);
    FIND_HISTO(TH1F, HistDsmL3InputPatchSumBits, file, HistDsmL3InputPatchSumBitsName);
    FIND_HISTO(TH1F, HistDsmL3InputBackToBackBit, file, HistDsmL3InputBackToBackBitName);
    FIND_HISTO(TH1F, HistDsmL3InputJPsiTopoBit, file, HistDsmL3InputJPsiTopoBitName);
    FIND_HISTO(TH1F, HistDsmL3InputJetPatchTopoBit, file, HistDsmL3InputJetPatchTopoBitName);

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

    if (!pad) return;
    pad->Clear();
    pad->cd(0);
    
    // 12-19 are the grey shades dark-light
    Int_t linesColor = 16;
    TPad* c = new TPad("pad2", "apd2",0.0,0.1,1.,1.);
    c->Draw();

    c->Divide(1, 2, 0.001, 0.001);

    for (int nonzs = 0;nonzs <= 1;nonzs++) {
    c->cd(1 + nonzs);
    FIND_HISTO(TH2F, HistSmdFeeSum, file, nonzs ? HistSmdFeeSumNonZSName : HistSmdFeeSumName);
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
			if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(lRdoBegin);
	    		lRdoBegin->SetLineColor(linesColor);
    		        lRdoBegin->SetLineWidth(1);
			lRdoBegin->Draw();
		    }
    		    TLine *lRdoEnd = new TLine(endrdo + 0.5, HistSmdFeeSum->GetYaxis()->GetBinLowEdge(1), endrdo + 0.5, HistSmdFeeSum->GetYaxis()->GetBinUpEdge(HistSmdFeeSum->GetYaxis()->GetNbins()));
    		    if (lRdoEnd) {
			if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(lRdoEnd);
	    		lRdoEnd->SetLineColor(linesColor);
    		        lRdoEnd->SetLineWidth(1);
			lRdoEnd->Draw();
		    }
    		    TString label;
    		    label = Form("RDO %i", currdo);
		    TLatex *text = new TLatex(beginrdo + 1 -0.5, 0.8 * HistSmdFeeSum->GetYaxis()->GetBinUpEdge(HistSmdFeeSum->GetYaxis()->GetNbins()), label.Data());
		    if (text) {
			if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(text);
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
    } // for(nonzs)
}	
//-------------------------------------------------------------------
void BEMCPlotsPresenter::displayPsdFeeSum(FileType file, TPad *pad, Int_t mDebug) {
    if (mDebug >= 10) cout << __FILE__ << ":" << __LINE__ << endl;

    if (!pad) return;
    pad->Clear();
    pad->cd(0);
    
    // 12-19 are the grey shades dark-light
    Int_t linesColor = 16;
    TPad* c = new TPad("pad2", "apd2",0.0,0.1,1.,1.);
    c->Draw();

    c->Divide(1, 2, 0.001, 0.001);

    for (int nonzs = 0;nonzs <= 1;nonzs++) {
    c->cd(1 + nonzs);
    FIND_HISTO(TH2F, HistPsdFeeSum, file, nonzs ? HistPsdFeeSumNonZSName : HistPsdFeeSumName);
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
			if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(lRdoBegin);
	    		lRdoBegin->SetLineColor(linesColor);
    		        lRdoBegin->SetLineWidth(1);
			lRdoBegin->Draw();
		    }
    		    TLine *lRdoEnd = new TLine(endrdo + 0.5, HistPsdFeeSum->GetYaxis()->GetBinLowEdge(1), endrdo + 0.5, HistPsdFeeSum->GetYaxis()->GetBinUpEdge(HistPsdFeeSum->GetYaxis()->GetNbins()));
    		    if (lRdoEnd) {
			if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(lRdoEnd);
	    		lRdoEnd->SetLineColor(linesColor);
    		        lRdoEnd->SetLineWidth(1);
			lRdoEnd->Draw();
		    }
    		    TString label;
    		    label = Form("RDO %i", currdo);
		    TLatex *text = new TLatex(beginrdo + 0.5 -0.5, 0.8 * HistPsdFeeSum->GetYaxis()->GetBinUpEdge(HistPsdFeeSum->GetYaxis()->GetNbins()), label.Data());
		    if (text) {
			if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(text);
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
    } // for(nonzs)
}	
//-------------------------------------------------------------------
void BEMCPlotsPresenter::displayTriggerCorruption(FileType file, TPad *pad, bool hold, Int_t mDebug) {
    if (mDebug >= 10) cout << __FILE__ << ":" << __LINE__ << endl;

    FIND_HISTO(TH1F, HistTriggerCorruptionHighTower, file, HistTriggerCorruptionHighTowerName);
    FIND_HISTO(TH1F, HistTriggerCorruptionPatchSum, file, HistTriggerCorruptionPatchSumName);
    FIND_HISTO(TH2F, HistDSM0HTCorr, file, HistDSM0HTCorrName);
    FIND_HISTO(TH2F, HistDSM0TPCorr, file, HistDSM0TPCorrName);

    if (!pad) return;
    pad->Clear();
    pad->cd(0);
    
    // 12-19 are the grey shades dark-light
    //Int_t linesColor = 16;
    TPad* c = new TPad("pad2", "apd2",0.0,0.1,1.,1.);
    c->Draw();
    c->cd(0);

    //c->Divide(2, 2, 0.001, 0.001);
    c->Divide(2, 1, 0.001, 0.001);
    
    /*
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
    */
    c->cd(1);
    if (HistDSM0HTCorr) {
      HistDSM0HTCorr->SetStats(0);
      HistDSM0HTCorr->Draw("COLZ");
    }
    c->cd(2);
    if (HistDSM0TPCorr) {
      HistDSM0TPCorr->SetStats(0);
      HistDSM0TPCorr->Draw("COLZ");
    }
}


//-------------------------------------------------------------------
void BEMCPlotsPresenter::displayAdcEtaPhi( FileType file, TPad *pad, Int_t mDebug)
{
   if (mDebug >= 10) cout << __FILE__ << ":" << __LINE__ << endl;
   
   FIND_HISTO(TH2F, Hist_ADCEtaPhi_TowHits, file, Hist_ADCEtaPhi_TowHitsName);
   FIND_HISTO(TH2F, Hist_ADCEtaPhi_Pre1Hits, file, Hist_ADCEtaPhi_Pre1HitsName);
   
   if (!pad) return;
   pad->Clear();
   pad->cd(0);
   
   TPad* c = new TPad("pad2", "apd2",0.0,0.1,1.,1.);
   c->Draw();
   c->cd(0);
   c->Divide(1, 2);
   
   c->cd(1);
   if (Hist_ADCEtaPhi_TowHits) {
     Hist_ADCEtaPhi_TowHits->SetStats(0);
     Hist_ADCEtaPhi_TowHits->Draw("H COLZ");
   }
   c->cd(2);
   if (Hist_ADCEtaPhi_Pre1Hits) {
     Hist_ADCEtaPhi_Pre1Hits->SetStats(0);
     Hist_ADCEtaPhi_Pre1Hits->Draw("H COLZ");
   }
   
}

#undef FIND_HISTO

//-------------------------------------------------------------------
void BEMCPlotsPresenter::displayTab(Int_t tab, Int_t panel, FileType file, TPad *pad, const Char_t *bemcStatusFilename, Int_t mDebug) {
//mDebug = 10;
    if (mDebug >= 10) cout << __FILE__ << ":" << __LINE__ << endl;
    if (BEMCPlotsCleanUp) {
	BEMCPlotsCleanUp->Delete();
    }
    if (!BEMCDecoderPresenter) BEMCDecoderPresenter = new StEmcDecoder();

    static bool first = true;
    static BemcTwMask *twMask = 0;
    if (first) {
	twMask = new BemcTwMask;
	bool twMaskFound = useBtowMask(bemcStatusFilename, twMask);
	if (!twMaskFound) {
    	    delete twMask;
    	    twMask=0;
	}
	first = false;
    }

    if (mDebug >= 2) cout << "tab = " << tab << endl;
    if (mDebug >= 2) cout << "panel = " << panel << endl;
    if (!file.file() || (mDebug >= 2)) cout << "file = " << file.file() << endl;
    if (!pad || (mDebug >= 2)) cout << "pad = " << pad << endl;
    if (!BEMCDecoderPresenter || (mDebug >= 2)) cout << "BEMCDecoderPresenter = " << BEMCDecoderPresenter << endl;

    if (tab == 0) {
	if (panel == 0) {
	    displayStatus(file, pad, mDebug);
	} else if (panel == 1) {
	    displayTowers(file, pad, mDebug);
	} else if (panel == 2) {
	    displaySMDPSD(file, pad, mDebug);
	} else if (panel == 3) {
	    displayTrigger(file, pad, mDebug);
	} else if (panel == 4) {
	    displayJet(file, pad, mDebug);
	} else if (panel == 5) {
	    displayRawAdc(file, pad, false, false, twMask, mDebug);
	} else if (panel == 6) {
	    displayJetPatchHT(file, pad, mDebug);
	} else if (panel == 7) {
	    displayJetPatchSum(file, pad, mDebug);
	    //	} else if (panel == 8) {
	    //  displayL0Input(file, pad, mDebug);
	}
	   else if (panel == 8) {
	    displayL1Input(file, pad, mDebug);
	} else if (panel == 9) {
	    displayL2Input(file, pad, mDebug);
	    //	} else if (panel == 10) {
	    // displaySmdFeeSum(file, pad, mDebug);
	    //	} else if (panel == 11) {
	    //  displayTriggerCorruption(file, pad, true, mDebug);
	    //	} else if (panel == 12) {
	    //  displayPsdFeeSum(file, pad, mDebug);
	    //	} else if (panel == 13) {
	    // displayRawAdc(file, pad, true, mDebug);
	    }
    }
    if (tab == 1) {
        if (panel == 0) {
            displayRawAdc(file, pad, false, true, twMask, mDebug);
        } else if (panel == 1) {
	    displayAdcEtaPhi(file, pad, mDebug);
        } else if (panel == 2) {
            displaySmdFeeSum(file, pad, mDebug);
        }  else if (panel == 3) {
            displayPsdFeeSum(file, pad, mDebug);
        } else if (panel == 4) {
            displayRawAdc(file, pad, true, false, twMask, mDebug);
        } else if (panel == 5) {
            displayL0Input(file, pad, mDebug);
        } else if (panel == 6) {
            displayTriggerCorruption(file, pad, true, mDebug);
        } 
    }
    if (mDebug >= 10) cout << __FILE__ << ":" << __LINE__ << endl;
}
//-------------------------------------------------------------------
void BEMCPlotsPresenter::displayTab(int tab, int panel, FileType file, TPad *pad, int mDebug) {
    BEMCPlotsPresenter::displayTab(tab, panel, file, pad, gEnv->GetValue("Online.bemcStatus", "bemcStatus.txt"), mDebug);
}
