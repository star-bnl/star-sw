#include "TCanvas.h"
#include "TF1.h"
#include "TFile.h"
#include "TGaxis.h"
#include "TGraph.h"
#include "TGraphErrors.h"
#include "TH1.h"
#include "TH2.h"
#include "TMath.h"
#include "TLegend.h"
#include "TLine.h"
#include "TPaveText.h"
#include "TString.h"
#include "TSystem.h"
#include "TStyle.h"

#include <cmath>
#include <fstream>
#include <iostream>
#include <map>
using namespace std;

//=============================
void NormalizeBsHisto(TH2F* H2)
{
	//Normalize bit shift QA histogram for the case of it merged from multiple files
	const float max = H2->GetMaximum();
	H2->Scale(1/max);

	//Remove ambigue bins: only bins with value 1 is valid
	for (int x=0; x<H2->GetNbinsX(); x++)
	for (int y=0; y<H2->GetNbinsY(); y++)
	{
		const float BC = H2->GetBinContent(x+1, y+1);
		if (BC < 1.0)
		{
			H2->SetBinContent(x+1, y+1, 0);
			H2->SetBinError  (x+1, y+1, 0);
		}
	}

	return;
}//NormalizeBsHisto

//======================================================================================
void bsQaShow(const char* inFile = "fmsBsQa.root", bool MARK = true, bool PRINT = false)
{
	enum {db, data};
	const int nSep = 75; //# of channels to be drawn per separated plot

	TFile* F = TFile::Open(inFile);
	if (!F || F->IsZombie()) { cout <<"Cannot open " <<inFile <<endl; return; }

	//-------------------------------------------

	//Get channel map from histograms
	map<int, int> nToCh[4];
	for (int a=0; a<4; a++) //detId
	{
		const int detId = a+8;
		TH2F* tempChMap = (TH2F*)F->Get(Form("ChMap_d%i", detId));
		if (!tempChMap) { cout <<"Cannot open channel map histogram!" <<endl; return; }

		int index = 0;
		for (int x=0; x<tempChMap->GetNbinsX(); x++)
		{
			const int ch = x+1;
			if (tempChMap->GetBinContent(x+1, 1) == 0) continue;
			else
			{
				nToCh[a].insert(std::pair<int, int>(index, ch));
				index++;
			}
		}
		tempChMap->Delete();
		//for (unsigned int x=0; x<nToCh[a].size(); x++) cout <<Form("%2i %3i %3i", detId, x, nToCh[a][x]) <<endl;
	}//a, detId

	//-------------------------------------------

	#if 1
	//Show QA: remove invalid channels and draw x channels per plot
	for (int a=0; a<4; a++) //detId
	{
		const int nSepHisto = nToCh[a].size()/nSep + 1;
		TH2F* H2Sep[2][nSepHisto];
		for (int b=0; b<2; b++) //DB or data
		{
			TH2F* H2_BS  = (TH2F*)F->Get(Form("BitShift_%s_d%i", b==0?"DB":"data", a+8));
			TH2F* H2_ADC = (TH2F*)F->Get(Form("Adc_d%i", a+8));
			if (!H2_BS || !H2_ADC) { cout <<"Cannot open histograms! (1)" <<endl; return; }
			NormalizeBsHisto(H2_BS);

			//Get average # of hit for this detId
			const float nCh = (H2_ADC->GetNbinsX()>300)?394:238;
			const float nHitAll = H2_ADC->GetEntries();
			const float nHitAvg = nHitAll/nCh;

			//Remove invalid channels + Group nSep channels per plot
			int Seq   = 0;
			int Count = 0;
			const int nBit = H2_BS->GetNbinsY();
			for (unsigned int x=0; x<nToCh[a].size(); x++)
			{
				//Declare new sub histogram
				if (Count == 0)
				{
					if (x != 0) Seq++;
					const char* tempN = Form("%s_%i", H2_BS->GetName(), Seq);
					const char* tempT = Form("%s, from ch = %i;ch;Bit (BS)", H2_BS->GetTitle(), nToCh[a][nSep*Seq]);
					H2Sep[b][Seq] = new TH2F(tempN, tempT, nSep,0,nSep, nBit,0.5,nBit+0.5);
					for (int i=0; i<nSep; i++)
					{
						const int chTarget = i + (nSep * Seq);
						if (nToCh[a][chTarget] == 0) continue;
						H2Sep[b][Seq]->GetXaxis()->SetBinLabel(i+1, Form("%i", nToCh[a][chTarget]));
					}
					H2Sep[b][Seq]->LabelsOption("v", "X");
					for (int y=0; y<nBit; y++)
					{
						if (y+1 <= 6) H2Sep[b][Seq]->GetYaxis()->SetBinLabel(y+1, Form("%2i (#plus%i)", y+1, y+1));
						else H2Sep[b][Seq]->GetYaxis()->SetBinLabel(y+1, Form("%2i (#minus%i)", y+1, abs(y-12)));
					}
					H2Sep[b][Seq]->GetYaxis()->SetTitleOffset(1.2);
				}

				//Copy bin content to new sub histogram
				const int xBinTarget = H2_BS->GetXaxis()->FindBin(nToCh[a][x] + 1.e-3);
				const int xBinToFill = x - (nSep * Seq) + 1;
				int nBitFilled = 0;
				for (int y=0; y<nBit; y++)
				{
					float BC = H2_BS->GetBinContent(xBinTarget, y+1); if (BC==0) continue;
					float BE = H2_BS->GetBinError  (xBinTarget, y+1);
					H2Sep[b][Seq]->SetBinContent(xBinToFill, y+1, BC);
					H2Sep[b][Seq]->SetBinError  (xBinToFill, y+1, BE);
					nBitFilled++;
				}

				//Check if this channel is good or not: 0 for good, 1 for dead, and 2 for bad
				int chQuality = 0;
				if (nBitFilled == nBit) chQuality = 1;
				else if (abs(nBitFilled) > 5)
				{
					TH1F* H1_ADC_1ch = (TH1F*)H2_ADC->ProjectionY("", nToCh[a][x], nToCh[a][x]);
					if (H1_ADC_1ch->GetEntries() < nHitAvg*1.e-2) chQuality = 1;
					else if (H1_ADC_1ch->GetRMS() < 1.) chQuality = 2;
					H1_ADC_1ch->Delete();
				}

				//Mark bad channels
				if (MARK==true && chQuality!=0)
				{
					for (int y=0; y<nBit; y++)
					{
						if (chQuality==1) H2Sep[b][Seq]->SetBinContent(xBinToFill, y+1, 0.1);
						if (chQuality==2) H2Sep[b][Seq]->SetBinContent(xBinToFill, y+1, 0.3);
					}
				}

				Count++;
				if (Count == nSep) Count = 0;
			}//x, loop over channel index (valid channels only)

			H2_BS ->Delete();
			H2_ADC->Delete();
		}//b, DB or data

		//Draw
		gStyle->SetOptDate(0);
		gStyle->SetOptStat(0);
		TCanvas *c1;
		int iCVS = 0;
		int iPAD = 1;
		for (int c=0; c<nSepHisto; c++)
		{
			if (iPAD==1)
			{
				c1 = new TCanvas(Form("FmsBsQa_d%i_%i", a+8,iCVS), Form("detId=%i, %i",a+8,iCVS), 1600, 900);
				c1->Divide(2, 2);
			}
			c1->cd(iPAD+0); H2Sep[0][c]->DrawCopy("col");
			c1->cd(iPAD+2); H2Sep[1][c]->DrawCopy("col");
			iPAD++;
			if (iPAD==3)
			{
				if (PRINT) c1->Print(Form("%s.png", c1->GetName()));
				iCVS++;
				iPAD=1;
			}
		}
	}//a, detId
	#endif

	//-------------------------------------------

	#if 10
	//Make and Show mismatching (DB <-> data) summary plot
	vector<int> ListMM1[4]; //Mismatch
	vector<int> ListMM2[4]; //Mismatch, only due to dead channels
	for (int a=0; a<4; a++)
	{
		//Get ADC histogram
		TH2F* H2_ADC = (TH2F*)F->Get(Form("Adc_d%i", a+8));
		const float nCh = (H2_ADC->GetNbinsX()>300)?394:238;
		const float nHitAll = H2_ADC->GetEntries();
		const float nHitAvg = nHitAll/nCh;

		//Get, Normalize, and Remove ambiguity of histograms
		TH2F* H2[2]; //DB or data
		for (int b=0; b<2; b++)
		{
			H2[b] = (TH2F*)F->Get(Form("BitShift_%s_d%i", b==0?"DB":"data", a+8));
			if (!H2[b]) { cout <<"Cannot open histograms! (2)" <<endl; return; }
			NormalizeBsHisto(H2[b]);
		}//b, DB or data

		//Listup mismatching channels
		const int nBit = H2[db]->GetNbinsY();
		for (unsigned int x=0; x<nToCh[a].size(); x++)
		{
			const int ch = nToCh[a][x];
			bool FlagMM = false;

			//Check in "DB-wise" point of view
			for (int y=0; y<nBit; y++)
			{
				const int BC_DB   = H2[db]  ->GetBinContent(ch, y+1); if (BC_DB == 0) continue;
				const int BC_data = H2[data]->GetBinContent(ch, y+1);
				if (BC_DB != BC_data)
				{
					ListMM1[a].push_back(ch);
					FlagMM = true;
					break;
				}
			}
			if (FlagMM == true) continue;

			int nBitFilled = 0;
			for (int y=0; y<nBit; y++) { if (H2[data]->GetBinContent(ch, y+1) != 0) nBitFilled++; }

			//Check if this channel is good or not: 0 for good, 1 for dead, and 2 for bad
			int chQuality = 0;
			if (nBitFilled == nBit) chQuality = 1;
			else if (abs(nBitFilled) > 5)
			{
				TH1F* H1_ADC_1ch = (TH1F*)H2_ADC->ProjectionY("", nToCh[a][x], nToCh[a][x]);
				if (H1_ADC_1ch->GetEntries() < nHitAvg*1.e-2) chQuality = 1;
				else if (H1_ADC_1ch->GetRMS() < 1.) chQuality = 2;
				H1_ADC_1ch->Delete();
			}

			//Check in data-wise point of view
			for (int y=0; y<nBit; y++) //Ascending order: targets positive bit shifted case
			{
				const int BC_data = H2[data]->GetBinContent(ch, y+1);
				if (BC_data != 0)
				{
					const int BC_DB = H2[db]->GetBinContent(ch, y+1);
					if (BC_data != BC_DB)
					{
						if (chQuality == 0) ListMM1[a].push_back(ch);
						else                ListMM2[a].push_back(ch);
						break;
					}
				}
				else break; //Met empty bin (corresponding ADc exist)
			}
		}//x, Loop over ch index (valid channels only): listup finished

		H2_ADC->Delete();
		H2[db]->Delete();
		H2[data]->Delete();
	}//a, detId

	//Fill QA histograms and Draw
	TH2F* H2_Mismatch[2][2];
	for (int a=0; a<2; a++) //DB or data
	for (int b=0; b<2; b++) //Pure mismatch or by dead channel
	{
		const char* tempN = Form("H2_MisMatch_%s_%i", a==0?"DB":"data", b);
		const char* tempT = Form("%s, %s;;Bit (BS)", a==0?"DB":"data",b==0?"Mismatch":"Mismatch due to bad channel");
		unsigned int nMM;
		if (b==0) nMM = ListMM1[0].size() + ListMM1[1].size() + ListMM1[2].size() + ListMM1[3].size();
		else      nMM = ListMM2[0].size() + ListMM2[1].size() + ListMM2[2].size() + ListMM2[3].size();
		H2_Mismatch[a][b] = new TH2F(tempN, tempT, (nMM>30)?nMM:30,0,(nMM>30)?nMM:30, 12,0.5,12+0.5);
		for (int x=0; x<H2_Mismatch[a][b]->GetNbinsX(); x++) H2_Mismatch[a][b]->GetXaxis()->SetBinLabel(x+1, "");
		for (int y=0; y<H2_Mismatch[a][b]->GetNbinsY(); y++)
		{
			if (y+1 <= 6) H2_Mismatch[a][b]->GetYaxis()->SetBinLabel(y+1, Form("%2i (#plus%i)", y+1, y+1));
			else          H2_Mismatch[a][b]->GetYaxis()->SetBinLabel(y+1, Form("%2i (#minus%i)", y+1, abs(y-12)));
		}
		H2_Mismatch[a][b]->LabelsOption("v", "X");
		H2_Mismatch[a][b]->GetYaxis()->SetLabelSize(0.045);
		H2_Mismatch[a][b]->GetYaxis()->SetTitleOffset(1.2);
	}//a, b

	int iMM1 = 1;
	int iMM2 = 1;
	for (int a=0; a<4; a++)
	{
		//Get, Normalize, and Remove ambiguity of histograms
		TH2F* H2[2]; //DB or data
		for (int b=0; b<2; b++)
		{
			H2[b] = (TH2F*)F->Get(Form("BitShift_%s_d%i", b==0?"DB":"data", a+8));
			if (!H2[b]) { cout <<"Cannot open histograms! (2)" <<endl; return; }

			//Nomarlize + Remove ambiguity
			const float tempMax = H2[b]->GetMaximum();
			H2[b]->Scale(1/tempMax);
			for (int x=0; x<H2[b]->GetNbinsX(); x++)
			for (int y=0; y<H2[b]->GetNbinsY(); y++)
			{
				const float BC = H2[b]->GetBinContent(x+1, y+1);
				if (BC < 1.0)
				{
					H2[b]->SetBinContent(x+1, y+1, 0);
					H2[b]->SetBinError  (x+1, y+1, 0);
				}
			}
		}//b, DB or data

		//Fill
		const int nBit = H2[db]->GetNbinsY();
		for (unsigned int x=0; x<ListMM1[a].size(); x++)
		{
			const int ch = ListMM1[a][x];
			for (int y=0; y<nBit; y++)
			{
				for (int b=0; b<2; b++) //DB or data
				{
					const int BC = H2[b]->GetBinContent(ch, y+1);
					H2_Mismatch[b][0]->SetBinContent(iMM1, y+1, BC);
					H2_Mismatch[b][0]->GetXaxis()->SetBinLabel(iMM1, Form("%i_%i", a+8, ch));
				}
			}
			iMM1++;
		}//x, ch
		for (unsigned int x=0; x<ListMM2[a].size(); x++)
		{
			const int ch = ListMM2[a][x];
			for (int y=0; y<nBit; y++)
			{
				for (int b=0; b<2; b++) //DB or data
				{
					const int BC = H2[b]->GetBinContent(ch, y+1);
					H2_Mismatch[b][1]->SetBinContent(iMM2, y+1, BC);
					H2_Mismatch[b][1]->GetXaxis()->SetBinLabel(iMM2, Form("%i_%i", a+8, ch));
				}
			}
			iMM2++;
		}//x, ch

		H2[db]  ->Delete();
		H2[data]->Delete();
	}//a, detId

	gStyle->SetOptDate(0);
	gStyle->SetOptStat(0);
	TCanvas *c2 = new TCanvas("FmsBsQa_Summary", "Summary", 1600, 900);
	c2->Divide(2, 2);
	for (int a=0; a<2; a++) //DB or data
	for (int b=0; b<2; b++)
	{
		c2->cd(2*a + b + 1);
		H2_Mismatch[a][b]->DrawCopy("col");
	}
	if (PRINT) c2->Print(Form("%s.png", c2->GetName()));
	#endif

	return;
}//Main
