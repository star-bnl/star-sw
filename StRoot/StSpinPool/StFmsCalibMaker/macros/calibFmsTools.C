/*
   FMS calibration tools

   Chong Kim
   UC Riverside
   ckim@bnl.gov

   * Functions are sorted in alphabetical order unless dependency problem exist
*/

#include "TBox.h"
#include "TCanvas.h"
#include "TDatime.h"
#include "TEllipse.h"
#include "TF1.h"
#include "TFile.h"
#include "TGaxis.h"
#include "TGraph.h"
#include "TGraphErrors.h"
#include "TH1.h"
#include "TH2.h"
#include "TKey.h"
#include "TMath.h"
#include "TLeaf.h"
#include "TLegend.h"
#include "TLine.h"
#include "TMarker.h"
#include "TPaveText.h"
#include "TRandom2.h"
#include "TStopwatch.h"
#include "TString.h"
#include "TSystem.h"
#include "TStyle.h"
#include "TTree.h"

#include <cmath>
#include <fstream>
#include <iostream>
#include <map>
using namespace std;

//----------------
enum CellStatIndex
{
    GOOD = 0,
    BAD  = 1,
    DEAD = 2,
    CONVERGED = 9
};

//----------------------------
typedef struct //Cell position
{
    int col;
    int row;
    float chX;
    float chY;
} st_pos;

//--------------------------
typedef struct //Fit results
{
    int nRefit;
    float chi2;
    float mass;
    float massErr;
    float width;
    float widthErr;
} st_fitR;

//---------------------------------
double F1Gaus(double* x, double* p)
{
    double X      = x[0];
    double Height = p[0];
    double Center = p[1];
    double Width  = p[2];
    double Return = Height * TMath::Gaus(X, Center, Width);
    return Return;
}//F1Gaus

//---------------------------------
double F1Pol3(double* x, double* p)
{
    double X = x[0];
    double Return = (X - p[0]) * (p[1] + p[2]*X + p[3]*std::pow(X, 2));
    return Return;
}//F1Pol3

//---------------------------------
double F1Comb(double* x, double* p)
{
    return F1Gaus(x, p) + F1Pol3(x, &p[3]);
}//F1Comb

//--------------------------------------------------------------------------------
void GetMapChToBS(const char* inList, map<int, int> chToBS[4], bool PRINT = false)
{
	//Uses file FmsBitShift.txt

    ifstream in;
    in.open(inList);
    if (!in.is_open()) { cout <<"Cannot open " <<inList <<endl; return; }
    while (in.is_open())
    {
        int detId, ch, bs;
        in >> detId >> ch >> bs;
        if (!in.good()) { break; in.close(); }

        chToBS[detId-8].insert(pair<int, int>(ch, bs));
        if (PRINT) cout <<Form("%2i %3i %2i", detId, ch, bs) <<endl;
    }

    return;
}//GetMapChToBS

//--------------------------------------------------------------------------------------------
void GetMapChToCellStat(const char* inList, map<int, int> chToCellStat[4], bool PRINT = false)
{
	//Uses file FmsCellStat.txt

    ifstream in;
    in.open(inList);
    if (!in.is_open()) { cout <<"Cannot open " <<inList <<endl; return; }
    while (in.is_open())
    {
        int detId, ch, stat;
        in >> detId >> ch >> stat;
        if (!in.good()) { break; in.close(); }

        chToCellStat[detId-8].insert(pair<int, int>(ch, stat));
        if (PRINT) cout <<Form("%2i %3i %i", detId, ch, stat) <<endl;
    }

    return;
}//GetMapChToCellStat

//----------------------------------------------------------------------------------------
void GetMapChToFitR(const char* inList, map<int, st_fitR> chToFitR[4], bool PRINT = false)
{
	//Uses file out_fmsCalibFit_?.txt, which is produced by FitMass

    ifstream in;
    in.open(inList);
    if (!in.is_open()) { cout <<"Cannot open " <<inList <<endl; return; }
    while (in.is_open())
    {
        int detId, ch, nRefit;
        float chi2, mass, massErr, width, widthErr;
        in >> detId >> ch >> nRefit >> chi2 >> mass >> massErr >> width >> widthErr;
        if (!in.good()) { break; in.close(); }

        st_fitR fitR;
        fitR.nRefit   = nRefit;
        fitR.chi2     = chi2;
        fitR.mass     = mass;
        fitR.massErr  = massErr;
        fitR.width    = width;
        fitR.widthErr = widthErr;
        chToFitR[detId-8].insert(pair<int, st_fitR>(ch, fitR));
        if (PRINT)
        {
            cout <<Form("%2i %3i %2i %7.3f %6.3f %6.3f %6.3f %6.3f",
                    detId,ch,nRefit,chi2,mass,massErr,width,widthErr) <<endl;
        }
    }

    return;
}//GetMapChToFitR

//--------------------------------------------------------------------------------------
void GetMapChToGain(const char* inList, map<int, float> chToGain[4], bool PRINT = false)
{
	//Uses file FmsMapBase.txt, NOTE that this is gain, NOT gainCorr!

    ifstream in;
    in.open(inList);
    if (!in.is_open()) { cout <<"Cannot open " <<inList <<endl; return; }
    while (in.is_open())
    {
        int detId, ch, col, row;
        float gain, chX, chY;
        in >> detId >> ch >> gain >> col >> row >> chX >> chY;
        if (!in.good()) { break; in.close(); }

        if (gain == 0.) continue; //NOT necessary after update in Feb. 2019, but leaving it as a fafeguard
        else
        {
            int a = detId-8;
            chToGain[a].insert(pair<int, float>(ch, gain));
            if (PRINT) cout <<Form("%2i %3i %5.3f", a+8, ch, gain) <<endl;
        }
    }

    return;
}//GetMapChToGain

//----------------------------------------------------------------------------------------------
void GetMapChToGainCorr(const char* inList, map<int, float> chToGainCorr[4], bool PRINT = false)
{
	//Uses file FmsGainCorr.txt (FmsDbMaker readable)

    ifstream in;
    in.open(inList);
    if (!in.is_open()) { cout <<"Cannot open " <<inList <<endl; return; }
    while (in.is_open())
    {
        int index, nstb, ch;
        float gainCorr;
        in >> index >> nstb >> ch >> gainCorr;
        if (!in.good()) { break; in.close(); }

        if (index == 1) continue; //PSU group's NSTB notation related, ignore it
        else
        {
            int a = nstb-1;
            chToGainCorr[a].insert(pair<int, float>(ch, gainCorr));
            if (PRINT) cout <<Form("%2i %3i %5.3f", a+8, ch, gainCorr) <<endl;
        }
    }

    return;
}//GetMapChToGainCorr

//-------------------------------------------------------------------------------------
void GetMapChToPos(const char* inList, map<int, st_pos> chToPos[4], bool PRINT = false)
{
	//Uses file FmsMapBase.txt

    ifstream in;
    in.open(inList);
    if (!in.is_open()) { cout <<"Cannot open " <<inList <<endl; return; }
    while (in.is_open())
    {
        int detId, ch, col, row;
        float gain, chX, chY;
        in >> detId >> ch >> gain >> col >> row >> chX >> chY;
        if (!in.good()) { break; in.close(); }

        if (gain == 0.) continue;
        else
        {
            int a = detId-8;
            st_pos pos;
            pos.col = col;
            pos.row = row;
            pos.chX = chX;
            pos.chY = chY;
            chToPos[a].insert(pair<int, st_pos>(ch, pos));
            if (PRINT) cout <<Form("%2i %3i %2i %2i %6.2f %6.2f", detId, ch, col, row, chX, chY) <<endl;
        }
    }

    return;
}//GetMapChToPos

//------------------------------------------------------------------------------------
void GetMapChToSkew(const char* inList, map<int, int> chToSkew[4], bool PRINT = false)
{
	//Read count for skewed channel monitor (how many times a channel judged as skewed)

    ifstream in;
    in.open(inList);
    if (!in.is_open()) { cout <<"Cannot open " <<inList <<endl; return; }
    while (in.is_open())
    {
        int detId, ch, nSkew;
        in >> detId >> ch >> nSkew;
        if (!in.good()) { break; in.close(); }

        chToSkew[detId-8].insert(pair<int, int>(ch, nSkew));
        if (PRINT) cout <<Form("%2i %3i %i", detId, ch, nSkew) <<endl;
    }

	return;
}//GetMapChToSkew

//----------------------------------------------------------------------------------
void GetMapIndexToCh(const char* inList, map<int, int> iToCh[4], bool PRINT = false)
{
	//Uses file FmsMapBase.txt
	
    int index[4] = {0};	//Note that index begins from 0, while channel # begins from 1

    ifstream in;
    in.open(inList);
    if (!in.is_open()) { cout <<"Cannot open " <<inList <<endl; return; }
    while (in.is_open())
    {
        int detId, ch, col, row;
        float gain, chX, chY;
        in >> detId >> ch >> gain >> col >> row >> chX >> chY;
        if (!in.good()) { break; in.close(); }

        if (gain==0.) continue;
        else
        {
            int a = detId-8;
            iToCh[a].insert(pair<int, int>(index[a], ch));
            if (PRINT) cout <<Form("%2i %3i %3i", detId, ch, index[a]) <<endl;
            index[a]++;
        }
    }

    return;
}//GetMapBase

//---------------------------------------------------------------------------
void GetMapManualGainCorr(const char* inList, map<int, float> manGainCorr[4])
{
	ifstream in;
	in.open(inList);
	if (!in.is_open())
	{
		cout <<Form("\nNo manual gainCorr update list found: skip manual correction...\n") <<endl;
		return;
	}

	cout <<"\nManual gainCorr update list found: enforce following values for next iteration..." <<endl;
	while (in.is_open())
	{
		int detId, ch;
		float gainCorr;
		in >> detId >> ch >> gainCorr;
        if (!in.good()) { break; in.close(); }

		cout <<Form("%2i, %3i: %4.3f", detId, ch, gainCorr) <<endl;
		manGainCorr[detId-8].insert(pair<int, float>(ch, gainCorr));
	}
	cout <<endl;

	return;
}//Intervent

//---------------------------------------------------------
void SetPi0Range(TH1F* H1, float* pi0R, bool PRINT = false)
{
	//Empirically, 1 sigma width of a good cell is 0.02 (large) and 0.03 (small)
	const float min = 0.135 - 0.06;
	const float max = 0.135 + 0.06;

    //Default peak range: [0.10, 0.25]
    const float tempPeak = H1->GetBinCenter(H1->GetMaximumBin());
    if (tempPeak >= min && tempPeak <= max)
	{
		pi0R[0] = min;
		pi0R[1] = max;
	}
    else
    {
        if (tempPeak < min) pi0R[0] = 0.;
        if (tempPeak > max) pi0R[1] = 0.35;
        if (PRINT) cout <<Form("Rearranging pi0 range of %s to [%4.3f, %4.3f]\n", H1->GetName(),pi0R[0],pi0R[1]);
    }

    return;
}//SetPi0Range

//---------------------------------------------
float SearchFirstNonzeroBinPosition(TH1F* H1BG)
{
    float BGStart = 0.;
    for (int a=0; a<H1BG->GetNbinsX(); a++)
    {
        int BinC = H1BG->GetBinContent(a+1);
        if (BinC != 0)
        {
            BGStart = H1BG->GetBinCenter(a+1);
            break;
        }
    }
    if (BGStart < 0.01 || BGStart > 0.10) BGStart = 0.05; //!

    return BGStart;
}//SearchFirstNonzeroBinPosition

//-----------
void FitMass(
		const int iIter,
		TH2F* H2_mass,
		map<int, int>iToCh,
		map<int, int>chToBS,
		map<int, int>chToCellStat,
		map<int, float>chToGainCorr,
		map<int, st_pos>chToPos,
		map<int, st_fitR>&chToFitR,
		bool PRINT = true
		)
{
    const int nEventsMin = 100;
	const int nRefitMax  = 10;
    const float maxMass  = 0.5;

	//Plot container to save it later
    TCanvas* c1;
	TLine* LPi0;
    if (PRINT)
    {
        gStyle->SetOptDate(0);
        gStyle->SetOptStat("e");
        gStyle->SetOptFit();
        c1 = new TCanvas("c1", "",  800, 800);

		LPi0 = new TLine(0.135, 0, 0.135, 1);
		LPi0->SetLineColor(kMagenta);
		LPi0->SetLineStyle(2);
    }

    //Prepare fit functions
    TF1* F1Pi0 = new TF1("F1Pi0", F1Gaus, 0, maxMass, 3);
    TF1* F1BG  = new TF1("F1BG",  F1Pol3, 0, maxMass, 4);
    TF1* F1    = new TF1("F1",    F1Comb, 0, maxMass, 7);
    F1Pi0->SetLineColor(4);
    F1Pi0->SetLineStyle(2);
    F1Pi0->SetParNames("GausH", "GausM", "GausW");
    F1BG->SetLineColor(3);
    F1BG->SetLineStyle(2);
    F1BG->SetParNames("Pol3p0", "Pol3p1", "Pol3p2", "Pol3p3");
    F1->SetLineColor(2);
    F1->SetLineStyle(2);
    F1->SetParNames("GausH", "GausM", "GausW", "Pol3p0", "Pol3p1", "Pol3p2", "Pol3p3");

	//Histogram container
    TH1F* H1Pi0 = new TH1F();
    TH1F* H1BG  = new TH1F();
    TH1F* H1    = new TH1F();

	//Loop over channels
    for (unsigned int b=0; b<iToCh.size(); b++)
    {
		H1Pi0->Reset();
		H1BG ->Reset();
		H1   ->Reset();

		//Get info
		const int ch = iToCh[b];
		const int tBS  = chToBS[ch];
		const int tCol = chToPos[ch].col;
		const int tRow = chToPos[ch].row;
		const float tGainCorr = chToGainCorr[ch];
		const char* titleInfo = Form("%5.3f, [%i, %i], BS=%i", tGainCorr, tCol, tRow, tBS);

		//Get mass distribution
		H1 = (TH1F*)H2_mass->ProjectionY(Form("%s_ch%i", H2_mass->GetName(), ch), ch, ch);
		if (H1->GetXaxis()->GetXmax() != maxMass) cout <<"WARNING: max X exceeds prepared fit range!" <<endl;

		//Skip bad, dead, or too small # of entries
		if (chToCellStat[ch]==BAD || chToCellStat[ch]==DEAD || H1->GetEntries()<nEventsMin)
		{
			if (PRINT)
			{
				c1->cd(1);
				c1->SetName(Form("%s", H1->GetName()));

				const char* titleStat = "";
				if      (chToCellStat[ch] == BAD)       titleStat = "BAD";
				else if (chToCellStat[ch] == DEAD)      titleStat = "DEAD";
				else if (H1->GetEntries() < nEventsMin) titleStat = Form("<%i", nEventsMin);
				H1->SetTitle(Form("i=%i, %s, %s", iIter, titleInfo, titleStat));
				H1->DrawCopy("hist e");

				c1->SetTitle(Form("Skip, %s", titleStat));
				c1->Write();
			}
			continue;
		}

		//---------------------------------------

		//Check if "this distribution's peak" is located near the pi0 mass, update otherwise
		float pi0R[2] = {0.135-0.06, 0.135+0.06};
		SetPi0Range(H1, pi0R, true);

		//Separate mass distribution into two: pi0 and BG
		const int   mMaxB = H1->GetXaxis()->FindBin(maxMass);
		const float pi0B0 = H1->GetXaxis()->FindBin(pi0R[0]);
		const float pi0B1 = H1->GetXaxis()->FindBin(pi0R[1]);
		H1Pi0 = (TH1F*)H1->Clone(Form("%s_pi0", H1->GetName()));
		H1BG  = (TH1F*)H1->Clone(Form("%s_bg",  H1->GetName()));
		for (int c=0; c<mMaxB; c++)
		{
			(c+1>=pi0B0 && c+1<=pi0B1)?H1BG->SetBinContent(c+1, 0):H1Pi0->SetBinContent(c+1, 0);
			(c+1>=pi0B0 && c+1<=pi0B1)?H1BG->SetBinError  (c+1, 0):H1Pi0->SetBinError  (c+1, 0);
		}

		//---------------------------------------

		//Preliminary fit on pi0 histogram to get proper seed parameters
		for (int i=0; i<F1Pi0->GetNpar(); i++) F1Pi0->ReleaseParameter(i);
		const float pi0H = H1Pi0->GetMaximum();
		const float pi0C = H1Pi0->GetBinCenter(H1Pi0->GetMaximumBin());
		const float pi0W = 0.03;
		const float pi0HL[2] = {pi0H*0.1, pi0H};
		const float pi0CL[2] = {(pi0C<0.1)?0:(pi0C-0.05), pi0C+0.05};
		const float pi0WL[2] = {H1Pi0->GetBinWidth(1), 0.15};
		F1Pi0->SetParameters(pi0H*0.9, pi0C, pi0W);
		F1Pi0->SetParLimits(0, pi0HL[0], pi0HL[1]);
		F1Pi0->SetParLimits(1, pi0CL[0], pi0CL[1]);
		F1Pi0->SetParLimits(2, pi0WL[0], pi0WL[1]);
		H1Pi0->Fit(F1Pi0->GetName(), "EQR0", "", pi0R[0], pi0R[1]);
		const float GausH = F1Pi0->GetParameter(0);
		const float GausC = F1Pi0->GetParameter(1);
		const float GausW = F1Pi0->GetParameter(2);

		//Preliminary fit on BG
		for (int i=0; i<F1BG->GetNpar(); i++) F1BG->ReleaseParameter(i);
		const float Pol3p0 = SearchFirstNonzeroBinPosition(H1BG);
		F1BG->SetParameters(1, 1, 1, 1);
		F1BG->FixParameter(0, Pol3p0);
		H1BG->Fit(F1BG->GetName(), "EQR0", "", 0, maxMass);
		const float Pol3p1 = F1BG->GetParameter(1);
		const float Pol3p2 = F1BG->GetParameter(2);
		const float Pol3p3 = F1BG->GetParameter(3);

		//Combined fit
		for (int i=0; i<F1->GetNpar(); i++) F1->ReleaseParameter(i);
		F1->SetParameters(GausH, GausC, GausW, Pol3p0, Pol3p1, Pol3p2, Pol3p3);
		F1->SetParLimits(0, pi0HL[0], pi0HL[1]);
		F1->SetParLimits(1, pi0CL[0], pi0CL[1]);
		F1->SetParLimits(2, pi0WL[0], pi0WL[1]);
		F1->FixParameter(3, Pol3p0);
		H1->Fit(F1->GetName(), "EQR0", "", 0, maxMass);

		//---------------------------------------

		//Try fit again if fit qaulity looks too bad
		int nRefit = 0;
		float massDev = fabs(F1->GetParameter(1) - 0.1349766);
		float fitChi2 = F1->GetChisquare()/F1->GetNDF();
		if (massDev>0.03 && (fitChi2<0.5 || fitChi2>3.0))
		{
			while (nRefit < nRefitMax)
			{
				nRefit++;
				if (massDev<0.03 && fitChi2>0.5 && fitChi2<3.0) break;
				//cout <<Form("Try fit again... %s, %2i/%i", H1->GetName(), nRefit, nRefitMax) <<endl;

				TDatime DT;
				TRandom2 RNDM(DT.GetTime() + nRefit);
				const float pNew[7] =
				{
					RNDM.Uniform(H1->GetMaximum()*0.5, H1->GetMaximum()),
					RNDM.Gaus(0.135, 0.05),
					RNDM.Uniform(H1->GetBinWidth(1), 0.3),
					Pol3p0, 1., 1., 1.
				}; //Set Gaussian part's parameters again

				for (int i=0; i<F1->GetNpar(); i++) { F1->ReleaseParameter(i); F1->SetParameter(i, pNew[i]); }
				F1->SetParLimits(0, H1->GetMaximum()*0.5, H1->GetMaximum());
				F1->SetParLimits(1, 0, maxMass);
				F1->SetParLimits(2, H1->GetBinWidth(1), 0.3);
				F1->FixParameter(3, Pol3p0);

				H1->Fit(F1->GetName(), "EQR0", "", 0, maxMass);
				massDev = fabs(F1->GetParameter(1) - 0.1349766);
				fitChi2 = F1->GetChisquare()/F1->GetNDF();
			}
		}//reFit

		//---------------------------------------

		//Save results
		st_fitR fitR;
		fitR.nRefit   = nRefit;
		fitR.chi2     = fitChi2;
		fitR.mass     = F1->GetParameter(1);
		fitR.massErr  = F1->GetParError(1);
		fitR.width    = F1->GetParameter(2);
		fitR.widthErr = F1->GetParError(2);
		chToFitR.insert(pair<int, st_fitR>(ch, fitR));

		if (PRINT)
		{
			c1->cd(1);
			c1->SetName(Form("%s", H1->GetName()));

			const char* titleStat = "";
			if      (chToCellStat[ch] == GOOD)      titleStat = "";
			else if (chToCellStat[ch] == CONVERGED) titleStat = ", CONV";
			const char* titleRefit = (nRefit!=0)?Form(", nRefit=%i", nRefit):"";
			H1->SetTitle(Form("i=%i, %s%s%s", iIter, titleInfo, titleStat, titleRefit));
			H1->DrawCopy("hist e");

			F1Pi0->SetParameter(0, F1->GetParameter(0));
			F1Pi0->SetParameter(1, F1->GetParameter(1));
			F1Pi0->SetParameter(2, F1->GetParameter(2));
			F1Pi0->Draw("same");
			F1BG->SetParameter(0, F1->GetParameter(3));
			F1BG->SetParameter(1, F1->GetParameter(4));
			F1BG->SetParameter(2, F1->GetParameter(5));
			F1BG->SetParameter(3, F1->GetParameter(6));
			F1BG->Draw("same");
			F1->Draw("same");

			LPi0->SetY2(H1->GetMaximum());
			LPi0->Draw("same");

			c1->SetTitle(titleRefit);
			c1->Write();
		}//Print

	}//b, channels

    H1Pi0->Delete();
    H1BG ->Delete();
    H1   ->Delete();
    F1Pi0->Delete();
    F1BG ->Delete();
    F1   ->Delete();

    return;
}//FitMass

//-------------------------------------------------------------------------------------------
void PrintCellStat(const char* outName, map<int, int>iToCh[4], map<int, int> chToCellStat[4])
{
    ofstream out;
    out.open(outName);
	for (unsigned int a=0; a<4; a++)
	for (unsigned int b=0; b<iToCh[a].size(); b++)
	{
		const int ch = iToCh[a][b];
		const int cellStat = chToCellStat[a][ch];
		out <<Form("%2i %3i %i", a+8, ch, cellStat) <<endl;
	}//a, b
    out.close();
	return;
}//PrintCellStat

//----------------------------------------------------------------------------------------------
void PrintFitResults(const char* outName, map<int, int> iToCh[4], map<int, st_fitR> chToFitR[4])
{
    ofstream out;
    out.open(outName);
    for (int a=0; a<4; a++)
    for (unsigned int b=0; b<iToCh[a].size(); b++)
    {
        const int detId = a+8;
        const int ch    = iToCh[a][b];

        const int nRefit     = chToFitR[a][ch].nRefit;
        const float chi2     = chToFitR[a][ch].chi2;
        const float mass     = chToFitR[a][ch].mass;
        const float massErr  = chToFitR[a][ch].massErr;
        const float width    = chToFitR[a][ch].width;
        const float widthErr = chToFitR[a][ch].widthErr;
        out <<Form("%2i %3i %2i %7.3f %6.5f %6.5f %6.5f %6.5f",
                detId,ch,nRefit,chi2,mass,massErr,width,widthErr) <<endl;
    }
    out.close();

    return;
}//PrintMapChToFitR

//----------------------------------------------------------------------
void PrintGainCorr(const char* outName, map<int, float> chToGainCorr[4])
{
    ofstream out;
    out.open(outName);
    for (int i=1; i<=2; i++) //Official FmsGainCorr format, I really hate this...
    {
        if (i==1) //NSTB dummies
        {
            for (int a=1; a<=6; a++)
            {
                if      (a<=2) { for (int b=1; b<=49; b++) out <<Form("%i %i %2i %5.3f", i, a, b, 0.000) <<endl; }
                else if (a<=4) { for (int b=1; b<=25; b++) out <<Form("%i %i %2i %5.3f", i, a, b, 0.000) <<endl; }
                else if (a<=6) { for (int b=1; b<= 7; b++) out <<Form("%i %i %2i %5.3f", i, a, b, 0.000) <<endl; }
            }
        }
        else if (i==2)
        {
            for (int a=0; a<4; a++)
            {
                const int maxCh = (a<2)?578:288;
                for (int b=0; b<maxCh; b++)
                {
                    const int ch = b+1;
                    const float gainCorr = chToGainCorr[a][ch];
                    out <<Form("%i %i %3i %5.3f", i, a+1, ch, gainCorr) <<endl;
                }//b, ch
            }//a, detId
        }
    }
    out.close();
    return;
}//PrintGainCorr

//------------------------------------------------------------------------------------------
void PrintSkewMonitor(const char* outName, map<int, int>iToCh[4], map<int, int> chToSkew[4])
{
    ofstream out;
    out.open(outName);
	for (unsigned int a=0; a<4; a++)
	for (unsigned int b=0; b<iToCh[a].size(); b++)
	{
		const int ch = iToCh[a][b];
		const int nSkew = chToSkew[a][ch];
		out <<Form("%2i %3i %i", a+8, ch, nSkew) <<endl;
	}//a, b
    out.close();
	return;
}//PrintCellStat

//-------------------
void TestConvergence(
        const int iIter,
        map<int, int> iToCh[4],
		map<int, int> chToCellStat[4],
        const char* outPath = "./Iterations"
        )
{
    //Check if minimum # of iterations satisfied for convergence test
    const int nIter = 3;
    if (iIter < nIter) { cout <<"Require more iterations: skip the convervence test..." <<endl; return; }
    else cout <<Form("Starting convergence test for iterations %i - %i...", iIter-nIter, iIter) <<endl;

    //To check N iterations you need N+1 set of gainCorr (ex. 3 iterations <-> 0-1, 1-2, and 2-3) 
    map<int, float> chToGainCorr[nIter+1][4];
    map<int, st_fitR> chToFitR[nIter+1][4];
    for (int i=0; i<=nIter; i++)
    {
        GetMapChToGainCorr(Form("%s/FmsGainCorr_%i.txt", outPath, (iIter-nIter)+i), chToGainCorr[i]);
        GetMapChToFitR(Form("%s/out_fmsCalibFit_%i.txt", outPath, (iIter-nIter)+i), chToFitR[i]);
    }

	//Test convergence
    //-------------------------------------------

    const int   convNReq = 3;     //# of "tolerance successively satisfied" required to judge convergence
    const float convTolM = 0.005; //Tolerance 1: allowed mass fluctuation WRT pi0 mass
    const float convTolG = 0.035; //Tolerance 2: allowed gainCorr fluctuation by "ratio to the previous value"

    int nConv[4] = {0};
	map<int, int> chToCellStatNext[4];
    for (unsigned int a=0; a<4; a++)               //detId
    for (unsigned int b=0; b<iToCh[a].size(); b++) //ch
    {
        const int ch = iToCh[a][b];
        int cellStat = chToCellStat[a][ch];

        if (cellStat == GOOD)
        {
            //1st judgment: btw adjacent iteration, by using mass fluctuation and gainCorr change
            int convCounter = 0;
            for (int i=0; i<nIter; i++)
            {
                const float massFitR = chToFitR[i][a][ch].mass;
                if (chToGainCorr[i][a][ch] == 0. || massFitR == 0.) continue;

                const float ratioAdj = fabs(chToGainCorr[i][a][ch] - chToGainCorr[i+1][a][ch])/chToGainCorr[i][a][ch];
                if ((fabs(massFitR - 0.1349766) < convTolM) && (ratioAdj < convTolG)) convCounter++;
            }

            //2nd judgment: btw first and last iteration
            if (convCounter >= convNReq)
            {
                const float gainCorr1st = chToGainCorr[0][a][ch];
                const float gainCorrFin = chToGainCorr[nIter][a][ch];
                const float ratioWide = fabs(gainCorr1st - gainCorrFin)/gainCorr1st;
                if (ratioWide < convTolG)
                {
                    cellStat = CONVERGED;
                    cout <<Form("Converged: %2i, %3i", a+8, ch) <<endl;
                }
            }
        }//Cell status = good (not bad/hot/converged, keep going)

        if (cellStat == CONVERGED) nConv[a]++;
		chToCellStatNext[a].insert(pair<int, int>(ch, cellStat));
    }//a, b

	//Print new cellStat
    //-------------------------------------------

	PrintCellStat(Form("FmsCellStatNew_%i.txt", iIter), iToCh, chToCellStatNext);
	gSystem->Exec(Form("mv FmsCellStat.txt %s/FmsCellStat_%i.txt", outPath, iIter));
	gSystem->Exec(Form("mv FmsCellStatNew_%i.txt FmsCellStat.txt", iIter));

	//Check convergence ratio
    //-------------------------------------------

    for (int a=0; a<4; a++)
    {
        const int nCh = iToCh[a].size();
        cout <<Form("detId=%2i: %3i/%3i (%6.4f)", a+8, nConv[a], nCh, (float)nConv[a]/nCh) <<endl;
    }
    const int   nConvAll = nConv[0] + nConv[1] + nConv[2] + nConv[3];
    const float rConvAll = (float)nConvAll/1264.;
    cout <<Form("Total: %i/1264 (%6.4f)", nConvAll, rConvAll) <<endl;

    return;
}//TestConvergence
