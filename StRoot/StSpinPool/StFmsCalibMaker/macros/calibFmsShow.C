/*
   FMS calibration tools for QA/plot production
   Aligned in alphabetical order unless dependency problem exists

   Chong Kim
   UC Riverside
   ckim@bnl.gov
*/

#include "calibFmsTools.C"

static map<int, int>    DumMapI[4]; //Default dummy maps
static map<int, float>  DumMapF[4];
static map<int, string> DumMapS[4];
static vector<int>      DumVecI[4];
static TH2F*            DumTH2F[4];

//-------------
void DrawAdcQa(
		map<int, int> iToCh[4],
		const char* inList, //List of files contain ADC vs. ch
		bool PRINT = false,
		map<int, string> chToInfo[4]    = DumMapS,
		map<int, int> chToBS[4]         = DumMapI, //If this list is given channels w/ BS > 0 will be scaled by it
		map<int, float> chToGain[4]     = DumMapF, 
		map<int, float> chToGainCorr[4] = DumMapF, //If this list is given hit energy will be drawn too
		vector<int> chMarked[4]         = DumVecI,
		TH2F* H2Mass[4]                 = DumTH2F  //If this TH2 is given mass distribution will be drawn on ADC
		)
{
	const int adcMin = 5; //Lower limit, empty any filled bins (adc <= 5) below this
	const int cutSig = 2; //# of sigma for the rough hot channel decision

	//Get list of runs
	string runList[500];
	unsigned int nRun = 0;
	ifstream in;
	in.open(inList);
	if (!in.is_open()) { cout <<"Cannot open " <<inList <<endl; return; }
	while (in.is_open())
	{
		string inFile;
		in >> inFile;
		if (!in.good()) { break; in.close(); }

		runList[nRun].clear();
		runList[nRun] = inFile;

		nRun++;
		if (nRun > sizeof(runList)/sizeof(runList[0])) cout <<"WARNING! # of runs exceeds allowed limit!" <<endl;
	}//Loop over runs

	//QA histograms
	TH2F* H2AdcQa1[4]; //Channel vs. run, each entry corresponds to # of hit/trig
	TH2F* H2AdcQa2[4]; //ADC vs. channel
	for (int a=0; a<4; a++)
	{
		const int nCh = iToCh[a].size();
		H2AdcQa1[a] = new TH2F(Form("AdcQa1_d%i", a+8), "", nRun,-0.5,nRun-0.5, nCh,0.5,nCh+0.5);
		H2AdcQa1[a]->SetTitle(Form("detId=%i, nHit/nTrig;run index;ch index", a+8));
		H2AdcQa1[a]->Sumw2();
	}

	//Fill QA histograms
	for (unsigned int i=0; i<nRun; i++)
	{
		const char* inFile = runList[i].c_str();
		TFile* F = TFile::Open(inFile);
		if (!F || F->IsZombie()) { cout <<"Cannot open file " <<inFile <<endl; return; }
		else cout <<Form("Open %s... %i", inFile, i) <<endl;

		TH1F* H1TempTrig = (TH1F*)F->Get("trig");
		if (!H1TempTrig || H1TempTrig->GetEntries()==0) { cout <<"Trigger TH1?" <<endl; return; }
		const float nTrig = H1TempTrig->GetEntries();
		H1TempTrig->Delete();

		for (int a=0; a<4; a++)
		{
			TH2F* H2TempAdc = (TH2F*)F->Get(Form("adc_d%i_wide", a+8));
			if (!H2TempAdc || H2TempAdc->GetEntries()==0) { cout <<"ADC TH2?" <<endl; return; }

			//QA1
			const int adcMinBin = H2TempAdc->GetYaxis()->FindBin(adcMin + 0.1);
			const int adcMaxBin = H2TempAdc->GetNbinsY();
			TH1F* H1TempAdc = (TH1F*)H2TempAdc->ProjectionX(Form("detId%i", a+8), adcMinBin, adcMaxBin);
			for (unsigned int b=0; b<iToCh[a].size(); b++)
			{
				const int ch = iToCh[a][b];
				if (ch == 0) continue;
				const float nHitNorm = H1TempAdc->GetBinContent(ch) / nTrig;
				H2AdcQa1[a]->SetBinContent(i+1, b+1, nHitNorm);
			}
			H1TempAdc->Delete();

			//QA2
			if (i==0)
			{
				H2AdcQa2[a] = (TH2F*)H2TempAdc->Clone(Form("AdcQa2_d%i", a+8));
				H2AdcQa2[a]->SetTitle(Form("detId=%i;ch;ADC", a+8));
			}
			else H2AdcQa2[a]->Add(H2TempAdc);
		
			H2TempAdc->Delete();
		}//a, detId
	}//i, Loop over runs

	//--------------------------------------------------------------------

	#if 1
	gStyle->SetOptDate(0);
	gStyle->SetTitleFontSize(0.06);

	//Draw QA1
	TCanvas* c1 = new TCanvas("adcQaAll", "AdcQaAll", 1600, 900);
	c1->Divide(2, 2);
	for (int a=0; a<4; a++)
	{
		c1->cd(a+1)->SetLogz();
		gPad->SetRightMargin(0.125);
		H2AdcQa1[a]->SetStats(false);
		H2AdcQa1[a]->Draw("colz");
	}
	if (PRINT) c1->Print(Form("%s.png", c1->GetName()));

	//-------------------------------------------

	//Do rough QA + Draw QA1A
	TCanvas* c1a = new TCanvas("adcQaHit", "AdcQaHit", 1600, 900);
	c1a->Divide(2, 2);
	TH1F* H1AdcQa1A[4];
	vector<int> HotList[4];
	for (int a=0; a<4; a++)
	{
		const int nCh = iToCh[a].size();
		if (nCh != H2AdcQa1[a]->GetNbinsY()) cout <<"WARNING! # of channels doesn't match!" <<endl;
		TH1F* H1Temp = (TH1F*)H2AdcQa1[a]->ProjectionY(); //Integrate over all runs
		H1Temp->Scale(1./nRun);

		//Get QA histogram
		const float xMin = 1.e-3;
		const float xMax = H1Temp->GetMaximum()*1.01;
		H1AdcQa1A[a] = new TH1F(Form("AdcQa1A_d%i", a+8), "", 150,xMin,xMax);
		H1AdcQa1A[a]->SetTitle(Form("detId=%i, Drawn x: [%4.3f, %4.3f];nHit/nTrig/nRun", a+8, xMin, xMax));
		H1AdcQa1A[a]->Sumw2();

		//Fill
		for (int b=0; b<nCh; b++)
		{
			const float nScaledHit = H1Temp->GetBinContent(b+1);
			H1AdcQa1A[a]->Fill(nScaledHit);
		}
		c1a->cd(a+1);
		H1AdcQa1A[a]->DrawCopy("hist e");

		//Get rough hot channel cut
		const float tempM = H1AdcQa1A[a]->GetMean();
		const float tempR = H1AdcQa1A[a]->GetRMS();
		const float tempC = tempM + tempR * cutSig;
		TLine* L1 = new TLine(tempC, 0, tempC, H1AdcQa1A[a]->GetMaximum());
		L1->SetLineColor(2);
		L1->SetLineStyle(2);
		L1->SetLineWidth(2);
		L1->Draw("same");

		for (int b=0; b<nCh; b++)
		{
			const int ch = iToCh[a][b];
			const float nScaledHit = H1Temp->GetBinContent(b+1);
			if (nScaledHit > tempC)
			{
				cout <<Form("Potential hot channels: %2i, %3i", a+8, ch) <<endl;
				HotList[a].push_back(ch);
			}
		}

		H1Temp->Delete();
	}//a, detId
	cout <<Form("Total: %i", HotList[0].size() + HotList[1].size() + HotList[2].size() + HotList[3].size()) <<endl;
	if (PRINT) c1a->Print(Form("%s.png", c1a->GetName()));

	//-------------------------------------------

	//Draw QA2
	bool showMark = false;
	bool showHitE = false;
	bool showMass = false;
	for (int a=0; a<4; a++) { if (chMarked[a].size() != 0) { showMark = true; break; } }
	for (int a=0; a<4; a++) { if (chToGain[a].size()!=0 && chToGainCorr[a].size()!=0) { showHitE = true; break; } }
	for (int a=0; a<4; a++) { DumTH2F[a] = new TH2F(); if (H2Mass[a]->GetEntries() > 0) { showMass = true; break; } }

	TCanvas* c2;
	const int nCOL = 4;
	const int nPAD = 16;
	int iCVS = 0;
	int iPAD = 1;
	for (int a=0; a<4; a++) //detId
	{
		if (!showMark)
		{
			iCVS = 0;
			iPAD = 1;
		}

		TH2F* H2Temp = (TH2F*)H2AdcQa2[a]->Clone();
		const int adcMinBin = H2AdcQa2[a]->GetYaxis()->FindBin(adcMin + 0.1); //Cut out pedestal tail
		for (int x=0; x<H2AdcQa2[a]->GetNbinsX(); x++)
		for (int y=0; y<adcMinBin; y++)
		{
			H2Temp->SetBinContent(x+1, y+1, 0);
			H2Temp->SetBinError  (x+1, y+1, 0);
		}

		//Get each ADC spectrum
		for (unsigned int b=0; b<iToCh[a].size(); b++)
		{
			const int ch = iToCh[a][b];

			//Draw only specified, if showMark is true
			if (showMark)
			{
				bool drawThis = false;
				for (unsigned int i=0; i<chMarked[a].size(); i++) { if (ch == chMarked[a][i]) drawThis = true; }
				if (!drawThis) continue;
			}

			//Generate new canvas
			if (iPAD == 1)
			{
				const int REX = 400*nCOL;
				const int REY = 225*nPAD/nCOL;
				c2 = new TCanvas(Form("adcQaSep%s_%i", showMark?"":Form("_d%i", a+8), iCVS), "", REX, REY);
				c2->SetTitle(c2->GetName());
				c2->Divide(nCOL, nPAD/nCOL);
			}
			c2->cd(iPAD)->SetLogy();

			//Draw
			TH1F* H1 = (TH1F*)H2Temp->ProjectionY(Form("d%i_ch%i", a+8, ch), ch, ch);
			H1->SetStats(true);
			H1->GetYaxis()->SetLabelSize(0.055);
			string Title = chToInfo[a][ch];
			if (!strcmp(Title.c_str(), "")) Title = Form("d%i_ch%i", a+8, ch);
			if (H1->GetXaxis()->GetBinUpEdge(H1->GetNbinsX()) > 250)
			{
				H1->GetXaxis()->SetRangeUser(0, 250);
				const int nHitOverflow = H1->Integral(251, H1->GetNbinsX());
				Title = Form("%s, ADC > 250: %i", Title.c_str(), nHitOverflow);
			}
			for (unsigned int c=0; c<HotList[a].size(); c++) { if (ch == HotList[a][c]) H1->SetLineColor(2); }
			if (chToBS[a].size()!=0 && chToBS[a][ch]>0)
			{
				const float scaleF = pow(2, chToBS[a][ch]);
				H1->Scale(1./scaleF);
				Title = Form("%s, Scaled (1/2^{%i})", Title.c_str(), chToBS[a][ch]);
			}
			if (showHitE) Title = Form("%s, GC = %4.3f", Title.c_str(), chToGainCorr[a][ch]);
			H1->SetTitle(Title.c_str());
			H1->DrawCopy("hist e");

			//Draw hit e distribution with 2nd y axis
			if (showHitE)
			{
				TH1F* H1e = new TH1F(Form("%s_hitE", H1->GetName()), ";hit E", 1000, 0, 250);
				for (int x=0; x<H1->GetNbinsX(); x++)
				{
					if (x > 250) continue;
					const float tempGain     = chToGain[a][ch];
					const float tempGainCorr = chToGainCorr[a][ch];
					const float tempHitE     = H1->GetBinCenter(x+1) * tempGain * tempGainCorr * 10; //x10

					const int xBin = H1e->GetXaxis()->FindBin(tempHitE);
					H1e->SetBinContent(xBin, H1->GetBinContent(x+1));
					H1e->SetBinError  (xBin, H1->GetBinError  (x+1));
				}

				H1e->SetLineColor(4);
				H1e->DrawCopy("hist same");
			}

			//Draw mass
			if (showMass)
			{
				TPad* tempPad = new TPad(Form("tempPad_d%i_ch%i", a+8, ch), "", 0,0,1,1);
				tempPad->SetFillColor(0);
				tempPad->SetFillStyle(4000);
				tempPad->SetFrameFillStyle(0);
				tempPad->SetGrid(0, 0);
				tempPad->SetTicks(0, 0);
				tempPad->Draw();
				tempPad->cd();

				TH1F* H1Mass = (TH1F*)H2Mass[a]->ProjectionY(Form("d%i_ch%i_mass", a+8, ch), ch, ch);
				H1Mass->SetLineColor(6);
				H1Mass->SetStats(true);
				H1Mass->GetXaxis()->SetLabelSize(0);
				H1Mass->GetXaxis()->SetTitleSize(0);
				H1Mass->GetYaxis()->SetLabelSize(0);
				H1Mass->GetYaxis()->SetTitleSize(0);
				H1Mass->DrawCopy("hist e");

				TLine* L1Mass = new TLine(0.135, 0, 0.135, H1Mass->GetMaximum());
				L1Mass->SetLineColor(6);
				L1Mass->SetLineStyle(2);
				L1Mass->SetLineWidth(2);
				L1Mass->Draw("same");
				//H1Mass->Delete();
			}

			//Update pad/canvas index
			iPAD++;
			if ( (iPAD == nPAD+1) || (!showMark && b==iToCh[a].size()-1) )
			{
				if (PRINT) c2->Print(Form("%s.png", c2->GetName()));
				iPAD = 1;
				iCVS++;
			}
		}//b, channel

		H2Temp->Delete();
	}//a, detId
	if (PRINT && showMark) c2->Print(Form("%s.png", c2->GetName()));

	if (PRINT)
	{
        gSystem->Exec("mkdir -p adcQa");
        gSystem->Exec("mv adcQa*.png adcQa");
	}
	#endif

	return;
}//DrawAdcQa

//-----------
void DrawMap(
		map<int, int> iToCh[4],
		map<int, st_pos> chToPos[4],
		const char* mapName = "FMS",
		bool smallOnly = false,
		bool showEta   = false,
		bool PRINT     = false,
		map<int, float> chToGainCorr[4] = DumMapF,
		map<int, int>   chToCellStat[4] = DumMapI,
		vector<int>     chMarked[4]     = DumVecI
		)
{
	/*
	   Draw 2D map of FMS cells with additional info
	   - iToCh and chToPos are essentially required
	   - Use chToGainCorr, chToCellStat, and chMarked by necessaity:
	   	 if not needed, provide dummy map on top of this code as argument
	*/

	bool showGain = false;
	bool showStat = false;
	bool showMark = false;
	for (int a=0; a<4; a++) { if (chToGainCorr[a].size()!=0) {showGain = true; break;} }
	for (int a=0; a<4; a++) { if (chToCellStat[a].size()!=0) {showStat = true; break;} }
	for (int a=0; a<4; a++) { if (chMarked[a].size()!=0)     {showMark = true; break;} }

	//-------------------------------------------

	gStyle->SetOptDate(0);
	gStyle->SetOptStat(0);

	//Draw map frame
	TCanvas* c1 = new TCanvas(Form("Map%s", mapName), mapName, PRINT?2700:1000, PRINT?2700:1000);
	c1->cd();
	const float fBin = (int)(smallOnly?110:210);
	const float fEnd = fBin/2;
	TH2F* H2F = new TH2F(Form("FRM%s", mapName), Form("%s;X;Y", c1->GetTitle()), fBin,-fEnd,fEnd, fBin,-fEnd,fEnd);
	if (showGain)
	{
		gPad->SetRightMargin(0.125);
		H2F->SetMaximum(5.0);
		H2F->Fill(fEnd, fEnd, H2F->GetMaximum());
	}
	H2F->GetYaxis()->SetTitleOffset(1.25);
	H2F->Draw("colz 9");

	//Draw rings of eta
	if (showEta)
	{
		const int   nRings    = smallOnly?10:8;
		const float stepWidth = smallOnly?0.1:0.2;
		const float minEta    = 4.1 - nRings*stepWidth;
		H2F->SetTitle(Form("%s, %2.1f #leq #eta #leq 4.1", c1->GetTitle(), minEta));
		for (int i=0; i<=nRings; i++)
		{
			const float tempEta    = 4.1 - stepWidth * i;
			const float tempTheta  = 2 * atan(exp(-tempEta));
			const float tempRadius = tan(tempTheta) * 720.0;
			TEllipse *E1 = new TEllipse(0, 0, tempRadius);
			E1->SetFillColor(0);
			E1->SetFillStyle(4000);
			E1->SetLineColor(2);
			E1->SetLineWidth(2);
			E1->SetLineStyle(2);
			E1->Draw("same");
		}
	}

	//Draw each cell
	for (unsigned int a=0; a<4; a++)
	for (unsigned int b=0; b<iToCh[a].size(); b++)
	{
		if (smallOnly && a<2) continue;

		const int ch = iToCh[a][b];
		const float x = chToPos[a][ch].chX;
		const float y = chToPos[a][ch].chY;
		const float cellHW = a<2?(5.8/2):(3.8/2);

		int boxColor = 33; //Light cyan
		if (showGain)
		{
			const float gainCorr = chToGainCorr[a][ch];
			if (fabs(gainCorr) >= H2F->GetMaximum()) boxColor = 100;
			else boxColor = (int)(fabs(gainCorr * 50.)/H2F->GetMaximum()) + 51;
		}

		if (showStat)
		{
			const int cellStat = chToCellStat[a][ch];
			if      (cellStat == DEAD) boxColor = showGain?17:0;
			else if (cellStat == BAD)  boxColor = 1;
			if (!showGain && cellStat == CONVERGED) boxColor = 220; //Light gold
		}

        TPaveText* PT = new TPaveText(x-cellHW, y-cellHW, x+cellHW, y+cellHW);
        PT->SetFillStyle(3001);
        PT->SetFillColor(boxColor);
        PT->SetTextColor(kBlack);
        PT->AddText(Form("%3i", ch));
        if (showGain) PT->AddText(Form("%4.3f", chToGainCorr[a][ch]));
        PT->Draw("9");

		if (showGain && chToCellStat[a][ch]==CONVERGED)
		{
			TMarker* MK = new TMarker(x, y, 20);
			MK->SetMarkerColor(220);
			if (boxColor>85 && boxColor<95) { MK->SetMarkerStyle(24); MK->SetMarkerColor(1); }
			MK->SetMarkerSize(PRINT?1.1:0.4);
			MK->Draw("same");
		}
		if (showStat && chToCellStat[a][ch]==DEAD)
		{
			TLine* DL1 = new TLine(x-cellHW,y-cellHW,x+cellHW,y+cellHW); DL1->SetLineWidth(2); DL1->Draw("same");
			TLine* DL2 = new TLine(x-cellHW,y+cellHW,x+cellHW,y-cellHW); DL2->SetLineWidth(2); DL2->Draw("same");
		}
		if (showMark)
		{
			bool Marked = false;
			for (unsigned int c=0; c<chMarked[a].size(); c++) { if (ch == chMarked[a][c]) Marked = true; }
			if (!Marked) continue;

			const int mCOL = 206; //Dark red
			PT->SetFillColor(mCOL);
			PT->SetTextColor(mCOL);
            TLine* LX1 = new TLine(x-cellHW,y-cellHW,x-cellHW,y+cellHW); LX1->SetLineColor(mCOL); LX1->Draw("same");
            TLine* LX2 = new TLine(x+cellHW,y-cellHW,x+cellHW,y+cellHW); LX2->SetLineColor(mCOL); LX2->Draw("same");
            TLine* LY1 = new TLine(x-cellHW,y-cellHW,x+cellHW,y-cellHW); LY1->SetLineColor(mCOL); LY1->Draw("same");
            TLine* LY2 = new TLine(x-cellHW,y+cellHW,x+cellHW,y+cellHW); LY2->SetLineColor(mCOL); LY2->Draw("same");
		}
	}

	if (PRINT) c1->Print(Form("%s%s%s.png", c1->GetName(), smallOnly?"_small":"", showMark?"_marked":""));

	return;
}//DrawMap

//--------------------
void DrawCompGainCorr(
		map<int, int> iToCh[4],
		map<int, int> chToCellStat[2][4],
		map<int, float> chToGainCorr[2][4],
		const char* setName1 = "Set1",
		const char* setName2 = "Set2",
		bool PRINT = false,
		bool excBadDead = false,
		bool convOnly = false
		)
{
	//Draw comparison between two different gainCorr set

	TH1F*   H1[2][4]; //Entries vs. gainCorr
	TGraph* G1[2][4]; //gainCorr vs. ch
	TGraph* G2[4];    //gainCorr difference divided by 1st set
	for (int i=0; i<2; i++) //Set
	for (int a=0; a<4; a++) //detId
	{
		H1[i][a] = new TH1F(Form("H1GainCorr_%i_d%i", i, a+8), "", 105, -0.1, 4.1);
		G1[i][a] = new TGraph();
		G1[i][a]->SetName(Form("G1GainCorr_%i_d%i", i, a+8));
		if (i==0) 
		{
			G2[a] = new TGraph();
			G2[a]->SetName(Form("G2GainCorrDelta_d%i", a+8));
		}

		for (unsigned int b=0; b<iToCh[a].size(); b++)
		{
			const int ch = iToCh[a][b];

			if (convOnly &&
				chToCellStat[0][a][ch]!=CONVERGED &&
				chToCellStat[1][a][ch]!=CONVERGED) continue;

			if (excBadDead &&
				(chToCellStat[0][a][ch]==BAD ||
				 chToCellStat[1][a][ch]==BAD ||
				 chToCellStat[0][a][ch]==DEAD ||
				 chToCellStat[1][a][ch]==DEAD) ) continue;

			const float maxGC = H1[i][a]->GetXaxis()->GetBinUpEdge(H1[i][a]->GetNbinsX());
			float tempGC = chToGainCorr[i][a][ch];
			if (tempGC > maxGC) tempGC = maxGC;

			H1[i][a]->Fill(tempGC);
			G1[i][a]->SetPoint(G1[i][a]->GetN(), ch, tempGC);
			if (i==0)
			{
				const float GC1 = chToGainCorr[0][a][ch];
				const float GC2 = chToGainCorr[1][a][ch];
				if (GC1==0. || GC2==0.) continue;

				//Enforce min/max to 1.5, otherwise b option in TGraph won't draw it
				float dGC = (GC1 - GC2)/GC1;
				if (dGC < -1.5) dGC = -1.5;
				if (dGC > +1.5) dGC = +1.5;
				G2[a]->SetPoint(G2[a]->GetN(), ch, dGC);
			}
		}//b
	}//i, a

	//-------------------------------------------

	gStyle->SetOptDate(0);
	gStyle->SetOptStat(0);

	const char* TitleOp = "";
	if (excBadDead) TitleOp = Form("%s, Excluded Bad/Dead", TitleOp);
	if (convOnly)   TitleOp = Form("%s, Converged Only", TitleOp);

	TCanvas* c1[3];
	for (int x=0; x<3; x++)
	{
		c1[x] = new TCanvas(Form("GCcomp%i_%sVs%s", x, setName1, setName2), "", 1600, 900);
		c1[x]->SetTitle(c1[x]->GetName());
		c1[x]->Divide(2, 2);
	}

	for (int i=0; i<2; i++)
	for (int a=0; a<4; a++)
	{
		c1[0]->cd(a+1);
        H1[i][a]->SetLineColor(i+1);
        if (i==0)
        {
            H1[i][a]->SetTitle(Form("detId=%i%s;gainCorr", a+8, TitleOp));
            const float max0 = H1[0][a]->GetMaximum();
            const float max1 = H1[1][a]->GetMaximum();
            if (max1 > max0) H1[0][a]->SetMaximum(max1*1.1);
        }
        if (i==1)
        {
            H1[i][a]->SetFillColor(i+1);
            H1[i][a]->SetFillStyle(3001);
            TLegend *L1 = new TLegend(0.70, 0.75, 0.95, 0.95);
            L1->SetMargin(0.2);
            L1->SetTextAlign(22);
            L1->AddEntry(H1[0][a], Form("%s, RMS: %4.3f", setName1, H1[0][a]->GetRMS()), "l");
            L1->AddEntry(H1[1][a], Form("%s, RMS: %4.3f", setName2, H1[1][a]->GetRMS()), "lf");
            L1->Draw("same");
        }
        H1[i][a]->DrawCopy(i==0?"hist e":"hist ef same");

		c1[1]->cd(a+1);
		const char* TitleG1 = Form("detId=%i%s;ch;gainCorr", a+8, TitleOp);
        if (i==0) gPad->DrawFrame(-10,-0.2,a<2?588:298,4.2, TitleG1);
        G1[i][a]->SetMarkerSize(0.85);
        G1[i][a]->SetMarkerColor(i+1);
        G1[i][a]->SetMarkerStyle(i*4 + 20);
        G1[i][a]->Draw("p same");
        if (i==1 && a==0)
        {
            TLegend *L2 = new TLegend(0.65, 0.80, 0.95, 0.95);
            L2->SetMargin(0.2);
            L2->SetTextAlign(22);
            L2->AddEntry(G1[0][a], setName1, "p");
            L2->AddEntry(G1[1][a], setName2, "p");
            L2->Draw("same");
        }

        if (i==0) 
		{
			c1[2]->cd(a+1);
			const char* TitleG2 = Form("detId=%i%s;ch;#DeltagainCorr (%s-%s)/%s",
					a+8, TitleOp, setName1, setName2, setName1);
			gPad->DrawFrame(-10, -1.5, a<2?588:298, 1.5, TitleG2);
			G2[a]->SetFillColor(2);
			G2[a]->SetFillStyle(3001);
			G2[a]->Draw("b same");
		}
	}//i, a

	if (PRINT)
	{
		for (int x=0; x<3; x++)
		{
			c1[x]->Print(Form("%s%s%s.png", c1[x]->GetName(), excBadDead?"_excBD":"", convOnly?"_conv":""));
		}
	}
	
	return;
}//DrawCompGainCorr

//-------------------
void DrawCompMassAll(
		map<int, int> iToCh[4],
		const int nComp,
		TH1F* (*H1)[4],
		bool Normalize = false,
		bool PRINT = false
		)
{
	/*
	   Draw comparison of mass distributions
	   - Draw inclusive mass separated by detId
	   - Can enforce nomalization by its own height to compare width
	*/

	gStyle->SetOptDate(0);
	gStyle->SetOptStat(0);

	TCanvas* c1 = new TCanvas("massAll", "massAll", 1600, 900);
	c1->Divide(2, 2);

	for (int a=0; a<4; a++)
	{
		c1->cd(a+1);

		TLegend* LEG = new TLegend(0.6, 0.85-nComp*0.055, 0.85, 0.85);
		LEG->SetMargin(0.3);

		float yMax = 0;
		for (int i=0; i<nComp; i++)
		{
			if (H1[i][a]->GetMaximum()*1.1 > yMax) yMax = H1[i][a]->GetMaximum()*1.1;
		}

		for (int i=0; i<nComp; i++)
		{
			TH1F* H1Temp = (TH1F*)H1[i][a]->Clone(Form("d%i_i%i", a+8, i));
			H1Temp->SetTitle(Form("detId=%i", a+8));

			if (Normalize) H1Temp->Scale(1/H1Temp->GetMaximum());
			else H1Temp->SetMaximum(yMax);

			H1Temp->SetLineColor((i+1)!=5?(i+1):95);
			if (i==nComp-1)
			{
				H1Temp->SetFillColor(H1Temp->GetLineColor());
				H1Temp->SetFillStyle(3001);
			}
			H1Temp->DrawCopy(i==0?"hist e":"hist e same");

			//Vertical pi0 line
			if (i==nComp-1)
			{
				TLine* L1 = new TLine(0.135, 0, 0.135, H1Temp->GetMaximum());
				L1->SetLineColor(1);
				L1->SetLineStyle(2);
				L1->SetLineWidth(2);
				L1->Draw("same");
			}

			//Legend: get title from given TH1 histogram, NOT temporary one
			LEG->AddEntry(H1Temp, H1[i][a]->GetTitle(), "lf");
			if (i==nComp-1) LEG->Draw("same");
		}//i, iteration index
	}//a, detId
	if (PRINT) c1->Print(Form("%s%s.png", c1->GetName(), Normalize?"_norm":""));

	return;
}//DrawCompMassAll

//-------------------
void DrawCompMassSep(
		map<int, int> iToCh[4],
		const int nComp,
		TH2F* (*H2)[4],
		bool Normalize = false,
		bool PRINT = false,
		map<int, string> chToInfo[4] = DumMapS,
		vector<int> chMarked[4] = DumVecI
		)
{
	/*
	   Draw comparison of mass distributions
	   - Draw mass distribution cell by cell
	   - Can draw only a few selective channels in chMarked
	   - Can enforce nomalization by its own height to compare width
	*/

	bool showMark = false;
	for (int a=0; a<4; a++) { if (chMarked[a].size() != 0) { showMark = true; break; } }

	//-------------------------------------------

	gStyle->SetOptDate(0);
	gStyle->SetOptStat(0);
	gStyle->SetTitleFontSize(0.06);

	TCanvas* c1;
	const int nCOL = 4;
	const int nPAD = 16;
	int iCVS = 0;
	int iPAD = 1;

	for (int a=0; a<4; a++) //detId
	{
		if (!showMark)
		{
			iCVS = 0;
			iPAD = 1;
		}

		for (unsigned int b=0; b<iToCh[a].size(); b++)
		{
			const int ch = iToCh[a][b];

			//Draw only specified, if showMark is true
			if (showMark)
			{
				bool drawThis = false;
				for (unsigned int i=0; i<chMarked[a].size(); i++) { if (ch == chMarked[a][i]) drawThis = true; }
				if (!drawThis) continue;
			}

			//Generate new canvas
			if (iPAD == 1)
			{
				const int REX = 400*nCOL;
				const int REY = 225*nPAD/nCOL;
				c1 = new TCanvas(Form("massSep%s_%i", showMark?"":Form("_d%i", a+8), iCVS), "", REX, REY);
				c1->SetTitle(c1->GetName());
				c1->Divide(nCOL, nPAD/nCOL);
			}
			c1->cd(iPAD);

			//Get maximum
			float yMax = 0;
			for (int i=0; i<nComp; i++)
			{
				TH1F* H1 = (TH1F*)H2[i][a]->ProjectionY("H1Temp", ch, ch);
				if (H1->GetNbinsX() == 250) H1->Rebin(5);
				if (H1->GetMaximum()*1.1 > yMax) yMax = H1->GetMaximum()*1.1;
				H1->Delete();
			}

			//Draw
			for (int i=0; i<nComp; i++)
			{
				TH1F* H1 = (TH1F*)H2[i][a]->ProjectionY(Form("d%i_ch%i_%i", a+8, ch, i), ch, ch);
				if (H1->GetNbinsX() == 250) H1->Rebin(5);
				if (Normalize && H1->GetEntries()!=0) H1->Scale(1/H1->GetMaximum());
				else H1->SetMaximum(yMax);

				if (i==0)
				{
					string Title = chToInfo[a][ch];
					if (!strcmp(Title.c_str(), "")) Title = Form("d%i_ch%i", a+8, ch);
					H1->SetTitle(Title.c_str());
				}

				H1->SetLineColor((i+1)!=5?(i+1):95);
				if (i==nComp-1 && i!=0)
				{
					H1->SetFillColor(H1->GetLineColor());
					H1->SetFillStyle(3001);
				}
				H1->DrawCopy(i==0?"hist e":"hist e same");

				if (i==nComp-1)
				{
					TLine* L1 = new TLine(0.135, 0, 0.135, H1->GetMaximum());
					L1->SetLineColor(1);
					L1->SetLineStyle(2);
					L1->SetLineWidth(2);
					L1->Draw("same");
				}

				H1->Delete();
			}//i

			//Update pad/canvas index
			iPAD++;
			if ( (iPAD == nPAD+1) || (!showMark && b==iToCh[a].size()-1) )
			{
				if (PRINT) c1->Print(Form("%s%s.png", c1->GetName(), Normalize?"_norm":""));
				iPAD = 1;
				iCVS++;
			}
		}//b, channel
	}//a, detId
	if (PRINT && showMark) c1->Print(Form("%s%s.png", c1->GetName(), Normalize?"_norm":""));

	if (PRINT)
	{
        gSystem->Exec(Form("mkdir -p massComp%s", Normalize?"_norm":""));
        gSystem->Exec(Form("mv massSep*.png massComp%s", Normalize?"_norm":""));
	}

	return;
}//DrawCompMassSep

//--------------------
void DrawConvProgress(
		const int nIter,
		const char* pathIter,
		map<int, int>iToCh[4],
		bool excBadDead = false, //Always get bad/dead cells from 1st available FmsCellStat.txt
		bool PRINT = false
		)
{
	//Draw progress of convergence over iterations

	int nCellAll = 1264;
	int nCellSep[4];
	for (unsigned int a=0; a<4; a++) nCellSep[a] = iToCh[a].size();

	TGraphErrors* G1All = new TGraphErrors();
	TGraphErrors* G1Sep[4];
	for (int a=0; a<4; a++) G1Sep[a] = new TGraphErrors();

	bool checkedFirstSet = false;
	for (int i=0; i<nIter; i++)
	{
		const char* cellStatFile = Form("%s/FmsCellStat_%i.txt", pathIter, i);
		if (FILE *FTest = fopen(cellStatFile, "r")) fclose(FTest);
		else continue;

		map<int, int> chToCellStatIter[4];
		GetMapChToCellStat(cellStatFile, chToCellStatIter);

		//Check # of valid cells
		if (excBadDead==true && checkedFirstSet==false)
		{
			for (unsigned int a=0; a<4; a++)
			{
				for (unsigned int b=0; b<iToCh[a].size(); b++)
				{
					const int ch = iToCh[a][b];
					const int cellStat = chToCellStatIter[a][ch];
					if (cellStat==BAD || cellStat==DEAD)
					{
						nCellAll--;
						nCellSep[a]--;
					}
				}//b
				cout <<Form("# of valid cells (detId=%2i): %3i", a+8, nCellSep[a]) <<endl;
			}//a
			cout <<Form("# of valid cells (total): %i", nCellAll) <<endl;
			checkedFirstSet = true;
		}

		//Count # of converged cells in this iteration
		int   nConvAll = 0;
		int   nConvSep[4] = {0};
		float rConvSep[4] = {0};
		for (int a=0; a<4; a++)
		{
			for (unsigned int b=0; b<iToCh[a].size(); b++)
			{
				const int ch = iToCh[a][b];
				if (chToCellStatIter[a][ch] == CONVERGED)
				{
					nConvAll++;
					nConvSep[a]++;
				}
			}
			rConvSep[a] = (float)nConvSep[a]/(float)nCellSep[a];
			G1Sep[a]->SetPoint(G1Sep[a]->GetN(), i, rConvSep[a]);
		}//a
		const float rConvAll = (float)nConvAll/(float)nCellAll;
		G1All->SetPoint(G1All->GetN(), i, rConvAll);
	}//i, iterations

	gStyle->SetOptDate(0);
	gStyle->SetOptStat(0);

	TLegend* LEG = new TLegend(0.55, 0.225, 0.825, 0.375);
	LEG->SetMargin(0.35);
	TCanvas* c1 = new TCanvas("convProg", "Convergence ratio over iterations", 1600, 900);
	c1->Divide(2, 2);
	for (int a=0; a<4; a++)
	{
		const char* tempTitle1 = excBadDead?", Excluded Bad/Dead":"";
        const char* tempTitle2 = Form("detId=%i%s;Iteration index;Convergence ratio", a+8, tempTitle1);
        c1->cd(a+1)->DrawFrame(-1, -0.05, nIter, 1.05, tempTitle2);
        G1Sep[a]->SetMarkerSize(1.1);
        G1Sep[a]->SetMarkerStyle(20);
        G1Sep[a]->Draw("pl same");

        if (a==0)
        {
            G1All->SetLineColor(2);
            G1All->SetMarkerColor(2);
            G1All->SetMarkerSize(1.1);
            G1All->SetMarkerStyle(24);

            LEG->AddEntry(G1Sep[a], Form("Ratio (each detId)"), "pl");
            LEG->AddEntry(G1All, Form("Ratio (all)"), "pl");
            LEG->Draw("same");
        }
        G1All->Draw("pl same");
    }
    if (PRINT) c1->Print(Form("%s%s.png", c1->GetName(), excBadDead?"_excBD":""));

	return;
}//DrawConvProgress

//----------------
void DrawIterMass(
		const int nIter,
		const char* pathIter,
		map<int, int> iToCh[4],
		map<int, int> chToCellStat[4],
		bool PRINT = false,
		vector<int> chMarked[4] = DumVecI
		)
{
	//Draw a cell's mass distribution over multiple iterations

	bool showMark = false;
	for (int a=0; a<4; a++) { if (chMarked[a].size() != 0) { showMark = true; break; } }

	//-------------------------------------------

	gStyle->SetOptDate(0);

	TFile* F[nIter];
	TCanvas* c1;
	const int nCOL = 3;
	const int nPAD = 9;
	int iCVS = 0;
	int iPAD = 1;

	for (int a=0; a<4; a++) //detId
	{
		if (!showMark)
		{
			iCVS = 0;
			iPAD = 1;
		}

		for (unsigned int b=0; b<iToCh[a].size(); b++)
		{
			const int ch = iToCh[a][b];
			if (!showMark && (chToCellStat[a][ch]==BAD || chToCellStat[a][ch]==DEAD))
			{
				cout <<Form("Invalid channel: detId%i_ch%i, skip...", a+8, ch) <<endl;
				continue;
			}

			//Draw only specified, if showMark is true
			if (showMark)
			{
				bool drawThis = false;
				for (unsigned int i=0; i<chMarked[a].size(); i++) { if (ch == chMarked[a][i]) drawThis = true; }
				if (!drawThis) continue;
			}

			//Loop over iteration index
			iCVS = 0;
			iPAD = 1;
			for (int i=0; i<nIter; i++)
			{
				F[i] = TFile::Open(Form("%s/out_fmsCalib_%i.root", pathIter, i));
				if (iPAD == 1)
				{
					c1 = new TCanvas(Form("mass_d%i_ch%i_%i", a+8, ch, iCVS), "", 400*nCOL, 300*(nPAD/nCOL));
					c1->SetTitle(c1->GetName());
					c1->Divide(nCOL, nPAD/nCOL);
				}

				//Draw
				TCanvas* tempCVS = (TCanvas*)F[i]->Get(Form("mass_d%i_ch%i", a+8, ch))->Clone();
				c1->cd(iPAD);
				tempCVS->DrawClonePad();
				iPAD++;

				//Update pad/canvas
				if ( (iPAD==nPAD+1) || (iPAD>nIter) || (!showMark && b==iToCh[a].size()) || (showMark && i==nIter-1) )
				{
					if (PRINT) c1->Print(Form("%s.png", c1->GetName()));
					iPAD = 1;
					iCVS++;
				}
			}//i, iteration

			for (int i=0; i<nIter; i++) F[i]->Close(); //FUCKING STUPID ROOT...
		}//b, channel
	}//a, detId

	if (PRINT)
	{
        gSystem->Exec("mkdir -p iterMass");
        gSystem->Exec("mv *.png iterMass");
	}

	return;
}//DrawIterMass

//----------------------
void DrawIterMassSingle(
		const int iIter,
		const char* pathIter,
		vector<int> chMarked[4] = DumVecI,
		const char* outDir = "iterMass"
		)
{
	//Printout entire channel by channel mass distribution of an iteration index

	bool showMark = false;
	for (int a=0; a<4; a++) { if (chMarked[a].size() != 0) { showMark = true; break; } }

	//-------------------------------------------

	gStyle->SetOptDate(0);

	TFile* F = TFile::Open(Form("%s/out_fmsCalib_%i.root", pathIter, iIter));
	if (!F || F->IsZombie()) return;

    TDirectory* cDir = gDirectory;
    TIter keyNext(cDir->GetListOfKeys());
    TKey* keyOld = 0;
    TKey* keyNew;
    TObject* obj;

    while (keyNew = (TKey*)keyNext())
    {
		if (showMark)
		{
			bool Proceed = false;
			for (unsigned int a=0; a<4; a++)
			for (unsigned int b=0; b<chMarked[a].size(); b++)
			{
				const char* chMarkedName = Form("mass_d%i_ch%i", a+8, chMarked[a][b]);
				if (!strcmp(chMarkedName, keyNew->GetName())) Proceed = true;
			}
			if (!Proceed) continue;
		}

        if (keyOld && !strcmp(keyOld->GetName(), keyNew->GetName())) continue;
        obj = keyNew->ReadObj();
		obj->Draw();
        obj->Print(Form("%s.png", obj->GetName()));
    }

    gSystem->Exec(Form("mkdir -p %s_i%i", outDir, iIter));
    gSystem->Exec(Form("mv mass_*.png %s_i%i", outDir, iIter));
	F->Close();

	return;
}//DrawIterMassSingle

//-----------------
void DrawIterPars(
		const int nIter,
		const char* pathIter,
		map<int, int> iToCh[4],
		map<int, int> chToCellStat[4], //Use cellStat file in final iteration index
		bool PRINT = false,
		vector<int> chMarked[4] = DumVecI
		)
{
	/*
	   Draw parameters vs. iteration index, channel by channel

	   - Chi2/NDF, Mass (obtained by fit), Width, and gainCorr applied
	   - Can draw only a few selective channels in chMarked
	*/

	bool showMark = false;
	for (int a=0; a<4; a++) { if (chMarked[a].size() != 0) { showMark = true; break; } }

	//-------------------------------------------

	//Read iteration results
	map<int, float> chToGainCorr[nIter][4];
	map<int, st_fitR> chToFitR[nIter][4];
	for (int i=0; i<nIter; i++)
	{
		GetMapChToGainCorr(Form("%s/FmsGainCorr_%i.txt", pathIter, i), chToGainCorr[i]);
		GetMapChToFitR(Form("%s/out_fmsCalibFit_%i.txt", pathIter, i), chToFitR[i]);
	}

    gStyle->SetOptDate(0);
    gStyle->SetOptStat(0);
    gStyle->SetLabelSize(0.055, "xyz");
    gStyle->SetTitleSize(0.060, "xyz");
    gStyle->SetTitleOffset(0.75, "xyz");
    gStyle->SetTitleFontSize(0.055);

	for (unsigned int a=0; a<4; a++)
	for (unsigned int b=0; b<iToCh[a].size(); b++)
	{
		const int detId = a+8;
		const int ch    = iToCh[a][b];
		if (chToCellStat[a][ch]==BAD || chToCellStat[a][ch]==DEAD) continue;
	
		//Draw only specified, if showMark is true
		if (showMark)
		{
			bool drawThis = false;
			for (unsigned int i=0; i<chMarked[a].size(); i++) { if (ch == chMarked[a][i]) drawThis = true; }
			if (!drawThis) continue;
		}

		//Prepare graphs for chi2, mass, width, and gainCorr
		TGraphErrors* G1[4];
		for (int x=0; x<4; x++)
		{
			G1[x] = new TGraphErrors();
			G1[x]->SetName(Form("G1_d%i_ch%i_%i", detId, ch, x));
			G1[x]->SetLineColor(x+1);
			G1[x]->SetMarkerColor(x+1);
			G1[x]->SetMarkerStyle(20);
		}

		//Fill the graphs
		for (int i=0; i<nIter; i++)
		{
			const float chi2     = chToFitR[i][a][ch].chi2;
			const float mass     = chToFitR[i][a][ch].mass;
			const float massErr  = chToFitR[i][a][ch].massErr;
			const float width    = chToFitR[i][a][ch].width;
			const float widthErr = chToFitR[i][a][ch].widthErr;
			const float gainCorr = chToGainCorr[i][a][ch];

            G1[0]->SetPoint(G1[0]->GetN(), i, chi2);
            G1[1]->SetPoint(G1[1]->GetN(), i, mass);
            G1[2]->SetPoint(G1[2]->GetN(), i, width);
            G1[3]->SetPoint(G1[3]->GetN(), i, gainCorr);
            G1[1]->SetPointError(G1[1]->GetN()-1, 0, massErr);
            G1[2]->SetPointError(G1[2]->GetN()-1, 0, widthErr);
		}
		if (G1[0]->GetN() == 0) continue;

        TCanvas* c1 = new TCanvas(Form("d%i_ch%i", detId, ch), Form("detId=%i, ch=%i", detId, ch), 800, 1000);
        c1->Divide(1, 4);
        TLine *L1[4];
        for (int x=0; x<4; x++)
        {
			const char* statTitle = (chToCellStat[a][ch]==CONVERGED)?"CONVERGED, ":"";
			const char* subTitle = "";
            if (x==0) subTitle = Form("chi2;Iteration;chi2/NDF");
            if (x==1) subTitle = Form("mass;Iteration;mass");
            if (x==2) subTitle = Form("width;Iteration;width");
            if (x==3) subTitle = Form("gainCorr;Iteration;gainCorr");
			const char* lastTitle = Form("%s%s, %s", statTitle, c1->GetTitle(), subTitle);

            const float yMin = G1[x]->GetHistogram()->GetMinimum()*0.9;
            const float yMax = G1[x]->GetHistogram()->GetMaximum()*1.1;
            c1->cd(x+1)->DrawFrame(-1,yMin,nIter,yMax, lastTitle);
            G1[x]->Draw("lp");

            float yLine;
            if (x==0) yLine = 1.0;
            if (x==1) yLine = 0.135;
            if (x==2) yLine = (a<2)?0.02:0.03;
            if (x==3) yLine = 1.0;
            L1[x] = new TLine(-1, yLine, nIter, yLine);
            L1[x]->SetLineColor(6);
            L1[x]->SetLineStyle(2);
            L1[x]->SetLineWidth(2);
			if (yLine>yMin && yLine<yMax) L1[x]->Draw("same");
        }

		if (PRINT)
		{
            c1->Print(Form("iterPars_%s%s.png", c1->GetName(), (chToCellStat[a][ch]==CONVERGED)?"_CONV":""));
			for (int x=0; x<4; x++) { G1[x]->Delete(); L1[x]->Delete(); }
			delete c1;
		}

	}//a (detId), b (ch)

	if (PRINT)
	{
        gSystem->Exec("mkdir -p iterPars");
        gSystem->Exec("mv *.png iterPars");
	}

	return;
}//DrawIterPars
