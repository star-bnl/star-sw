#include "TBox.h"
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
#include "TLegendEntry.h"
#include "TLine.h"
#include "TPad.h"
#include "TPaveText.h"
#include "TString.h"
#include "TSystem.h"
#include "TStyle.h"

#include <cmath>
#include <fstream>
#include <iostream>
#include <map>
using namespace std;

typedef struct { int detId, ch, bs; float x, y; } st_info;

//Read bsQa results, then visualize it according to FMS geomery
//===================================================================================================================
void bsQaShowMap(const char* inList = "FmsBsGainSample.txt", const char* inMap = "fmsCellMap.txt", bool PRINT = true)
{
	std::vector<st_info> Info;

	//Read
	ifstream in1, in2;
	in1.open(inList);
	in2.open(inMap);
	if (!in1.is_open()) { cout <<"Cannot open file: " <<inList <<endl; return; }
	if (!in2.is_open()) { cout <<"Cannot open file: " <<inMap  <<endl; return; }
	while (in1.is_open())
	{
		int detId1, detId2, ch1, ch2, bs, valid;
		float x, y;
		in1 >> detId1 >> ch1 >> bs;
		in2 >> detId2 >> ch2 >> valid >> x >> y;

		if (!in1.good() || !in2.good()) break;
		if ((detId1!=detId2) || (ch1!=ch2)) { cout <<"WARNING! File format does NOT match!" <<endl; return; }
		if (valid != true) continue;
		//cout <<Form("%2i %3i %6.2f %6.2f %2i %s", detId1, ch1, x, y, bs, bs==0?"":"!") <<endl;

		st_info tempInfo;
		tempInfo.detId = detId1;
		tempInfo.ch    = ch1;
		tempInfo.bs    = bs;
		tempInfo.x     = x;
		tempInfo.y     = y;
		Info.push_back(tempInfo);
	}
	in1.close();
	in2.close();

	//Draw
	//-------------------------------------------

	string inListName = inList;
	std::size_t strPos = inListName.find(".txt");
	string outName = inListName.substr(0, strPos);

	gStyle->SetOptDate(0);
	gStyle->SetOptStat(0);

	TCanvas* c1 = new TCanvas(Form("bsQaMap_%s", outName.c_str()), outName.c_str(), 1200, 900);
	c1->cd()->SetRightMargin(0.3);
	TH2F* H2Frame = new TH2F("Frame", "", 210,-105,105, 210,-105,105);
	H2Frame->SetTitle(Form("%s;X;Y", c1->GetTitle()));
	H2Frame->Draw();

	int nCell[4] = {0}; //# of total valid cells, by detId
	int nNZBS[4] = {0}; //# of non-zero BS cells, by detId
	int nCell_nstb[4] = {0}; //# of non-zero BS cells, by nstb
	int nNZBS_nstb[4] = {0}; //# of non-zero BS eells, by nstb

	int Colors[] = {51, 56, 61, 66, 71, 17, 80, 85, 90, 95, 100};
	for (unsigned int a=0; a<Info.size(); a++)
	{
		st_info tempInfo = Info[a];
		const int detId = tempInfo.detId;
		const int bs = tempInfo.bs;
		const float x = tempInfo.x;
		const float y = tempInfo.y;
		const float cellHW = detId<10?(5.8/2):(3.8/2);

		nCell[detId-8]++;
		if (bs != 0) nNZBS[detId-8]++;

		int nstb = -1;
		if      (x>0 && y>0) nstb = 0; //ST
		else if (x>0 && y<0) nstb = 1; //SB
		else if (x<0 && y>0) nstb = 2; //NT
		else if (x<0 && y<0) nstb = 3; //NB
		nCell_nstb[nstb]++;
		if (bs != 0) nNZBS_nstb[nstb]++;

		TPaveText* pt = new TPaveText(x-cellHW, y-cellHW, x+cellHW, y+cellHW);
		pt->SetFillStyle(3001);
		pt->AddText(Form("%i", bs));
		pt->Draw("same");

		TBox* box = new TBox(x-cellHW, y-cellHW, x+cellHW, y+cellHW);
		box->SetFillColor(Colors[bs+5]);
		box->SetFillStyle(3001);
		box->SetLineColor(1);
		box->SetLineWidth(1);
		box->Draw("same");
	}

	TLegend* L1 = new TLegend(0.72, 0.1, 0.95, 0.9);
	L1->SetMargin(0.1);
	L1->AddEntry((TObject*)0, "Non-zero BS ratio", "");
	L1->AddEntry((TObject*)0, "", "");
	L1->AddEntry((TObject*)0, "All:", "");
	const int nCell_all = nCell[0] + nCell[1] + nCell[2] + nCell[2];
	const int nNZBS_all = nNZBS[0] + nNZBS[1] + nNZBS[2] + nNZBS[3];
	L1->AddEntry((TObject*)0, Form("%i/%i (%4.3f)", nNZBS_all, nCell_all, (float)nNZBS_all/(float)nCell_all), "");
	L1->AddEntry((TObject*)0, "", "");
	L1->AddEntry((TObject*)0, "by detId", "");
	for (int a=0; a<4; a++)
	{
		const char* ENT = Form("detId %i: %i/%i (%4.3f)", a+8,nNZBS[a],nCell[a],(float)nNZBS[a]/(float)nCell[a]);
		L1->AddEntry((TObject*)0, ENT, "");
	}
	L1->AddEntry((TObject*)0, "", "");
	L1->AddEntry((TObject*)0, "by NSTB", "");
	for (int i=0; i<4; i++)
	{
		const char* ENT = Form("%s%s: %i/%i (%4.3f)", i<2?"S":"N", i%2==0?"T":"B",
				nNZBS_nstb[i], nCell_nstb[i], (float)nNZBS_nstb[i]/(float)nCell_nstb[i]);
		L1->AddEntry((TObject*)0, ENT, "");
	}
	L1->AddEntry((TObject*)0, "", "");
	L1->Draw();

	if (PRINT) c1->Print(Form("%s.png", c1->GetName()));
	return;
}//Main
