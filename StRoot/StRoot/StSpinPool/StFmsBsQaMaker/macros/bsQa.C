#include "TCanvas.h"
#include "TF1.h"
#include "TFile.h"
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

//===================================================================================================================
vector<int> GetListBad(TH2F* ADC, TH2F* BS, TH2F* ChMap, int detId, int runNo, bool SHOW = false, bool PRINT = false)
{
	vector<int> List;
	if (BS->GetMaximum() != 1) { cout <<"WARNING! BS QA histogram is NOT normalized!" <<endl; return List; }

	//Get average # of hit
	const float nCh = (ADC->GetNbinsX()>300)?394:238;
	const float nHitAll = ADC->GetEntries();
	const float nHitAvg = nHitAll/nCh;

	//Bad channels decision
	for (int x=0; x<BS->GetNbinsX(); x++) //Bin # = channel
	{
		//0. Good (default)
		//1. Dead: # of occupied bit (nBitF) = 12 or # of entries < 1 % of average # of hit
		//2. Bad: ADC RMS < RMS cut (1.0, small RMS = ADCs are concentrated at certain #)
		int flagStat = 0;

		//Invalid ch or Dead?
		bool chValid = ChMap->GetBinContent(x+1, 1);
		const int nBit = BS->GetNbinsY();
		int nBitF = 0; //# of bit left filled
		for (int y=0; y<nBit; y++) { if (BS->GetBinContent(x+1, y+1) == 1) nBitF++; }
		if (chValid==false || nBitF==nBit)
		{
			flagStat = 1;
			List.push_back(flagStat); //Regard it as dead
			continue;
		}

		//Valid + Some bits are occupied. Now check ADC distribution itself
		TH1F* ADC_1ch = (TH1F*)ADC->ProjectionY("", x+1, x+1);
		const float tHit = ADC_1ch->GetEntries();
		const float tRMS = ADC_1ch->GetRMS();
		if (tHit < nHitAvg*1.e-2) flagStat = 1;
		else if (tRMS < 1.) flagStat = 2;
		ADC_1ch->Delete();

		List.push_back(flagStat);
	}//x, ch

	//Crosscheck functions
	//-------------------------------------------

	if (SHOW)
	{
		for (unsigned int x=0; x<List.size(); x++)
		{
			if (List[x] == false) continue; //Good ch
			if (ChMap->GetBinContent(x+1, 1) == false) continue; //Invalid ch
			TH1F* ADC_1ch = (TH1F*)ADC->ProjectionY("", x+1, x+1);
			const int nHit = ADC_1ch->GetEntries();
			cout <<Form("Bad: detId = %2i, ch = %3i / Flag = %i", detId, x+1, List[x]) <<endl;
			ADC_1ch->Delete();
		}
	}
	if (PRINT) //Use this in batch mode
	{
		TCanvas *c1 = new TCanvas("c1", "", 800, 800);
		for (unsigned int x=0; x<List.size(); x++)
		{
			if (List[x] == false) continue;
			if (ChMap->GetBinContent(x+1, 1) == false) continue; //Invalid ch
			c1->Clear();
			c1->SetName(Form("Bad_run%i_d%i_ch%i", runNo, detId, x+1));
			c1->cd();
			TH1F* ADC_1ch = (TH1F*)ADC->ProjectionY("", x+1, x+1);
			ADC_1ch->SetTitle(Form("Bad: run = %i detId = %2i, ch = %3i, flag = %i", runNo, detId, x+1, List[x]));
			ADC_1ch->GetXaxis()->SetRangeUser(0,250);
			ADC_1ch->DrawCopy("hist e");
			c1->Print(Form("%s.png", c1->GetName()));
		}
		delete c1;
	}

	return List;
}//GetListBad

//========================================================================
vector<int> GetListBS(TH2F* BS, TH2F* ChMap, int detId, bool SHOW = false)
{
	vector<int> List;
	if (BS->GetMaximum() != 1) { cout <<"WARNING! BS QA histogram is NOT normalized!" <<endl; return List; }

	//A channel can have only 1 type of BS: positive, 0, or negative
	const int nBit = BS->GetNbinsY();
	for (int x=0; x<BS->GetNbinsX(); x++) //Bin # (x+1) = channel
	{
		int bsValue = 0;
		bool chValid = ChMap->GetBinContent(x+1, 1);
		if (!chValid) { List.push_back(bsValue); continue; }

		int nBitFilled = 0;
		bool bsPos = false;
		bool bsNeg = false;
		for (int y=0; y<nBit; y++)
		{
			const int BC = BS->GetBinContent(x+1, y+1);
			if (BC==1) nBitFilled++;

			if (y==0 && BC==1) bsPos = true;
			if (y==nBit-1 && BC==1 && bsPos==false) bsNeg = true;
		}

		if (nBitFilled!=0 || nBitFilled!=nBit) //Both valid and not dead
		{
			if (bsPos==true)
			{
				for (int y=0; y<nBit; y++)
				{
					if (BS->GetBinContent(x+1, y+1) == 1) bsValue++;
					else break;
				}
			}
			else if (bsNeg==true)
			{
				for (int y=nBit-1; y>-1; y--)
				{
					if (BS->GetBinContent(x+1, y+1) == 1) bsValue--;
					else break;
				}
			}
		}

		List.push_back(bsValue);
		if (SHOW) cout <<Form("BS: %2i, %3i / %2i", detId, x+1, bsValue) <<endl;
	}//x, ch

	return List;
}//GetListBS

//=====================================================================
void DrawAdc(TH2F* H2, int runNo, int detId, int ch, bool PRINT = true)
{
	TH1F* H1 = (TH1F*)H2->ProjectionY("", ch, ch);
	H1->SetTitle(Form("d%i_ch%i_run%i", detId, ch, runNo));	

	int xMax = 1;
	for (int x=0; x<H1->GetNbinsX(); x++) { if (H1->GetBinContent(x+1) != 0) xMax = x+1; }
	H1->GetXaxis()->SetRangeUser(0, xMax + 10);
	H1->GetXaxis()->SetLabelSize(0.05);
	H1->GetXaxis()->SetTitleOffset(1.25);
	H1->GetYaxis()->SetLabelSize(0.05);

	gStyle->SetOptDate(0);
	gStyle->SetOptStat("emr");
	TCanvas *c1 = new TCanvas("c1", H1->GetTitle(), 800*1.5, 600*1.5);
	c1->Divide(1, 2);
	c1->cd(1)->SetLogy();
	H1->DrawCopy("hist e");
	c1->cd(2);
	H1->GetXaxis()->SetRangeUser(0, 32);
	H1->DrawCopy("hist e");
	H1->Delete();

	if (PRINT) c1->Print(Form("%s.png", c1->GetTitle()));
	delete c1;
	return;
}//DrawAdc

//Main
//==================================================================================
void bsQa(const char* inList = "inFiles_pptrans.list", int LogLv = 1, int PRINT = 0)
{
	/*
	   LogLv:
	   0: No log file will be created
	   1: Show runs and responsible channels when new list being produced
	   2: Add mismatching (DB <-> data) channels to 1.
	   3: Add bad channels to 2.

	   PRINT:
	   0: No updated list file will be printed out
	   1: BS list will be printed out
	   2: Bad channels list will be printed out in addition to 1.
	*/

	//Blacklists: skip master list update process for these channels
	const int ListSkip[][4] =
	{
		//Add new row in following format to mark: run# (mark begins), run# (mark ends), detId, and ch
		//{, , , }, //

		//RUN15 pp200trans
		{16073030, 16073037,  8, 483}, //Small # of entries (62) at 16073030 caused accidental update
		{16068056, 16082011,  8, 566}, //This channel is simply bad: but it keep escapes bad ch judgment routine
		{16073032, 16073034, 10,   9}, //One or Two fucking entry at ADC ~490
		{16078028, 16078042, 10,  13}, //Update at 16077021 (0 -> -1) seems reasonable
		{16073034, 16077021, 10,  47}, //Update at 16073031 (-4 -> -3) seems reasonable
		{16066035, 16073030, 10,  48}, //Bad channel
		{16078032, 16078033, 10,  51}, //Adjacent run with only one channel fluctuating... skip it
		{16066050, 16066050, 10,  65}, //No problem in algorithm, but it seems large ADC (1 entry) is accidental
		{16077027, 16077040, 10,  73}, //One entry at ADC = 1324 caused update - it continually in and out
		{16077032, 16093019, 10,  78}, //Heavily jump in and out. Cannot gaurantee quality of list
		{16073030, 16073033, 10, 151}, //Small RMS (0.85 at run16073030) caused accidental update
		{16073030, 16073037, 10, 176}, //Small RMS (0.88) at 16073030 caused accidental update
		{16073037, 16073038, 10, 186}, //One ADC entry ~1,300 at run 16073037 caused update
		{16073031, 16073040, 10, 282}, //No problem in algorithm but to reduce # of lists
		{16073035, 16073040, 11, 118}, //Bad channel judgment caused accidental update
		{16086050, 16086051, 11, 213}, //Adjacent run with only one channel fluctuating... skip it
		{16067021, 16072024, 11, 287}, //ADC starts from 32 in this period, but from run 16072038 starts from 16
		{16073031, 16077021, 11, 287}, //Marked as bad channnel: thus old DB value will overwrite. Make it skip
		//run 16085006 removed from good run list: overall short statistics

		//RUN15 pAu
		{16129050, 16129050, 10, 211}, //Typical jump channel, short statistics for this particular run
		{16129050, 16129051, 10, 221}, //Typical jump channel, short statistics for this particular run
		//run 16134016 removed from good run list: overall short statistics

		//RUN15 pAl
		{16164006, 16167091, 10,  30}, //BS_DB at 16160018 is -1. But at later runs sometimes BS_data judged as 0
		{16160020, 16169041, 10,  31}, //Actual BS is 2. Suspect in general small statistics cause continual jump
		{16160023, 16169041, 10,  32}, //Actual BS is 4. Same type of cell to above ch 10_31
		{16160018, 16162040, 10,  43}, //Actual BS is 2, but BS_DB is -1. Plus low statistics caused update
		{16160024, 16169041, 10, 177}  //Actual BS is 4. Statistics is plenty. Sometimes RMS of ADC is < 1
	};
	const int nListSkip = sizeof(ListSkip)/sizeof(ListSkip[0]);

	//-------------------------------------------

	//Master lists for continual update/printout
	vector<int> ListBad[4];
	vector<int> ListBS[4];

	TFile *F;
	ifstream in;
	in.open(inList);
	if (!in.is_open()) { cout <<"Cannot open " <<inList <<endl; return; }
	ofstream out;

	int iRun = 0;
	int runNoPrevious = 0;
	string inFile;
	while (in.is_open())
	{
		//Open file
		in >> inFile;
		if (!in.good()) { break; in.close(); }
		F = TFile::Open(inFile.c_str());
		if (!F || F->IsZombie()) { cout <<"Cannot open the file " <<inFile.c_str() <<endl; return; }
		cout <<Form("Processing... %s, %3i", F->GetName(), iRun) <<endl;

		//Get run number
		std::size_t strPos = inFile.find(".root");
		string runNoStr = inFile.substr(strPos-8, 8);
		const int runNo = std::atoi(runNoStr.c_str());
		if (LogLv>0)
		{
			if (iRun==0) out.open("log_fmsBsQa.txt");
			else         out.open("log_fmsBsQa.txt", ios::app);
			out <<Form("Run %i", runNo) <<endl;
		}

		//-------------------------------------------------

		//Switch flag if new values found (CAVEAT: this is independent of mismatch btw DB <-> data)
		bool FlagNew_dead = false;
		bool FlagNew_bs   = false;
		for (int a=0; a<4; a++) //Loop over detId
		{
			TH2F* H2_ADC     = (TH2F*)F->Get(Form("Adc_d%i", a+8));
			TH2F* H2_BS_DB   = (TH2F*)F->Get(Form("BitShift_DB_d%i", a+8));
			TH2F* H2_BS_data = (TH2F*)F->Get(Form("BitShift_data_d%i", a+8));
			TH2F* H2_ChMap   = (TH2F*)F->Get(Form("ChMap_d%i", a+8));
			if (!H2_ADC || !H2_BS_DB || !H2_BS_data || !H2_ChMap) { cout <<"Cannot open histogram!" <<endl; return; }

			//Normalize and Remove ambiguity of BS QA histogram
			NormalizeBsHisto(H2_BS_DB);
			NormalizeBsHisto(H2_BS_data);
		
			//Get dead channels list, Compare with existing value, and Update old one if necessary
			vector<int> tempBad = GetListBad(H2_ADC, H2_BS_data, H2_ChMap, a+8, runNo, false, false);
			if (iRun!=0 && ListBad[a].size() != tempBad.size()) { cout <<"Size mismatch! (1)" <<endl; return; }
			if (iRun==0 || !std::equal(ListBad[a].begin(), ListBad[a].end(), tempBad.begin()))
			{
				if (iRun!=0 && LogLv>2)
				{
					for (unsigned int x=0; x<ListBad[a].size(); x++)
					{
						bool dead_old = ListBad[a][x];
						bool dead_new = tempBad[x];
						if (dead_old != dead_new)
						{
							out <<Form("Updating dead: %2i, %3i, %i -> %i", a+8,x+1,dead_old,dead_new) <<endl;
						}
					}
				}
				ListBad[a].clear();
				ListBad[a] = tempBad;
				FlagNew_dead = true;
			}

			//Get BS lists (DB, data), Compare them, and Update DB list if necessary
			//Exception 1: skip if the channel is dead
			//Exception 2: data BS is negative and its absolute value is larger than DB
			vector<int> tempBS_db   = GetListBS(H2_BS_DB,   H2_ChMap, a+8, false);
			vector<int> tempBS_data = GetListBS(H2_BS_data, H2_ChMap, a+8, false);
			vector<int> tempBS_new;
			if (tempBS_db.size() != tempBS_data.size()) { cout <<"Size mismatch! (2)" <<endl; return; }
			for (unsigned int x=0; x<tempBS_db.size(); x++)
			{
				int bs_db   = tempBS_db[x];
				int bs_data = tempBS_data[x];
				if (abs(bs_db)>12 || abs(bs_data)>12) { cout <<Form("Invalid BS found! %2i %2i\n", bs_db,bs_data); }
				int bs_new = bs_db;

				if (bs_db!=bs_data && tempBad[x]==false) //Mismatch, NOT dead
				{
					//Judge which bs should is correct
					if (bs_data >= 0) bs_new = bs_data;
					else //bs_data < 0
					{
						if (bs_db >= 0) bs_new = 0; //Statistics issue, in this case bs_data < 0 is NOT real BS
						else //bs_data < 0 and bs_db < 0
						{
							if (abs(bs_data) < abs(bs_db)) bs_new = bs_data;
						}
					}

					if (LogLv>1 && bs_db!=bs_new)
					{
						out <<Form("BS mismatch: %2i, %3i | data: %3i, DB: %2i -> %2i",
								a+8,x+1,bs_data,bs_db,bs_new) <<endl;
					}
				}//Mismatch

				//Check if this channel is blacklisted: skip if true
				bool FlagSkip = false;
				for (int i=0; i<nListSkip; i++)
				{
					if (runNo>=ListSkip[i][0] && runNo<=ListSkip[i][1] &&
						a+8==ListSkip[i][2] && x+1==(unsigned int)ListSkip[i][3]) FlagSkip = true;
				}
				if (FlagSkip==true) bs_new = ListBS[a][x];

				tempBS_new.push_back(bs_new);
			}//Loop over channels

			//Update old BS (DB) list if difference found
			if (iRun==0 || !std::equal(ListBS[a].begin(), ListBS[a].end(), tempBS_new.begin()))
			{
				if (iRun!=0 && LogLv>0)
				{
					for (unsigned int x=0; x<tempBS_new.size(); x++)
					{
						int bs_old = ListBS[a][x];
						int bs_new = tempBS_new[x];
						if (bs_old != bs_new)
						{
							out <<Form("Updating BS: %2i, %3i | %2i -> %2i", a+8,x+1,bs_old,bs_new) <<endl;
							//DrawAdc(H2_ADC, runNo, a+8, x+1, true); //MUST be used in batch mode
						}
					}
				}
				ListBS[a].clear();
				ListBS[a] = tempBS_new;
				FlagNew_bs = true;
			}

			H2_ADC->Delete();
			H2_BS_DB->Delete();
			H2_BS_data->Delete();
		}//a, detId
		F->Close();
		iRun++;
		if (LogLv>0) { out <<endl; out.close(); }

		//-------------------------------------------------

		//Printout updated DB values
		if (PRINT>1 && FlagNew_dead)
		{
			cout <<Form("List updated: run %i -> run %i, Dead channels", runNoPrevious, runNo) <<endl;
			out.open(Form("FmsDead_y2015_run%i.txt", runNo));
			for (int a=0; a<4; a++)
			for (unsigned int b=0; b<ListBad[a].size(); b++)
			{
				out <<Form("%2i %3i %i", a+8, b+1, ListBad[a][b]) <<endl;
			}
			out.close();
		}
		if (PRINT>0 && FlagNew_bs)
		{
			cout <<Form("List updated: run %i -> run %i, Bit shift", runNoPrevious, runNo) <<endl;
			out.open(Form("FmsBsGain_y2015_run%i.txt", runNo));
			for (int a=0; a<4; a++)
			for (unsigned int b=0; b<ListBS[a].size(); b++)
			{
				out <<Form("%2i %3i %2i", a+8, b+1, ListBS[a][b]) <<endl;
			}
			out.close();
		}

		runNoPrevious = runNo;
	}//Loop over runs 

	return;
}//Main
