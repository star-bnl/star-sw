#include "calibFmsShow.C"

void TestShow(bool PRINT = true)
{
	//How to use prepared QA functions

    map<int, int>    iToCh[4]; //!
    map<int, int>    chToBS[4];
    map<int, int>    chToCellStat[4]; //!
    map<int, float>  chToGainCorr[4]; //! gainCorr applied for current iteration
	map<int, float>  chToGain[4];
    map<int, st_pos> chToPos[4];
    GetMapIndexToCh   ("./FmsMapBase.txt",     iToCh);
    GetMapChToBS      ("./FmsMapBitShift.txt", chToBS);
    GetMapChToCellStat("./FmsCellStat.txt",    chToCellStat);
    GetMapChToGainCorr("./FmsGainCorr.txt",    chToGainCorr);
	GetMapChToGain    ("./FmsMapBase.txt",     chToGain);
    GetMapChToPos     ("./FmsMapBase.txt",     chToPos);

	//Mark channels of interest
	vector<int> Marked[4];
	Marked[0].push_back(22);
	Marked[0].push_back(45);
	Marked[2].push_back(46);
	Marked[2].push_back(52);
	Marked[3].push_back(11);

	//Draw ADC QA
	//-------------------------------------------

	#if 0
	//Prepare each cell's info
	map<int, string> cellInfo[4];
	for (unsigned int a=0; a<4; a++)
	for (unsigned int b=0; b<iToCh[a].size(); b++)
	{
		const int ch = iToCh[a][b];
		const int bs = chToBS[a][ch];
		string tempInfo = Form("d%i_ch%i, BS=%i", a+8, ch, bs);
		cellInfo[a].insert(pair<int, string>(ch, tempInfo));
	}

	//Prepare marked channels list
	vector<int> MarkedBad[4];
	for (unsigned int a=0; a<4; a++)
	for (unsigned int b=0; b<iToCh[a].size(); b++)
	{
		const int ch = iToCh[a][b];
		const int cs = chToCellStat[a][ch];
		if (cs == BAD) MarkedBad[a].push_back(ch);
	}

	//Prepare mass distributions: will be drawn on top of ADCs as a crosscheck
	const char* inFile = "/star/u/ckimstar/work/fms_calib/RUN15pptrans_final/xChecks/fmsCalib_day8081.root";
	TFile* F = TFile::Open(inFile);
	TH2F* H2_mass[4];
	for (int a=0; a<4; a++) H2_mass[a] = (TH2F*)F->Get(Form("mass_d%i_fine", a+8));

	//List of ROOT files contain ADC vs. ch histograms
	const char* inList = "./inListQa_day8081.txt";

	//DrawAdcQa(iToCh, inList, PRINT, cellInfo);
	//DrawAdcQa(iToCh, inList, PRINT, cellInfo, chToBS, chToGain, chToGainCorr);
	//DrawAdcQa(iToCh, inList, PRINT, cellInfo, chToBS, chToGain, chToGainCorr, MarkedBad);
	//DrawAdcQa(iToCh, inList, PRINT, cellInfo, chToBS, chToGain, chToGainCorr, DumVecI, H2_mass);
	#endif

	//Draw 2D map
	//-------------------------------------------

	#if 0
	bool smallOnly = false;
	bool showEta   = true;
	DrawMap(iToCh, chToPos, "Test1");
	DrawMap(iToCh, chToPos, "Test2", smallOnly, showEta, PRINT, DumMapF, chToCellStat, Marked);
	DrawMap(iToCh, chToPos, "Test3", smallOnly, showEta, PRINT, chToGainCorr, DumMapI, Marked);
	DrawMap(iToCh, chToPos, "Test4", smallOnly, showEta, PRINT, chToGainCorr, chToCellStat, DumVecI);
	DrawMap(iToCh, chToPos, "TestA", smallOnly, showEta, PRINT, chToGainCorr, chToCellStat, Marked);
	#endif

	//Draw gainCorr comparison
	//-------------------------------------------

	#if 0
	map<int, int>   chToCS[2][4];
	map<int, float> chToGC[2][4];
	const char* inPath = "/star/u/ckimstar/work/fms_calib/RUN15pptrans_final";
    GetMapChToCellStat(Form("%s/cellStat/FmsCellStat_day7779.txt", inPath), chToCS[0]);
    GetMapChToCellStat(Form("%s/cellStat/FmsCellStat_day8081.txt", inPath), chToCS[1]);
    GetMapChToGainCorr(Form("%s/gainCorr/FmsGainCorr_day7779_fin.txt", inPath), chToGC[0]);
    GetMapChToGainCorr(Form("%s/gainCorr/FmsGainCorr_day8081_fin.txt", inPath), chToGC[1]);

	bool excBadDead = true;
	bool convOnly   = true;
	DrawCompGainCorr(iToCh, chToCS, chToGC, "Sample1", "Sample2", PRINT, excBadDead, convOnly);
	#endif

	//Draw comparison of mass
	//-------------------------------------------

	#if 0
	const int nComp = 9;
	TFile* F[nComp];
	TH2F* H2_mass[nComp][4];

	//Retrieve target TH2
	for (int i=0; i<nComp; i++)
	{
		const char* inPath = "/gpfs01/star/subsysg/FPS/ckimstar/out_iterations/Iterations_day8081_0820";
		F[i] = TFile::Open(Form("%s/fmsCalib_%i.root", inPath, i));
		for (int a=0; a<4; a++)
		{
			H2_mass[i][a] = (TH2F*)F[i]->Get(Form("mass_d%i_fine", a+8));
			H2_mass[i][a]->SetTitle(Form("Test_%i", i)); //Will be used as legend title
		}
	}

	//Additional info will be shown in cell by cell title
	map<int, string> cellInfo[4];
	for (unsigned int a=0; a<4; a++)
	for (unsigned int b=0; b<iToCh[a].size(); b++)
	{
		const int ch = iToCh[a][b];
		const int bitShift = chToBS[a][ch];
		const int cellStat = chToCellStat[a][ch];
		const float gainCorr = chToGainCorr[a][ch];
		string tempInfo = Form("d%i_ch%i, BS=%i, STAT=%i, GC=%4.3f", a+8, ch, bitShift, cellStat, gainCorr);
		cellInfo[a].insert(pair<int, string>(ch, tempInfo));
	}

	//Draw separately, channel by channel: run in batch mode if you don't use marked cells list
	//DrawCompMassSep(iToCh, nComp, H2_mass, false, true, cellInfo, Marked);
	//DrawCompMassSep(iToCh, nComp, H2_mass, false, false, cellInfo);
	#endif

	//Draw progress of convergence over iterations
	//--------------------------------------------

	#if 0
	const int nIter = 9;
	const char* inPath = "/gpfs01/star/subsysg/FPS/ckimstar/out_iterations/Iterations_day8081_0820";
	bool excBadDead = true;

	//DrawConvProgress(nIter, inPath, iToCh);
	//DrawConvProgress(nIter, inPath, iToCh, excBadDead, PRINT);
	#endif

	//Draw mass distribution vs. iteration index
	//-------------------------------------------

	#if 0
	//This function designed seriously inefficient way due to ROOT enforces STUPID interactive session
	//If you're not using marked cells list, I STRONGLY RECOMMEND EXECUTE IN BATCH MODE for your mental health

	const int nIter = 9;
	const char* inPath = "/gpfs01/star/subsysg/FPS/ckimstar/out_iterations/Iterations_day8081_0820";
	map<int, int> chToCSIter[4];
    GetMapChToCellStat(Form("%s/FmsCellStat_fin.txt", inPath), chToCSIter);

	//DrawIterMass(nIter, inPath, iToCh, chToCSIter, PRINT);
	//DrawIterMass(nIter, inPath, iToCh, chToCSIter, PRINT, Marked);
	#endif

	//Print all channel by channel mass distribution in an iteration index: use batch mode
	//------------------------------------------------------------------------------------

	#if 0
	const int iIter = 0;
	const char* inPath = "/gpfs01/star/subsysg/FPS/ckimstar/out_iterations/Iterations_day8081_0820";

	//DrawIterMassSingle(iIter, inPath);
	//DrawIterMassSingle(iIter, inPath, Marked, "iterMassSingle");
	#endif

	//Draw calibration related parameters vs. iteration index
	//-------------------------------------------------------

	#if 0
	const int nIter = 9;
	const char* inPath = "/gpfs01/star/subsysg/FPS/ckimstar/out_iterations/Iterations_day8081_0820";

	//DrawIterPars(nIter, inPath, iToCh, chToCellStat, true);
	//DrawIterPars(nIter, inPath, iToCh, chToCellStat, false, Marked);
	#endif

	return;
}//Main
