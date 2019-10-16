void loadLibraries(bool fmsLibs = true);
void runMudst(
		const char*  inFile = "inFiles_fms.list",
		const char* outFile = "fmsCalib.root"
		)
{
	TStopwatch sw;
	sw.Start();

	loadLibraries();
	gSystem->Load("StFmsCalibMaker");

	gMessMgr->SetLimit("I", 0);
	gMessMgr->SetLimit("Q", 0);
	gMessMgr->SetLimit("W", 0);

	StChain* chain = new StChain("StChain");
	chain->SetDEBUG(0);

	//-------------------------------------------

	//Arguments(default): mode, nameMode, directory (./), file, filter (.), max files (10), and name (MuDst)
	StMuDstMaker* muDstMk = new StMuDstMaker(0, 0, "", inFile, ".MuDst.root", 1000, "MuDst");
	cout <<Form("\nEntries in %s: %i", inFile, muDstMk->tree()->GetEntries()) <<endl;

	St_db_Maker* StarDb = new St_db_Maker("StarDb", "MySQL:StarDb", "$STAR/StarDb");
	StarDb->SetAttr("blacklist", "emc");
	StarDb->SetAttr("blacklist", "eemc");
	StarDb->SetAttr("blacklist", "ftpc");
	StarDb->SetAttr("blacklist", "ist");
	StarDb->SetAttr("blacklist", "mtd");
	StarDb->SetAttr("blacklist", "pmd");
	StarDb->SetAttr("blacklist", "pp2pp");
	StarDb->SetAttr("blacklist", "pxl");
	StarDb->SetAttr("blacklist", "ssd");
	StarDb->SetAttr("blacklist", "svt");
	StarDb->SetAttr("blacklist", "tof");
	StarDb->SetAttr("blacklist", "tpc");
	StarDb->SetDebug();

	StFmsDbMaker* fmsDb = new StFmsDbMaker("fmsDb");
	fmsDb->setDebug(0);
	fmsDb->readGainCorrFromText(true); cout <<"WARNING! External gain correction factors are being used!\n" <<endl;

	StEventMaker*    eventMk    = new StEventMaker();
	StFmsHitMaker*   fmsHitMk   = new StFmsHitMaker();
	StFmsPointMaker* fmsPointMk = new StFmsPointMaker();
	//fmsHitMk  ->SetReadMuDst();
	//fmsPointMk->SetReadMuDst();

	//-------------------------------------------

	StFmsCalibMaker* fmsCalib = new StFmsCalibMaker();
	TString outName(outFile);
	if (!strcmp(outFile, "")) { outName = inFile; outName.ReplaceAll("MuDst", "fmsCalib"); }
	fmsCalib->SetOutName((const char*)outName);
	fmsCalib->ReadCellStat("FmsCellStat.txt");
	fmsCalib->ReadBbcSlewing("/star/u/ckimstar/work/fms_calib/Oleg_bbc_slewing_run15_pp200.dat"); //Apply zVtx
	//fmsCalib->VpdTimingCut(1300); cout <<"WARNING! VPD west timing cut is being applied!\n" <<endl;
	//fmsCalib->GetQaHistAdc(480202); //BHT1*VPDMB-30, RUN15pp200trans
	//fmsCalib->GetQaHist();
	//fmsCalib->GetQaTree();
	//fmsCalib->GetMap();

	//-------------------------------------------

	chain->Init();
	chain->EventLoop();
	chain->Finish();
	delete chain;

	sw.Stop();
	sw.Print();
	std::cout <<"Done!" <<endl;
	return;
}//Main

//==============================
void loadLibraries(bool fmsLibs)
{
    //STAR libraries for chain, MuDST, logger etc
    gROOT->Macro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
	loadSharedLibraries();
    gROOT->Macro("loadMuDst.C");
    //gROOT->Macro("LoadLogger.C");

    gSystem->Load("StTpcDb");
    gSystem->Load("St_db_Maker");
    gSystem->Load("StDaqLib");
    gSystem->Load("StDbBroker");
    gSystem->Load("StDbUtilities");
    gSystem->Load("StDetectorDbMaker");
    gSystem->Load("StEEmcDbMaker");
    gSystem->Load("StEEmcUtil");
    gSystem->Load("StEmcRawMaker");
    gSystem->Load("StEmcADCtoEMaker");
    gSystem->Load("StEmcTriggerMaker");
    gSystem->Load("StEpcMaker");
    gSystem->Load("StEventMaker");
    gSystem->Load("StMCAsymMaker");
    gSystem->Load("StPreEclMaker");
    gSystem->Load("StRandomSelector");
    gSystem->Load("StSpinDbMaker");
    gSystem->Load("StTriggerFilterMaker");
    gSystem->Load("StTriggerUtilities");
    if (fmsLibs)
	{
        gSystem->Load("libMinuit.so");
        gSystem->Load("StFmsUtil");
        gSystem->Load("StFmsDbMaker");
        gSystem->Load("StFmsHitMaker");
        gSystem->Load("StFmsPointMaker");
        gSystem->Load("StFmsFpsMaker");
    }

	return;
}//loadLibraries
