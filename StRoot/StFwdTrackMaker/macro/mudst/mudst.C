//usr/bin/env root4star -l root -l -q  $0; exit $?
// that is a valid shebang to run script as executable, but with only one arg

void loadLibs();
void mudst( const Char_t * fileList = "mudst.lis", int nEvents = 5000, int nFiles = 1 ){
	cout << "FileList: " << fileList << endl;
	cout << "nFiles: " << nFiles << endl;
	cout << "nEvents: " << nEvents << endl;

	// First load some shared libraries we need
	loadLibs();

	// create the chain
	StChain *chain  = new StChain("StChain");

	// create the StMuDstMaker
	StMuDstMaker *muDstMaker = new StMuDstMaker(  	0,
													0,
													"",
													fileList,
													"MuDst.root",
													nFiles
												);

	// Initialize the database
	// cout << endl << "============  Data Base =========" << endl;
	// St_db_Maker *dbMk = new St_db_Maker("db","MySQL:StarDb","$STAR/StarDb","StarDb");


	gSystem->Load("StFwdUtils.so");
    StFwdAnalysisMaker * fwdAna = new StFwdAnalysisMaker();
	fwdAna->setMuDstInput();
    chain->AddMaker(fwdAna);

	StFcsDbMaker * fcsDb = new StFcsDbMaker();
    chain->AddMaker(fcsDb);
	fcsDb->SetDebug();

	
	// Initialize chain
	Int_t iInit = chain->Init();
	
	cout << "CHAIN INIT DONE?" << endl;
	// ensure that the chain initializes
	if ( iInit ) 
		chain->Fatal(iInit,"on init");

	// print the chain status
	// chain->PrintInfo();

	//_____________________________________________________________________________
    //
    // MAIN EVENT LOOP
    //_____________________________________________________________________________
	for (int i = 0; i < nEvents; i++) {
        chain->Clear();
        if (kStOK != chain->Make())
            break;
    }
	
	// Chain Finish
	if (nEvents > 1) {
		cout << "FINISH up" << endl;
		chain->Finish();
	}

	delete chain;

	

}



void loadLibs(){	
	// if (gClassTable->GetID("TTable") < 0) {
	// 	gSystem->Load("libStar");
	// 	gSystem->Load("libPhysics");
	// }  
	cout << "LL0" << endl;
	gSystem->Load("libStarClassLibrary.so");
	gSystem->Load("libStarRoot.so");
	cout << "LL1" << endl;
	gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
	loadSharedLibraries();
	cout << "LL2" << endl;
	
	gSystem->Load("StarMagField");
	gSystem->Load("StMagF");
	gSystem->Load("StDetectorDbMaker");
	gSystem->Load("StTpcDb");
	gSystem->Load("StDaqLib");
	gSystem->Load("StDbBroker");
	gSystem->Load("StDbUtilities");
	gSystem->Load("St_db_Maker");

	gSystem->Load("StEvent");
	gSystem->Load("StEventMaker");
	gSystem->Load("StarMagField");
 
	gSystem->Load("libGeom");
	gSystem->Load("St_g2t");
	
	// Added for Run16 And beyond
	gSystem->Load("libGeom.so");
	
	gSystem->Load("St_base.so");
	gSystem->Load("StUtilities.so");
	gSystem->Load("libPhysics.so");
	gSystem->Load("StarAgmlUtil.so");
	gSystem->Load("StarAgmlLib.so");
	gSystem->Load("libStarGeometry.so");
	gSystem->Load("libGeometry.so");
	
	gSystem->Load("xgeometry");
 
	gSystem->Load("St_geant_Maker");


	// needed since I use the StMuTrack
	gSystem->Load("StarClassLibrary");
	gSystem->Load("StStrangeMuDstMaker");
	gSystem->Load("StMuDSTMaker");
	gSystem->Load("StBTofCalibMaker");
	gSystem->Load("StVpdCalibMaker");
	gSystem->Load("StBTofMatchMaker");
	gSystem->Load("StFcsDbMaker");	


}
