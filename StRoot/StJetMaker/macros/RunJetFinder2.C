
// NOTE - chain needs to be declared global so for StHbtEventReader
//=========================================================================================
class  StChain;
StChain *chain;
int total=0;

void RunJetFinder2(int nevents=300,
		   const char* dir = "",
		   const char* file = "/star/data16/reco/ppLong-1/FullField/P03ih/2003/150/st_physics_4150010_raw_0010005.MuDst.root",
		   const char *filter = "",
		   const char *outfile="4150010_raw_0010005")
{
    if (gClassTable->GetID("TTable") < 0) {
	gSystem->Load("libStar");
	gSystem->Load("libPhysics");
    }
    gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
    loadSharedLibraries();
    gSystem->Load("StMagF");
    gSystem->Load("StTpcDb");
    gSystem->Load("StDbUtilities");
    gSystem->Load("StDaqLib");
    gSystem->Load("StEmcADCtoEMaker");
    gSystem->Load("StEpcMaker");
    gSystem->Load("StDbLib");
    gSystem->Load("StDbBroker");  
    gSystem->Load("St_db_Maker");
    gSystem->Load("StJetFinder");
    gSystem->Load("StJetMaker");
    gSystem->Load("StSpinPoolThomasUtilities");

    double pi = atan(1.0)*4.0;
    cout << " loading done " << endl;
   
    chain= new StChain("StChain"); 
    chain->SetDebug(1);

    //Instantiate the MuDstReader
    StMuDebug::setLevel(1); 
    StMuDstMaker* muDstMaker = new StMuDstMaker(0,0,dir,file,filter,10,"MuDst");

    //StMuDbReader...
    StMuDbReader* db = StMuDbReader::instance();

    //StMuDst2StEventMaker
    StMuDst2StEventMaker* eventMaker = new StMuDst2StEventMaker("MuDst2StEvent");

    //Database
    St_db_Maker *dbMk = new St_db_Maker("StarDb", "MySQL:StarDb");
    dbMk->SetDateTime(20030101,10000); 

    //EmcAdc2EMaker
    StEmcADCtoEMaker *adc = new StEmcADCtoEMaker();

    //PrecEclMaker
    //StPreEclMaker *pecl = new StPreEclMaker();

    //EpcMaker
    //StEpcMaker *epc = new StEpcMaker();
  
    //Instantiate the StEmcTpcFourPMaker
    StEmcTpcFourPMaker* emcFourPMaker = new StEmcTpcFourPMaker("EmcTpcFourPMaker", muDstMaker, 30, 30, .3, .3, .003, adc);
    emcFourPMaker->setUseType(StEmcTpcFourPMaker::Hits);//if don't have this line then default is 0 (which is hits)
    //emcFourPMaker->UseSimpleADCCal(); //turn this on for simulation only!
    //emcFourPMaker->UsePedSubKludge();
    emcFourPMaker->setMaxPoints(150);
    emcFourPMaker->setMinPointThreshold(.3);

    //Instantiate the JetMaker
    char* emcOutfile = new char[256];
    strcpy(emcOutfile, outfile); strcat(emcOutfile, "emc");
    StJetMaker* emcJetMaker = new StJetMaker("emcJetMaker", emcFourPMaker, muDstMaker, emcOutfile);

    //Now setup two jet analyses that use the same track/jet cuts
    
    //set the analysis cuts: (see StJetMaker/StppJetAnalyzer.h -> class StppAnaPars )
    StppAnaPars* anapars = new StppAnaPars();
    anapars->setFlagMin(0); //track->flag() > 0
    anapars->setNhits(15); //track->nHitsFit()>15    anapars->setCutPtMin(0.2); //track->pt() > 0.2
    anapars->setAbsEtaMax(1.6); //abs(track->eta())<1.6
    anapars->setJetPtMin(5.0);
    anapars->setJetEtaMax(100.0);
    anapars->setJetEtaMin(0);
    anapars->setJetNmin(0);

    //Setup the cone finder (See StJetFinder/StConeJetFinder.h -> class StConePars)
    StConePars* cpars = new StConePars();
    cpars->setGridSpacing(56, -1.6, 1.6, 120, -pi, pi);
    cpars->setConeRadius(0.7);
    cpars->setSeedEtMin(0.5);
    cpars->setAssocEtMin(0.1);
    cpars->setSplitFraction(0.5);
    cpars->setPerformMinimization(true);
    cpars->setAddMidpoints(true);
    cpars->setRequireStableMidpoints(true);
    cpars->setDoSplitMerge(true);
    cpars->setDebug(false);
    emcJetMaker->addAnalyzer(anapars, cpars, "MkConeJetsPt02R07");

    //Setup the cone finder (See StJetFinder/StCdfChargedConeJetFinder.h -> class StCdfChargedConePars)
    StCdfChargedConePars* ccdfpars = new StCdfChargedConePars();
    ccdfpars->setGridSpacing(56, -1.6, 1.6, 120, -pi, pi);
    ccdfpars->setConeRadius(0.7);
    ccdfpars->setSeedEtMin(1.0);
    ccdfpars->setAssocEtMin(0.1);
    ccdfpars->setDebug(false);
    emcJetMaker->addAnalyzer(anapars, ccdfpars, "MkCdfChargedJetsPt02R07");

    //Setup the kt=finder (See StJetFinder/StKtCluFinder.h -> class StKtCluPars)
    StKtCluPars* ktpars = new StKtCluPars();
    ktpars->setR(1.0);
    ktpars->setDebug(false);
    emcJetMaker->addAnalyzer(anapars, ktpars, "MkKtJet");
  
    chain->Init();
    chain->PrintInfo();

    cout <<"\tLoop on branches"<<endl;

    TChain* muchain = muDstMaker->chain();
    assert(chain);
    TObjArray* branches = muchain->GetListOfBranches();
    assert(branches);
    TObjArray branchesToKill;
    branchesToKill.Add( new TObjString("KingAssoc") );
    branchesToKill.Add( new TObjString("McKink") );
    branchesToKill.Add( new TObjString("Xi") );
    branchesToKill.Add( new TObjString("L3AlgoAccept") );
    branchesToKill.Add( new TObjString("McV0") );
    branchesToKill.Add( new TObjString("RichSpectra") );
    branchesToKill.Add( new TObjString("XiAssoc") );
    branchesToKill.Add( new TObjString("L3AlgoReject") );
    branchesToKill.Add( new TObjString("McXi") );
    branchesToKill.Add( new TObjString("StrangeCuts") );
    branchesToKill.Add( new TObjString("L3Tracks") );
    branchesToKill.Add( new TObjString("V0") );
    branchesToKill.Add( new TObjString("Kink") );
    branchesToKill.Add( new TObjString("McEvent") );
    branchesToKill.Add( new TObjString("OtherTracks") );
    branchesToKill.Add( new TObjString("V0Assoc") );
    for (int ib=0; ib<branches->GetLast()+1; ++ib) {
	TBranch* branch = dynamic_cast<TBranch*>((*branches)[ib]);
	if (!branch) {cout <<"\tNull branch"<<endl; abort();}
	const char* bname = branch->GetName();
	TString bnameString(bname);
	cout <<"\t--- Found branch:\t"<<bnameString<<endl;
	for (int jb=0; jb<branchesToKill->GetLast()+1; ++jb) {
	    TObjString* tos = static_cast<TObjString*>( branchesToKill[jb] );
	    TString btk = tos->GetString();
	    //cout <<"\t\tcompare to:\t"<<btk<<endl;
	    if (bnameString.Contains(btk)) {
		cout <<"\tdeactivating branch:\t"<<bname<<endl;
		muchain->SetBranchStatus(bname, 0);
	    }
	}
    }

    for (Int_t iev=0;iev<nevents; iev++) {
	cout << "****************************************** " << endl;
	cout << "Working on eventNumber " << iev << endl;
	cout << "*************************1***************** " << endl;
	chain->Clear();
	int iret = chain->Make(iev); 
	total++;
	if (iret) {
	    cout << "Bad return code!" << endl;
	    break;
	}
    } 
    chain->Finish(); 
    cout << "****************************************** " << endl;
    cout << "total number of events  " << total << endl;
    cout << "****************************************** " << endl;      
}







