
// NOTE - chain needs to be declared global so for StHbtEventReader
//=========================================================================================
class  StChain;
StChain *chain;
int total=0;

void RunJetFinder2(
		   //const char* dir = "/star/data42/reco/ppLong-1/FullField/P04if/2003/145/",
		   //const char* file = "st_physics_4145041_raw_0040025.MuDst.root"
		   const char* dir = "",
		   const char* file = "/star/data16/reco/dAuCombined/FullField/P03ih/2003/065/st_physics_4065003_raw_0040054.MuDst.root",
		  const char *filter = "",
		  const char *outfile="Jets_out_")
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
    StPreEclMaker *pecl = new StPreEclMaker();

    //EpcMaker
    StEpcMaker *epc = new StEpcMaker();
  
    //Instantiate the StEmcTpcFourPMaker
    StEmcTpcFourPMaker* emcFourPMaker = new StEmcTpcFourPMaker("EmcTpcFourPMaker", muDstMaker, 30, 30, .3, .3, .003, adc);
    emcFourPMaker->setUseType(StEmcTpcFourPMaker::Hits);//if don't have this line then default is 0 (which is hits)
    emcFourPMaker->UseSimpleADCCal();

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
    cpars->setSplitFraction(0.0);
    cpars->setPerformMinimization(true);
    cpars->setAddMidpoints(true);
    cpars->setRequireStableMidpoints(true);
    cpars->setDoSplitMerge(true);
    cpars->setDebug(false);
    emcJetMaker->addAnalyzer(anapars, cpars, "MkConeJetsPt02R07");

    //Setup the cone finder (See StJetFinder/StKtCluFinder.h -> class StKtCluPars)
    StKtCluPars* ktpars = new StKtCluPars();
    ktpars->setR(1.0);
    ktpars->setDebug(false);
    emcJetMaker->addAnalyzer(anapars, ktpars, "MkKtJet");
  
    chain->Init();
    chain->PrintInfo();

    //int nevents =1000000;
    int nevents = 20;
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







