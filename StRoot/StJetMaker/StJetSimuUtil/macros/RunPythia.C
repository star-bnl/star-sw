//RunPythia.C

// NOTE - This macro is ONLY for running MC simulation data!!
//=========================================================================================
class  StChain;
StChain *chain;
int total=0;

void RunPythia(
	       int nevents = 100,
	       const char *dir ="",
	       const char* infile ="/star/data18/reco/pp200/pythia6_203/default/pt15/y2004x/gheisha_on/trs_ij/pds1214_69_5000evts.MuDst.root",
	       const char* outdir = "./processed/")
{
    /*
    TString ofile = TString(outdir) + TString(infile) + TString("_simu.jet.root");
    TString sofile = TString(outdir) + TString(infile) + TString("_simu.out.root");
    */
    
    TString ofile = "blah_simu.jet.root";
    TString sofile = "blah_simu.out.root";
    const char* outfile = ofile.Data();
    const char *soutfile = sofile;

    cout <<"write files:\t"<<outfile<<"\tand:\t"<<soutfile<<endl;
    
    if (gClassTable->GetID("TTable") < 0) {
	gSystem->Load("libStar");
	gSystem->Load("libPhysics");
    }
    gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
    loadSharedLibraries();
    gSystem->Load("StMagF");
    gSystem->Load("StTpcDb");
    gSystem->Load("StDbUtilities");
    gSystem->Load("StMcEvent");
    gSystem->Load("StMcEventMaker");
    gSystem->Load("StDaqLib");
    gSystem->Load("StEmcRawMaker");
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

    // StIOMaker - to read geant files
    StIOMaker* ioMaker = new StIOMaker();
    ioMaker->SetFile(infile);
    //ioMaker->SetFile(fname);
    ioMaker->SetIOMode("r");
    ioMaker->SetBranch("*",0,"0");             //deactivate all branches
    ioMaker->SetBranch("geantBranch",0,"r");   //activate geant Branch

    // Instantiate StMcEventMaker
    class StMcEventMaker *mcEventMaker = new StMcEventMaker();
    mcEventMaker->doPrintEventInfo = false;
    mcEventMaker->doPrintMemoryInfo = false;

    //Instantiate the MuDstReader
    StMuDebug::setLevel(1); 
    StMuDstMaker* muDstMaker = new StMuDstMaker(0,0,dir,infile,"",10,"MuDst");

    //Database
    St_db_Maker *dbMk =new St_db_Maker("db","MySQL:StarDb","$STAR/StarDb","StarDb");
    dbMk->SetDateTime(20031120,0);

    //EmcAdc2EMaker
    StEmcADCtoEMaker *adc = new StEmcADCtoEMaker();

    //Instantiate the StEmcTpcFourPMaker
    StEmcTpcFourPMaker* emcFourPMaker = new StEmcTpcFourPMaker("EmcTpcFourPMaker", muDstMaker, 30, 30, .3, .3, .003, adc);
    emcFourPMaker->setUseType(StEmcTpcFourPMaker::Hits);//if don't have this line then default is 0 (which is hits)
    emcFourPMaker->setMaxPoints(150);
    emcFourPMaker->setMinPointThreshold(.3);

    //Instantiate Trigger maker for simulation
    StJetSimuTrigMaker *trig=new StJetSimuTrigMaker("SimuTrig");
    trig->setPrintOption(0);
    
    //Instantiate Maker with Pythia event record etc for simulation
    StJetSimuWeightMaker *weight= new StJetSimuWeightMaker("SimuWeight");
    weight->setPrintOption(0);

    //Instantiate Tree Maker for simulation
    StJetSimuTreeMaker *stree= new StJetSimuTreeMaker("SimuTree",soutfile);
    stree->setPrintOption(0);

    //Pythia4pMaker
    //StPythiaFourPMaker* pythiaFourPMaker = new StPythiaFourPMaker("StPythiaFourPMaker",weight, mcEventMaker);

    //Instantiate the JetMaker
    StJetMaker* emcJetMaker = new StJetMaker("emcJetMaker", muDstMaker, outfile);

    //set the analysis cuts: (see StJetMaker/StppJetAnalyzer.h -> class StppAnaPars )
    StppAnaPars* anapars = new StppAnaPars();
    anapars->setFlagMin(0); //track->flag() > 0
    anapars->setNhits(0); //track->nHitsFit()>15
    anapars->setCutPtMin(0.0001); //track->pt() > 0.2
    anapars->setAbsEtaMax(5.0); //abs(track->eta())<5.0
    anapars->setJetPtMin(5.0); //MLM, remember to change this back to 5!
    anapars->setJetEtaMax(5.0);
    anapars->setJetEtaMin(0);
    anapars->setJetNmin(0);
    

    //Setup the cone finder (See StJetFinder/StConeJetFinder.h -> class StConePars)
    StConePars* cpars = new StConePars();
    cpars->setGridSpacing(200, -5.0, 5.0, 120, -pi, pi);
    cpars->setConeRadius(0.7);
    cpars->setSeedEtMin(0.5);
    cpars->setAssocEtMin(0.01);
    cpars->setSplitFraction(0.5);
    cpars->setPerformMinimization(true);
    cpars->setAddMidpoints(true);
    cpars->setRequireStableMidpoints(true);
    cpars->setDoSplitMerge(true);
    cpars->setDebug(false);
 
    //Setup the kt=finder (See StJetFinder/StKtCluFinder.h -> class StKtCluPars)
    StKtCluPars* ktpars = new StKtCluPars();
    ktpars->setR(1.0);
    ktpars->setDebug(false);

    //Creat jet finders
    //emcJetMaker->addAnalyzer(anapars, cpars, pythiaFourPMaker, "PythiaConeJetsPt02R07");  //cone + pythia
    //emcJetMaker->addAnalyzer(anapars, ktpars, pythiaFourPMaker, "PythiaKtJet"); //kt + pythia
    //emcJetMaker->addAnalyzer(anapars, cpars, emcFourPMaker, "RecoConeJetsPt02R07"); //cone + reco
    emcJetMaker->addAnalyzer(anapars, ktpars, emcFourPMaker, "RecoKtJet"); //kt + Reco
    
    chain->Init();
    chain->PrintInfo();
    
    TChain* muchain = muDstMaker->chain();
    
    for (Int_t iev=0;iev<nevents; iev++) {
	cout << "****************************************** " << endl;
	//cout << "Working on eventNumber:\t" << iev << "\t of:\t"<<ntotal<<endl;
	cout << "Working on eventNumber:\t" << iev <<endl;
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
