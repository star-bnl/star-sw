// NOTE - This macro is ONLY for running MC simulation data!!
// If using StJetSimuWeightMaker class on simulation data before 2004 you will
// need to comment out all references to St_g2t_pythia class. These tables 
// were not included until 2004
//=========================================================================================
class  StChain;
StChain *chain;
int total=0;
#include <string>

void RunJetSimuFinder(int nevents = 100,
		      const char* fname = "./firstPass/reco/pythia_a_D96D83CC9A4C45CD72DAA45AFF8D6021_11.MuDst.root",
		      const char* fbname = "pythia_a_D96D83CC9A4C45CD72DAA45AFF8D6021_11",
		      //const char* fname = "/star/data18/reco/pp200/pythia6_203/default/pt15/y2004x/gheisha_on/trs_ij/pds1214_69_5000evts.MuDst.root",
		      //const char* fbname = "pds1214_69_5000evts",
		      const char* outdir = "./processed/"
		      )
{
    /*
    string basename = firstHalf(fname,"/",".MuDst.root");
    TString mudstfile = TString("/") + TString(basename) + TString(".MuDst.root");
    TString geantfile = TString("/") + TString(basename) + TString(".geant.root");
    */
    
    TString mudstfile(fname);
    TString geantfile = mudstfile;
    geantfile.ReplaceAll("MuDst","geant");
    
    cout <<"read mudst file:\t"<<mudstfile<<endl;
    cout <<"read geant file:\t"<<geantfile<<endl;

    TString jetfile = TString(outdir) + TString(fbname) + TString(".jet.root");
    TString simufile = TString(outdir) + TString(fbname) + TString(".simu.root");
    TString histfile = TString(outdir) + TString(fbname) + TString(".jethist.root");
    
    cout <<"write jet file:\t"<<jetfile<<endl;
    cout <<"write simu file:\t"<<simufile<<endl;
    cout <<"write hist file:\t"<<histfile<<endl;
    
    if (gClassTable->GetID("TTable") < 0) {
	gSystem->Load("libStar");
	gSystem->Load("libPhysics");
    }
    gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
    loadSharedLibraries();
    //gSystem->Load("StMagF");
    gSystem->Load("StTpcDb");
    gSystem->Load("StDbUtilities");
    gSystem->Load("StMcEvent");
    gSystem->Load("StMcEventMaker");
    gSystem->Load("StDaqLib");
    gSystem->Load("StEmcRawMaker");
    gSystem->Load("StEmcADCtoEMaker");
    gSystem->Load("StPreEclMaker");
    gSystem->Load("StEpcMaker");
    gSystem->Load("StEmcSimulatorMaker");
    gSystem->Load("StEmcUtil");
    gSystem->Load("StDbLib");
    gSystem->Load("StDbBroker");
    gSystem->Load("StDetectorDbMaker");
    gSystem->Load("St_db_Maker");
    gSystem->Load("StEEmcDbMaker");
    gSystem->Load("StEEmcUtil");// needed by EEMC-Db
    gSystem->Load("StJetFinder");    
    gSystem->Load("StJetMaker");
  
    double pi = atan(1.0)*4.0;
    cout << " loading done " << endl;
  
    chain= new StChain("StChain"); 
    chain->SetDebug(1);
    gMessMgr->SwitchOff("D");
    gMessMgr->SwitchOff("I");

    //StIOMaker - to read geant files
    StIOMaker* ioMaker = new StIOMaker();
    ioMaker->SetFile(geantfile.Data());
    ioMaker->SetIOMode("r");
    ioMaker->SetBranch("*",0,"0");             //deactivate all branches
    ioMaker->SetBranch("geantBranch",0,"r");   //activate geant Branch
    //ioMaker->SetBranch("eventBranch",0,"r");   //don't read event branch!
    //ioMaker->SetDebug();
    
    // Instantiate StMcEventMaker - to get pythia pid from McEvent 
    class StMcEventMaker *mcEventMaker = new StMcEventMaker();
    mcEventMaker->doPrintEventInfo = false;
    mcEventMaker->doPrintMemoryInfo = false;

    //Instantiate the MuDstReader
    StMuDebug::setLevel(1); 
    StMuDstMaker* muDstMaker = new StMuDstMaker(0,0,"",mudstfile.Data(),"",1e6,"MuDst");

    //Instantiate Trigger maker for simulation -- only need ADC from MuDst -- no calibration
    StJetSimuTrigMaker *trig=new StJetSimuTrigMaker("SimuTrig");
    trig->setPrintOption(0);
  
    //Database -- get a real calibration (this is ok, MLM)
    St_db_Maker *dbMk =new St_db_Maker("db","MySQL:StarDb","$STAR/StarDb","StarDb");
    dbMk->SetDateTime(20040427,101529); //run 5118011 status in DB

    //Database interface
    StDetectorDbMaker* detDbMk = new StDetectorDbMaker();

    //Endcap DB
    StEEmcDbMaker* eemcb = new StEEmcDbMaker("eemcDb");

    //get BEMC calibration (this is ok, MLM)
    //StEmcADCtoEMaker *adc = new StEmcADCtoEMaker(); // this will just convert what's in MuDst to ADC, use for data only!
    StEmcSimulatorMaker* emcSim = new StEmcSimulatorMaker(); //use this instead to "redo" converstion from geant->adc
    StPreEclMaker* preEcl = new StPreEclMaker(); //need this to fill new StEvent information

    //Instantiate Maker with Pythia event record etc for simulation (this guy doesn't break chain, MLM)
    StJetSimuWeightMaker *weight= new StJetSimuWeightMaker("SimuWeight");
    weight->setPrintOption(0);
  
    //Instantiate Tree Maker for simulation (this guy doesn't break chain, MLM)
    StJetSimuTreeMaker *stree= new StJetSimuTreeMaker("SimuTree",simufile.Data());
    stree->setPrintOption(0);
  
    //test Mike's new 4p maker:
    StBET4pMaker* bet4pMaker = new StBET4pMaker("BET4pMaker",muDstMaker);

    //Pythia4pMaker
    StPythiaFourPMaker* pythiaFourPMaker = new StPythiaFourPMaker("StPythiaFourPMaker",weight, mcEventMaker);

    //Instantiate the JetMaker
    StJetMaker* emcJetMaker = new StJetMaker("emcJetMaker", muDstMaker, jetfile.Data());

    //Instantiate Jet Histogram Maker
    StJetHistMaker* jetHistMaker = new StJetHistMaker(muDstMaker, histfile.Data() );
  
    //Now setup two jet analyses that use the same track/jet cuts  
    //set the analysis cuts: (see StJetMaker/StppJetAnalyzer.h -> class StppAnaPars )
    StppAnaPars* anapars = new StppAnaPars();
    anapars->setFlagMin(0); //track->flag() > 0
    anapars->setNhits(20); //track->nHitsFit()>20
    anapars->setCutPtMin(0.2); //track->pt() > 0.2
    anapars->setAbsEtaMax(1.6); //abs(track->eta())<1.6
    anapars->setJetPtMin(5.0);
    anapars->setJetEtaMax(100.0);
    anapars->setJetEtaMin(0);
    anapars->setJetNmin(0);
  
    //Setup the kt finder for measured particles (See StJetFinder/StKtCluFinder.h -> class StKtCluPars)
    StKtCluPars* ktpars = new StKtCluPars();
    ktpars->setR(0.7);
    ktpars->setDebug(false);
    emcJetMaker->addAnalyzer(anapars, ktpars, bet4pMaker, "KtJet");

    //set the analysis cuts for pythia clustering: (see StJetMaker/StppJetAnalyzer.h -> class StppAnaPars )
    StppAnaPars* pythiapars = new StppAnaPars();
    pythiapars->setFlagMin(0);
    pythiapars->setNhits(0);
    pythiapars->setCutPtMin(0.0001);
    pythiapars->setAbsEtaMax(5.0);
    pythiapars->setJetPtMin(5.0);
    pythiapars->setJetEtaMax(5.0);
    pythiapars->setJetEtaMin(0);
    pythiapars->setJetNmin(0);
    
    emcJetMaker->addAnalyzer(pythiapars, ktpars, pythiaFourPMaker, "PythiaKtJet");
    
    chain->Init();
    chain->PrintInfo();

    //Note: ------------ Must do this stuff after Init()
    int controlVal = 2;
    controlEmcSimulatorMaker_st* simControl = emcSim->getControlSimulator()->GetTable();
    simControl->keyDB[0] = controlVal;
    simControl->keyDB[1] = 0;
    simControl->keyDB[2] = controlVal;
    simControl->keyDB[3] = controlVal;
    //keyDB[det] = 0 -> NO database (default value)
    //           = 1 - only gains are applied
    //           = 2 - gains and pedestals are applied
    // In other words, for pure MC should be 2, and
    // for embedding should be 1.

    chain->ls(3);

    TChain* fileChain = muDstMaker->chain();
    assert(fileChain);
    int ntotal = fileChain->GetEntries();

    //int nevents =1000000;
    for (Int_t iev=0;iev<nevents; iev++) {
	cout << "****************************************** " << endl;
	cout << "Working on eventNumber:\t" << iev <<"\tof:\t"<<ntotal<<endl;
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


string firstHalf(string infile, string begin, string end, int offset=0)
{
    unsigned int where1 = infile.find(begin);
    unsigned int where2 = infile.find(end);
    if (where2==infile.npos) {
	return 0;
    }
    
    int start=where1+begin.size()+offset;
    int stop=where2;
    if (stop<=start) {
	cout <<"error, mismatch.abort()"<<endl;
	abort();
    }
    //cout <<"numbers of events is between indices: ["<<start<<","<<stop<<")"<<endl;
    string number;
    for (int i=start; i<stop; ++i) {
	//cout <<"\ti:\t"<<i<<"\t"<<infile[i]<<endl;
	number += infile[i];
    }
    return number;
}

