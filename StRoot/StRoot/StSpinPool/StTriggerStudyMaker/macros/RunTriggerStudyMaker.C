#include <vector>
using namespace std;
class  StChain;
StChain *chain;
int total=0;

void RunTriggerStudyMaker(int nevents=10,
			const char* file = "test.list",
			  const char* outfile = "triggerstudy.root",
			//const char* file="st_physics_7156024_raw_1010001.MuDst.root",
			const char* dir = "./",
			const char *filter = "",
			const char* outPath = "./")
{
    cout <<"MuDst chain file:\t"<<file<<endl;
    //abort();
    gROOT->Macro("LoadLogger.C");
    gROOT->Macro("loadMuDst.C");
    gSystem->Load("StDaqLib");
    gSystem->Load("StDbBroker");
    gSystem->Load("StDetectorDbMaker");
    gSystem->Load("St_db_Maker");
    gSystem->Load("StEEmcUtil");
    gSystem->Load("StEEmcDbMaker");
    gSystem->Load("StTriggerFilterMaker");
    gSystem->Load("StEmcRawMaker");
    gSystem->Load("StEmcADCtoEMaker");
    gSystem->Load("StTriggerUtilities");
    gSystem->Load("StTriggerStudyMaker");
    
    
    double pi = atan(1.0)*4.0;
    cout << " loading done " << endl;
   
    chain= new StChain("StChain"); 
    chain->SetDebug(1);



    //Instantiate the MuDstReader
    StMuDebug::setLevel(1); 
    StMuDstMaker* muDstMaker = new StMuDstMaker(0,0,dir,file,filter,1000000,"MuDst");

    //Database
    St_db_Maker *dbMk = new St_db_Maker("StarDb", "MySQL:StarDb");
    StEEmcDbMaker* eemcb = new StEEmcDbMaker("eemcDb");

    //Block for TriggerSimuMaker
    
    StEmcADCtoEMaker *adc = new StEmcADCtoEMaker();

    StTriggerSimuMaker* trigsim = new StTriggerSimuMaker();
    trigsim->useBbc();
    trigsim->useBemc();
    trigsim->bemc->setConfig(StBemcTriggerSimu::kOffline);
    StGenericL2Emulator* simL2Mk = new StL2_2006EmulatorMaker;
    assert(simL2Mk);
    simL2Mk->setSetupPath("/afs/rhic.bnl.gov/star/users/kocolosk/public/StarTrigSimuSetup/");
    simL2Mk->setOutPath(outPath);
    trigsim->useL2(simL2Mk);

    StTriggerStudyMaker* trigstudy = new StTriggerStudyMaker(outfile);
    
    //end TriggerSimuMaker block
 
    chain->Init();
    
    chain->PrintInfo();
    
    for (Int_t iev=0;iev<nevents; iev++) {
      /*
    cout << "****************************************** " << endl;
    cout << "Working on eventNumber " << iev << endl;
    cout << "*************************1***************** " << endl;
      */
    chain->Clear();
    int iret = chain->Make(iev); 
    total++;
    
    if (iret && iret!=kStSkip) {
      cout << "Bad return code!" <<iret<< endl;
        break;
    }
    
    //Here's where you can do your analysis, for an example look in this method
    } 
    chain->Finish(); 
    cout << "****************************************** " << endl;
    cout << "total number of events  " << total << endl;
    cout << "****************************************** " << endl;      
    
    
}
