#include <vector>
using namespace std;
class  StChain;
StChain *chain;
int total=0;

void RunLuminosityMaker(int nevents=10,
			const char* file = "test.list",
			//const char* file="st_physics_7156024_raw_1010001.MuDst.root",
          const char* dir = "./",
          const char *filter = "")
{
    cout <<"MuDst chain file:\t"<<file<<endl;
    //abort();
    gROOT->Macro("LoadLogger.C");
    gROOT->Macro("loadMuDst.C");
    gSystem->Load("StDaqLib");
    gSystem->Load("StDbBroker");
    gSystem->Load("StDetectorDbMaker");
    gSystem->Load("St_db_Maker");
    gSystem->Load("StTriggerFilterMaker");
    gSystem->Load("StEmcRawMaker");
    gSystem->Load("StEmcADCtoEMaker");
    gSystem->Load("StTriggerUtilities");
    gSystem->Load("StLuminosityMaker");
    
    
    double pi = atan(1.0)*4.0;
    cout << " loading done " << endl;
   
    chain= new StChain("StChain"); 
    chain->SetDebug(1);



    //Instantiate the MuDstReader
    StMuDebug::setLevel(1); 
    StMuDstMaker* muDstMaker = new StMuDstMaker(0,0,dir,file,filter,1000000,"MuDst");

    //Database
    St_db_Maker *dbMk = new St_db_Maker("StarDb", "MySQL:StarDb");

    /*
    StTriggerFilterMaker* trigfilt = new StTriggerFilterMaker();
    trigfilt->addTrigger(96011);
    trigfilt->addTrigger(96201);
    trigfilt->addTrigger(96211);
    trigfilt->addTrigger(96221);
    trigfilt->addTrigger(96233);
    */

    //Block for TriggerSimuMaker
    
    StEmcADCtoEMaker *adc = new StEmcADCtoEMaker();

    StTriggerSimuMaker* trigsim = new StTriggerSimuMaker();
    trigsim->useBbc();
    trigsim->useBemc();
    trigsim->bemc->setConfig(StBemcTriggerSimu::kOffline);
    /*
    StGenericL2Emulator* simL2Mk = new StL2_2006EmulatorMaker;
    assert(simL2Mk);
    simL2Mk->setSetupPath("./StRoot/StJetMaker/StarTrigSimuSetup/");
    char outPath[200];
    sprintf(outPath,"./out/");   
    simL2Mk->setOutPath(outPath);
    trigsim->useL2(simL2Mk);
    */
    
    //end TriggerSimuMaker block
 
    StLuminosityMaker* lum = new StLuminosityMaker();
    lum->setVertexCutcm(60);
    //lum->setMode("pp2005");
/*
    lum->addTrigger(96011);
    lum->addTrigger(96201);
    lum->addTrigger(96221);
    lum->addTrigger(96211);
    lum->addTrigger(96233);*/
    lum->setCrossSectionNB(26.1e6);
    //lum->getTriggersFromFilterMaker();
    /*
    lum->setMode("pp2006");//see StLuminosityMaker.cxx for accepted modes
    */
    lum->addTrigger(117001);//mb..this trigger must be added first
    lum->addTrigger(137221);//bemc-jp1-mb
    lum->addTrigger(137222);//bemc-jp1-mb
    lum->addTrigger(137822);//bemc-http-mb-fast
    lum->addTrigger(117300);//zb
    lum->addTrigger(137571);//bemc-jp1
    lum->addTrigger(137575);//bemc-jp0-etot
    lum->addTrigger(137585);//bemc-jp2
    lum->addTrigger(137586);//bemc-http
    lum->addTrigger(137611);//bemc-http-mb-l2gamma
    lum->addTrigger(137501);//bemc-jp0-mb
    lum->addTrigger(137622);//bemc-jp0-etot-mb-L2jet
    //lum->addTrigger(137461);//fpd1-tpcdead-fast

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
    
    //vector<StLuminosityHolder> holder;
    TClonesArray* holder = lum->getHolder();
    StLuminosityHolder* dumlum;

    for(int i = 0; i < holder->GetEntries(); i++){
      char name[100];
      dumlum = (StLuminosityHolder*)holder->At(i);
      int run1 = dumlum->getRunNumber();
      sprintf(name,"lum%i.data",run1);
      ofstream lumout(name);
      vector<unsigned int> triggers = dumlum->getTriggers();
      vector<unsigned int> NTotal = dumlum->getNTotal();
      vector<unsigned int> NCuts = dumlum->getNCuts();
      vector<unsigned int> NVert = dumlum->getNVertex();
      vector<unsigned int> NTrig = dumlum->getNSoftTrig();
      vector<float> LumTotal = dumlum->getLumTotal();
      vector<float> LumCuts = dumlum->getLumCuts();
      vector<float> LumVert = dumlum->getLumVertex();
      vector<float> LumTrig = dumlum->getLumSoftTrig();
      vector<float> Prescales = dumlum->getPrescales();
      for(unsigned int j = 0; j < dumlum->getTriggers().size(); j++){
	lumout<<run1<<"\t"<<triggers[j]<<"\t"<<Prescales[j]<<"\t"<<NTotal[j]<<"\t"<<NCuts[j]<<"\t"<<NVert[j]<<"\t"<<NTrig[j]<<"\t"<<LumTotal[j]<<"\t"<<LumCuts[j]<<"\t"<<LumVert[j]<<"\t"<<LumTrig[j]<<endl;
      }
      lumout.close();
    }
    
}
