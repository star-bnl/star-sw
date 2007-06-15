#include <iomanip>

void RunGammaPythiaMaker(	      
	       int nevents = 10,
	       const char *dir ="",
	       const char* file="/star/institutions/mit/betan/Simulation/photon_9_11.MuDst.root",
	       const char *fname="/star/institutions/mit/betan/Simulation/photon_9_11.geant.root",
	       const char *filter = "",
	       )
{
  gROOT->LoadMacro("StRoot/StGammaMaker/macros/loadGammaLibs.C");
  loadGammaLibs();

  StChain* chain = new StChain("StChain"); 
  chain->SetDebug(1);
  gMessMgr->SwitchOff("D");
  gMessMgr->SwitchOff("I");
  
  StIOMaker* ioMaker = new StIOMaker();
  ioMaker->SetFile(fname);
  ioMaker->SetIOMode("r");
  ioMaker->SetBranch("*",0,"0");             //deactivate all branches
  ioMaker->SetBranch("geantBranch",0,"r");   //activate geant Branch
    
  StMuDebug::setLevel(1); 
  StMuDstMaker* muDstMaker = new StMuDstMaker(0,0,dir,file,filter,1e6,"MuDst"); 

  StMcEventMaker* mcEventReader = new StMcEventMaker;

  StGammaPythiaMaker *weight = new StGammaPythiaMaker("GammaPythia");
  
  TChain* fileChain = muDstMaker->chain();
  int total = 0;
  int ntotal = fileChain->GetEntries();
  
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
    cout << "Number of pi0: " << weight->numberOfPion0() << '\n';
    cout << "Number of prompt photons: " << weight->numberOfPrompt() <<'\n';
    cout << "Number of decay photons: " << weight->numberOfDecay() << '\n';
    cout << endl;
  }
  
  chain->Finish();
  cout << "****************************************** " << endl;
  cout << "total number of events  " << total << endl;
  cout << "****************************************** " << endl;
}
