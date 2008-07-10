class  StChain;
StChain *chain;
int total=0;
TObjArray  *HList=0;
TFile *fd=0;

void RunJetSimuSkimReader(int nevents=100,
			  const char* jetInFile = "/star/data04/sim/staszak/2006jets/2006jets_30grids/jets_pt4_01.root",
			  const char* skimInFile = "/star/data04/sim/staszak/2006jets/2006jets_30grids/skim_pt4_01.root",
			  const char* histfile = "pt4.hist.root")
{
  
 
  setMacroPath();
  gROOT->Macro("LoadJetLibraries.C");
  double pi = atan(1.0)*4.0;
  cout << " loading done " << endl;
   
  chain= new StChain("StChain"); 
  chain->SetDebug(1);
  gMessMgr->SwitchOn("D");
  gMessMgr->SwitchOff("I");
  
  //Instantiate the JetReader
  StJetSimuReader* jetReader = new StJetSimuReader("JetReader",0);
  
  chain->Init();
  
  //these 3 lines are critical and must be called after chain->Init()
  //Call in exactly this order
  jetReader->InitFile(jetInFile);
  jetReader->InitJetSkimFile(skimInFile);
  int ready = jetReader->preparedForDualRead();
  HList=new TObjArray ;
  jetReader->SetHList(HList);
  jetReader->PythiaAnaHisto();
  
  chain->PrintInfo();
  
  for (Int_t iev=0;iev<nevents; iev++) {
    cout << "****************************************** " << endl;
    cout << "Working on eventNumber " << iev << endl;
    cout << "*************************1***************** " << endl;
    chain->Clear();
    int iret = chain->Make(iev); 
    total++;
    if (iret)  {
      cout << "Bad return code!" << endl;
      break;
    }
    //Here's where you can do your analysis, for an example look in this method
    jetReader->PythiaAna();
  } 
  chain->Finish(); 
  cout << "****************************************** " << endl;
  cout << "total number of events  " << total << endl;
  cout << "****************************************** " << endl;      
  

  fd=new TFile(histfile,"recreate");
  cout<<"Opening "<<histfile<<endl;
  assert(fd->IsOpen());
  cout<<"Open Baby!"<<endl;
  HList->Write();
  printf("%d histos are written  to '%s' ...\n",HList->GetEntries(),fd->GetName());
  fd->Close();
  
}

void setMacroPath() 
 {
   TString path(gROOT->GetMacroPath());
   path = TString(gSystem->Getenv("STAR")) + "/StRoot/StJetMaker/macros:" + path; 
   path = "./StRoot/StJetMaker/macros:" + path;
   path = "./StJetMaker/macros:" + path;
   path = "./macros:" + path;
   path = "../macros:" + path;
   path = ".:" + path;
   gROOT->SetMacroPath(path);
 }


