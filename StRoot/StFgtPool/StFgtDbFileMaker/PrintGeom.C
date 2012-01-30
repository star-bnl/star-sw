class StChain;
StChain *chain;

void PrintGeom(Int_t nevents=1)  
{
  gROOT->Macro("loadMuDst.C");
  gROOT->Macro("LoadLogger.C");
 
  gSystem->Load("StFgtUtil");
  gSystem->Load("StFgtDbFileMaker");

  chain =new StChain("StChain");
  
  StFgtDbFileMaker *fgtDbF=new StFgtDbFileMaker();
  chain->Init();
  chain->PrintInfo();
  
  Int_t total=0;

  for (Int_t iev=0; iev<nevents; iev++) {
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
  } 
  chain->Finish(); 
}
