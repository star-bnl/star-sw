TObjArray  *HList;
TH2F* h2D=0;

class St_db_Maker;
St_db_Maker *stDb = 0;

//=================================================
plDAQ2Ped(char *core0 = "" ) { 
  
  char *path=""; 
  TString fullInpName=path;  fullInpName+=core0;
  fullInpName+=".hist.root";  
  fd=new TFile(fullInpName);
  if(! fd->IsOpen()) {
    printf("ERROR: input histo file not found, quit\n",fullInpName.Data());
    return;
  }
  
  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();
  
  // libraries below are needed for DB interface
  assert( !gSystem->Load("StDbBroker")); 
  assert( !gSystem->Load("St_db_Maker")); 
  assert( !gSystem->Load("StEEmcDbMaker"));
  
  assert( !gSystem->Load("StEEmcUtil"));
  assert( !gSystem->Load("StEEmcDAQ2Ped"));

  StChain* chain = new StChain("StChain");

  stDb = new St_db_Maker("StarDb", "MySQL:StarDb");
  
  // for slope fits need a timestamp for pedestals (set by hand for now)
  stDb->SetDateTime(20090424,0);//time stamp for run 10114001
  
  StEEmcDbMaker *myDb=new StEEmcDbMaker("eemcDb");

  HList=new  TObjArray;
  StEEmcDAQ2Ped* daq2ped=new StEEmcDAQ2Ped("daq2ped",fd);
  daq2ped->SetHList(HList);
  
  daq2ped->ChooseSet(0); //sets marker type for slope histos
  daq2ped->MappingFile("crMap.csv"); //file to store Mapmt mapping (sometimes useful)
  chain->Init();
  chain->Make();
  chain->Finish();

  TString out;
  out+="Rnnn";
  out+="ped.hist.root";
  TFile f( out,"recreate");
  assert(f.IsOpen());
  printf("%d histos are written  to '%s' ...\n",HList->GetEntries(),out.Data());
  HList->Write();
  f.Close();
  
}



