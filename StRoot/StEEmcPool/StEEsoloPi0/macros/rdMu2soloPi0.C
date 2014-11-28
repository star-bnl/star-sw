class StChain;
class StMuEmcCollection;

class StEEmcDb;
class StMuDstMaker;
class TChain;

StEEmcDb  *myDb;
StMuDstMaker* muMk;
StChain *chain=0;

int rdMu2soloPi0( 
		 char* file="R7114062.lis", 
		 int nEve=1e3,
		 Int_t nFiles  = 999,
		 char* outDir   = "out2/"
		 ){ 
  char* inDir   = "./runList/";
  
  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();
  cout << " loading done " << endl;
  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");
  gSystem->Load("StEEmcUtil");  
  gSystem->Load("StEEmcDbMaker");
  gSystem->Load("StEEsoloPi0");  

  gROOT->Macro("LoadLogger.C");

  // create chain    
  chain = new StChain("StChain"); 

  printf("adding muDst from '%s' ....\n",file);
  // Now we add Makers to the chain...   
  muMk = new StMuDstMaker(0,0,inDir,file,"MuDst.root",nFiles);
  TChain* tree=muMk->chain(); assert(tree); 
  int nEntries=tree->GetEntries();
  printf("total eve in chain =%d\n",nEntries);

  St_db_Maker *stDb = new St_db_Maker("StarDb", "MySQL:StarDb");

  new StEEmcDbMaker("eemcDb");
  // ->setSectors(5,8);

  myMk3=new StEEsoloPi0Maker("soloPi0","MuDst");
  TObjArray  HList;
  myMk3->SetHList(&HList);

#if 0  // flags for M-C events
  stDb->SetDateTime(20031120,0);
  stDb->SetFlavor("sim","eemcPMTcal");
  stDb->SetFlavor("sim","eemcPIXcal");
  stDb->SetFlavor("sim","eemcPMTped");
  stDb->SetFlavor("sim","eemcPMTstat");
  stDb->SetFlavor("sim","eemcPMTname");
  stDb->SetFlavor("sim","eemcADCconf");
  myMk3->SetMCflag();
#endif
  
 
  chain->Init();
  chain->ls(3);
  //  return;
  int eventCounter=0;
  int stat=0;
  int t1=time(0);
  //---------------------------------------------------
  while ( stat==0 ) {// loop over events
    if(eventCounter>=nEve) break;
    eventCounter++;
    chain->Clear();
    stat = chain->Make();

    if(eventCounter%300!=0)continue;

    printf("\n====================%d  processing  ==============\n", eventCounter);

  }
  printf("sorting done, nEve=%d of %d\n",nEve, nEntries);
  int t2=time(0);
  float rate=1.*eventCounter/(t2-t1);
  float nMnts=(t2-t1)/60.;
  printf("sorting done %d of   nEve=%d, CPU rate=%.1f Hz, tot %.1f minutes\n",eventCounter,nEntries,rate,nMnts);

   chain->Finish();
   //   HList.ls();
   TString fullName=file;  fullName.ReplaceAll(".lis",".hist.root");
   fullName=outDir+fullName;
   TFile f( fullName,"recreate");
   assert(f.IsOpen());
   printf("%d histos are written  to '%s' ...\n",HList.GetEntries(),fullName.Data());
   HList.Write();
   f.Close();
   assert(!f.IsOpen());

}
