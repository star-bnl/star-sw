class StChain;
class StMuEmcCollection;

class StEEmcDbMaker;
class StMuDstMaker;
class TChain;

StEEmcDbMaker  *myDb;
StMuDstMaker* muMk;
StChain *chain=0;

int rdMu2SmdCal( int nEve=1000 ){

  Int_t nFiles  = 10; 
  char* file="st_physics_5089002_raw_3010001.MuDst.root";
  char* inDir   = "/star/data41/reco/eemcCalibration/ReversedFullField/P04id/2004/089/";

  //M-C input
  file="mcpi+_5000_06TC05_20.MuDst.root";
  inDir="/star/data04/sim/jwebb/MonteCarlo/single_gamma/";
  //M-C end

  TString fullName=file;

  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();
  cout << " loading done " << endl;
  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");
  gSystem->Load("StEEmcDbMaker");
  gSystem->Load("StEEmcUtil");  
  gSystem->Load("StEzSmdCal");

  // create chain    
  chain = new StChain("StChain"); 

  printf("adding muDst from run '%s' ....\n",fullName.Data());

  // Now we add Makers to the chain...   
  muMk = new StMuDstMaker(0,0,inDir,fullName,".MuDst.root",nFiles);
  TChain* tree=muMk->chain(); assert(tree); 
  int nEntries=tree->GetEntries();
  printf("total eve in chain =%d\n",nEntries);

  St_db_Maker *dbMk = new St_db_Maker("StarDb", "MySQL:StarDb");
  myDb=new StEEmcDbMaker("eemcDb");
  myDb->setSectors(6,6);
  //myDb->setPreferedFlavor("expoSlope1","eemcPIXcal");
  gMessMgr->SwitchOff("D");
  gMessMgr->SwitchOff("I");

  TObjArray  HList;
  myMk3=new StEEsmdCalMaker("eeSmdCal","MuDst");
  myMk3->Set(&HList);
  myMk3->SetSector(6);
  myMk3->SetMCflag();
  float thrMipSmdE=0.2;
  int emptyStripCount=8;
  int iTagLayer=3; //0-3 means T,P,Q,R
  myMk3->setMipCuts(thrMipSmdE,emptyStripCount,iTagLayer);

  chain->ls();
  chain->Init();
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
    if(eventCounter%100==0)
      printf("\n====================%d  processing  ============\n", eventCounter);
    
  }
  printf("sorting done, nEve=%d of %d\n",nEve, nEntries);
  //  return;
  int t2=time(0);
  if(t2==t1) t2=t1+1;
  float tMnt=(t2-t1)/60.;
  float rate=1.*nEve/(t2-t1);
  printf("sorting done %d of   nEve=%d, CPU rate=%.1f Hz, total time %.1f minute(s) \n",eventCounter,nEntries,rate,tMnt);

   chain->Finish();

   // save output histograms

   // HList.ls();
   myMk3->saveHisto("bbb");

   h=(TH1F *) HList.FindObject("myStat");   assert(h);
  float *my=h->GetArray();
  printf("%d -->UxV multi=%.1f  one=%.1f any=%.1f non1x1=%.1f\n",nEve,my[2],my[3],my[4],my[5]);
  printf(" -->MIP tw=%.1f cntr=%.1f \n",my[6],my[7]);
}

