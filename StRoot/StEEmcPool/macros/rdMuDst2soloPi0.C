#include <StRoot/StEEmcDbMaker/EEmcDbItem.h>
class StChain;
class StMuEmcCollection;
class   EEmcDbItem;
class StEEmcDbMaker;
class StMuDstMaker;
class TChain;

StEEmcDbMaker  *myDb;
StMuDstMaker* muMk;
StChain *chain=0;


int rdMuDst2soloPi0
(
 TString fullName="aa2",
 int nEve=50000,
 Int_t nFiles  = 100, 
 char* file="eemcCalib.lisX1M", // eemcCalib  800K eve
 char* inDir   = "./"
){ 
  
  // inDir  = "/star/data37/reco/eemcCalibration/ReversedFullField/P04id/2004/085/";
  //file="st_physics_5085013_raw_2040001.MuDst.root"; // eemcCalib
  //file="st_physics_5085017_raw_4010001.MuDst.root"; // prod62GeV


  // file="R5091010muDst.lis"; // eemcCalib 28.8K eve

  //file="R5085017muDst.lis"; //prod62


  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();
  cout << " loading done " << endl;
  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");
  gSystem->Load("StEEmcDbMaker");
  gSystem->Load("StEEmcUtil");  
  gSystem->Load("StEEsoloPi0");  

  // create chain    
  chain = new StChain("StChain"); 

  printf("adding muDst from '%s' ....\n",file);
  // Now we add Makers to the chain...   
  muMk = new StMuDstMaker(0,0,inDir,file,"MuDst.root",nFiles);
  TChain* tree=muMk->chain(); assert(tree); 
  int nEntries=tree->GetEntries();
  printf("total eve in chain =%d\n",nEntries);
  //  return;  
  myDb=new StEEmcDbMaker("eemcDb");
  St_db_Maker *dbMk = new St_db_Maker("StarDb", "MySQL:StarDb");

  myMk3=new StEEsoloPi0Maker("soloPi0","MuDst");
  TObjArray  HList;
  myMk3->Set(&HList);
  // myMk1->setSectors(1,8);
  myDb->setTimeStampDay(20040320);  // format: yyyymmdd
  //myMk1->setPreferedFlavor("set-b","eemcPMTcal");



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

    if(eventCounter%100!=0)continue;

    printf("\n\n ====================%d  processing  ==============\n", eventCounter);

  }
  printf("sorting done, nEve=%d of %d\n",nEve, nEntries);
  int t2=time(0);
  float rate=1.*nEve/(t2-t1);
   printf("sorting done %d of   nEve=%d, CPU rate=%.1f Hz\n",eventCounter,nEntries,rate);

   chain->Finish();
   //   HList.ls();
   fullName+=".hist.root";
   TFile f( fullName,"recreate");
   assert(f.IsOpen());
   printf("%d histos are written  to '%s' ...\n",HList.GetEntries(),fullName.Data());
   HList.Write();
   f.Close();
   assert(!f.IsOpen());

}
