class StChain;
class StMuEmcCollection;

class StEEmcDbMaker;
class StMuDstMaker;
class TChain;
class   TObjArray;


StEEmcDbMaker  *myDb;
StMuDstMaker* muMk;
StChain *chain=0;
TObjArray * HList;


int rdMu2SmdCal( int nEve=100 ){
  int firstSec=5;
  int lastSec=5;
 
  Int_t nFiles  = 10; 
  char* file="st_physics_5089002_raw_3010001.MuDst.root";
  char* inDir   = "/star/data41/reco/eemcCalibration/ReversedFullField/P04id/2004/089/";
  char *outname="jan";

  //M-C input Jason
  // file="mcpi+_5000_06TC05_20.MuDst.root";
  file="mcgamma_500_06TC05_5.MuDst.root";
  inDir="/star/data04/sim/jwebb/MonteCarlo/single_gamma/";

  //M-C input Murad
  inDir="/star/data04/sim/msar/cal/";  
  //  file="mc_pi-_10000_sector05_1.MuDst.root";
  file="mc_pi-_1000_sector05_5.MuDst.root";
  // file="Jan.MuDst.root";
  TString fullName=file;

  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();
  cout << " loading done " << endl;
  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");
  gSystem->Load("StEEmcDbMaker");
  gSystem->Load("StEEmcUtil");  
  //  return;
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
  myDb->setSectors(firstSec, lastSec);
  myDb->setPreferedFlavor("expoSlope1","eemcPIXcal");
  myDb->changeGains("msarCalib/towgains.dat");
  myDb->changeGains("msarCalib/SMDgains.dat");
  myDb->changeGains("msarCalib/PQRgains.dat");
  myDb->changeMask( "msarCalib/setM1.dat");
 
  gMessMgr->SwitchOff("D");
  gMessMgr->SwitchOff("I");

  HList=new TObjArray;

  //........... sorters ..........
  float thrMipSmdE=0.1; // MeV
  int emptyStripCount=8;
  float twMipRelEneLow=0.3, twMipRelEneHigh=2.5;
  float presMipElow=0.3, presMipEhigh=3.;

  const int mSect=1+lastSec-firstSec; assert(mSect>0); assert(mSect<=12);
  StEEsmdCalMaker **sorterA=new  StEEsmdCalMaker * [mSect];

 int j;
 for(j=0;j<mSect;j++) {
    sorterA[j]=new  StEEsmdCalMaker("eeSmdCal","MuDst");
    sorterA[j]->SetSector(j+firstSec);
    sorterA[j]->SetHList(HList);
    sorterA[j]->setTwCuts( presMipElow,presMipEhigh );
    sorterA[j]->setSmdCuts(thrMipSmdE/1000,emptyStripCount);
    sorterA[j]->setPQRCuts(presMipElow,presMipEhigh);
    sorterA[j]->SetMCflag();
  }

  chain->ls(3);
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


  sorterA[0]->finish(1);
  chain->Finish();
  //  return;
  // save output histograms 
  // HList.ls();
  TString out="outX/";
  out+=outname;
  out+=".hist.root";
  TFile f( out,"recreate");
  assert(f.IsOpen());
  printf("%d histos are written  to '%s' ...\n",HList->GetEntries(),out.Data());
  HList->Write();
  f.Close();


   h=(TH1F *) HList->FindObject("myStat05"); 
   assert(h);
  float *my=h->GetArray();
  printf("%d -->UxV multi=%.1f  one=%.1f any=%.1f non1x1=%.1f\n",nEve,my[2],my[3],my[4],my[5]);
  printf(" -->MIP tw=%.1f cntr=%.1f \n",my[6],my[7]);

  //  h2=(TH1F *) HList.FindObject("xy05ct");   assert(h2);
  // h2->Draw();
}

