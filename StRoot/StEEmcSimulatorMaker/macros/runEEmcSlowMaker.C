class StChain;
class StMuEmcCollection;

class StEEmcDbMaker;
class StMuDstMaker;
class TChain;
class TObjArray;


StEEmcDbMaker *myDb;
StMuDstMaker* muMk;
StChain *chain=0;
TObjArray *HList;

// Comment out the line below to read EEMC data from StEvent (default is MuDst)
#define useMuDst

int runEEmcSlowMaker( int nEve=5000 ){

  int firstSec=1;
  int lastSec=12;

  Int_t nFiles=10; 

#ifdef useMuDst
  //char* file="rcf1210_164_4219evts.MuDst.root";
  char* file="mcgamma_svtin_nominalvtx_5000evts.MuDst.root";
#else
  //char* file="rcf1210_164_4219evts.event.root";
  char* file="mcgamma_svtin_nominalvtx_5000evts.event.root";
#endif

  //char* inDir="/star/data19/reco/pp200/pythia6_203/default/minbias/y2004a/gheisha_on/trs_ij/";
  char* inDir="/star/data04/sim/jwebb/2006/";
  char *outname="jan";
  TString fullName=file;

  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();
  cout << " loading done " << endl;
  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");
  gSystem->Load("StEEmcDbMaker");
  gSystem->Load("StEEmcUtil");
  gSystem->Load("StEEmcSimulatorMaker");

  // create chain    
  chain = new StChain("StChain"); 

  printf("adding MuDst/StEvent from run '%s' ....\n",fullName.Data());

  // Now we add Makers to the chain...
#ifdef useMuDst
  StMuDstMaker* muMk = new StMuDstMaker(0,0,inDir,fullName,".MuDst.root",nFiles);
  TChain* tree=muMk->chain(); assert(tree); 
  int nEntries=tree->GetEntries();
  printf("total eve in chain =%d\n",nEntries);
#else
  StIOMaker* ioMaker = new StIOMaker;
  ioMaker->SetFile(gSystem->ConcatFileName(inDir, file));
  ioMaker->SetBranch("*", 0, "0");
  ioMaker->SetBranch("eventBranch", 0, "r");
  ioMaker->SetBranch("geantBranch", 0, "r");
#endif

  St_db_Maker *stDb = new St_db_Maker("StarDb", "MySQL:StarDb");
  stDb->SetDateTime(20031120,0);
  stDb->SetFlavor("sim","eemcPMTcal");
  stDb->SetFlavor("sim","eemcPIXcal");
  stDb->SetFlavor("sim","eemcPMTped");
  stDb->SetFlavor("sim","eemcPMTstat");
  stDb->SetFlavor("sim","eemcPMTname");
  stDb->SetFlavor("sim","eemcADCconf");

  StEEmcDbMaker* myDb=new StEEmcDbMaker("eemcDb");
  myDb->setSectors(firstSec, lastSec);


  gMessMgr->SwitchOn("D");
  gMessMgr->SwitchOn("I");
  gMessMgr->SwitchOn("W");

  HList=new TObjArray;

  /*  
   * The slow simulator is setup by default with the best
   * estimates we have for the single p.e. resolution, energy
   * deposited by a single mip in an smd/pre/postshower layer,
   * and number of photoelectrons produced per mip.  If one
   * really wants to play with fire, these are documented
   * in the header file.
   *
   */
  StEEmcSlowMaker *slowSim=new StEEmcSlowMaker();
  slowSim->setDropBad(0);   // 0=no action, 1=drop chn marked bad in db
  slowSim->setAddPed(0);    // 0=no action, 1=ped offset from db
  slowSim->setSmearPed(0);  // 0=no action, 1=gaussian ped, width from db
  slowSim->setOverwrite(1); // 0=no action, 1=overwrite muDst values

#ifdef useMuDst
  slowSim->setSource("MuDst");
#else
  slowSim->setSource("StEvent");
#endif

  /// Uncomment the following line to save debuging histograms
  //$$$  slowSim->SetHList(HList);
  
  // Put your analysis maker here
  // StMyAnaMaker *bhla =new  StMyAnaMaker;

  chain->ls(3);
  chain->Init();

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

  //  return;
  // save output histograms 
  // HList.ls();

  /// Change to 1 to save debugging histograms
#if 0  
  TString out="./";
  out+=outname;
  out+=".hist.root";
  TFile f( out,"recreate");
  assert(f.IsOpen());
  printf("%d histos are written  to '%s' ...\n",HList->GetEntries(),out.Data());
  HList->Write();
  f.Close();
#endif  
}
