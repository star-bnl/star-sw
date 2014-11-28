// read muDst & geant.root in sync

class StChain;
StChain *chain=0;
class St_particle  ;
class  particle_st;

//================================================
//================================================
int rdMuDstAndGeant2Lcp( int maxEve=2, int subSet=2721, char * wrkDir="./wrkLcpX/"){ 
  float maxEta=1.0;

  char * evePath="/star/data26/reco/pp200/pythia_6.203/default/minbias/year2003/hadronic_on/trs_ic/";
  TString coreFile="rcf1200_";
  coreFile+=subSet;
  coreFile+="_2000evts";
  printf("Working on subSet '%s'\n",coreFile.Data());
	 
  TString muDstFile=evePath+coreFile+".MuDst.root";
  TString geantFile =evePath+coreFile+".geant.root";
  
  
  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();
  cout << " loading done " << endl;
  
  // Load my maker
  assert(gSystem->Load("StEEmcPoolLCP")==0 );

  // create chain    
  chain = new StChain("StChain");   
  //chain->SetDebug();
  //chain->PrintInfo();
  
  // set up maker in read mode  
  StIOMaker* IOMk = new StIOMaker("IO","r",geantFile,"bfcTree");
  IOMk->SetDebug();
  IOMk->SetIOMode("r");
  IOMk->SetBranch("*",0,"0");
  IOMk->SetBranch("geantBranch",0,"r");
  
  
  int maxList=1;
  muMk = new StMuDstMaker(0,0,"/",muDstFile,"MuDst.root",maxList); 
  // Now we add Makers to the chain...   
  
  StGeant2LcpTreeMaker *myMk2 = new StGeant2LcpTreeMaker("jasGG","MuDst");
  myMk2->SetOutDir(wrkDir);
  myMk2->InitRunFromMake(888999);


  StMuLcp2TreeMaker *myMk = new StMuLcp2TreeMaker("jasEE","MuDst");
  myMk->SetOutDir(wrkDir);
  myMk->SetMaxEta(maxEta);
  myMk->InitRunFromMake(888999);

  chain->Init();
  chain->ls(3);
  
  // StMuDebug::setLevel(1);  // switch of some debug output
  
  int eventCounter=0;
  
  printf(" requested maxEve=%d\n",maxEve);
  //---------------------------------------------------
  while ( 1) {// loop over events
    eventCounter++;;
    if(eventCounter >maxEve) break;
    chain->Clear();
    int stat = chain->Make();
    if(stat) break;
    //continue;
    // Access to muDst .......................
    StMuEvent* muEve = muMk->muDst()->event();  
    int n = muMk->muDst()->primaryTracks()->GetEntries();  // get number of primary tracks
    StEventInfo &info=muEve->eventInfo();
    
    printf("eve=%d Nprim=%d ID=%d runID=%d\n",eventCounter,n,info.id(),info.runId());
    
  }
  return;  
}
  


//  /star/data31/reco/ppMinBias/FullField/P03if/2002/...
// /star/data31/reco/ppMinBias/ReversedFullField/P03if/2002/...

///star/data29/reco/pp200/pythia6_203/default/pt5/year2003/gheisha_on/trs_if/","rcf1202_2178_1000evts.MuDst.root",
