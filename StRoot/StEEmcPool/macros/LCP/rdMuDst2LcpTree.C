class StChain;
StChain *chain=0;

int rdMuDst2LcpTree(  const char *muDstList="400K.lis", // sample of R3012008+10+12+13
		      int maxList=6,
		      char * wrkDir="./wrkLcpX/",
		      int off48=0, 
		      int maxEve=3000,
		      float maxEta=1.0
		      ){ 

  if (gClassTable->GetID("TTable") < 0) {
    gSystem->Load("libTable");
    gSystem->Load("libPhysics");
  }     
  printf("Use muDst list= '%s'\n",muDstList);
  
  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();
  cout << " loading done " << endl;
  
  // Load my maker
  assert(gSystem->Load("LCP")==0 );
  //assert(gSystem->Load("MikesRejector")==0 );// Mike's rejector
   
  // create chain    
  chain = new StChain("StChain");   
  
  StMuDbReader* db = StMuDbReader::instance();
  muDstMk = new StMuDstMaker(0,0,"",muDstList,"MuDst.root",maxList);  
  
  // Now we add Makers to the chain...   
  
  StMuLcp2TreeMaker *myMk = new StMuLcp2TreeMaker("jasEE","MuDst");
  myMk->SetOutDir(wrkDir);
  myMk->SetOff48(off48);
  
  // avaliable switches
  // myMk->SetMinNFitPoint(nFitP); 
  // myMk->SetMaxDCAxy(maxDCAxy);
  myMk->SetMaxEta(maxEta);
  // myMk->SetMinPt(minPt);
  // myMk->SetMinFitPfrac(FitPfrac);
  // myMk->InitRunFromMake(xRun); // if used turns off TTree, saves only histo, allows to add runs 
   
  chain->Init();
  chain->ls(3);
  // StMuDebug::setLevel(4);  // switch of some debug output
  
  int eventCounter=0;
 
  printf(" requested maxEve=%d\n",maxEve);
  //---------------------------------------------------
  while ( 1) {// loop over events
    eventCounter++;;
    if(eventCounter >maxEve) break;
    chain->Clear();
    int stat = chain->Make();
    if(stat) break;
    if(eventCounter%1000) continue;
    StMuEvent* e = muDstMk->muDst()->event();
    
    int n = muDstMk->muDst()->primaryTracks()->GetEntries();  // get number of primary tracks
    printf(".C: eve=%d Nprim=%d\n",eventCounter,n);
    
  }
  
 
  //  chain->Finish();
  
}





//  /star/data31/reco/ppMinBias/FullField/P03if/2002/...
// /star/data31/reco/ppMinBias/ReversedFullField/P03if/2002/...

///star/data29/reco/pp200/pythia6_203/default/pt5/year2003/gheisha_on/trs_if/","rcf1202_2178_1000evts.MuDst.root",
