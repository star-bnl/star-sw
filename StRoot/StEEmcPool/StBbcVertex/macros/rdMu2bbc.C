
class StChain;
class StMuEmcCollection;

class StMuDstMaker;
class TChain;

StMuDstMaker* muMk;
StChain *chain=0;

int rdMu2bbc(
 TString fullName="ccX",
 int nEve=10000,
 Int_t nFiles  = 4,
 char* file="inp/R5112017.lis", // min-b
 //char* file="inp/R5118053.lis", // prodPP
 char* inDir   = "./",
 char* outDir   = "outBbc/"
){ 
  
  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();
  cout << " loading done " << endl;
  gSystem->Load("myTest1");  

  // create chain    
  chain = new StChain("StChain"); 

  printf("adding muDst from '%s' ....\n",file);
  // Now we add Makers to the chain...   
  muMk = new StMuDstMaker(0,0,inDir,file,"MuDst.root",nFiles);
  TChain* tree=muMk->chain(); assert(tree); 
  int nEntries=tree->GetEntries();
  printf("total eve in chain =%d\n",nEntries);

  myMk3=new StBbcVertexMaker("bbcVertex","MuDst");
  TObjArray  HList;
  myMk3->SetHList(&HList);
  myMk3->readCalib("outBbc/bbcEcalib2.dat");
  myMk3->readCalib("outBbc/bbcWcalib2.dat");
  myMk3->setTdcCalib(2.0); // cm/tdc ch

  gMessMgr->SwitchOff("D");
  gMessMgr->SwitchOn("I");
 
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

    if(eventCounter%1000!=0)continue;

    printf("\n\n ====================%d  processing  ==============\n", eventCounter);

  }
  printf("sorting done, nEve=%d of %d\n",nEve, nEntries);
  int t2=time(0);
  float rate=1.*eventCounter/(t2-t1);
  float nMnts=(t2-t1)/60.;
  printf("sorting done %d of   nEve=%d, CPU rate=%.1f Hz, tot %.1f minutes\n",eventCounter,nEntries,rate,nMnts);

   chain->Finish();
   //   HList.ls();
   fullName+=".hist.root";
   TFile f( outDir+fullName,"recreate");
   assert(f.IsOpen());
   printf("%d histos are written  to '%s' ...\n",HList.GetEntries(),fullName.Data());
   HList.Write();
   f.Close();
   assert(!f.IsOpen());

}
