class StChain;
class StMuEmcCollection;

class StEEmcDbMaker;
class StMuDstMaker;
class TChain;

StEEmcDbMaker  *myDb;
StMuDstMaker* muMk;
StChain *chain=0;

// override DB_time_stamp to retrieve some DB information
int rdDb( char *outH="F7327", int idat=20050624,  int itim=91958 ) {
  
  // provide any input file to make the chain exec InitRun()  
  char* inDir   = "/star/data05/scratch/balewski/2005-bXing-muDst4/171/";
  char* Rrun    ="st_physics_6171021_raw_2020001.MuDst.root";
  Int_t nFiles  = 1;
  int nEve=1;
  
  char *outPath="iterF/";

  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();
  cout << " loading done " << endl;

  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");
  gSystem->Load("StSpinDbMaker");
    
  // create chain    
  chain = new StChain("StChain"); 
  
  printf("adding muDst from '%s' ....\n",Rrun);
  // Now we add Makers to the chain...   
  muMk = new StMuDstMaker(0,0,inDir,Rrun,"MuDst.root",nFiles);
  TChain* tree=muMk->chain(); assert(tree); 
  int nEntries=(int)tree->GetEntries();
  printf("total eve in chain =%d\n",nEntries);
  
  St_db_Maker *stDb = new St_db_Maker("StarDb", "MySQL:StarDb");

  stDb->SetDateTime(idat,itim);
 
  spDb=new StSpinDbMaker("spinDb");
  
  gMessMgr->SwitchOff("D");
  gMessMgr->SwitchOn("I");
  hbxI=new TH1F("bXI","Intended fill pattern vs. STAR bXing; bXing at STAR IP",120,-0.5,119.5);

  
  int t1=time(0);
  chain->Init();
  int t2=time(0);
  printf("%d seconds used to Init() the chain\n",t2-t1);

  chain->ls(3);
  //  return;
  int eventCounter=0;
  int stat=0;
  t1=time(0);

  //---------------------------------------------------
  while ( 1 ) {// loop over events
    if(eventCounter>=nEve) break;
    chain->Clear();
    stat = chain->Make();
    if(stat) break; // end of events
    eventCounter++;
   }

  if(eventCounter>0) printf("sorting done, nEve=%d of :%d :%d :is missing\n",eventCounter, nEntries,-eventCounter+nEntries);
  t2=time(0);
  if(t2==t1) t2=t1+1;
  float rate=1.*eventCounter/(t2-t1);
  float nMnts=(t2-t1)/60.;  printf("sorting done %d of   nEve=%d, CPU rate=%.1f Hz tot %.1f minutes\n",eventCounter,nEntries,rate,nMnts);
  
  TString fileH=outPath; fileH+=outH;  fileH+=".hist.root";
  printf(" saving -->%s\n",fileH.Data());
  new TFile(fileH,"recreate");
  
  //play with spinDb information
  // spDb->print(0); // 0=short, 1=huge
  const int * spin8bits=spDb->getSpin8bits();
  for(int bx=0;bx<120;bx++){
    bool isFilled=(spin8bits[bx] & 0x11)==0x11;
    if(isFilled) hbxI->Fill(bx);
  }
  hbxI->Write();

  chain->Finish();
   
  
}
