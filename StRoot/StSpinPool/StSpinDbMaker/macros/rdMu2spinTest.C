class StChain;
class StMuEmcCollection;

class StEEmcDbMaker;
class StMuDstMaker;
class TChain;

StEEmcDbMaker  *myDb;
StMuDstMaker* muMk;
StChain *chain=0;


int rdMu2spinTest( char* Rrun    ="R6173011",
		   int off7=0,  int off48=0,
		   Int_t nFiles  = 20,
		   char* inDir   = "./lis/",
		   int nEve=2000)
{

  #define  USE_DB  
  char *outPath="iter1/";

  //old

 // char* file="/star/data46/reco/ppProduction/FullField/P05if/2005/173/st_physics_6173071_raw_2030015.MuDst.root"
 // char* file="/star/data03/daq/2005/janMisc/st_physics_6156028_raw_2040010.MuDst.root"
  //char* file="/star/data05/scratch/balewski/2005-bXing-muDst/174/st_fast_6174031_raw_2100003.MuDst.root"


  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();
  cout << " loading done " << endl;

  TString fileL=Rrun; fileL+=".lis";
  int runNo=atoi(Rrun+1);

#ifdef USE_DB
  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");
  gSystem->Load("StSpinDbMaker");
#endif
    
  // create chain    
  chain = new StChain("StChain"); 

  printf("adding muDst from '%s' ....\n",fileL.Data());
  // Now we add Makers to the chain...   
  muMk = new StMuDstMaker(0,0,inDir,fileL,"MuDst.root",nFiles);
  TChain* tree=muMk->chain(); assert(tree); 
  int nEntries=(int)tree->GetEntries();
  printf("total eve in chain =%d\n",nEntries);

#ifdef USE_DB
  St_db_Maker *stDb = new St_db_Maker("StarDb", "MySQL:StarDb");
  spDb=new StSpinDbMaker("spinDb");
  
  gMessMgr->SwitchOff("D");
  gMessMgr->SwitchOn("I");
  hbxI=new TH1F("bXI","Intended fill pattern vs. STAR bXing; bXing at STAR IP",120,-0.5,119.5);
#endif

  hbx48=new TH1F("bX48","Rate vs. true bXing from bx48; bXing at STAR IP",120,-0.5,119.5);

  hbx7=new TH1F("bX7","Rate vs. true bXing from bx7; bXing at STAR IP",120,-0.5,119.5);

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
   // Access to muDst .......................
    StMuEvent* muEve = muMk->muDst()->event();
    int nPrim = muMk->muDst()->primaryTracks()->GetEntries();  // get number of primary tracks
    StEventInfo &info=muEve->eventInfo();

    StL0Trigger *trig=&(muEve->l0Trigger());
    int bx48=trig->bunchCrossingId();
    int bx7=trig->bunchCrossingId7bit(runNo);
    int bxStar48= (bx48+off48)%120;
    int bxStar7= (bx7+off7)%120;
    hbx48->Fill(bxStar48);
    hbx7->Fill(bxStar7);
   if(eventCounter%500==0) printf("\n\n ====================%d  processing eventID %d nPrim=%d ==============\n", eventCounter++,info.id(),nPrim);

  }

  if(eventCounter>0) printf("sorting done, nEve=%d of :%d :%d :is missing\n",eventCounter, nEntries,-eventCounter+nEntries);
  t2=time(0);
  if(t2==t1) t2=t1+1;
  float rate=1.*eventCounter/(t2-t1);
  float nMnts=(t2-t1)/60.;  printf("sorting done %d of   nEve=%d, CPU rate=%.1f Hz tot %.1f minutes\n",eventCounter,nEntries,rate,nMnts);
  
  TString fileH=outPath; fileH+=Rrun; fileH+=".hist.root";
  printf(" saving -->%s\n",fileH.Data());
  new TFile(fileH,"recreate");
  hbx48->Write();
  hbx7->Write();
  
#ifdef USE_DB
  //play with spinDb information
  // spDb->print(0);
  const int * spin8bits=spDb->getSpin8bits();
  for(int bx=0;bx<120;bx++){
    bool isFilled=(spin8bits[bx] & 0x11)==0x11;
    if(isFilled) hbxI->Fill(bx);

    //    if(spDb->isBXfilledUsingBX48(bx48)) hbxI->Fill(bxStar);
    //printf("bxStar=%3d   bx48=%3d   fill=%d\n",bxStar,bx48,spDb->isBXfilledUsingBX48(bx48));
  }
  hbxI->Write();
#endif
  //fileH->Close();
  chain->Finish();
  
  
  
}
