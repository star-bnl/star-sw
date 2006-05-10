
class StChain;
StChain *chain=0;
TH1F *hbx=0, *hfb=0;
TH2F *hs8ev=0,*hs8db=0, *hs4db=0, *hdbx=0;
TFile *hf=0;

void rdSt2spinTest( Int_t nevents=10){

  char *   fname="/star/data03/daq/2005/janMisc/st_physics_6156028_raw_2040010.event.root";
  //char *   fname="/star/data03/daq/2005/janMisc/st_physics_5135068_raw_2020006.event.root";
  initHisto();
  
  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();
  cout << " loading done " << endl;
  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");
  gSystem->Load("StSpinDbMaker");
    
  // create chain    
  chain = new StChain("bfc"); 
 
  // StIOMaker - to read files ...
  StIOMaker* ioMaker = new StIOMaker();  
  ioMaker->SetFile(fname); 
  ioMaker->SetIOMode("r");
  ioMaker->SetBranch("*",0,"0");  //deactivate all branches
  ioMaker->SetBranch("eventBranch",0,"r");   //activate Event Branch
  ioMaker->SetIOMode("r"); 
 
  St_db_Maker *stDb = new St_db_Maker("StarDb", "MySQL:StarDb");
  spDb=new StSpinDbMaker("spinDb");
  
  gMessMgr->SwitchOff("D");
  gMessMgr->SwitchOn("I");

  chain->ls(3);
  Int_t initStat = chain->Init(); 
  if (initStat) chain->Fatal(initStat, "during Init()");
    
  int istat=0,iev=0;

  // Do the event loop    
  while(1) {
    if (iev>=nevents) break;
    chain->Clear();
    istat = chain->Make(); 
    iev++; 
    if(istat) break; 
    cout << "---------------------- Processing Event : " << iev << " ---------------------- " << istat<<endl;

    if(iev==1){// fill pattern acquired for first event
      // spDb->print(0);
      for(int bx48=0;bx48<120;bx48++){
	int bxStar=spDb->BXstarUsingBX48(bx48);
	if(spDb->isBXfilledUsingBX48(bx48)) hfb->Fill(bxStar);
	//printf("bxStar=%3d   bx48=%3d   fill=%d\n",bxStar,bx48,spDb->isBXfilledUsingBX48(bx48));
      }
    }
    if (istat  == kStEOF || istat == kStFatal) break;
    
    StEvent* mEvent = (StEvent*)chain->GetInputDS("StEvent");
    assert(mEvent);// fix your chain or open the right event file
    StTriggerIdCollection *tic=mEvent->triggerIdCollection();
    assert(tic); 

    StTriggerData* trgD= mEvent->triggerData(); assert(trgD);
    int bx48=trgD->bunchId48Bit();
    int bx7=trgD->bunchId7Bit();
    int bxStar48=spDb->BXstarUsingBX48(bx48);
    int bxStar7=spDb->BXstarUsingBX7(bx7);
    int bxPhase=spDb->offsetBX48minusBX7(bx48,bx7);
    int spin8ev=trgD->spinBit();
    int spin8db=spDb->spin8usingBX48(bx48);
    int spin4db=spDb->spin4usingBX48(bx48);
    //.... do histo
    hbx->Fill(bxStar48);
    hdbx->Fill(bxStar48,bxStar7);
    hs8ev->Fill(bxStar48,spin8ev);
    if(spDb->isBXfilledUsingBX48(bx48)){
      hs8db->Fill(bxStar48,spin8db);
      hs4db->Fill(bxStar48,spin4db);
    }
    int nV=mEvent->numberOfPrimaryVertices();
    printf("eveID=%d  nPrimVert=%d  ", mEvent->id(),nV);
    printf("bx48=%3d bx7= %3d bxPhase=%3d spin8: ev=0x%02x : 0x%02x=db bxStar48=%3d \n",bx48,bx7,bxPhase,spin8ev,spin8db,bxStar48);
    // assert(spin8ev>10);
  } // Event Loop
 hf->Write();
    
}


//========================
void initHisto() {
  hf=new TFile("outx.hist.root","recreate");
  hbx=new TH1F("bx","Rate vs. true bXing from bx48; bXing at STAR IP",120,-0.5,119.5);
  hfb=new TH1F("fb","Intended fill pattern; bXing at STAR IP",120,-0.5,119.5);

  hs8ev=new TH2F("s8ev","spin8(eve) vs. true bXing from bx48; bXing at STAR; spin8 from event",120,-0.5,119.5,100,-0.5,99.5);
  hs8db=new TH2F("s8db","spin8(db) vs. true bXing from bx48; bXing at STAR; spin8 from DB",120,-0.5,119.5,100,-0.5,99.5);

  hs4db=new TH2F("s4db","spin4(db) vs. true bXing from bx48; bXing at STAR; spin4 from DB",120,-0.5,119.5,100,-0.5,15.5);

  hdbx=new TH2F("dbx","bx7 vs. true bXing from bx48; bXing at STAR;bx7",120,-0.5,119.5,120,-0.5,119.5);
}
