// read TTree

anaEEexample(int neve=300,  TString Tname0="/star/u/eemcdb/dataFeb11/run00006.eeTree", int flag=0, float Emax=40.){  
  TString Tname0="../sim2003/mc_eve2";
  gSystem->Load("StRoot/StEEmcUtil/EEevent/libEEevent.so");
  gSystem->Load("StRoot/StEEmcUtil/anaEE/libanaEE.so");
  
  //  gStyle->SetPalette(1,0);
  gStyle->SetOptStat(111111);
  
  TString Tname;
  Tname=Tname0;
  
  TFile *of=new TFile(Tname+"hist.root","recreate"); // output histogram file
  anaEEexample ana(of);
  
  
  printf("read upto %d events from file=%s.root\n",neve,Tname.Data());
  TFile *f = new TFile(Tname+".root");
  assert(f->IsOpen());
  TTree *t4 = (TTree*)f->Get("EEtree");
  assert(t4);
  // create a pointer to an event object. This will be used
  // to read the branch values.
  EEeventDst *event = new EEeventDst();
  
  //if (gROOT->IsBatch()) return;  new TBrowser();  t4->StartViewer();  return;
  TBranch *br = t4->GetBranch("EEdst");
  br->SetAddress(&event);
  Int_t nevent = (Int_t)t4->GetEntries();

  // ...............  LOOP over events
  for (Int_t ie=0;ie<nevent;ie++) {    
    if(ie>=neve) break;
    printf("\niEve=%d  ---------- \n",ie);
    
    br->GetEntry(ie);  
    //event->print();
    ana.process(event);
  }
  
  printf("\n\nTotal events in B TTree=%d\n",nevent);
    
  of->ls();
  of->Write();
  TH1F* h0=(TH1F* )of->Get("de");
  h0->Draw();
  
}


  
#if 0
  
  // allocate memory for needed branches 
  TClonesArray *secA=new TClonesArray("EEsectorDst",1000);
  TBranch *BRsec = t4->GetBranch("Sec");   // set the branch address
  BRsec->SetAddress(&secA);

  int eveID=0;
  TBranch *BRid = t4->GetBranch("ID");
  BRid->SetAddress(&eveID);

  Int_t nevent = (Int_t)t4->GetEntries();
  printf("Total events in TTree=%d\n",nevent);
  
  
  // ...........  LOOP OVER EVENTS ............
  
  for (Int_t ie=0;ie<nevent;ie++) {    
    if(ie>=neve) break;
    int i;
    
    //read this branch only
    BRid->GetEntry(ie);  
    BRsec->GetEntry(ie);  
    
    if(ie%1==0) printf("\n\iEve=%d  nSec=%d with data \n",ie,secA->GetEntries());
    
    //    if(ie%1==0) printf("\n\iEve=%d eveID=%d, eveType=%d, nSec=%d with data :\n",ie,eveID,eveType,secA->GetEntries());
    if(ie%1==0) printf("\n\iEve=%d eveID=%d, nSec=%d with data :\n",ie,eveID,secA->GetEntries());
    
    
    int is;
    for(is=0;is<secA->GetEntries();is++) {
      EEsectorDst *sec=(EEsectorDst*)secA->At(is);
      if(ie<1) sec->print();
      
      TClonesArray *hitA;
      int ih;
      
      TClonesArray *hitAA[]={sec->getPre1Hits(),sec->getPre2Hits(),sec->getTwHits(),sec->getPostHits(),sec->getSmdUHits(),sec->getSmdVHits()};
      int iz;
      for(iz=0;iz<4;iz++) { // over tower/pre/post
	hitA=hitAA[iz];
	if(ie<1)    printf("  sectorID=%d  iz=%d nHit=%d :\n",sec->getID(),iz,hitA->GetEntries());	
      }// end of loop over pre1/2/Tw/Post       
      
      
      for(iz=4;iz<6;iz++) { // over SMD U/V
	hitA=hitAA[iz];
	if(ie<1) printf("  sectorID=%d  iz=%d nHit=%d :\n",sec->getID(),iz,hitA->GetEntries());
	for(ih=0;ih<hitA->GetEntries();ih++) {
	  EEsmdHitDst *hit2=(EEsmdHitDst*)hitA->At(ih);
	  int strip;
	  float ener;
	  hit2->get(strip,ener);
	  if(ie<1) printf("    ih=%d strip=%d etaBin=%d ener=%f\n",ih, sec->getID(), strip,ener);
	  hit->print();	
	}
      }// end of loop over pre1/2/Tw/Post       
      
      
    }// end of loop over sector
    
  } 
#endif





