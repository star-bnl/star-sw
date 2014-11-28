// read TTree, find isolated towers, produce ADC

rdEEeventMipSolo(int neve=1000,  TString Tname0="/star/u/eemcdb/dataFeb11/run00006.eeTree", int flag=0, float Emax=40.){  
  Tname0="../4piotr/dAu1K.eeTree";
  //   Tname0="feb24/run00003.eeTree";
  gSystem->Load("StRoot/StEEmcUtil/EEevent/libEEevent.so");
  
  gStyle->SetPalette(1,0);
  gStyle->SetOptStat(111111);

  TString fname="out.hist.root";
  TFile *fd=new TFile(fname,"recreate");
  
  TH1F *h1[8];
  TH2F *h2[8];
  initHisto(h2,h1,fd);
  TH2F *heve=new TH2F("tw2D","eta vs. phi , EEMC towers",60,-0.5,59.5,12,0.5,12.5);
  
  const int mxEta=12;
  const int mxPhi=60;
  int valA[mxEta*mxPhi];

  TString Tname;
  Tname=Tname0;
  
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
  for (Int_t ie=0;ie<nevent;ie++) {    
    if(ie>=neve) break;
    if(ie<5 || ie%50==0)
     printf("\niEve=%d  ---------- \n",ie);
    int i;
    
    //read this branch only
    br->GetEntry(ie);  
    if(ie<5) event->print();
 
    int nSec=event->Sec->GetEntries();
   if(ie<5) printf("%d sectors with data\n",nSec); 

   heve->Reset();
   memset(valA,0,sizeof(valA));
   int is;
   for(is=0;is<nSec;is++) {
     EEsectorDst *sec=(EEsectorDst*)event->Sec->At(is);
     //      sec->print();
     TClonesArray *hitA=sec->getTwHits();
     assert(hitA);
     int ih;
     for(ih=0;ih<hitA->GetEntries();ih++) {
       char sub;
       int eta;
       float ener;
       EEtwHitDst *hit=(EEtwHitDst*)hitA->At(ih);
       hit->get(sub,eta,ener);
       h2[0]->Fill(ener,eta);
       h1[0]->Fill(ener);
       int iphi=5*(sec->getID()-1) +(sub-'A');
       int jj=mxPhi*(eta-1)+iphi; 
       heve->Fill(iphi,eta,ener);
       if(ener<256) valA[jj]=ener+0.5;
       if(ie<5) printf("    ih=%d sec=%d sub=%c etaBin=%d ener=%f\n",ih, sec->getID(), sub, eta,ener);
       //	hit->print();	
     }
     
   }// end of loop over sectors
    
   //search for Solo towers
   int k0;
   for(k0=0;k0<mxEta*mxPhi;k0++) {
     if(valA[k0]==0) continue;
     int ieta=k0/mxPhi;
     int iphi=k0%mxPhi;
     // look for neighbours
     int sumS=sumSurr(ieta,iphi,valA,mxPhi,mxEta);
     //printf("iphi=%d eta=%d adc=%d sumS=%d\n",iphi,ieta+1,valA[k0],sumS);
     if(sumS<=0) {
       h2[1]->Fill(valA[k0],ieta+1);
       h1[1]->Fill(valA[k0]);
     }     else {
       h2[2]->Fill(valA[k0],ieta+1);
       h1[2]->Fill(valA[k0]);
     }
     sumS=sumSurr2(ieta,iphi,valA,mxPhi,mxEta);
     if(sumS<=0) {
       h2[3]->Fill(valA[k0],ieta+1);
       h1[3]->Fill(valA[k0]);
     }     else {
       h2[4]->Fill(valA[k0],ieta+1);
       h1[4]->Fill(valA[k0]);
     }
   }
   //   printf("press enter ..."); char c; scanf("%c",&c); 
  } // loop over events
  printf("\n\nTotal events in B TTree=%d\n",nevent);
  heve->Draw("colz");
  heve->SetMaximum(20);
  heve->SetMinimum(0);
  
  gPad->SetGrid(); 
  //  return;
  TCanvas * c=new TCanvas("aa","bb",500,600);
  c->Divide(1,5);
  
  for(i=0;i<5;i++){
    c->cd(i+1);
    h2[i]->Draw("colz");
  }

  TCanvas * c=new TCanvas("aa1","bb1",500,600);
  c->Divide(1,5);
  
  for(i=0;i<5;i++){
    c->cd(i+1);
    h1[i]->Draw();
  }

  fd->ls();
  fd->Write();
  c->Print("all.ps");

  c=new TCanvas("aa2","bb2",600,400);
  c->Divide(2,2);
  c->cd(1);
  TH2F *hx=(TH2F *hx)h2[0]->Clone();
  hx->Divide(h2[1],h2[2]); hx->Draw("colz");
  c->cd(2);
  hx=(TH2F *hx)h2[0]->Clone();
  hx->Divide(h2[3],h2[2]); hx->Draw("colz");

  c->cd(3);
  TH1F *hy=(TH1F*) h1[0]->Clone();
  hy->Divide(h1[1],h1[2]); hy->Draw();
  c->cd(4);
  hy=(TH1F*) h1[0]->Clone();
  hy->Divide(h1[3],h1[2]); hy->Draw();


}

//====================================

int sumSurr(int ieta, int iphi,int *valA, int nPhi, int nEta){
  int sum=0;
  assert(iphi>0);
  assert(iphi<nPhi-1);

  // above
  int jeta=ieta+1;
  if(jeta<nEta) {
    sum+=valA[ nPhi*jeta + iphi-1 ];
    sum+=valA[ nPhi*jeta + iphi ];
    sum+=valA[ nPhi*jeta + iphi+1 ];
  }

  // below
  jeta=ieta-1;
  if(jeta>=0) {
    sum+=valA[ nPhi*jeta + iphi-1 ];
    sum+=valA[ nPhi*jeta + iphi ];
    sum+=valA[ nPhi*jeta + iphi+1 ];
  }

  // before/after
  sum+=valA[ nPhi*ieta + iphi-1 ];
  sum+=valA[ nPhi*ieta + iphi+1 ];
  
  return sum;
}

//====================================

int sumSurr2(int ieta, int iphi,int *valA, int nPhi, int nEta){
  int sum=0;
  assert(iphi>1);
  assert(iphi<nPhi-2);


  int i,j;
  sum=-valA[ nPhi*ieta + iphi];

  for(i=ieta-2; i<=ieta+2;i++){
    if(i>=nEta || i<0) continue;
    for(j=iphi-2;j<=iphi+2;j++){
      // printf("%d %d\n",i,j);
      sum+=valA[ nPhi*i + j ];
    }
  }
  
  return sum;
}


//====================================
void initHisto(TH2F **h2, TH1F **h1, TFile *fd) {

    int i;
    for(i=0;i<5;i++) { //
      char tt1[100], tt2[100];
      sprintf(tt1,"etaC%d",i);
      sprintf(tt2,"eta-bin vs. raw ADC , condition=%d",i);
      TH2F *h = new TH2F(tt1,tt2, 25,-0.,50.,12,.5,12.5);
      h->SetDirectory(fd);
      h->Sumw2();
      h2[i]=h;

      sprintf(tt1,"C%d",i);
      sprintf(tt2," raw ADC , condition=%d",i);
      TH1F *hx = new TH1F(tt1,tt2, 50,-0.5,49.5);
      hx->SetDirectory(fd);
      hx->Sumw2();
      h1[i]=hx;
   }
}


