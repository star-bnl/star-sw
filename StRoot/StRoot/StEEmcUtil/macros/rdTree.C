// read TTree

rdTree(int neve=100,  TString Tname0="mc_eveID1", int flag=0, float Emax=40.){  
 gROOT->LoadMacro("bfc.C"); 
  bfc(0,"fzin sim_T gen_T","mc_eveID6.fzd");

  gSystem->Load("EEmc.so"); 
  //  gStyle->SetPalette(1,0);
  gStyle->SetOptStat(111111);

 TString Tname;

 // Tname="mc_one_pid3eta30.pt1.75";
 // Tname="/star/data22/MC/balewski/eemc_sim2002/new/"+Tname0;
 // Tname="/star/data22/MC/balewski/eemc_sim2002b/"+Tname0;
 Tname=Tname0;

 printf("read upto %d events from file=%s.root\n",neve,Tname.Data());
 TFile *f = new TFile(Tname+".root");
 assert(f->IsOpen());
 TTree *t4 = (TTree*)f->Get("t4");
  
  // create a pointer to an event object. This will be used
  // to read the branch values.
  EEevent *event = new EEevent();
  
  //  if (gROOT->IsBatch()) return;  new TBrowser();  t4->StartViewer();  return;

  // allocate memory for needed branches 
  TClonesArray *secA=new TClonesArray("EEsectorDst",1000);

  // set the branch address
  TBranch *BRsec = t4->GetBranch("Sec");

  BRsec->SetAddress(&secA);
  
  Int_t nevent = (Int_t)t4->GetEntries();
  printf("Total events in TTree=%d\n",nevent);

  const int nh=6;
  TH2F *h[nh];
  h[0]=new TH2F("pr1","EE Pre-1  #eta-Bin vs #phi-sector",60,1.,13.,12,0.5,12.5); 
  h[1]=new TH2F("pr2","EE Pre-2  #eta-Bin vs #phi-sector",60,1.,13.,12,0.5,12.5); 
  h[2]=new TH2F("tw","EE towers  #eta-Bin vs #phi-sector",60,1.,13.,12,0.5,12.5); 
  h[3]=new TH2F("Post","EE Post  #eta-Bin vs #phi-sector",60,1.,13.,12,0.5,12.5); 
  h[4]=new TH2F;
  h[5]=new TH2F;

  TH2F *hde[nh];
  hde[0]=new TH2F("deD1","EE energy loss : Pre-1  vs. Tower  (GeV)",50,.0,20.,50,0.,1.); 
  hde[1]=new TH2F("deD2","EE energy loss : Post  vs. Tower  (GeV)",50,.0,20.,50,0.,.5); 
  hde[2]=new TH2F("deD3","EE energy loss : Pre-1  vs. Post  (GeV)",50,.0,1.,50,0.,.5); 
  hde[3]=new TH2F;
  hde[4]=new TH2F;
  hde[5]=new TH2F;


  TH1F *hde1[nh];
  hde1[0]=new TH1F("dePr1","EE energy deposit : Pre-1 (GeV)",500,.0,Emax/150.);
  hde1[1]=new TH1F("dePr2","EE energy deposit : Pre-2 (GeV)",500,.0,Emax/150.);
  hde1[2]=new TH1F("deTw","EE energy deposit : Tower (GeV)",500,.0,Emax/7.);
  hde1[3]=new TH1F("dePo","EE energy deposit : Post (GeV)",200,.0,Emax/150.);
  hde1[4]=new TH1F("deU","EE energy deposit : smdU (GeV)",300,.0,Emax/70.);
  hde1[5]=new TH1F("deV","EE energy deposit : smdV (GeV)",300,.0,Emax/70.);
  TH1F *hdeSum=new TH1F("deT+S","EE energy deposit : Tower+smdU+V (GeV)",600,.0,Emax/7.);

  int i;
  for(i=0;i<nh;i++) {
    h[i]->GetXaxis()->SetTitle("#phi sectors [1-12]");
    h[i]->GetYaxis()->SetTitle("#eta bins (bin1 #rightarrow #eta=2.0)");
    h[i]->GetZaxis()->SetTitle("Energy deposit (GeV)");
  }

  // ...........  LOOP OVER EVENTS ............

  for (Int_t ie=0;ie<nevent;ie++) {    
    if(ie>=neve) break;
    int i;
    //for(i=0;i<nh;i++) h[i]->Reset(); // clear eve-by-eve histo  
    
    //read this branch only
    BRsec->GetEntry(ie);  
    
    // if(ie%20==0) printf("\n\iEve=%d eveID=%d, eveType=%d, nSec=%d with data :\n",ie,eveID,eveType,secA->GetEntries());
   if(ie%20==0) printf("\n\iEve=%d  nSec=%d with data \n",ie,secA->GetEntries());
    
    const int nz=6;
    float enerS[nz]; //4 Tower~like detectors +2 Smd
    {int i; for(i=0;i<nz;i++) enerS[i]=0;}

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
	for(ih=0;ih<hitA->GetEntries();ih++) {
	  char sub;
	  int eta;
	  float ener;
	  EEtwHitDst *hit=(EEtwHitDst*)hitA->At(ih);
	  hit->get(sub,eta,ener);
	 if(ie<1) printf("    ih=%d sec=%d sub=%c etaBin=%d ener=%f\n",ih, sec->getID(), sub, eta,ener);
	  float x=sec->getID()+0.1+0.2*(sub-'A');
	  h[iz]->Fill(x,eta,ener);
	  enerS[iz]+=ener;
	  // hit->print();	
	}
      }// end of loop over pre1/2/Tw/Post       
      
     
      for(iz=4;iz<6;iz++) { // over tower/pre/post
	hitA=hitAA[iz];
	 if(ie<1) printf("  sectorID=%d  iz=%d nHit=%d :\n",sec->getID(),iz,hitA->GetEntries());
	for(ih=0;ih<hitA->GetEntries();ih++) {
	  EEsmdHitDst *hit2=(EEsmdHitDst*)hitA->At(ih);
	  int strip;
	  float ener;
	  hit2->get(strip,ener);
	  if(ie<1) printf("    ih=%d strip=%d etaBin=%d ener=%f\n",ih, sec->getID(), strip,ener);
	  //	  h[iz]->Fill(x,eta,ener);
	  enerS[iz]+=ener;
	  // hit->print();	
	}
      }// end of loop over pre1/2/Tw/Post       
      
      
    }// end of loop over sector
    
    hde[0]->Fill(enerS[2],enerS[0]);
    hde[1]->Fill(enerS[2],enerS[3]);
    hde[2]->Fill(enerS[3],enerS[0]);

    for(i=0;i<nz;i++) hde1[i]->Fill(enerS[i]);
    
    hdeSum->Fill(enerS[2]+enerS[4]+enerS[5]);

    if(ie<1) for(i=0;i<nz;i++) printf("enerS[%d]=%f\n",i,enerS[i]);

    }// end of loop over events
    

  printf("Total events in B TTree=%d\n",nevent);

  TFile fh(Tname0+".hist.root","recreate");
  
  hde1[2]->Fit("gaus");// tottal tower energy
  hde1[2]->GetFunction("gaus")->SetLineColor(kRed);

  hdeSum->Fit("gaus");// tottal tower energy
  hdeSum->GetFunction("gaus")->SetLineColor(kRed);

  for(i=0;i<nz;i++) {
    h[i]->Write();
    hde[i]->Write();
    hde1[i]->Write();
  }    
  hdeSum->Write();
  fh.ls();

  TCanvas *can = new TCanvas("cx","main plot",600,800);
  can->Range(0,0,1,1);
    
  TPad *pad0 = new TPad("pad0", "apd0",0.0,0.90,1.,1.);
  pad0->Draw();
  pad0->cd();
  pad0->Range(0,0,1,1);
    
  TLatex *tlx=new TLatex(0.12,0.5,strstr(Tname0.Data(),"/mc_"));
  tlx->SetTextSize(0.4);
  tlx->Draw();
    
  can->cd();
  TPad *c1 = new TPad("pad1", "apd1",0.0,0.0,1.,.90);
  c1->Draw();
  
  c1->Divide(2,4);
  for(i=0;i<nz;i++) {
    c1->cd(i+1);
    hde1[i]->Draw();
    //    if(i<=3)
    gPad->SetLogy();
  }
  c1->cd(7);
  hdeSum->Draw();
  gPad->SetLogy();
  

  if(flag) {
    TString psFile=Tname0+".ps";
    can->Print(psFile.Data());
    printf("## cp %s target ",psFile.Data());
  }
  return;
  c1=new TCanvas();
  c1->Divide(2,2);
  for(i=0;i<3;i++) {
    c1->cd(i+1);
    hde[i]->Draw("box");
  }
    c1->cd(4);
    h[2]->Draw("box");
    if(flag)c1->Print("fig2.ps");
    
    
    c1=new TCanvas();
    c1->Divide(2,2);   
    printf("Content of all used event DISPLAY\n");
    for(i=0;i<nh;i++) {
      c1->cd(i+1);
      h[i]->Draw("LEGO2");
      h[i]->Print();
    }
    if(flag) c1->Print("fig3.ps");
  
}

