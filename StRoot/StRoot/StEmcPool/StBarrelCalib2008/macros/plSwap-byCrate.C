//  cat ps/pmt3* |ps2pdf - pmt3.pdf 

plSwap_byCrate(int icr=2, int set=2) { // BPRS crateID=0,...,3

  //  char *fname="outFid-swap12/sum.hist.root";
 
  char *fname="sum.hist.root";
  if(set==0) fname="sum-noswap.hist.root";
  if(set==1) fname="sum-swap14.hist.root";
  if(set==2) fname="/star/data05/scratch/balewski/sched-swapA/R9068088.barCal.hist.root";
  if(set==99) fname="sum.hist.root";
  // char *fname="out1e/R9067013f.barCal.hist.root";

  fd=new TFile(fname); assert(fd->IsOpen());
  printf("Work with %s\n", fd->GetName());
  gStyle->SetOptStat(1000010);
  gStyle->SetPalette(1,0);
  gStyle->SetOptFit(1);

  TH2F* hr= BPRS_c0;
  TH2F* hm=mipBprsAdc;
  float y1=-20, y2=80;
  
  TString crName="aaa";
  c=new TCanvas();
  
  switch(icr) {

  case 0: 
  case 2: 
    {
      float x1=1,x2=340,x3=1541,x4=2400;
      crName="PSD1W"; 
      if(icr==2) {
	crName="PSD1E"; 
	x1=2401; x2=2900; x3=4101; x4=4800;
      }
      c->SetTitle(crName);
      TH2F* hA[4]; hA[0]=hr; hA[2]=hm;
      for(int i=0;i<2;i++){
	hA[1+2*i]=(TH2F*)  hA[2*i]->Clone();
	hA[2*i]->SetAxisRange(x1,x2);
	hA[2*i]->SetAxisRange(x1,x2);
	hA[1+2*i]->SetAxisRange(x3,x4);
	hA[1+2*i]->SetAxisRange(x3,x4);
      }
    
      c->Divide(2,2);
      for(int i=0;i<4;i++){
	c->cd(i+1); hA[i]->Draw("colz");
	if(i<2)  gPad->SetLogz();
      }
    }
    break;

  case 1: 
  case 3: 
    {
      float  x1=341,x2=1540;
      crName="PSD19W"; 
      if(icr==3) {
	crName="PSD20E";
	x1=2901;x2=4100; 
      }  
      c->SetTitle(crName);
      c->Divide(1,2);
      c->cd(1); hr->Draw("colz"); gPad->SetLogz();gPad->SetGrid();
      hr->SetAxisRange(x1,x2);
      hr->SetAxisRange(y1,y2,"y");
      c->cd(2); hm->Draw("colz")  ;gPad->SetGrid();
      hm->SetAxisRange(x1,x2);
      hm->SetAxisRange(y1,y2,"y");
    }
  } // end of case


  return;



  int ipmt=kpmt-1;
  for(int i=0;i<4;i++) {
    float id1=1461+i*20+ipmt*4;
    float id2=id1+4;
    for(int j=0;j<4;j++) {
      int id=id1+j; 
      printf("==========%d\n",id);
      // continue;
      {
	TH1F *hr=getSlice( BPRS_c0,id, "raw");
	TH1F *hm=getSlice(mipBprsAdc,id, "mip"); 
	c=new TCanvas("a","a",500,500);
	c=new TCanvas();
	c->Divide(1,2);
	c->cd(1);
	hr->Draw();gPad->SetLogy();gPad->SetGrid();
	hr->Fit("gaus");
	c->cd(2);
	hm->Draw();  //hm->Rebin();
	gPad->SetGrid();
	char txt1[100];
	sprintf(txt1,"ps/pmt%d_id%d.ps",kpmt,id);
	c->Print(txt1);
      }
    }
   }
  printf("MANY pages has piled up!!!\n");
}

//================
void markMP1() {
  float y=-15;
  for(int i=0;i<4;i++) {
    float x1=1461+i*20-0.5;
    float x2=x1+4;
    ln=new TLine(x1,y,x2,y);
    ln->Draw();
    ln->SetLineColor(kRed); ln->SetLineWidth(2.);
  }
}

//=================
TH1F * getSlice(TH2F * h2, int id, char *ctit) {
  axX=h2->GetXaxis();
  float x1=axX->GetXmin();
  float x2=axX->GetXmax();
  int nbX=axX->GetNbins();
  printf("X-axis range  --> [%.1f, %.1f], nb=%d %s\n",x1,x2,nbX,axX->GetTitle());

  axY=h2->GetYaxis();
  float y1=axY->GetXmin();
  float y2=axY->GetXmax();
  int nbY=axY->GetNbins();
  printf("Y-axis range  --> [%.1f, %.1f], nb=%d\n",y1,y2,nbY);

  assert(id>=1 && id<=nbX);
  // do projections
  char txt1[100], txt2[1000];
  sprintf(txt1,"%s_id%d",ctit,id);
  sprintf(txt2,"%s soft id=%d;%s ",ctit, id,axY->GetTitle());
  TH1F*h=new TH1F(txt1,txt2,nbY,y1,y2); // working histo for 1-D spectrum
  
  int i;
  for(i=1;i<=nbY;i++) h->SetBinContent(i,h2->GetBinContent(id,i));
  //for(i=1;i<=nbY;i++) printf("%d %f \n",i,h2->GetBinContent(ih,i));


  float x1=-20, x2=70;
  h->SetAxisRange(x1,x2);
  h->SetEntries(h->Integral());
 
  return h;
}


//===============
void plCuts() {
  
  c=new TCanvas();
  c->Divide(2,2);
  
  c->cd(1); 
  mipZver->Draw();
  float x=50, y=5e6;
  ln=new TLine(x,0,x,y);    ln->Draw();
  ln->SetLineColor(kRed);
  ln=new TLine(-x,0,-x,y);    ln->Draw();
  ln->SetLineColor(kRed);
  mipZverAc->Draw("same"); mipZverAc->SetLineColor(kBlue);


  c->cd(2); 
  mipTrPt->Draw();gPad->SetLogy();
  x=0.3; y=5e7;
  ln=new TLine(x,0,x,y);    ln->Draw();
  ln->SetLineColor(kRed);
  mipTrPtAc->Draw("same"); mipTrPtAc->SetLineColor(kBlue);

   c->cd(3); 
   mipDeDx->Draw("colz");gPad->SetLogz();
   float  y=3.4;
   ln=new TLine(0,y,20,y);    ln->Draw();
   ln->SetLineColor(kRed);
   y=1.5;
   ln=new TLine(0,y,20,y);    ln->Draw();
   ln->SetLineColor(kRed);
   
   c->cd(4); 
   mipTrRZ21->Draw("colz");


}
