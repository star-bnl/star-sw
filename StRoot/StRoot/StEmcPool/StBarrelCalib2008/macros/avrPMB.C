// averages mip peak position for BPM tubes

 float x1=1+0.05, x2=x1+60;
int nbx=10*60;
TH1F *hBPMg=new TH1F("bpmG","Average MIP position per PMB, err=RMS; X= PMB + pmt/10; MIP (adc)",nbx,x1,x2);
TH1F *hBPMu=new TH1F("bpmU","Number of used pixels  per PMB; X= PMB + pmt/10; number of tiles",nbx,x1,x2);
TH1F *hBPMq=new TH1F("bpmQ","QA error flag per PMB (Y=0 is good); X= PMB_ID + pmt_ID/10;error code",nbx,x1,x2);

FILE *fcsv=0;

final() {
  fcsv=fopen("bprsPMBmipGain.csv","w");
  fprintf(fcsv,"average MIP ADC over 16 (or less) working BPRS tiles; ver=1.6\n");
  fprintf(fcsv,"PMB,pmt, QAflag, nUsedPix,  avrMIP (adc), rmsMIP (adc),PDF page # , all mapped softIDs\n");

  for(int b=1; b<=60;b++) 
    avrPMB(b);
  fprintf(fcsv,"\nQAflag=0x0 : good MIP signal in this pmt \n");
  fprintf(fcsv,"QAflag  bit=0x1 : no histo \n");
  fprintf(fcsv,"QAflag  bit=0x2 : too few working pixels \n");
  fprintf(fcsv,"QAflag  bit=0x4 : too narrow RMS \n");
  fprintf(fcsv,"QAflag  bit=0x8 : too low mean ADC \n");
  fclose(fcsv); fcsv=0;
  
  c=new TCanvas("aa1","aa1",1300,800);
  c->Divide(1,3);    
  gStyle->SetOptStat(10); 
  c->cd(1); hBPMg->Draw();     gPad->SetGrid(); 
  c->cd(3); hBPMu->Draw();        gPad->SetGrid(); 
  c->cd(2); hBPMq->Draw();        gPad->SetGrid(); 
  hBPMg->SetAxisRange(0.,100.);   hBPMg->SetMarkerColor(kRed); hBPMg->SetMarkerStyle(5);
  hBPMu->SetAxisRange(0.,100.);  hBPMu->SetFillColor(kBlue);hBPMu->SetLineColor(kBlue);
  hBPMq->SetAxisRange(0.,100.);   hBPMq->SetFillColor(kRed);hBPMq->SetLineColor(kRed);
}


//=================================
avrPMB(int box0=2) {
  gStyle->SetOptStat(1110);
  gStyle->SetOptFit(1);
  gStyle->SetPalette(1,0);
   // get mapping of softID to this PMT
  //  buildSoftList(box,pmt);

  // get spectra for every BPRS pixel
  char *fnameO="calib-nov-21-2008/barrelMipSpectV6ok.hist.root"; 
  fd=new TFile(fnameO); assert(fd->IsOpen());
  printf("Read %s\n", fd->GetName()); 
  //fd->ls();

  float  cut_nUsed=5;
  float  cut_adcL=2.5;
  float  cut_rms=0.5;

  int box1=box0, box2=box0;
  for(int box=box1; box<=box2; box++) {
    c=new TCanvas("aa","aa",800,500); c->Divide(4,2);    
    c->cd(8); hBPMg->Draw();      
    c->cd(7); hBPMu->Draw();      
    c->cd(6); hBPMq->Draw();      
    hBPMg->SetAxisRange(box,box+1.);
    hBPMu->SetAxisRange(box,box+1.);
    hBPMq->SetAxisRange(box,box+1.);
    char tt[100];
    for( int pmt=1; pmt<=5; pmt++) {
      sprintf(tt,"gainBPM%d_%d",box,pmt);    
      printf(" get:%s:\n",tt);
      TH1F *h=fd->Get(tt); 
      if(h==0) { 
	if(fcsv) fprintf("%d,%d, 0,1, noHisto",box,pmt);
	printf("missing=%s \n",tt);  continue;}
      float xVal=box+pmt/10.; 
      c->cd(pmt);
      h->Draw(); h->SetMaximum(10);
      float nUsed=h->GetEntries();
      hBPMu->Fill(xVal,nUsed);
      float mean=h->GetMean();
      float rms=h->GetRMS();
      int flag=0;
      if(nUsed<cut_nUsed) flag+=2;
      if(rms<cut_rms)   flag+=4;
      if(mean<cut_adcL)  flag+=8; 
      if(flag)hBPMq->Fill(xVal,flag);
      if(flag==0) { int bin=hBPMg->FindBin(xVal);
	hBPMg->SetBinContent(bin,mean);
	hBPMg->SetBinError(bin,rms);
      }
      if(fcsv) {
	fprintf(fcsv,"%d,%d,%d, %d,  %.2f, %.2f, %2d,",box,pmt,flag,nUsed,mean,rms,5*(box-1)+pmt);
	printSoftList(box,pmt);  
      }
    }
  } // LOOP over boxes

}

//------------------------
void printSoftList(int box=11, int pmt=1) {
   fd1=new TFile("calib-nov-8-2008/map-softID-bprsPmt-Rory.root"); assert(fd1->IsOpen());  
   //  printf("Opened: %s\n",fd1->GetName());// fd1->ls();
  TH1I * mapBprsPmt=(TH1I*) fd1->Get("bprsPmt"); assert(mapBprsPmt);
  //mapBprsPmt->Draw();
  
  int y= box*10+pmt;
  for(int b=1;b<=4800;b++) {
    if(y!=(int)mapBprsPmt->GetBinContent(b)) continue;
    if(fcsv)fprintf(fcsv,"%d ",b);
  }
  if(fcsv)   fprintf(fcsv,"\n");
  fd1->Close();
}
