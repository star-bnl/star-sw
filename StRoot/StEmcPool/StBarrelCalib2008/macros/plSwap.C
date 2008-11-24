//  cat ps/pmt3* |ps2pdf - pmt3.pdf 

void doAll() {
  for(int i=1;i<=24;i++) plSwap(i);
}

plSwap(float n6=15, int set=20) { 
  int isBprs=1;
  TString tscore="BTOW tower";

  char *fname="fixMe";
  if(set==10) fname="sum-swapPnTn.hist.root";
  if(set==12) fname="sum-swapTyPy.hist.root";

  if(set==20) fname="sumD43-70.hist.root";
 

  fd=new TFile(fname); assert(fd->IsOpen());
  printf("Work with %s\n", fd->GetName());
  gStyle->SetOptStat(0);
  gStyle->SetPalette(1,0);
  gStyle->SetOptFit(1);


  TH2F* hr= BPRS_c0;
  // TH2F* hm=mipBprsAdc;
  TH2F* hm=mipBprsTr;

  if(!isBprs) {  // BTOW
    hr= BTOW_c0;
    TH2F* hm=mipBtowAdc;
  }

  float y1=-20, y2=80;

  TH1I *idealMap=new TH1I("Map11", "ideal map 1:1",4800,0.5,4800.5);
  for(int i=1;i<=4800;i++) idealMap->SetBinContent(i,i);
  idealMap->SetLineStyle(3); idealMap->SetLineColor(kBlack);

    
   float x1=n6*200-200;
   float x2=n6*200;
   char txt[100];
   sprintf(txt,"%s softID=[%d,%d], with Jan's swaps",tscore.Data(),x1,x2);
   x1-=10;
   x2+=10;
   
   cM=new TCanvas(tscore+" MAP",tscore+" MAP",480,480); 
   hMap=(TH2F*)swapScan;
   hMap->SetAxisRange(x1,x2,"x");
   hMap->SetAxisRange(x1,x2,"y");
   hMap ->Draw("colz");
   hMap ->SetMinimum(20);
   float yMax= hMap->GetMaximum();
   if(yMax>20) hMap ->SetMaximum(0.8*yMax);
   idealMap->Draw("same");
   gPad->SetGrid();
   
   //  return;
   c=new TCanvas(txt,txt,1300,400); 
   
   c->Divide(1,2);
   c->cd(1); hr->Draw("colz"); gPad->SetLogz();
   hr->SetAxisRange(x1,x2);
   hr->SetAxisRange(y1,y2,"y");
   if(isBprs) {
     drawPMBoxes(x1,-22); 
     TString tt="Raw BPRS spectra, pedestal subtracted, Rory's & Jan's swaps used";
     if(!isBprs)tt="Raw BTOW spectra, pedestal subtracted, Jan's swaps used";
     tx=new TText(x1+80,y2+7,tt); tx->Draw(); tx->SetTextColor(55); 
   } else {
     drawTowers(x1); 
   }
   //  return; // tmp
   c->cd(2); hm->Draw("colz")  ;
   if(!isBprs)  hm->Rebin2D(1,2);
   hm->SetAxisRange(x1,x2);
   hm->SetAxisRange(-10,50,"y");
   hm->SetMaximum(50);
   if(isBprs) {
     drawPMBoxes(x1,-10.5 ); 
     TString tt="BPRS spectra, gated with TPC MIP tracks, Rory's & Jan's swaps used";
     if(!isBprs)tt="BTOW spectra,  gated with TPC MIP tracks, Jan's swaps used";
     tx=new TText(x1+82,53,tt); tx->Draw(); tx->SetTextColor(55); 
   } else {
     drawTowers(x1); 
   }

   sprintf(txt,"ps/btowAdc%04d.ps",x1);
   c->Print(txt);
   return;
   
}

//================
void markPM1(float id1, float y11) {
  float y=y11-10, yH=100;
  for(int i=0;i<4;i++) {
    float x1=id1+i*20;
    float x2=x1+4;
    ln=new TLine(x1,y+10,x2,y+10);    ln->Draw();
    ln->SetLineColor(kRed); ln->SetLineWidth(2.);
    ln=new TLine(x1,y,x1,yH);    ln->Draw();
    ln->SetLineColor(kRed); 
    for(int j=0;j<5;j++) {
      float d=j*4;
      int iCol=kRed+j;

      ln=new TLine(x2+d,y,x2+d,yH);    ln->Draw();  
      ln->SetLineColor(iCol);  ln->SetLineStyle(3); 
      {
	char c='F'+i;
	TString tt=c; tt+="-";tt+=j+1;
	tx=new TText(x1+.5+d,y+4,tt); tx->Draw(); tx->SetTextColor(iCol);
	tx->SetTextSize(0.04);
      }
    }
  }
}

//================
drawTowers(int x1) {
  float y1=-32, y2=90;
  float x0=(x1-1)/20;
  x0=0.5+x0*20;
  for(int k=0;k<10;k++) {
    float x=x0+k*20;
    ln=new TLine(x,y1,x,y2);    ln->Draw();   ln->SetLineColor(kMagenta);
    x=x+10;
    ln=new TLine(x,y1,x,y2);    ln->Draw();   ln->SetLineColor(kMagenta); ln->SetLineStyle(2);
  }
}
//================
drawPMBoxes(int x1,float y11) {
    float y1=-32, y2=90;
    if(x1<2400) {
      int zoff=339;
      for(int k=0;k<30;k++) {
	int zid=zoff+k*80;
	float id=zid%2400 +1+0.5;
	int zpmt=24;
	int pmtid=1+(zpmt+30-k)%30;
	//	printf("k=%d id=%.1f pmt=%d\n",k,id,pmtid);
	ln=new TLine(id,y1,id,y2);
	ln->Draw();   ln->SetLineColor(kMagenta);// ln->SetLineWidth(2.);
	TString tt="PMT-"; tt+=pmtid;
	tx=new TText(id+30,y2-8,tt); tx->Draw(); tx->SetTextColor(kMagenta);
	markPM1(id,y11);
      }
    } else  {
      int zoff=2420;
      for(int k=0;k<30;k++) {
	int zid=zoff+k*80;
	float id=zid%2400 +2400+0.5;
	int zpmt=32;
	int pmtid=31+(zpmt+k)%30;
	//	printf("k=%d id=%.1f pmt=%d\n",k,id,pmtid);
  	ln=new TLine(id,y1,id,y2);
	ln->Draw();   ln->SetLineColor(kMagenta);// ln->SetLineWidth(2.);
	TString tt="PMT-"; tt+=pmtid;
	tx=new TText(id+30,y2-8,tt); tx->Draw(); tx->SetTextColor(kMagenta);
	markPM1(id,y11);
      }
    }
  }


