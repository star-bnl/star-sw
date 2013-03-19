static const int NTB=15;

static const int kFgtNumDiscs=6;
static const int kFgtNumQuads=4;  
static const int kFgtNumElecIds=30720;

static const int NHist=10;     
static const int N1dHist=12;   
static const int N2dHist=5;   
static const int NTrkHist=6;

static TH1F *hist0[NHist];                               //! Histos for whole fgt
static TH1F *hist1[kFgtNumDiscs][kFgtNumQuads][N1dHist]; //! 1d histos for each disc/quad
static TH2F *hist2[kFgtNumDiscs][N2dHist];               //! 2d histos for each disc
static TH1F *histTrk[kFgtNumQuads][NTrkHist];            //! Histos for tracks

static const char* cquad[kFgtNumQuads]={"A","B","C","D"}; 
static const char* cHist[NHist]={"MaxTimeBin","ADC","DataSize","ZSdata","10sigma","Nevt","LandauChi2","LandauSig","LandauMpv","Mpv-3Sig"};

static const char* c1dHist[N1dHist]={"NHitStrip", "PhiHit",   "RHit", "NCluster","ClusterSize","ClusterCharge","MaxADC","ChargeAsy","CluChargeT","MaxADCT","ChargeAsyTrk","LandauN"};
//Log 0=linear 1=log
static const int   l1dHist[N1dHist]={          1,        1,        1,          1,            1,              0,       0,          0,           0,        0,             0,        0};
//Mode 0=4pad 6discs on a plot, 1=24pads for each quad
static const int   m1dHist[N1dHist]={          1,        1,        1,          1,            1,              1,       1,          1,            1,        1,            1,        1};
//Fit 0=no fit show mean, 1=gaussian fit show rms, 2=landau fit, show peak
static const int   f1dHist[N1dHist]={          0,       -1,       -1,          0,            0,              2,       2,          1,            2,        2,            1,        2};

//2d hists
static const char* c2dHist[N1dHist]={"XY","ADCvsTB","APVTB","XYT","APVTB2"};

//Trk hists
static const char* cTrkHist[NTrkHist]={"NTrk","NHitTrk","DCA","ZVTX","Chi2","HitDisc"};
static const int   lTrkHist[NTrkHist]={     1,        1,    0,     0,    0,         0};

static const int color[kFgtNumDiscs]={46,2,8,4,6,9};
static const int colorQuad[kFgtNumDiscs]={2,8,4,6};

TCanvas *c1;
static TFile* file;
static int runnum, yearday, png, pdf;

float mPeakC[kFgtNumDiscs][kFgtNumQuads];
float mPeakA[kFgtNumDiscs][kFgtNumQuads];
float mFracA[kFgtNumDiscs][kFgtNumQuads];
float mPeakL[kFgtNumDiscs][kFgtNumQuads];

void colortable(){
  static const UInt_t Number = 3;
  Double_t Red[Number]    = { 0.00, 0.00, 1.00};
  Double_t Green[Number]  = { 0.00, 1.00, 0.00};
  Double_t Blue[Number]   = { 1.00, 0.00, 0.00};
  Double_t Length[Number] = { 0.00, 0.50, 1.00 };
  Int_t nb=200;
  TColor::CreateGradientColorTable(Number,Length,Red,Green,Blue,nb);
}

void save(char* name){
  char fname[100];
  if(png){
    if(yearday==0){
      sprintf(fname,"plot/%d_%s.png",runnum,name);    
    }else{
      sprintf(fname,"%d/%d_%s.png",yearday,runnum,name);    
    }
    c1->SaveAs(fname);
  }  
}

void plot1d(int hid) {
  gStyle->SetOptStat(0);
  char c[50];
  c1->Clear();  
  if(m1dHist[hid]==0){ 
    c1->Divide(2,2); 
    for(int quad=0; quad<kFgtNumQuads; quad++){
      TVirtualPad* pad1 = c1->cd(quad+1); 
      pad1->SetLogy(l1dHist[hid]);
      double xmin, xmax, ymin=0.0, ymax=0.0;
      if(l1dHist[hid]==1) ymin=0.1;
      for(int disc=0; disc<kFgtNumDiscs; disc++){
	sprintf(c,"%1d%1s-%s",disc+1,cquad[quad],c1dHist[hid]);
	//printf("Getting %s\n",c);
	TH1F *h = hist1[disc][quad][hid] = (TH1F*)file->Get(c);
	xmin=h->GetXaxis()->GetXmin();
	xmax=h->GetXaxis()->GetXmax();
	double m=h->GetMaximum();
	if(ymax<m) ymax=m;
	printf("disc=%d max=%6.1f ymax=%6.1f xmin=%6.1f xmax=%6.1f\n",disc+1,m,ymax,xmin,xmax);
      }
      sprintf(c,"Quad%1s-%s",cquad[quad],c1dHist[hid]);
      TH2F *frame = new TH2F(c,c,1,xmin,xmax,1,ymin,ymax*1.2); frame->SetStats(0); frame->Draw();
      for(int disc=0; disc<kFgtNumDiscs; disc++){
	TH1F *h=hist1[disc][quad][hid];
	h->SetLineColor(color[disc]); h->SetLineWidth(3); h->Draw("SAME");  
	if(f1dHist[hid]==0){
	  float mean=h->GetMean();
	  sprintf(c,"%1d%s mean=%6.2f",disc+1,cquad[quad],mean);
	}else if(f1dHist[hid]==1){	  
	  int res = h->Fit("gaus","0Q");
	  TF1 *f = h->GetFunction("gaus");
	  float sig = f->GetParameter(2);	
	  if(res==0 && sig>0.0001 && h->GetEntries()>5){
	    f->SetLineColor(color[disc]); f->SetLineWidth(2); f->Draw("SAME");
	    sprintf(c,"%1d%s sig=%6.3f",disc+1,cquad[quad],sig);
	  }else{
	    sprintf(c,"%1d%s",disc+1,cquad[quad]);
	  }
	}else if(f1dHist[hid]==2){	  
	  int res = h->Fit("landau","0Q");
	  TF1 *f = h->GetFunction("landau");
	  float peak = f->GetParameter(1);	
	  if(res==0 && peak>0 && h->GetEntries()>5){
	    f->SetLineColor(color[disc]); f->SetLineWidth(2); f->Draw("SAME");
	    sprintf(c,"%1d%s mpv=%6.0f",disc+1,cquad[quad],peak);
	  }else{
	    sprintf(c,"%1d%s",disc+1,cquad[quad]);
	  }
	}
	TText *t1;
	float x1= 0.2, x2= 0.55;
	float y1=0.8 - 0.07*disc;
	float y2=0.8 - 0.07*(disc-3);
        if(disc<3) { t1 = new TText(x1,y1,c); }
        else       { t1 = new TText(x2,y2,c); }
	t1->SetNDC();
	t1->SetTextSize(0.04); 
	t1->SetTextColor(color[disc]); 
	t1->Draw();
      }
    }
  }else{
    c1->Divide(4,6);
    gStyle->SetOptStat(0);
    gStyle->SetOptTitle(0);
    gStyle->SetOptFit(0);
    for(int disc=0; disc<kFgtNumDiscs; disc++){
      for(int quad=0; quad<kFgtNumQuads; quad++){
	TPad* pad = c1->cd(disc*4+quad+1);
	pad->SetRightMargin(0.01); pad->SetLeftMargin(0.1);
	pad->SetTopMargin(0.01);   pad->SetBottomMargin(0.1);
	pad->SetLogy(l1dHist[hid]);
	sprintf(c,"%1d%1s-%s",disc+1,cquad[quad],c1dHist[hid]);
	TH1F *h = hist1[disc][quad][hid] = (TH1F*)file->Get(c);
	h->SetFillColor(color[disc]);
	h->GetXaxis()->SetLabelSize(0.1);
	h->GetYaxis()->SetLabelSize(0.1);
	h->GetXaxis()->SetNdivisions(205);
	h->Draw();

	if(f1dHist[hid]==0){
	  float mean=h->GetMean();
	  sprintf(c,"%1d%s mean=%6.2f",disc+1,cquad[quad],mean);
	}else if(f1dHist[hid]==1){	  
	  int res = h->Fit("gaus","0Q");
	  TF1 *f = h->GetFunction("gaus");
	  float sig = f->GetParameter(2);	
	  if(res==0 && sig>0.0001 && h->GetEntries()>5){
	    f->SetLineColor(color[disc]); f->SetLineWidth(2); f->Draw("SAME");
	    sprintf(c,"%1d%s sig=%6.3f",disc+1,cquad[quad],sig);
	  }else{
	    sprintf(c,"%1d%s",disc+1,cquad[quad]);
	  }
	}else if(f1dHist[hid]==2){	  
	  int res = h->Fit("landau","0Q");
	  TF1 *f = h->GetFunction("landau");
	  float peak = f->GetParameter(1);	
	  if(res==0 && peak>0 && h->GetEntries()>5){
	    f->SetLineColor(1); f->SetLineWidth(2); f->Draw("SAME");
	    sprintf(c,"%1d%s mpv=%6.0f",disc+1,cquad[quad],peak);
	  }else{
	    sprintf(c,"%1d%s",disc+1,cquad[quad]);
	  }
	  if(hid==8) {mPeakC[disc][quad]=peak;}
	  if(hid==9) {
	    mPeakA[disc][quad]=peak;	    
	    int n=h->GetEntries();
	    if(n>0){
	      float cut=2800;
	      int bin=h->FindBin(cut);
	      int nbin=h->GetNbinsX();
	      float sat=h->Integral(bin,nbin+1);
	      //printf("bin=%d nbin=%d sat=%f\n",bin,nbin,sat);
	      mFracA[disc][quad]=sat/float(n);
	    }
	  }
	  if(hid==11) {mPeakL[disc][quad]=peak;} // printf("LandauPeak=%f\n",peak);}
	}
	TText *t1 = new TText(0.3,0.85,c);
	t1->SetNDC();
	t1->SetTextSize(0.15); 
	t1->SetTextColor(color[disc]); 
	t1->Draw();
      }
    }
  }
  c1->Update();
  save(c1dHist[hid]);
}

void plot2d(int hid) {
  gStyle->SetOptStat(0);
  char c[50];
  c1->Clear();
  if(hid!=2 && hid!=4){
    c1->Divide(2,3);
    for(int disc=0; disc<kFgtNumDiscs; disc++){
      TPad *pad = c1->cd(disc+1);
      pad->SetLogz(1);
      pad->SetTopMargin(0.01);   pad->SetBottomMargin(0.02);
      sprintf(c,"Disc%1d%s",disc+1,c2dHist[hid]);
      //printf("Getting %s\n",c);
      TH2F *h = hist2[disc][hid] = (TH2F*)file->Get(c);
      h->Draw("COLZ");  
    }
  }else if(hid==2){  // special case for timing per APVboard
    gStyle->SetOptTitle(0);
    gStyle->SetOptFit(0);
    c1->Divide(2,1);
    char txt[100];
    //int disc=3; char name[100]="MaxAdc";
    int disc=4; char name[100]="LandauMPV";
    //int disc=5; char name[100]="LandauMPV-3Sing";
    TVirtualPad *pad2 = c1->cd(1);
    pad2->Divide(1,2); 
    TVirtualPad *pad3=pad2->cd(1);
    pad3->SetLogz(1);  pad2->SetLogz(0); 
    sprintf(c,"Disc%1d%s",disc,c2dHist[hid]);
    TH2F *h = hist2[disc][hid] = (TH2F*)file->Get(c);
    h->Draw("COLZ");
    TText *tt1= new TText(0.05,0.1,"(RDO-1)*12+ARM*2+GRP"); tt1->SetTextAngle(90); tt1->SetNDC(); tt1->Draw();    
    sprintf(txt,"Tbin for %s",name);
    TText *tt2= new TText(0.3,0,txt); tt2->SetNDC(); tt2->Draw();
    
    TVirtualPad* pad4 = c1->cd(2);
    pad4->SetTopMargin(0.01); pad4->SetBottomMargin(0.1);
    int maxid=0;
    float off,max=0;
    TH1D *h1[24];
    float mean[24];
    for(int i=0; i<24; i++){
      char ccc[10]; sprintf(ccc,"_%d_%d",disc,i);
      h1[i] = h->ProjectionX(ccc,i+1,i+1); 
      if(h1[i]->GetMaximum() > max && i!=0) {max=h1[i]->GetMaximum(); maxid=i; }
    }
    off=max/4.0;
    printf("max=%f off=%f\n",max,off);
    for(int i=0; i<24; i++){
      h1[i]->GetXaxis()->SetRangeUser(2,11);
      int res = h1[i]->Fit("gaus","0Q");
      TF1* f=h1[i]->GetFunction("gaus");
      if(h1[i]->GetMaximum()>max/3 && res==0){
	mean[i] = f->GetParameter(1);
	//mean[i]=h1[i]->GetMean();
      }else{mean[i]=0;};
      //printf("%d mean=%f\n",i,mean[i]);
    }
    //h1[maxid]->SetLineColor(maxid+1); h1[maxid]->SetLineWidth(2); h1[maxid]->Draw("PL");
    for(int rdo=1; rdo<=2; rdo++){
      for(int arm=0; arm<6; arm++){
	for(int grp=0; grp<2; grp++){
	  i=(rdo-1)*12+arm*2+grp;
	  int nb=h1[i]->GetNbinsX();
	  for(int t=0; t<nb; t++){ h1[i]->AddBinContent(t+1,off*i); }
	  h1[i]->SetLineColor(i%6+1); h1[i]->SetLineWidth(3);
	  if(i==0) {
	    h1[i]->SetMinimum(0);
	    h1[i]->SetMaximum(max*6.5);
	    h1[i]->Draw("PL");
	  } else {h1[i]->Draw("PL same");}	
	  char name[100]; 
	  sprintf(name,"Rdo%1dArm%1dGrp%1d",rdo,arm,grp);
	  TText *tx = new TText(8.5,(max/4.0)*(i+0.2),name); tx->SetTextColor(i%6+1); tx->SetTextSize(0.03);
	  tx->Draw();
	}
      }
    }
    //    TText *tt3= new TText(0.95,0.1,"offsets added by (RDO-1)*12+ARM*2+GRP"); tt3->SetTextAngle(90); tt3->SetNDC(); tt3->Draw();
    TText *tt4= new TText(0.4,0,txt); tt4->SetNDC(); tt4->Draw();
    
    //correlation
    float t2[24]={-8.47, -5.16, -0.21, -2.23,  1.11, -4.09, 
		  -3.13, -9.08, -5.88, -7.01, -6.22, -9.79,  
		  0.75, -8.91,  0.16,  1.12, -0.99, -4.56,  
		  7.57, -3.68,  7.12, -6.54, -4.08, -8.21};
    TGraph *g= new TGraph(1);
    int j=0;
    for(int i=0; i<24; i++){
      if(mean[i]>0) {g->SetPoint(j,(mean[i]-6.0)*27,t2[i]); j++;}
    }
    TVirtualPad* pad5=pad2->cd(2);
    g->SetMarkerStyle(20+i/6); g->SetMarkerSize(1);
    g->Draw("ap");
    for(int i=0; i<24; i++){
      TGraph *g2= new TGraph(1);
      if(mean[i]>0) {g2->SetPoint(j,(mean[i]-6.0)*27,t2[i]); j++;}
      g2->SetMarkerStyle(20+i/6); g2->SetMarkerSize(2); g2->SetMarkerColor(i%6+1);
      g2->Draw("p");
    }
    
    TText *tt5= new TText(0.05,0.1,"(VPHASE_ADC-1.2V)/0.95V*27nsec/2"); tt5->SetTextAngle(90); tt5->SetNDC(); tt5->Draw();
    TText *tt6= new TText(0.5,0,"(Tbin-6)*27nsec"); tt6->SetNDC(); tt6->Draw(); 
  }else{  // special case for timing per APVboard
    gStyle->SetOptTitle(0);
    gStyle->SetOptFit(0);
    c1->Divide(4,6);
    char txt[100];
    //int disc=3; char name[100]="MaxAdc";
    int disc=4; char name[100]="LandauMPV";
    //int disc=5; char name[100]="LandauMPV-3Sing";
    sprintf(c,"Disc%1d%s",disc,c2dHist[2]);
    TH2F *h = hist2[disc][2] = (TH2F*)file->Get(c);
    TH1D *h1[24];
    float mean[24];
    for(int rdo=1; rdo<=2; rdo++){
      for(int arm=0; arm<6; arm++){
	for(int grp=0; grp<2; grp++){   
	  int i=(rdo-1)*12+arm*2+grp;
	  TVirtualPad *pad2 = c1->cd(i+1);
	  pad2->SetTopMargin(0.01); pad2->SetBottomMargin(0.1);
	  char ccc[10]; sprintf(ccc,"_%d_%d",disc,i);
	  h1[i]=h->ProjectionX(ccc,i+1,i+1);       
	  h1[i]->GetXaxis()->SetRangeUser(2,12); h1[i]->SetFillColor(4);
	  h1[i]->GetXaxis()->SetLabelSize(0.1); h1[i]->GetYaxis()->SetLabelSize(0.1);
	  h1[i]->Draw(); 
	  int res = h1[i]->Fit("gaus","Q");
	  TF1* f=h1[i]->GetFunction("gaus"); f->SetLineColor(2); f->SetLineWidth(2);
	  if(res==0){
	    mean[i] = f->GetParameter(1);
	  }else{mean[i]=0;};
	  char name[100]; 
	  sprintf(name,"Rdo%1dArm%1d-%1d",rdo,arm,grp);
	  TText *tx = new TText(0.5,0.85,name); tx->SetTextSize(0.1); tx->SetNDC();
	  tx->Draw();
	  if(mean[i]>0){
	    sprintf(name,"peak=%4.1f",mean[i]);
	    TText *tx2 = new TText(0.55,0.75,name); tx2->SetTextSize(0.12); tx2->SetNDC();
	    tx2->Draw();
	  }
	}
      }
    }
  }
  c1->Update();
  save(c2dHist[hid]);
}

void plotTrk() {
  gStyle->SetOptStat(0);
  gStyle->SetOptTitle(1);
  char c[50];
  c1->Clear();  
  c1->Divide(2,3); 
  for(int hid=0; hid<6; hid++){
    TVirtualPad* pad = c1->cd(hid+1); 
    pad->SetLogy(lTrkHist[hid]);
    double xmin, xmax, ymin=0.0, ymax=0.0;
    if(lTrkHist[hid]==1) ymin=0.1;
    for(int quad=0; quad<kFgtNumQuads; quad++){
      sprintf(c,"Quad%1s-%s",cquad[quad],cTrkHist[hid]);
      //printf("Getting %s\n",c);
      TH1F *h = histTrk[quad][hid] = (TH1F*)file->Get(c);
      xmin=h->GetXaxis()->GetXmin();
      xmax=h->GetXaxis()->GetXmax();
      double m=h->GetMaximum();
      if(ymax<m) ymax=m;
      //printf("quad=%d max=%6.1f ymax=%6.1f xmin=%6.1f xmax=%6.1f\n",quad,m,ymax,xmin,xmax);
    }
    ymax*=1.2; if(lTrkHist[hid]==1){ymax*=20.0;}
    sprintf(c,"%s",cTrkHist[hid]);
    TH2F *frame = new TH2F(c,c,1,xmin,xmax,1,ymin,ymax); frame->SetStats(0); frame->Draw();
    for(int quad=0; quad<kFgtNumQuads; quad++){
      TH1F *h=histTrk[quad][hid];
      h->SetLineColor(colorQuad[quad]); h->SetLineWidth(3); 
      if(hid<5){
	h->Draw("SAME");  
	float mean=h->GetMean();
	if(hid==1 || hid==4) {sprintf(c,"%s mean=%6.4f",cquad[quad],mean);}
	else                 {sprintf(c,"%s mean=%6.2f",cquad[quad],mean);}
      }else{
	h->SetMarkerColor(colorQuad[quad]); h->SetMarkerStyle(20); h->SetMarkerSize(1);
	h->Draw("SAME PL");
	sprintf(c,"Quad%s",cquad[quad]);
      }
      TText *t1;
      float x1= 0.2, x2= 0.55;
      float y1=0.8 - 0.07*quad;
      float y2=0.8 - 0.07*(quad-2);
      if(quad<2) { t1 = new TText(x1,y1,c); }
      else       { t1 = new TText(x2,y2,c); }
      t1->SetNDC();
      t1->SetTextSize(0.06); 
      t1->SetTextColor(colorQuad[quad]); 
      t1->Draw();
    }
  }
  c1->Update();
  save("trk");
}

void makeqaplot(int run=0, int plt=0, int save=0){
  runnum=run;
  yearday=run/1000;
  if(save==0) {png=0; pdf=0;}
  if(save==1) {png=1; pdf=0;}
  if(save==2) {png=0; pdf=1;}
  if(save==3) {png=1; pdf=1;}

  c1 = new TCanvas("c1","QA",50,50,800,800);
  gStyle->SetLabelSize(0.04,"xy");
  //colortable();
  gStyle->SetPalette(1);
  gStyle->SetStatW(0.4);

  char fname[50];
  if(run==0) {sprintf(fname,"fgtQA.root");}
  else {sprintf(fname,"%d/fgtQA_%d.root",yearday,run);}

  cout << "Opening "<<fname<<endl;
  file=new TFile(fname,"");

  char c[50];
  if(plt==0 || plt==1) {
    gStyle->SetOptStat(111110);
    c1->Divide(1,3); 
    for(int i=0; i<3; i++){
      TVirtualPad* pad = c1->cd(i+1);
      int log=0;
      if(i>0) {log=1;} 
      pad->SetLogy(log); 
      hist0[i]=(TH1F*) file->Get(cHist[i]);
      hist0[i]->SetFillColor(kBlue); 
      hist0[i]->Draw();
      if(i==2){
	float ntot  = float(hist0[i]->GetEntries());
	if(ntot>0.0){
	  int    nbin = hist0[i]->GetNbinsX();
	  float nofgt = hist0[i]->Integral(1,1);	
	  int     bin = hist0[i]->FindBin(float(kFgtNumElecIds));
	  float nonzs = hist0[i]->Integral(bin-1,nbin+1);
	  float    zs = hist0[i]->Integral(2,bin-2);
	  hist0[i]->GetXaxis()->SetRange(2, bin-2);
	  float mean =  hist0[i]->GetMean() / float(kFgtNumElecIds);
	  char c[100];
	  sprintf(c,"Total  %d",ntot);                         TText *t1 = new TText(0.3,0.8,c); t1->SetNDC(); t1->SetTextSize(0.06); t1->Draw();
	  sprintf(c,"NoFGT  %d (%5.2f)",nofgt,nofgt/ntot);     TText *t2 = new TText(0.3,0.7,c); t2->SetNDC(); t2->SetTextSize(0.06); t2->Draw();
	  sprintf(c,"NoneZS %d (%5.2f)",nonzs,nonzs/ntot);     TText *t3 = new TText(0.3,0.6,c); t3->SetNDC(); t3->SetTextSize(0.06); t3->Draw();
	  sprintf(c,"ZS     %d (%5.2f)",zs,zs/ntot);           TText *t4 = new TText(0.3,0.5,c); t4->SetNDC(); t4->SetTextSize(0.06); t4->Draw();
	  sprintf(c,"Mean ZS data size/fullsize= %5.3f",mean); TText *t5 = new TText(0.3,0.4,c); t5->SetNDC(); t5->SetTextSize(0.06); t5->Draw();
	  if(mean>0.08) { t5->SetTextColor(2); }
	  else          { t5->SetTextColor(4); }
	}
      }
    }
    c1->Update();
    save("plot");
  }  
  if(plt==0 || plt==2) {
    c1->Clear();
    gStyle->SetOptStat(0);
    gStyle->SetOptTitle(0);
    c1->Divide(1,3);
    for(int i=3; i<6; i++){ hist0[i]=(TH1F*) file->Get(cHist[i]); }
    int nevt=hist0[5]->GetEntries();
    printf("Nevent=%d\n",nevt);
    TVirtualPad* pad;
    pad=c1->cd(1); pad->SetLogy(); pad->SetTopMargin(0.01); pad->SetRightMargin(0.01);
    hist0[3]->GetXaxis()->SetLabelSize(0.07); hist0[3]->GetYaxis()->SetLabelSize(0.07);     
    hist0[3]->SetFillColor(kRed);  hist0[3]->Scale(1/float(nevt)); hist0[3]->Draw();
    TText *tx= new TText(0.87,0.0,"EleID"); tx->SetNDC(); tx->SetTextSize(0.1); tx->Draw();
    TText *t3= new TText(0.05,0.1,"F3=frac in 3sig/2tb"); t3->SetNDC(); t3->SetTextSize(0.08); t3->SetTextAngle(90); t3->Draw();

    pad=c1->cd(2); pad->SetLogy(); pad->SetTopMargin(0.01); pad->SetRightMargin(0.01);
    hist0[4]->GetXaxis()->SetLabelSize(0.07); hist0[4]->GetYaxis()->SetLabelSize(0.07); 
    hist0[4]->SetFillColor(kBlue); hist0[4]->Scale(1/float(nevt)); hist0[4]->Draw(); 
    tx->Draw();
    TText *t4= new TText(0.05,0.1,"F10=frac in 10sig & >500"); t4->SetNDC(); t4->SetTextSize(0.08); t4->SetTextAngle(90); t4->Draw();

    float min=-4;
    int max=hist0[3]->GetNbinsX();
    printf("Max=%d\n",max);
    TH1F *h1 = new TH1F("ZSdataFrac","ZSdataFrac",50,min,0);
    TH1F *h2 = new TH1F("10SigmaFrac","10SigmaFrac",50,min,0);
    float f1[kFgtNumElecIds],f2[kFgtNumElecIds];
    for(int i=0; i<max; i++){
      f1[i] = log10(hist0[3]->GetBinContent(i+1)); if(f1[i]<min) {f1[i]=min;} h1->Fill(f1[i]);
      f2[i] = log10(hist0[4]->GetBinContent(i+1)); if(f2[i]<min) {f2[i]=min;} h2->Fill(f2[i]);
    }
    pad = c1->cd(3); pad->Divide(2,1);
    TVirtualPad *pad2;
    pad2 = pad->cd(1);
    pad2->SetLogy(); pad2->SetTopMargin(0.01); pad2->SetRightMargin(0.01);
    h1->GetXaxis()->SetLabelSize(0.1); h1->GetYaxis()->SetLabelSize(0.1);
    h2->GetXaxis()->SetLabelSize(0.1); h2->GetYaxis()->SetLabelSize(0.1);
    h1->SetLineColor(kRed);  h2->SetLineColor(kBlue); 
    if(h1->GetMaximum()>h2->GetMaximum()){
      h1->Draw(); h2->Draw("SAME");
    }else{
      h2->Draw(); h1->Draw("SAME");
    }
    TText *t5= new TText(0.2,0.88,"Log(F3)");  t5->SetNDC(); t5->SetTextSize(0.1); t5->SetTextColor(2); t5->Draw();
    TText *t6= new TText(0.6,0.88,"Log(F10)"); t6->SetNDC(); t6->SetTextSize(0.1); t6->SetTextColor(4); t6->Draw();

    //read ped file
    int eid, t;
    float ped[kFgtNumElecIds],rms[kFgtNumElecIds],p,r;
    int status[kFgtNumElecIds]; memset(status,0,sizeof(status));
    cout<<"Reading Ped File "<<endl;
    std::ifstream in("ped.txt");
    if (!in.is_open()) {
      cout << "Can't find file!\n"; 
      exit(0); 
    }   
    while (!in.eof()){
      in >> eid >> t >> p >> r;
      ped[eid]=p;
      rms[eid]=r;
    }  
    in.close();
    TH1F * hr1= new TH1F("RMS","RMS",60,0,120);
    TH1F * hr2= new TH1F("RMS2","RMS2",60,0,120);
    TH1F * hr3= new TH1F("RMS3","RMS3",60,0,120);
    TH1F * hr4= new TH1F("RMS4","RMS4",60,0,120);
    TH1F * hr5= new TH1F("RMS5","RMS5",60,0,120);
    float f1l=-3.5, f1h=-0.5;
    float f2l=-3.5, f2h=-1.2;
    for(int i=0; i<kFgtNumElecIds; i++){
      if(f1[i]<f1l) status[i]+=1;
      if(f1[i]>f1h) status[i]+=2;
      if(f1[i]<f2l) status[i]+=4;
      if(f1[i]>f2h) status[i]+=8;
      hr1->Fill(rms[i]);
      if(f1[i]<f1l) {hr2->Fill(rms[i]);}
      if(f1[i]<f1l || f1[i]>f1h) {hr3->Fill(rms[i]);}
      if(f1[i]<f1l || f1[i]>f1h || f2[i]<f2l) {hr4->Fill(rms[i]);} 
      if(f1[i]<f1l || f1[i]>f1h || f2[i]<f2l || f2[i]>f2h) {hr5->Fill(rms[i]);} 
    }
    char filename[150];
    if(nevt>=1000){
      sprintf(filename,"%d/status/status.%d.txt",yearday,run);
    }else{
      sprintf(filename,"%d/status/status.%d.lowstat.txt",yearday,run);
    }
    FILE* pfile=fopen(filename,"w");
    if(!pfile){
      printf("Couldn't open file %s\n",filename);
    }else{
      printf("Writing %s\n",filename);
      for(int i=0; i<kFgtNumElecIds; i++){
	fprintf(pfile,"%d 0x%d\n",i,status[i]);
      }
      fclose(pfile);
    }
    
    pad2 = pad->cd(2);
    pad2->SetLogy(0); pad2->SetTopMargin(0.01); pad2->SetRightMargin(0.01);
    hr1->GetXaxis()->SetLabelSize(0.1); hr1->GetYaxis()->SetLabelSize(0.05);
    hr1->SetFillColor(3); hr1->Draw(); 
    hr5->SetFillColor(9); hr5->Draw("same"); 
    hr4->SetFillColor(4); hr4->Draw("same"); 
    hr3->SetFillColor(6); hr3->Draw("same"); 
    hr2->SetFillColor(2); hr2->Draw("same"); 
    char cc1[100]; sprintf(cc1,"Log(F3)<%4.1f",f1l);
    char cc2[100]; sprintf(cc2,"Log(F3)>%4.1f",f1h);
    char cc3[100]; sprintf(cc3,"Log(F10)<%4.1f",f2l);
    char cc4[100]; sprintf(cc4,"Log(F10)>%4.1f",f2h);
    TText *t7 = new TText(0.6,0.88,cc1);   t7->SetNDC();  t7->SetTextSize(0.07);  t7->SetTextColor(2);  t7->Draw();
    TText *t8 = new TText(0.6,0.78,cc2);   t8->SetNDC();  t8->SetTextSize(0.07);  t8->SetTextColor(6);  t8->Draw();
    TText *t9 = new TText(0.6,0.68,cc3);   t9->SetNDC();  t9->SetTextSize(0.07);  t9->SetTextColor(4);  t9->Draw();
    TText *t10= new TText(0.6,0.58,cc4);   t10->SetNDC(); t10->SetTextSize(0.07); t10->SetTextColor(9); t10->Draw();
    TText *t11= new TText(0.6,0.48,"OK");  t11->SetNDC(); t11->SetTextSize(0.07); t11->SetTextColor(3); t11->Draw();
    TText *t12= new TText(0.8,0.15,"PedRMS"); t12->SetNDC(); t12->SetTextSize(0.07); t12->Draw();
    c1->Update();
    save("frac");
  }
  if(plt==0 || plt==3) {
    c1->Clear();
    c1->Divide(2,2); 
    gStyle->SetOptTitle(1);
    gStyle->SetOptStat(111110);
    for(int i=0; i<4; i++){
      TVirtualPad* pad = c1->cd(i+1);
      int log=0;
      pad->SetLogy(log); 
      hist0[i+6]=(TH1F*) file->Get(cHist[i+6]);
      hist0[i+6]->SetFillColor(kBlue); 
      hist0[i+6]->Draw();
    }
    c1->Update();
    save("pfit");
  }  

  memset(mPeakC,0,sizeof(mPeakC));
  memset(mPeakA,0,sizeof(mPeakA));
  memset(mFracA,0,sizeof(mFracA));
  memset(mPeakL,0,sizeof(mPeakL));
  for(int i=0; i<N1dHist; i++) { if(plt==0 || plt==10+i) plot1d(i);}
  for(int i=0; i<N2dHist; i++) { if(plt==0 || plt==50+i) plot2d(i);}
  if(plt==0 || plt==60) plotTrk();

  if(plt==0){
    char filename[100];
    sprintf(filename,"%d/gain.%d.txt",yearday,run);
    FILE* file=fopen(filename,"w");
    if(!file){
      printf("Couldn't open $file\n",filename);
    }else{
      printf("Writing %s\n",filename);
      for(int i=0; i<kFgtNumDiscs; i++){
	for(int j=0; j<kFgtNumQuads; j++){
	  fprintf(file,"%d %d %f %f %f %f\n",i,j,mPeakC[i][j],mPeakA[i][j],mFracA[i][j],mPeakL[i][j]);
	}
      }
      fclose(file);
    }    
  }  
}
