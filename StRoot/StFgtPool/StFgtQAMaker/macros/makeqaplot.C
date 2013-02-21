static const int kFgtNumDiscs=6;
static const int kFgtNumQuads=4;  

static const int NHist=3;     
static const int N1dHist=7;   
static const int N2dHist=2;   
static const int NTrkHist=4;

static TH1F *hist0[NHist];                               //! Histos for whole fgt
static TH1F *hist1[kFgtNumDiscs][kFgtNumQuads][N1dHist]; //! 1d histos for each disc/quad
static TH2F *hist2[kFgtNumDiscs][N2dHist];               //! 2d histos for each disc
static TH1F *histTrk[kFgtNumQuads][NTrkHist];            //! Histos for tracks

static const char* cquad[kFgtNumQuads]={"A","B","C","D"}; 
static const char* cHist[NHist]={"MaxTimeBin","ADC","DataSize"};

static const char* c1dHist[N1dHist]={"NHitStrip", "PhiHit",   "RHit", "NCluster","ClusterSize","ClusterCharge","ChargeAsy"};
static const int   l1dHist[N1dHist]={          1,        1,        1,          1,            1,              0,          0};
static const int   m1dHist[N1dHist]={          0,        1,        1,          0,            0,              0,          0};

static const char* c2dHist[N1dHist]={"XY","ADCvsTB"};

static const int color[kFgtNumDiscs]={1,2,8,4,6,9};


TCanvas *c1;
static TFile* file;
static int runnum, yearday, png, pdf;

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
      c1->cd(quad+1); 
      gPad->SetLogy(l1dHist[hid]);
      double xmin, xmax, ymin=0.0, ymax=0.0;
      if(l1dHist[hid]==1) ymin=0.1;
      for(int disc=0; disc<kFgtNumDiscs; disc++){
	sprintf(c,"%1d%1s-%s",disc+1,cquad[quad],c1dHist[hid]);
	printf("Getting %s\n",c);
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
	h->SetLineColor(color[disc]); 
	h->SetLineWidth(3);
	h->Draw("SAME");  
	float mean=h->GetMean();
	sprintf(c,"%1d%s mean=%6.2f",disc+1,cquad[quad],mean);
        TText *t1;
	//	float x1=xmin*0.9 + 0.1*xmax;
	//      float x2=xmin*0.5 + 0.5*xmax;
        //if(disc<3) { t1 = new TText(x1,ymax*(1.1-0.1*disc),c); }
        //else       { t1 = new TText(x2,ymax*(1.1-0.1*(disc-3)),c); }
	float x1= 0.2, x2= 0.5;
	float y1=0.8 - 0.07*disc;
	float y2=0.8 - 0.07*(disc-3);
        if(disc<3) { t1 = new TText(x1,y1,c); }
        else       { t1 = new TText(x2,y2,c); }
	t1->SetNDC();
	t1->SetTextSize(0.05); 
	t1->SetTextColor(color[disc]); 
	t1->Draw();
      }
    }
  }else{
    c1->Divide(4,6);
    for(int disc=0; disc<kFgtNumDiscs; disc++){
      for(int quad=0; quad<kFgtNumQuads; quad++){
	c1->cd(disc*4+quad+1);
	gPad->SetLogy(l1dHist[hid]);
	sprintf(c,"%1d%1s-%s",disc+1,cquad[quad],c1dHist[hid]);
	TH1F *h = hist1[disc][quad][hid] = (TH1F*)file->Get(c);
	h->SetFillColor(color[disc]);
	h->Draw();
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
  c1->Divide(2,3);
  for(int disc=0; disc<kFgtNumDiscs; disc++){
    c1->cd(disc+1);
    sprintf(c,"Disc%1d%s",disc+1,c2dHist[hid]);
    printf("Getting %s\n",c);
    TH2F *h = hist2[disc][hid] = (TH2F*)file->Get(c);
    h->Draw("COLZ");  
  }
  c1->Update();
  save(c2dHist[hid]);
}

void makeqaplot(int run=0, int plt=0, int save=1, int day=0){
  runnum=run;
  yearday=day;
  if(save==0) {png=0; pdf=0;}
  if(save==1) {png=1; pdf=0;}
  if(save==2) {png=0; pdf=1;}
  if(save==3) {png=1; pdf=1;}

  c1 = new TCanvas("c1","QA",50,0,800,800);
  gStyle->SetLabelSize(0.04,"xy");
  gStyle->SetPalette(1);
  gStyle->SetStatW(0.4);

  char fname[50];
  if(run==0) {sprintf(fname,"fgtQA.root");}
  else {sprintf(fname,"%d/fgtQA_%d.root",day,run);}

  cout << "Opening "<<fname<<endl;
  file=new TFile(fname,"old");

  char c[50];
  if(plt==0 || plt==1) {
    gStyle->SetOptStat(111110);
    c1->Divide(1,3); 
    for(int i=0; i<NHist; i++){
      c1->cd(i+1);
      int log=0;
      if(i>0) {log=1;} 
      gPad->SetLogy(log); 
      hist0[i]=(TH1F*) file->Get(cHist[i]);
      hist0[i]->SetFillColor(kBlue); 
      hist0[i]->Draw();
    }
    c1->Update();
    c1->Update();
    save("plot");
  }  
  for(int i=0; i<N1dHist; i++) { if(plt==0 || plt==10+i) plot1d(i);}
  for(int i=0; i<N2dHist; i++) { if(plt==0 || plt==20+i) plot2d(i);}
}
