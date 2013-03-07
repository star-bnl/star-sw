void resid(int run=0, int plt=1, int png=0, int ba=1, int quad=0, int disc=0){
  static const int kFgtNumDiscs=6;
  static const int kFgtNumQuads=4;  
  static const int NAXIS=4;                      
  TH1F *hist[kFgtNumDiscs][kFgtNumQuads][NAXIS]; 
  TH2F *hist2[kFgtNumDiscs][kFgtNumQuads][NAXIS*2]; 
  
  TCanvas *c1 = new TCanvas("c1","trk",1200,1000);
  gStyle->SetLabelSize(0.04,"xy");
  gStyle->SetPalette(1);
  gStyle->SetStatW(0.4);

  int yearday=run/1000;
  char *fname[3]={"alignment.root","alignment_before.root","alignment_after.root"};
  if(run>0) {
    sprintf(fname[0],"%d/alignment_%d.root",yearday,run);
    sprintf(fname[1],"%d/alignment_before_%d.root",yearday,run);
    sprintf(fname[2],"%d/alignment_after_%d.root",yearday,run);
  }

  const char* caxis[NAXIS]={"dx","dy","dr","dphi"};
  const char* caxis2[NAXIS*2]={"dx.vs.x","dx.vs.y",  "dy.vs.x",  "dy.vs.y",
                               "dr.vs.r","dr.vs.phi","dphi.vs.r","dphi.vs.phi"};
  const float max[NAXIS]={0.1,0.1,0.1,0.02};
  const char* cquad[kFgtNumQuads]={"A","B","C","D"};
  const int color[kFgtNumDiscs]={1,2,8,4,6,9};

  cout << "Opening "<<fname[ba]<<endl;
  TFile* file=new TFile(fname[ba],"old");

  for(int idisc=0; idisc<kFgtNumDiscs; idisc++){
    for(int iquad=0; iquad<kFgtNumQuads; iquad++){
      for(int axis=0; axis<NAXIS; axis++){
	char c[50];
	sprintf(c,"%1d%1s-%s",idisc+1,cquad[iquad],caxis[axis]);
	hist[idisc][iquad][axis]=(TH1F*) file->Get(c);
      }
      for(int axis=0; axis<NAXIS*2; axis++){
	char c[50];
	sprintf(c,"%1d%1s-%s",idisc+1,cquad[iquad],caxis2[axis]);
	hist2[idisc][iquad][axis]=(TH2F*) file->Get(c);
      }
    }
  }
  
  if(plt==1 || plt==0){
    c1->Divide(2,2);
    for(int axis=0; axis<NAXIS; axis++){
      c1->cd(axis+1);    
      double xmin, xmax, ymax=0.0;
      for(int idisc=0; idisc<kFgtNumDiscs; idisc++){
	//cout << "axis=" << axis << " idisc=" << idisc<<endl;
	TH1F *h=hist[idisc][quad][axis];
	if(!h || (disc>0 && disc!=idisc+1) ) continue;
	xmin=h->GetXaxis()->GetXmin();
	xmax=h->GetXaxis()->GetXmax();
	double m=h->GetMaximum();
	if(ymax<m) ymax=m;
	printf("disc=%d idisc+1=%d max=%6.1f ymax=%6.1f\n",disc,idisc+1,m,ymax);
      }    
      TH2F *frame = new TH2F("frame","",1,xmin,xmax,1,0.0,ymax*1.2); frame->SetStats(0);
      frame->Draw();
      for(int idisc=0; idisc<kFgtNumDiscs; idisc++){
	TH1F *h=hist[idisc][quad][axis];
	if(!h || (disc>0 && disc!=idisc+1)) continue;
	if(disc>0) h->SetStats(1111111);
	h->SetLineColor(color[idisc]); h->Draw("SAME");  
	h->Fit("gaus","0");
	TF1 *g = h->GetFunction("gaus");
	g->SetLineColor(color[idisc]); g->Draw("same");
	double s=g->GetParameter(2);
	char c[20]; 
	if(axis==3) {sprintf(c,"%1d%s sig=%6.4f",idisc+1,cquad[quad],s);}
	else        {sprintf(c,"%1d%s sig=%6.3f",idisc+1,cquad[quad],s);}
	TText *t1;
	if(disc>0 || (disc==0 && idisc<3)) { t1 = new TText(xmin*0.9,ymax*(1.1-0.1*idisc),c); }
	else                               { t1 = new TText(xmax*0.3,ymax*(1.1-0.1*(idisc-3)),c); }
	t1->SetTextSize(0.05); t1->SetTextColor(color[idisc]); t1->Draw(); 
      }
      TText *t2 = new TText(xmax*0.9,-ymax*0.10,caxis[axis]);
      t2->SetTextSize(0.05); t2->Draw();
    }
    c1->Update();
    if(png){
      char cpng[100];
      sprintf(cpng,"%d/resid1d,%d.png",yearday,run);
      c1->SaveAs(cpng);
    }
  }
  if(plt==2 || plt==0){
    c1->Divide(2,4);
    for(int axis=0; axis<2*NAXIS; axis++){
      c1->cd(axis+1);
      TH2F *h=hist2[disc-1][quad][axis];
      if(!h || disc==0) continue;
      h->Draw("COLZ");
    }
    c1->Update();
    if(png){
      char cpng[100];
      sprintf(cpng,"%d/resid2d,%d.png",yearday,run);
      c1->SaveAs(cpng);
    }
  }
}
