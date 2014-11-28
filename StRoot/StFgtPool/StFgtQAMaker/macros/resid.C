void resid(int run=0, int plt=1, int quad=-1, int png=0, int ba=1, int disc=0){
  static const int kFgtNumDiscs=6;
  static const int kFgtNumQuads=4;  
  static const int NAXIS=4;                      

  TH1F *hist[kFgtNumDiscs][kFgtNumQuads][NAXIS]; 
  TH2F *hist2[kFgtNumDiscs][kFgtNumQuads][NAXIS*2]; 
  
  TCanvas *c1 = new TCanvas("c1","trk",700,800);
  gStyle->SetLabelSize(0.04,"xy");
  gStyle->SetPalette(1);
  gStyle->SetStatW(0.4);
  
  int yearday=run/1000;
  char fname[3][150]={"alignment.root","alignment_before.root","alignment_after.root"};
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
  TFile* file=new TFile(fname[ba],"");

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
 
  char cc[100], cpdf[100];
  sprintf(cpdf,"%d/resid1d.%d.pdf",yearday,run);
  if(png==2) {sprintf(cc,"%s[",cpdf); c1->Print(cc,"pdf");}

  int quad1=quad, quad2=quad;
  if(quad==-1) {quad1=0; quad2=kFgtNumQuads-1;}
  
  for(int q=quad1; q<=quad2; q++){    
    if(plt==1 || plt==0){    
      c1->Clear();
      c1->Divide(2,2);
      for(int axis=0; axis<NAXIS; axis++){
	c1->cd(axis+1);    
	double xmin, xmax, ymax=0.0;
	for(int idisc=0; idisc<kFgtNumDiscs; idisc++){
	  //cout << "axis=" << axis << " idisc=" << idisc<<endl;
	  TH1F *h=hist[idisc][q][axis];
	  if(!h || (disc>0 && disc!=idisc+1) ) continue;
	  xmin=h->GetXaxis()->GetXmin();
	  xmax=h->GetXaxis()->GetXmax();
	  double m=h->GetMaximum();
	  if(ymax<m) ymax=m;
	  //printf("disc=%d idisc+1=%d max=%6.1f ymax=%6.1f\n",disc,idisc+1,m,ymax);
	}    
	char c[50];
	sprintf(c,"frame%1s-%s",cquad[q],caxis[axis]);
	TH2F *frame = new TH2F(c,"",1,xmin,xmax,1,0.0,ymax*1.2); frame->SetStats(0);
	frame->Draw();
	for(int idisc=0; idisc<kFgtNumDiscs; idisc++){
	  TH1F *h=hist[idisc][q][axis];
	  if(!h || (disc>0 && disc!=idisc+1)) continue;
	  if(disc>0) h->SetStats(1111111);
	  h->SetLineColor(color[idisc]); h->Draw("SAME");  
	  double s=0;
	  if(h->GetEntries()>10){
	    h->Fit("gaus","0Q");
	    TF1 *g = h->GetFunction("gaus");
	    g->SetLineColor(color[idisc]); g->Draw("same");
	    s=g->GetParameter(2);
	  }
	  char c[40]; 
	  if(axis==3) {sprintf(c,"%1d%s sig=%6.4f",idisc+1,cquad[q],s);}
	  else        {sprintf(c,"%1d%s sig=%6.3f",idisc+1,cquad[q],s);}
	  TText *t1;
	  if(disc>0 || (disc==0 && idisc<3)) { t1 = new TText(xmin*0.9,ymax*(1.1-0.1*idisc),c); }
	  else                               { t1 = new TText(xmax*0.2,ymax*(1.1-0.1*(idisc-3)),c); }
	  t1->SetTextSize(0.05); t1->SetTextColor(color[idisc]); t1->Draw(); 
	}
	TText *t2 = new TText(xmax*0.9,-ymax*0.10,caxis[axis]);
	t2->SetTextSize(0.05); t2->Draw();
      }
      c1->Update();
      char cpng[100];
      if(png==1){
	sprintf(cpng,"%d/resid1d,%d.%d.png",yearday,run,q);
	c1->SaveAs(cpng);
      }else if(png==2) {c1->Print(cpdf,"pdf");}
    }
    
    if(plt==2 || plt==0){
      int disc1=disc;
      int disc2=disc;
      if(disc==0){ disc1=1; disc2=6; }
      for(int d=disc1; d<disc2; d++){
	c1->Clear();
	c1->Divide(2,4);
	for(int axis=0; axis<2*NAXIS; axis++){
	  c1->cd(axis+1);
	  TH2F *h2=hist2[d-1][q][axis];
	  //if(!h || disc==0) continue;
	  h2->Draw("COLZ");
	}
	c1->Update();
	if(png==1){
	  char cpng[100];
	  sprintf(cpng,"%d/resid2d.%d.d%d.q%d.png",yearday,run,d,q);
	  c1->SaveAs(cpng);
	}else if(png==2) {c1->Print(cpdf,"pdf");}
      }
    }
  }
  if(png==2) {sprintf(cc,"%s]",cpdf); c1->Print(cc,"pdf");}
}
