static const int NQ=4;
static const int NL=3;
static const int NS=21;
static const int NID=NQ*NL*NS;

static const int NCUT1=6;
static const int NCUT2=6;

TH2F* mH2[NQ][NL][NCUT1];   
TH1F* mHd[NQ][NL][NCUT1];   
TH2F* mHd2[NQ][NL][NCUT1];   

static int LOG=0;
static TCanvas *c1;
static TCanvas *c2;
static TCanvas *c3;
static TString* FILENAME;
TFile* mTFile;

//for single point histogram
const int col1[NCUT1]={1,2,4,6,8,9};
const char* CCUT1[NCUT1]={"All","E>6GeV","gamma","hadron","electron","others"};

//for point pair histogram
const int col2[NCUT2]={1,2,4,6,8,9};
const char* CCUT2[NCUT2]={"All/50","E1,E2>6GeV,E>20GeV","gg","hh","ee","others"};

static const float zfms=730.0;
static float dx[NID];
static float dy[NID];
static float dz[NID];
static float xx[NID];
static float yy[NID];
static float zz[NID];

static float FIT[NID];
static float SIG[NID];
static float GEOM[NID];

int getSlatid(int q, int l, int s){
  return (q-1)*NL*NS + (l-1)*NS + s-1;
}

int getQLS(int slatid, int *q, int *l, int *s){
  s = slatid%NQ + 1;
  l = (slatid/NQ)%NL + 1;
  q = (slatid/NQ/NL)%NQ + 1;
}

float project(float x, float z, float zp, float vz){
  //printf("x=%6.2f z=%6.2f zp=%6.2f vtx=%6.2f  proj=%6.2f\n",x,z,zp,vz, x/(z-vz)*(zp-vz));
  return x/(z-vz)*(zp-vz);
}

void readFpsPosition(char* input="fpsgeom.txt"){
  FILE *FP = fopen(input,"r");
  if(!FP) { printf("Could not open %s\n",input); exit;}
  printf("Reading %s\n",input);
  char line[1000];  
  int slatid,q,l,s,n=0;
  float ix,iy,iz,idx,idy,idz;
  while(fgets(line,1000,FP)!=NULL){
    sscanf(line,"%d %d %d %d %f %f %f %f %f %f",
	   &slatid,&q,&l,&s,&idx,&idy,&idz,&ix,&iy,&iz);
    //printf("Id=%3d Q=%3d L=%3d S=%3d D=%9.4f %9.4f %9.4f X=%9.4f %9.4f %9.4f\n",
    //  slatid,q,l,s,idx,idy,idz,ix,iy,iz);
    xx[slatid]=ix;
    yy[slatid]=iy;
    zz[slatid]=iz;
    dx[slatid]=idx;
    dy[slatid]=idy;
    dz[slatid]=idz;
    //if(q==1 && l==3)
    if(0)
      printf("Id=%3d Q=%3d L=%3d S=%3d x=%9.4f %9.4f %9.4f d=%9.4f %9.4f %9.4f\n",
	     slatid,q,l,s,
	     xx[slatid],yy[slatid],zz[slatid],
	     dx[slatid],dy[slatid],dz[slatid]);
    n++;
  }  
  printf("Found %d entries\n",n);
}

void plotd(int quad=0, int layer=0, int cut=0){
  gStyle->SetOptStat(0);
  c1->Clear();
  TVirtualPad *pad;
  if(quad>0 || layer>0) {pad = c1->cd(0);}
  for(int l=1; l<=NL; l++){
    if(layer>0 && layer!=l) continue;
    if(layer==0){c1->Clear();}
    if(quad==0) {c1->Divide(2,2);}
    for(int q=1; q<=NQ; q++){      
      if(quad>0 && quad!=q) continue;
      if(quad==0) {pad=c1->cd(q);}
      pad->SetLogy(LOG);
      mHd[q-1][l-1][cut]->Draw();
    }
    if(layer==0){c1->SaveAs(Form("plot2/fmsfpsd_L%1d.png",layer));}
  }
  if(quad>0 || layer>0) {c1->SaveAs(Form("plot2/fmsfpsd_Q%1dL%1d.png",quad,layer));}
}

void plot2d(int quad=0, int layer=0, int slat=0, int cut=0, int val=0, int scale=1, int slice=1){
  gStyle->SetOptStat(0);
  c1->Clear();
  TVirtualPad *pad;
  if(quad>0 || layer>0) {pad = c1->cd(0);}
  for(int l=1; l<=NL; l++){
    if(layer>0 && layer!=l) continue;
    if(layer==0){c1->Clear();}
    if(slice==0 && quad==0) {c1->Divide(2,2);}
    for(int q=1; q<=NQ; q++){      
      if(quad>0 && quad!=q) continue;
      if(quad==0) {pad=c1->cd(q);}
      pad->SetLogz(LOG);
      TH2F* h;
      if(val==0) h=mH2[q-1][l-1][cut];
      if(val==1) h=mHd2[q-1][l-1][cut];
      TH1D* projx=h->ProjectionX();
      TH2F* h2=(TH2F*)h->Clone(Form("Q%1dL%1d_scaled",q,l)); h2->Reset();
      int nx=h->GetNbinsX();
      int ny=h->GetNbinsY();
      for(int x=0; x<=nx+1; x++){	
	float w=projx->GetBinContent(x);
	for(int y=0; y<=ny+1; y++){
	  int bin=h->GetBin(x,y);
	  float v=h->GetBinContent(bin);
	  float newv=0.0;
	  if(w>0.0) newv=v/w;
	  h2->SetBinContent(bin,newv);
	  //printf("x=%3d y=%3d w=%6.2f v=%6.2f v/w=%8.5f\n",x,y,w,v,newv);
	}
      }
      if(slice==0){
	//h2->SetMinimum(h2->GetMaximum()*0.6);
	if(scale)  {h2->Draw("colz");}
	else       {h->Draw("colz");}  
      }else{
	c1->Clear();
	if(slat==0) {c1->Divide(3,7);}
	for(int s=1; s<=NS; s++){
	  int slatid=getSlatid(q,l,s);
	  if(slat>0 && s!=slat) continue;
	  if(s>19 && (q==2 || q==4)) continue;
	  char cc[100];
	  sprintf(cc,"Q%1dL%1dS%02d",q,l,s);
	  TH1D *hp = h2->ProjectionX(cc,s,s);
	  float xxx,dxx;

	  if(0){ //playing by hand
	    if(l==1){
	      xxx=xx[slatid]; dxx=dx[slatid];
	      if(q<=2) xxx=xxx/1.02 - 0.5;
	      if(q>=3) xxx=xxx*1.015 + 1.5;
	    }else if(l==2){ 
	      xxx=yy[slatid]; dxx=dy[slatid];
	      xxx=xxx/1.010 + 0.5;
	    }else if(l==3){ 
	      xxx=yy[slatid]; dxx=dy[slatid];
	      xxx=xxx/1.018 + 0.5;
	    }
	  }
	  if(1){ //offsets are now in geom file
	    float factor=1.00;
	    if(l==1){
	      xxx=xx[slatid]; dxx=dx[slatid];
	      if(q<=2) xxx=xxx/factor;
	      //if(q>=3) xxx=xxx*1.015;
	      if(q>=3) xxx=xxx/factor;
	    }else if(l==2){ 
	      xxx=yy[slatid]; dxx=dy[slatid];
	      xxx=xxx/factor;
	    }else if(l==3){ 
	      xxx=yy[slatid]; dxx=dy[slatid];
	      xxx=xxx/factor;
	    }
	  }

	  float xmin=xxx-3.0*dxx;
	  float xmax=xxx+3.0*dxx;
	  printf("xmin/xmax=%f %f\n",xmin,xmax);
	  int bmin=hp->FindBin(xmin);
	  int bmax=hp->FindBin(xmax);
	  printf("bmin/bmax=%f %f\n",bmin,bmax);
	  hp->GetXaxis()->SetRange(bmin,bmax);
	  float min=hp->GetMinimum(0.02); 
	  float max=hp->GetMaximum();
	  float delta=max-min;
	  float peak=hp->GetBinCenter(hp->GetMaximumBin());
	  float avg=hp->Integral(bmin,bmax)/(bmax-bmin+1);
	  float sig=dxx/2.0/2.0;
	  //printf("min/max/delta/peak/avg/sig=%f %f %f %f %f %f\n",min,max,delta,peak,avg,sig);
	  float ymin=min-delta*0.1;
	  float ymax=max+delta*0.3;
	  //hp->SetMaximum(ymax);
	  //hp->SetMinimum(ymin);
	  if(slat==0){
	    pad=c1->cd(s);
	    float mergin=0.005;
	    pad->SetRightMargin(mergin); pad->SetLeftMargin(mergin);
	    pad->SetTopMargin(mergin);   pad->SetBottomMargin(0.01);
	  }
	  if(q==1 || quad>0 || slat>0) {hp->Draw();}
	  else                         {hp->Draw("same");}
	  //draw lines from geometry parameters
	  for(int i=-1; i<2; i++){
	    float proj=xxx+i*dxx*0.5;
	    if(i==0) GEOM[slatid]=proj;
	    //printf("proj,ymin,ymax=%f %f %f\n",proj,ymin,ymax);
	    TLine *ll = new TLine(proj,ymin,proj,ymax);
	    ll->SetLineColor(kBlue); ll->SetLineWidth(2);
	    if(i!=0) ll->Draw();
	  }
	  //fit
	  TF1 *f=new TF1("ff", "gaus(0)+[3]", xmin, xmax);
	  xmin=proj-3.0*dxx;
	  xmax=proj+3.0*dxx;
	  //printf("xmin=%6.3f xmax=%6.3f max=%6.3f avg=%6.3f max-avg=%6.3f geom=%6.3f dxx=%6.3f\n",
	  // xmin,xmax,max,avg,max-avg,GEOM[slatid],dxx);
	  f->SetParameter(0,max-avg);
	  //f->SetParameter(1,GEOM[slatid]);
	  f->SetParameter(1,peak);
	  f->SetParameter(2,sig);
	  f->SetParameter(3,avg);
	  int res = hp->Fit("ff","0R");
	  FIT[slatid]=f->GetParameter(1);
	  SIG[slatid]=f->GetParameter(2);
	  //printf("A=%6.3f FIT=%6.3f SIG=%6.2f Base=%6.3f\n",f->GetParameter(0),FIT[slatid],SIG[slatid],f->GetParameter(3));
	  f->SetLineColor(kRed); 
	  f->Draw("same");
	} //loop over slat
	c1->SaveAs(Form("plot2/fmsfps2_Q%1dL%1d_cut%d_slice.png",q,layer,cut));
      } //if slice==1
    } //loop over quad
    if(slice==0) c1->SaveAs(Form("plot2/fmsfps2_Q%1dL%1d_cut%d.png",quad,layer,cut));
  } //loop over layer
  //if(quad>0 || layer>0) {c1->SaveAs(Form("plot2/fmsfps2_Q%1dL%1d_cut%d.png",quad,layer,cut));}
  if(slice==1 && slat==0){
    for(int l=1; l<=NL; l++){
      if(layer>0 && l!=layer) continue;
      c2->Clear();
      if(quad==0) c2->Divide(2,2);
      for(int q=1; q<=NQ; q++){
	if(quad>0 && q!=quad) continue;
	if(quad==0) c2->cd(q);
	TGraphErrors *g=new TGraphErrors(1);
	char t[100];
	sprintf(t,"Q%1dL%1d Fit-DB vs slat",q,l);
	g->SetTitle(t);
	float x1,x2;
	int pm[NQ][NL]={{1,1,1},{1,-1,-1},{-1,1,1},{-1,-1,-1}};
	if(pm[q-1][l-1]==1) {x1=-10.0;  x2=110.0;}
	else                {x1=-110.0; x2=10.0;}
	TH2F *frame=new TH2F(t,t,1,x1,x2,1,-10.0,10.0);
	frame->Draw();
	int np=0;
	for(int s=1; s<=NS; s++){
	  int slatid=getSlatid(q,l,s);
	  double d=FIT[slatid]-GEOM[slatid];
	  if(fabs(d)<2.0 && SIG[slatid]>0.5 && SIG[slatid]<6.0){
	    g->SetPoint(np,GEOM[slatid],d);
	    g->SetPointError(np,0.0,SIG[slatid]);
	    np++;
	  }
	}
	g->SetMarkerStyle(20); g->SetMarkerColor(kRed);
	g->Draw("pw");
	//fit     
	TF1 *f2=new TF1("f2", "[0]+[1]*x", -100, 100);
	f2->SetParameter(0,0.0);
	f2->SetParameter(1,0.0);
	int res = g->Fit("f2","0RQ");
	f2->SetLineColor(kBlue);
	f2->Draw("same");
      }
      c2->SaveAs(Form("plot2/fmsfps2_Q%1dL%1d_cut%d_align.png",quad,layer,cut));
    }
  }
}

void pltcut(char* name, TCanvas* c, int div, int log=0, int legend=0){
    char cc[100];
    TString hname(name);
    if(log==0) c->cd(div);
    if(log==1) c->cd(div)->SetLogy();
    if(name!=""){
	for(int cut=0; cut<NCUT1; cut++){
	    sprintf(cc,"%s_c%d",name,cut);
	    TH1* h=(TH1*)mTFile->Get(cc); 
	    if(cut==0 && hname.Contains("p_")) h->Scale(0.05);
	    h->SetLineColor(col1[cut]); h->SetMarkerColor(col1[cut]);
	    if(log==1) h->SetMinimum(0.1);    
	    if(name=="NP") h->SetMaximum(h->GetMaximum()*10.0);    
	    TString opt = "";
	    if(cut>0) opt+="same";
	    h->Draw(opt.Data());
	}
    }
    if(legend){
	for(int cut=0; cut<NCUT1; cut++){
	    TText* t=new TText(0.7,0.6-0.07*cut,CCUT1[cut]);
	    t->SetTextSize(0.07); t->SetNDC(); t->SetTextColor(col1[cut]);
	    t->Draw();
	}
    }
}

void pltcut2(char* name, TCanvas* c, int div, int log=0, int legend=0){
    char cc[100];
    TString hname(name);
    if(log==0) c->cd(div);
    if(log==1) c->cd(div)->SetLogy();
    if(log==2) c->cd(div)->SetLogz();
    if(name!=""){
	for(int cut=0; cut<NCUT2; cut++){
	    if(name=="p_pid" && cut==0) continue;
	    sprintf(cc,"%s_c%d",name,cut);
	    TH1* h=(TH1*)mTFile->Get(cc); 
	    if(cut==0) h->Scale(1/40.0);
	    if(cut==3 && name=="p_m1") h->Scale(20.0);
	    if(cut==4 && name=="p_m1") h->Scale(40.0);
	    h->SetLineColor(col2[cut]); h->SetMarkerColor(col2[cut]);
	    if(log==1) h->SetMinimum(0.1);    
	    if(name=="p_NP") h->SetMaximum(h->GetMaximum()*1000.0);    
	    TString opt = "";
	    if(cut>0) opt+="same";
	    //if(name=="p_ept") opt+="box";    
	    if(name=="p_pid") opt+="colz";    
	    h->Draw(opt.Data());
	}
    }
    if(legend){
	for(int cut=0; cut<NCUT2; cut++){
	    TText* t=new TText(0.7,0.6-0.07*cut,CCUT2[cut]);
	    t->SetTextSize(0.07); t->SetNDC(); t->SetTextColor(col2[cut]);
	    t->Draw();
	}
    }
}

void plotfms(){
  c1->Clear();
  c1->Divide(2,3);
  pltcut("NP", c1,1,1,1);
  pltcut("e",  c1,2,1);
  pltcut("elo",c1,3,1);
  pltcut("pt", c1,4,1);
  pltcut("ept",c1,5,0);
  pltcut("eta",c1,6,1);
  c1->SaveAs("plot2/fms1.png");

  c2->Clear();
  c2->Divide(2,3);
  pltcut("phi", c2,1,1,1);
  pltcut("x",   c2,2,1);
  pltcut("y",   c2,3,1);
  pltcut("xy",  c2,4,0);
  pltcut("pid", c2,5,0);
  pltcut("pid2",c2,6,0);
  c2->SaveAs("plot2/fms2.png");   
  
  c3->Clear();
  c3->Divide(2,3);
  c3->cd(1); xy_c0->Draw("");
  c3->cd(2); xy_c1->Draw("");
  c3->cd(3); xy_c2->Draw("");
  c3->cd(4); xy_c3->Draw("");
  c3->cd(5); xy_c4->Draw("");
  c3->cd(6); xy_c5->Draw("");
  c2->SaveAs("plot2/fms3.png");
}

void plotfmsPair(){
  c1->Clear();
  c1->Divide(2,3);
  pltcut2("p_NP",  c1, 1, 1);
  pltcut2("p_e",   c1, 2, 1);
  pltcut2("p_pt",  c1, 3, 1);
  pltcut2("p_ept", c1, 4, 0);
  pltcut2("p_eta", c1, 5, 1);
  pltcut2("p_phi", c1, 6, 1, 1);
  c1->SaveAs("plot2/pair1.png");

  c2->Clear();
  c2->Divide(2,3);
  pltcut2("p_m1",   c2, 1, 0);
  pltcut2("p_m2",   c2, 2, 1);
  pltcut2("p_zgg",  c2, 3, 1);
  pltcut2("p_r30",  c2, 4, 1);
  pltcut2("p_r100", c2, 5, 1, 1);
  pltcut2("p_pid",  c2, 6, 2);
  c2->SaveAs("plotpair2.png");   
}

void readfile(int file=0){
    switch(file){
    case 0: mTFile = new TFile("hist/fmsps.root","old"); break;
    case 1: mTFile = new TFile("sim/test_electron.fmsfps.root"); break;
    case 2: mTFile = new TFile("sim/test_pion.fmsfps.root"); break;
    case 3: mTFile = new TFile("sim/test_gamma.fmsfps.root"); break;
    case 4: mTFile = new TFile("sim/test_pi0.fmsfps.root"); break;
    case 5: mTFile = new TFile("sim/test_mu-.fmsfps.root"); break;
    }									    
    char c[100];
    for(int cut=0; cut<NCUT1; cut++){
	for(int q=0; q<NQ; q++){
	    for(int l=0; l<NL; l++){
		sprintf(c,"FMSFPS_Q%1dL%1d_c%d",q+1,l+1,cut);
		mH2[q][l][cut]=(TH2F*)mTFile->Get(c);
		sprintf(c,"FMS-FPS_Q%1dL%1d_c%d",q+1,l+1,cut);
		mHd[q][l][cut]=(TH1F*)mTFile->Get(c);
		sprintf(c,"FMSFPSd_Q%1dL%1d_c%d",q+1,l+1,cut);
		mHd2[q][l][cut]=(TH2F*)mTFile->Get(c);
	    }
	}
    }
}

void openCanvas(){
  c1 = new TCanvas("FPS","FPS",50,10,700,800);
  c2 = new TCanvas("FPS2","FPS2",750,10,700,800);
  c3 = new TCanvas("FPS3","FPS3",1450,10,700,800);
  gStyle->SetPalette(1);
  gStyle->SetStatW(0.4);
}

void plot(int file=0, int plt=0, int quad=0, int layer=0, int slat=0, int cut=0, int log=0){  
  LOG=log;
  readFpsPosition();
  readfile(file);
  openCanvas();  
  if(plt==1 || plt==0) plotfms();
  if(plt==2 || plt==0) plotfmsPair();
  if(plt==3 || plt==0) plot2d(quad,layer,slat,cut,0,1,0);
  if(plt==4 || plt==0) plot2d(quad,layer,slat,cut,0,1,1);
  if(plt==5 || plt==0){
    for(int l=1; l<=NL; l++){
      plot2d(0,l,0,cut,0,1,0);
      plot2d(0,l,0,cut,0,1,1);
    }
  }
}
