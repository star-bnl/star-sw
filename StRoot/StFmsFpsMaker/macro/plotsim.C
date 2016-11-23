static int LOG=0;
static TCanvas *c1;
static TCanvas *c2;
static TCanvas *c3;
static TFile* mTFile=0;

static const int NP=5;
static const char* CP[NP+1]={"gamma","pi-","electron","mu-","pi0","data"};
static const char* CMG[2]={"nomerge","merge"};
static char* DIR;
static char* DIRDATA;
static int MERGE;

void plot(char* name, int cut=0, char* opt=""){
  c1->Clear();
  c1->Divide(2,2);
  for(int p=0; p<NP-1; p++){
      readfile(p);
      TVirtualPad *pad = c1->cd(p+1);
      if(LOG==1) pad->SetLogy();
      if(LOG==2) pad->SetLogz();
      char cc[100], tit[100];
      sprintf(cc,"%s_c%d",name,cut);
      sprintf(tit,"%s %s cut=%d",CP[p],name,cut); 
      TH1* h=(TH1*)mTFile->Get(cc); 
      h->SetTitle(tit);
      h->Draw(opt);
  }
  c1->Update();
  char file[100];
  sprintf(file,"plotsim/%s_cut%d.png",name,cut);
  c1->SaveAs(file);
}

void plotcut(char* name1, char* name2, int p, int cut0=0, int cut1=1, int cut2=6, char* opt=""){
  c1->Clear();
  c2->Divide(3,2);
  readfile(p);
  int n=1;
  for(int v=0; v<2; v++){
      char* name=name1;
      if(v==1) name=name2;
      for(int c=0; c<3; c++){
	  int cut=cut0;
	  if(c==1) cut=cut1;
	  if(c==2) cut=cut2;
	  TVirtualPad *pad = c2->cd(n);
	  if(LOG==1) pad->SetLogy();
	  if(LOG==2) pad->SetLogz();
	  char cc[100], tit[100];
	  sprintf(cc,"%s_c%d",name,cut);
	  sprintf(tit,"%s %s cut=%d",CP[p],name,cut); 
	  TH1* h=(TH1*)mTFile->Get(cc); 
	  h->SetTitle(tit);
	  h->Draw(opt);
	  n++;
      } 
  }
  c2->Update();
  char file[100];
  sprintf(file,"plotsim/fvc_%s_%s.png",name1,name2,CP[p]);
  c2->SaveAs(file);
}


void readfile(int particle=0, int merge=0){
    char file[100];
    if(particle==NP){
	sprintf(file,"%s_%s/fmsfps.root",DIRDATA,CMG[merge]);
    }else{
	sprintf(file,"%s_%s/test_%s.fmsfps.root",DIR,CMG[merge],CP[particle]);
    }
    printf("Reading %s\n",file);
    mTFile = new TFile(file);    
}

void openCanvas(){
  c1 = new TCanvas("FPS","FPS",50,20,700,800);
  c2 = new TCanvas("FPS2","FPS2",750,20,1100,800);
  c3 = new TCanvas("FPS3","FPS3",1450,20,700,800);
  gStyle->SetPalette(1);
  gStyle->SetStatW(0.4);
}

void drawfms(int cut){
    readfile(cut,MERGE);

    c1->cd(0);
    TH1* h=(TH1*)mTFile->Get("p_xy_c2");
    h->Draw("colz");    
    gROOT->Macro("draw_survey_sim.C(1)");
    c1->SaveAs("plotsim/fvc_nocut.png");

    c3->cd(0);
    TH1* h=(TH1*)mTFile->Get("p_xy_c3");
    h->Draw("colz");    
    gROOT->Macro("draw_survey_sim.C(1)");
    c3->SaveAs("plotsim/fvc_fvcut.png");
}

void drawhit(int p=0){
    readfile(p);
    c2->Divide(2,1);
    c2->cd(1);
    TH2* h=(TH2*)mTFile->Get("FmsHitLarge");
    h->Draw("colz");
    c2->cd(2);
    TH2* h=(TH2*)mTFile->Get("FmsHitSmall");
    h->Draw("colz");    
    c2->SaveAs("plotsim/fmshit.png");
}

void comp(char* name, float xmax=0.0, int tit=0, float scale=0.0, int skipdata=0){    
    //const int N=4;
    //int p[N]={1,0,2,4};
    //int c[N]={4,2,6,8};
    const int N=6;
    int p[N]={5,1,0,2,4,3};
    int c[N]={1,4,2,6,8,5};
    int n[N];
    TH1F* h[N];
    for(int i=0; i<N; i++){
	readfile(p[i],MERGE);
	h[i]=(TH1F*)mTFile->Get(name);
	n[i]=h[i]->GetEntries();
	//printf("i=%d nData=%d\n",i,n[i]);
	if(i==0 && skipdata==0) {
	    float max=h[i]->GetMaximum();
	    h[i]->SetMaximum(max*1.3);
	    h[i]->SetLineWidth(2); 
	    h[i]->SetLineColor(c[i]);
	    if(p[i]==5) h[i]->SetFillColor(kCyan);
	    if(xmax!=0.0) h[i]->GetXaxis()->SetRangeUser(0.0,xmax);
	    h[i]->Draw();
	}
    }
    for(int i=1; i<N; i++){
	float factor=1.0;
	//TString ss(name); if(ss.Contains("L3")) factor*=0.5;
	if(skipdata==1){
	    if(i==1) {
		float max=h[i]->GetMaximum();
		h[i]->SetMaximum(max*scale);
		printf("scale=%f\n",scale);
	    }
	}else{
	    if(scale>0.0) {
		factor=scale;
	    }else{
		factor=float(n[0])/float(n[i]);
	    }
	    h[i]->Scale(factor);
	}
	h[i]->SetLineColor(c[i]); 
	h[i]->SetLineWidth(2); 
	if(skipdata==1 && i==1) {h[i]->Draw();}
	else {h[i]->Draw("same");}
    }
    if(tit==1){ 
	for(int i=0; i<N; i++){
	    TText* c1=new TText(0.6,0.8-i*0.07,CP[p[i]]); c1->SetNDC(); c1->SetTextColor(c[i]); c1->SetTextSize(0.08); c1->Draw();
	}
    }
}

void compfps(int p1=2){
    c1->Divide(1,3);
    c1->cd(1); comp("FpsMipL1",1);
    c1->cd(2); comp("FpsMipL2");
    c1->cd(3)->SetLogy(); comp("FpsMipL3");
    c1->Update();
    c1->SaveAs("plotsim/compfps.png");
}

void compclu(){
    c1->Divide(2,3);
    c1->cd(1)->SetLogy(); comp("SigMaxL",2.0,1);
    c1->cd(2)->SetLogy(); comp("SigMaxS",2.0);
    c1->cd(3)->SetLogy(); comp("SigMinL",1.2);
    c1->cd(4)->SetLogy(); comp("SigMinS",1.2);
    c1->cd(5)->SetLogy(); comp("Chi2L",500.0);
    c1->cd(6)->SetLogy(); comp("Chi2S",500.0);
    //c1->cd(5); comp("Chi2L",500.0);
    //c1->cd(6); comp("Chi2S",500.0);
    c1->Update();
    c1->SaveAs("plotsim/compclu.png");
}

void compmass(int cut){
    c1->Divide(1,2);
    char cc[100];
    sprintf(cc,"p_m1_c%d",cut);
    c1->cd(1); comp(cc,0.0,1,7.0);
    sprintf(cc,"p_eta_c%d",cut);
    c1->cd(2); comp(cc,0.0,0,50.0);
    c1->Update();
    c1->SaveAs("plotsim/compmass.png");
}

void compmip(){
    c1->Divide(1,2);
    char cc[100];
    sprintf(cc,"e_c%d",1);
    //c1->cd(1)->SetLogy(); comp(cc,0.0,1,50.0);
    c1->cd(1); comp(cc,0.0,1,70.0);
    sprintf(cc,"elo_c%d",0);
    c1->cd(2); comp(cc,0.0,0,70.0);
    c1->Update();
    c1->SaveAs("plotsim/compmip.png");
}


void pairxy(int cut){    
    c2->Divide(2,3);
    for(int p=0; p<NP+1; p++){	
	readfile(p,MERGE);
	if(LOG==0) c2->cd(p+1);
	if(LOG==1) c2->cd(p+1)->SetLogz();
	char cc[100];
	sprintf(cc,"p_xy_c%d",cut);
	TH2F* h=(TH2F*)mTFile->Get(cc); 
	h->Draw("colz");	
	TText* t1= new TText(50,80,CP[p]); t1->SetTextSize(0.1); t1->Draw();
    }
    char file[100];
    sprintf(file,"plotsim/pairxy.%s.c%d.png",DIR,cut);
    c2->SaveAs(file);
}

void drawfps(int p=2){
    readfile(p);
    c1->Divide(1,3);
    c1->cd(1); TH1* h=(TH1*)mTFile->Get("FpsMipL1"); h->Draw("");
    c1->cd(2); TH1* h=(TH1*)mTFile->Get("FpsMipL2"); h->Draw("");    
    c1->cd(3); TH1* h=(TH1*)mTFile->Get("FpsMipL3"); h->Draw("");    
    c1->Update();
    c1->SaveAs("plotsim/fpsmip.png");
}

void drawntow(int p=0){
    readfile(p);
    c1->Divide(2,2);
    c1->cd(1)->SetLogy(); NTowL->Draw();    
    int i1=NTowL->Integral(0,51);
    int i2=NTowL->Integral(25,51);
    printf("all=%d above25=%d fraction=%f\n",i1,i2,float(i2)/float(i1));
    TH1F* h=new TH1F; NTowL->Copy(*h);
    h->SetFillColor(2); h->GetXaxis()->SetRangeUser(25,51); h->Draw("same");
    char c[100]; sprintf(c,"%f",float(i2)/float(i1));
    TText* t1=new TText(0.6,0.4,c);
    t1->SetTextColor(2); t1->SetNDC(); t1->Draw();

    c1->cd(2)->SetLogy(); NTowS->Draw();

    c1->cd(3)->SetLogz(); NTowEL->Draw("colz");
    int binx0 = NTowEL->GetXaxis()->FindBin(0); 
    int binx1 = NTowEL->GetXaxis()->FindBin(25); 
    int binx2 = NTowEL->GetXaxis()->FindBin(51); 
    int biny1 = NTowEL->GetYaxis()->FindBin(3.0); 
    int biny2 = NTowEL->GetYaxis()->FindBin(101.0); 
    int i3=NTowEL->Integral(binx0,binx2,biny1,biny2);
    int i4=NTowEL->Integral(binx1,binx2,biny1,biny2);
    printf("all=%d above25&3GeV=%d fraction=%f\n",i3,i4,float(i4)/float(i3));
    sprintf(c,"Above 3GeV \n %f",float(i4)/float(i3));
    TText* t2=new TText(0.3,0.8,c);
    t2->SetTextColor(1); t2->SetNDC(); t2->Draw();
    TH2F* h2=new TH2F; NTowEL->Copy(*h2);
    h2->GetXaxis()->SetRangeUser(binx1,binx2);
    h2->GetYaxis()->SetRangeUser(biny1,biny2);
    h2->Draw("same");

    c1->cd(4)->SetLogz(); NTowES->Draw("colz");
    char cc[100];
    sprintf(cc,"plotsim/ntow_%s.png",CP[p]);
    c1->SaveAs(cc);
}
    
void plotsim(int plt=1, int merge=0, int cut=1, int log=0, char* dir="sim", char* datadir="hist"){  
    printf("plt=%d merge=%d cut=%d log=%d dir=%s datadir=%s\n",
	   plt,merge,cut,log,dir,datadir);
    MERGE=merge;
    DIR=dir;
    DIRDATA=datadir;
    LOG=log;
    gStyle->SetOptStat(0);
    openCanvas();  
    if(plt==1 || plt==0) plot("ept",cut);
    if(plt==2 || plt==0) plot("pid",cut);
    if(plt==3 || plt==0) plot("pid2",cut);
    if(plt==4 || plt==0) plot("x",cut);
    if(plt==5 || plt==0) plot("pt",cut);
    if(plt==6 || plt==0) plot("p_dgg",cut);
    if(plt==7 || plt==0) drawntow(cut);
    if(plt==10 || plt==0) plotcut("xy","pt",0);
    if(plt==11 || plt==0) plotcut("p_dgg","pt",0);
    if(plt==12 || plt==0) drawfms(cut);
    if(plt==20 || plt==0) drawhit(cut);
    if(plt==21 || plt==0) drawfps(cut);
    if(plt==22 || plt==0) compfps(cut);
    if(plt==23 || plt==0) compclu();
    if(plt==24 || plt==0) compmass(cut);
    if(plt==25 || plt==0) pairxy(cut);
    if(plt==26 || plt==0) compmip();
}
