static const int kNPtBin=6;
static const int NCUT=18;
static const char* CCUT[NCUT+1]={"Before Mass Cut","Inclusive","Exclusive",
				 "BBCE<3000","3000<BBCE<20000","20000<BBCE<30000",
				 "Abort Gap","Jet Trigger","Double Triggers",
				 "On/Off Mass","Off/On Mass","Off/Off Mass",
				 "SmallBS","LargeBS","Top2",
				 "30000<BBCE<40000","40000<BBCE","NoBBCTOF",
				 "RealPi0"};
static const char CBEAM[20];

static const double PI=TMath::Pi();
static const double twoPI=PI*2.0;

const float MassCut0=0.07;
const float MassCut1=0.2;
const float MassCut2=0.35;
static Double_t ZggCut=0.7;
static int OPT=4;
static int RUN=1;
static int RUNpp=16077027;
static int RUNpAu=16125053;
static TCanvas *c1;
static TCanvas *c2;
static TCanvas *c3;
static TString* FILENAME;
TFile* mTFile;

float PAR[kNPtBin][kNPtBin][5];

double wrapAround(double phi){
    double res=phi;
    while(res>=1.5*PI) {res-=twoPI;}
    while(res<-0.5*PI) {res+=twoPI;}
    return res;
}

void mixing(TH1F* h1, TH1F* h2, TH1F* d, TH1F* mix, int sub=1){
    int t1=h1->GetEntries();
    int t2=h1->GetEntries();
    int n1=h1->GetNbinsX();
    int n2=h2->GetNbinsX();
    for(int i=1; i<=n1; i++){
	double p1=h1->GetBinCenter(i);
	double c1=h1->GetBinContent(i);
	for(int j=1; j<=n2; j++){
	    double p2=h2->GetBinCenter(j);
	    double c2=h2->GetBinContent(j);
	    double dp=wrapAround(p1-p2+PI/n1/100.0);
	    mix->Fill(dp,c1*c2);
	}
    }
    if(sub==1) mix->Add(d,-1.0);
    mix->Scale(n1/mix->Integral());
}

Double_t twoGaus(Double_t *x, Double_t *par){
    Double_t OP=1.0/sqrt(2.0*PI);
    Double_t x1 = x[0];
    while(x1<-PI) x1+=2.0*PI;
    while(x1>=PI) x1-=2.0*PI;
    Double_t x2 = x[0]-PI;
    while(x2<-PI) x2+=2.0*PI;
    while(x2>=PI) x2-=2.0*PI;
    Double_t f = par[0]/2.0/PI;
    f += par[1]*OP/par[2]*exp(-x1*x1/2.0/par[2]/par[2]);
    f += par[3]*OP/par[4]*exp(-x2*x2/2.0/par[4]/par[4]);
    return f;
}

Double_t oneGaus(Double_t *x, Double_t *par){
    Double_t OP=1.0/sqrt(2.0*PI);
    Double_t x1 = x[0]+PI;
    while(x1<-PI) x1+=2.0*PI;
    while(x1>=PI) x1-=2.0*PI;
    Double_t f = par[0]/2.0/PI;
    f += par[1]*OP/par[2]*exp(-x1*x1/2.0/par[2]/par[2]);
    return f;
}

void readfile(int opt=-1, int run=-1){
    char file[100];
    if(opt<0) opt=OPT;
    if(run<0)   {sprintf(file,"FPS/hist_opt%d/st_fms_%d_raw_merged.dipi0.root",opt,RUN); sprintf(CBEAM,"%d",RUN);}
    if(run>100) {sprintf(file,"FPS/hist_opt%d/st_fms_%d_raw_merged.dipi0.root",opt,run); sprintf(CBEAM,"%d",run);}
    if(run==1)  {sprintf(file,"FPS/hist_opt%d/st_fms_%d_raw_merged.dipi0.root",opt,RUNpp);sprintf(CBEAM,"%d",RUNpp);}
    if(run==2)  {sprintf(file,"FPS/hist_opt%d/st_fms_%d_raw_merged.dipi0.root",opt,RUNpAu);sprintf(CBEAM,"%d",RUNpAu);}
    if(run==11) {sprintf(file,"FPS/hist/pptrans/dipi0.root");  sprintf(CBEAM,"pp");}
    if(run==15) {sprintf(file,"FPS/hist/pAu1/dipi0.root");     sprintf(CBEAM,"pAu1");}
    if(run==12) {sprintf(file,"FPS/hist/pAu2/dipi0.root");     sprintf(CBEAM,"pAu2");}
    if(run==13) {sprintf(file,"FPS/hist/pAl/dipi0.root");      sprintf(CBEAM,"pAl");}
    if(run==20) {sprintf(file,"FPS/hist_pythia6/dipi0.all.root"); sprintf(CBEAM,"pythia");}
    
    printf("Reading %s\n",file);
    mTFile = new TFile(file);
}

void openCanvas(){
  c1 = new TCanvas("DiPi0_1","DiPi0_1",0,20,600,700);
  c2 = new TCanvas("DiPi0_2","DiPi0_2",550,20,600,700);
  c3 = new TCanvas("DiPi0_3","DiPi0_3",1100,20,600,700);
  gStyle->SetPalette(1);
  gStyle->SetStatW(0.4);
}

void plot1d(TCanvas* c, char* name){
    TH1F *h;
    c->Divide(1,3);
    c->cd(1)->SetLogy();
    readfile(OPT,11);
    h = (TH1F*)mTFile->Get(Form("%s",name));   h->Draw(); h->SetLineWidth(2); h->SetMinimum(1.0);
    h = (TH1F*)mTFile->Get(Form("%sAG",name)); h->Draw("same"); h->SetLineColor(2);h->SetLineWidth(2);
    t=new TText(0.2, 0.25,"pp collisions"); t->SetNDC(); t->Draw();
    t=new TText(0.2, 0.20,"pp abort-gap");  t->SetNDC(); t->Draw(); t->SetTextColor(2);
    c->cd(2)->SetLogy();
    readfile(OPT,13);
    h = (TH1F*)mTFile->Get(Form("%s",name));   h->Draw(); h->SetLineWidth(2); h->SetMinimum(1.0);
    h = (TH1F*)mTFile->Get(Form("%sAG",name)); h->Draw("same"); h->SetLineColor(2);h->SetLineWidth(2);
    t=new TText(0.2, 0.25,"pAl collisions"); t->SetNDC(); t->Draw();
    t=new TText(0.2, 0.20,"pAl abort-gap");  t->SetNDC(); t->Draw(); t->SetTextColor(2);
    c->cd(3)->SetLogy();
    readfile(OPT,12);
    h = (TH1F*)mTFile->Get(Form("%s",name));  h->Draw(); h->SetLineWidth(2);h->SetMinimum(1.0);
    h = (TH1F*)mTFile->Get(Form("%sAG",name)); h->Draw("same"); h->SetLineColor(2); h->SetLineWidth(2);
    readfile(OPT,15);
    h = (TH1F*)mTFile->Get(Form("%s",name));   h->Draw("same"); h->SetLineColor(4); h->SetLineWidth(2);
    h = (TH1F*)mTFile->Get(Form("%sAG",name)); h->Draw("same"); h->SetLineColor(6); h->SetLineWidth(2);
    t=new TText(0.2,  0.25,"pAu2 collisions"); t->SetNDC(); t->Draw();
    t=new TText(0.2,  0.20,"pAu2 abort-gap");  t->SetNDC(); t->Draw(); t->SetTextColor(2);
    t=new TText(0.35, 0.25,"pAu1 collisions"); t->SetNDC(); t->Draw(); t->SetTextColor(4);
    t=new TText(0.35, 0.20,"pAu1 abort-gap");  t->SetNDC(); t->Draw(); t->SetTextColor(6);
    c->SaveAs(Form("plot/%s.png",name));
}

void plot2d(TCanvas* c, char* name){
    TH2F *h2;
    c->Divide(2,2);
    c->cd(1)->SetLogz();
    readfile(OPT,11);
    h2 = (TH2F*)mTFile->Get(Form("%s",name));
    h2->Draw("colz");
    t=new TText(0.75, 0.15,"pp"); t->SetNDC(); t->Draw();
    c->cd(2)->SetLogz();
    readfile(OPT,13);
    h2 = (TH2F*)mTFile->Get(Form("%s",name));
    h2->Draw("colz");
    t=new TText(0.75, 0.15,"pAl"); t->SetNDC(); t->Draw();
    c->cd(3)->SetLogz();
    readfile(OPT,15);
    h2 = (TH2F*)mTFile->Get(Form("%s",name));
    h2->Draw("colz");
    t=new TText(0.75, 0.15,"pAu1"); t->SetNDC(); t->Draw();
    c->cd(4)->SetLogz();
    readfile(OPT,12);
    h2 = (TH2F*)mTFile->Get(Form("%s",name));
    h2->Draw("colz");
    t=new TText(0.75, 0.15,"pAu2"); t->SetNDC(); t->Draw();
    c->SaveAs(Form("plot/%s.png",name));    
}

void bbc(int plt=0){
    gStyle->SetOptStat(0);
    if(plt==0 || plt==1){
	plot1d(c1,"BBCE");
	plot1d(c2,"BBCMult");
	plot1d(c3,"TOF");
    }
    if(plt==0 || plt==2){
	plot2d(c1,"BBC_BBCMult");
	plot2d(c2,"BBC_TOF");
	plot2d(c3,"BBCMult_TOF");	
    }
    if(plt==0 || plt==3){
	plot2d(c1,"TOF_TOF");
    }
}

TH1F* mix(int run=21, int cut=1, int bin1=3, int bin2=1, int plot=1, int method=1, TH1F* mix0=0, TH1F* mix1=0){
    readfile(OPT,run);
    TH1F* h1= (TH1F*)mTFile->Get(Form("phi1_%1d%1d_c%d",bin1,bin2,cut));
    TH1F* h2= (TH1F*)mTFile->Get(Form("phi2_%1d%1d_c%d",bin1,bin2,cut));
    TH1F* h3= (TH1F*)mTFile->Get(Form("dphi_%1d%1d_c%d",bin1,bin2,cut));
    TH1F* h4= (TH1F*)h3->Clone(Form("mix_%1d%1d_c%d",bin1,bin2,cut));
    TH1F* h5= (TH1F*)h3->Clone(Form("dphi_corr_%1d%1d_c%d",bin1,bin2,cut));
    h4->Reset();
    h5->Reset();
    mixing(h1,h2,h3,h4,1);
    h5->Divide(h3,h4);
    mix0=h4;

    TH1F* h11= (TH1F*)mTFile->Get(Form("phi0_%1d_c%d",bin1,cut));
    TH1F* h12= (TH1F*)mTFile->Get(Form("phi0_%1d_c%d",bin2,cut));
    TH1F* h13= (TH1F*)mTFile->Get(Form("dphi_%1d%1d_c%d",bin1,bin2,cut));
    TH1F* h14= (TH1F*)h3->Clone(Form("mix2_%1d%1d_c%d",bin1,bin2,cut));
    TH1F* h15= (TH1F*)h3->Clone(Form("dphi_corr2_%1d%1d_c%d",bin1,bin2,cut));
    h14->Reset();
    h15->Reset();
    mixing(h11,h12,h13,h14,0);
    h15->Divide(h13,h14);
    mix1=h14; 

    if(plot==1){
	TText* t;
	gStyle->SetOptStat(0);
	c1->Divide(2,2);
	c1->cd(1); h1->SetMinimum(0); h1->SetLineWidth(2); h1->Draw();
	h11->Scale(h1->GetEntries()/h11->GetEntries()); h11->SetLineWidth(2); h11->SetLineColor(6); h11->Draw("same");
	t = new TText(0.2, 0.85,Form("%s %s",CBEAM,CCUT[cut])); t->SetNDC(); t->SetTextSize(0.06); t->SetTextColor(1); t->Draw();
	c1->cd(2); h2->SetMinimum(0); h2->SetLineWidth(2); h2->Draw();
	if(h11!=h12)h12->Scale(h2->GetEntries()/h12->GetEntries()); 
	h12->SetLineWidth(2); h12->SetLineColor(6); h12->Draw("same");
	float m4=h4->GetMaximum();
	c1->cd(4); h4->SetMinimum(0); h4->SetLineWidth(2); h4->SetMaximum(m4*1.1); h4->Draw();
	h14->SetLineWidth(2); h4->SetLineColor(6); h14->Draw("same");
	c1->cd(3); 
	h3->Rebin(2); h5->Rebin(2); h15->Rebin(2);
	float m3=h3->GetBinContent(h3->GetNbinsX()*3/4);
	h3->SetMinimum(0); h3->SetLineWidth(2); h3->SetMaximum(m3*1.5); h3->SetLineColor(1); h3->Draw();
	h5->SetLineWidth(2);  h5->SetLineColor(2);  h5->Draw("same");
	h15->SetLineWidth(2); h15->SetLineColor(6); h15->Draw("same");
	t= new TText(0.20, 0.25,"UnCorrected"); t->SetTextSize(0.05); t->SetTextColor(1); t->SetNDC(); t->Draw();
	t= new TText(0.20, 0.20,"Corrected");   t->SetTextSize(0.05); t->SetTextColor(2); t->SetNDC(); t->Draw();
	t= new TText(0.20, 0.15,"Corrected2");  t->SetTextSize(0.05); t->SetTextColor(6); t->SetNDC(); t->Draw();
	c1->SaveAs(Form("plot/mix_%s_%1d%1d_c%d.png",CBEAM,bin1,bin2,cut));
    }

    if(method==0) return h5;
    else return h15;
}

void plot(char* v, int run, int cut, TCanvas* cvs, int fit=0, int bin1=-1, int bin2=-1, float ymax=0.0, int rebin=0){   
    int bin0=1;
    char c[100];
    char c2[100];
    readfile(OPT,run);
    gStyle->SetOptStat(0);
    gStyle->SetOptFit(0);
    gStyle->SetOptTitle(0);
    cvs->Clear();
    int b1=bin1, b2=bin1+1, b3=bin2, b4=b3;
    if(bin1<0) {cvs->Divide(kNPtBin-bin0,kNPtBin-bin0); b1=bin0; b2=kNPtBin; b3=bin0;}
    float ptcut[kNPtBin+1]={0.5,1.0,1.5,2.0,2.5,3.0,10.0};
    memset(PAR,0,sizeof(PAR));
    TText* t;
    for(int i=b1; i<b2; i++){
	if(bin1<0) b4=i;
	for(int j=b3; j<=b4; j++){
	    int cc=cut;
	    if(cut==99 && (v=="m1" || v=="m2")) cc=0;
	    if(cut==99 && v=="dphi") cc=1;
	    TH1F *h,*h2,*h3,*h4;
	    TH2F *h2d;

	    //get normalizations
	    float norm0=1.0;
	    float norm1=1.0;
	    float norm2=1.0;
	    float norm3=1.0;
	    if(v=="m0" || v=="phi0"){}
	    else{
		sprintf(c,"m0_%1d_c%d",i,cc);
		TH1F* h0 = (TH1F*)mTFile->Get(c);
		norm0 = h0->GetEntries();
		sprintf(c,"m2_%1d%1d_c%d",i,j,cc);
		TH1F* h1 = (TH1F*)mTFile->Get(c);
		norm1 = h1->GetEntries();
		printf("n0=%d n1=%d\n",norm0,norm1);
		norm2=norm1/norm0;
		norm3=norm2/ZggCut;
	    }

	    int opt=1, log=0;
	    if(v=="mix"){
		h3=mix(run,cc,i,j,0,1,h,h2);
		h1->SetLineColor(1);
		h2->SetLineColor(6);
	    }else if(v=="corr"){
		h = mix(run,cc,i,j,0,1);
		if(rebin>0) h->Rebin(rebin);
		if(fit==1) h->GetXaxis()->SetRangeUser(PI/2.0,3.0*PI/2.0);
		int nbin = h->GetNbinsX();
                h->Sumw2();
                if(run<20) {h->Scale(1.0/norm0*nbin/2.0/PI/ZggCut);}
                else       {h->Scale(1.0/norm0*nbin/2.0/PI);}
	    }else if(v=="norm"){
		sprintf(c,"dphi_%1d%1d_c%d",i,j,cc);
                h = (TH1F*)mTFile->Get(c);
		if(rebin>0) h->Rebin(rebin);
		if(fit==1) h->GetXaxis()->SetRangeUser(PI/2.0,3.0*PI/2.0);
		int nbin = h->GetNbinsX();
		h->Sumw2();
		if(run<20) {h->Scale(1.0/norm0*nbin/2.0/PI/ZggCut);}
		else       {h->Scale(1.0/norm0*nbin/2.0/PI);}
	    }else if(v=="mall"){
		sprintf(c,"m0_%1d_c%d",i,0);
		sprintf(c2,"m0_%1d%1d_c%d",i,j,0);
		h4 = (TH1F*)((TH1F*)mTFile->Get(c))->Clone(c2);
		float norm4 = h4->Integral(h4->GetXaxis()->FindBin(MassCut0),h4->GetXaxis()->FindBin(MassCut1));
		h4->Scale(norm1/norm4);
		h = (TH1F*)((TH1F*)mTFile->Get(c))->Clone(c2);
		sprintf(c,"m0_%1d_c%d",i,cc);
		sprintf(c2,"m0_%1d%1d_c%d",i,j,cc);
		h = (TH1F*)((TH1F*)mTFile->Get(c))->Clone(c2); h->SetLineColor(6);
		h->Scale(norm2); 
		sprintf(c,"m1_%1d%1d_c%d",i,j,cc);
                h2 = (TH1F*)mTFile->Get(c); h2->SetLineColor(2);
		sprintf(c,"m2_%1d%1d_c%d",i,j,cc); 
                h3 = (TH1F*)mTFile->Get(c); h3->SetLineColor(4);	   
	    }else if(v=="z12"){
		sprintf(c,"z1_%1d%1d_c%d",i,j,cc);
                h = (TH1F*)mTFile->Get(c); h->SetLineColor(2);
		sprintf(c,"z2_%1d%1d_c%d",i,j,cc); 
                h2 = (TH1F*)mTFile->Get(c); h2->SetLineColor(4);
	    }else if(v=="m0" || v=="phi0"){
		sprintf(c,"%s_%1d_c%d",v,i,cc);
		printf("%s\n",c);
		h = (TH1F*)mTFile->Get(c);		
	    }else if(v=="bbce"){
		opt=1;
		log=1;
		sprintf(c,"%s_%1d%1d_c%d",v,i,j,cc);
		printf("%s\n",c);
		h = (TH1F*)mTFile->Get(c);		
		h->SetMinimum(0.1);
	    }else if(v=="phi1" || v=="phi2"){
		opt=1;
		sprintf(c,"%s_%1d%1d_c%d",v,i,j,cc);
		printf("%s\n",c);
		h = (TH1F*)mTFile->Get(c);		
	    }else if(v=="phi1dphi"){
		opt=2;
		sprintf(c,"%s_%1d%1d_c%d",v,i,j,cc);
		printf("%s\n",c);
		h = (TH1F*)mTFile->Get(c);		
	    }else{
		sprintf(c,"%s_%1d%1d_c%d",v,i,j,cc);
		printf("%s\n",c);
		h = (TH1F*)mTFile->Get(c);		
	    }
	    if(bin1<0){
		TVirtualPad *pad = cvs->cd((i-bin0)*(kNPtBin-bin0)+(j-bin0)+1);
		pad->SetRightMargin(0.01);
		pad->SetLeftMargin(0);
		pad->SetTopMargin(0);
		pad->SetBottomMargin(0.01);
		pad->Draw();		
	    }
	    h->SetMinimum(0.0);
	    if(ymax>0.0) h->SetMaximum(ymax);
	    if(opt==0) h->Draw("e");
	    if(opt==1) h->Draw();
	    if(opt==2) h->Draw("colz");
	    if(v=="mall"){
		h4->Draw("same");
		h2->Draw("same");
		h3->Draw("same");
	    }
	    if(v=="z12" || v=="mix") h2->Draw("same");
	    TF1* f;
	    if((v=="norm" || v=="corr") && fit==1){
		f=new TF1("oneGaus",oneGaus,+PI/2.0,+PI*3.0/2.0,3);
		f->SetParameters(norm3/10.0,norm3/2.0,0.3);
		f->SetParNames("Const","IntFar","SigFar");
		f->SetParLimits(0,0.0,1.0);
		f->SetParLimits(1,0.0,1.0);
		f->SetParLimits(2,0.0,3.0);
		f->SetLineColor(2); f->SetLineWidth(1);
		h->Fit("oneGaus","Q");
		PAR[i][j][0]=f->GetParameter(0);
		PAR[i][j][1]=f->GetParameter(1);
		PAR[i][j][2]=f->GetParameter(2);
		printf("norm=%8.6f Int=%8.6f IntFar=%8.6 SigFar=%6.3f C=%8.6f\n",
		       norm3,PAR[i][j][1]+PAR[i][j][0],PAR[i][j][1],PAR[i][j][2],PAR[i][j][0]);
	    }
	    if((v=="norm" || v="corr") && fit==2){
		f=new TF1("twoGaus",twoGaus,-PI/2.0,+PI*3.0/2.0,5);
		f->SetParameters(norm3/10.0,norm3/2.0,0.3,norm3/2.0,0.3);
		f->SetParNames("Const","IntNear","SigNear","IntFar","SigFar");
		f->SetParLimits(0,0.0,1.0);
		f->SetParLimits(1,0.0,1.0);
		f->SetParLimits(2,0.0,3.0);
		f->SetParLimits(3,0.0,1.0);
		f->SetParLimits(4,0.0,3.0);
		f->SetLineColor(2); f->SetLineWidth(1);
		h->Fit("twoGaus","Q");
		PAR[i][j][0]=f->GetParameter(0);
		PAR[i][j][1]=f->GetParameter(1);
		PAR[i][j][2]=f->GetParameter(2);
		PAR[i][j][3]=f->GetParameter(3);
		PAR[i][j][4]=f->GetParameter(4);
		printf("norm=%8.6f Int=%8.6f IntNear=%8.6f IntFar=%8.6f SigNear=%6.3f  SigFar=%6.3f C=%8.6f\n",
		       norm3,PAR[i][j][1]+PAR[i][j][3]+PAR[i][j][0],PAR[i][j][1],PAR[i][j][3],
		       PAR[i][j][2],PAR[i][j][4],PAR[i][j][0]);
	    }

	    float xx=0.60, yy=0.85, dy=0.04, size=0.04;
	    if(bin1<0){xx=0.45, yy=0.90, dy=0.08, size=0.08;}
	    if(v=="z12") {xx=0.1; yy=0.35;}
	    t = new TText(xx, yy,    Form("pT1=%3.1f-%3.1f",ptcut[i],ptcut[i+1])); t->SetNDC(); t->SetTextSize(size); t->Draw();
	    t = new TText(xx, yy-dy, Form("pT2=%3.1f-%3.1f",ptcut[j],ptcut[j+1])); t->SetNDC(); t->SetTextSize(size); t->Draw();
	    if(norm3>0.0){
		t = new TText(xx, yy-dy*2, Form("P=%7.5f",norm3)); t->SetNDC(); t->SetTextSize(size); t->Draw();
	    }
	    if((v=="norm" || v="corr")  && fit==1){
		t = new TText(xx, yy-dy*3, Form("PBg=%7.5f",  PAR[i][j][0])); t->SetNDC(); t->SetTextSize(size); t->Draw();
		t = new TText(xx, yy-dy*4, Form("Paway=%7.5f",PAR[i][j][1])); t->SetNDC(); t->SetTextSize(size); t->Draw();
		t = new TText(xx, yy-dy*5, Form("Saway=%4.2f",PAR[i][j][2])); t->SetNDC(); t->SetTextSize(size); t->Draw();
	    }
	    if((v=="norm" || v="corr")  && fit==2){
		t = new TText(xx, yy-dy*3, Form("Pnear=%7.5f",PAR[i][j][1])); t->SetNDC(); t->SetTextSize(size); t->Draw();
		t = new TText(xx, yy-dy*4, Form("Paway=%7.5f",PAR[i][j][3])); t->SetNDC(); t->SetTextSize(size); t->Draw();
		t = new TText(xx, yy-dy*5, Form("PBg=%7.5f",  PAR[i][j][0])); t->SetNDC(); t->SetTextSize(size); t->Draw();
		t = new TText(xx, yy-dy*6, Form("Snear=%4.2f",PAR[i][j][2])); t->SetNDC(); t->SetTextSize(size); t->Draw();
		t = new TText(xx, yy-dy*7, Form("Saway=%4.2f",PAR[i][j][4])); t->SetNDC(); t->SetTextSize(size); t->Draw();
	    }
	    if(cut==88) h->SetLineColor(2);
	    if(cut==99 && (v=="m1"|| v=="m2")){
		sprintf(c,"%s_%1d%1d_c%d",v,i,j,1);
		h = (TH1F*)mTFile->Get(c);
		h->SetLineColor(4);
		h->Draw("same");
		sprintf(c,"%s_%1d%1d_c%d",v,i,j,2);
		h = (TH1F*)mTFile->Get(c);
		h->SetLineColor(2);
		h->Draw("same");
	    }
	    if(cut==99 && v=="dphi"){
		h->SetLineColor(4);
		sprintf(c,"%s_%1d%1d_c%d",v,i,j,2);
                h = (TH1F*)mTFile->Get(c);
                h->SetLineColor(2);
                h->Draw("same");		
	    }
	    if(bin1>0){
		t = new TText(0.1, 0.92,Form("%s %s",CBEAM,CCUT[cut])); t->SetNDC(); t->SetTextSize(0.07); t->SetTextColor(1); t->Draw();
		if     (run==1 || run==11) {sprintf(c,"plot/dipi0_pp_%s_c%d_bin%d%d.png",v,cut,i,j);}
		else if(run==5 || run==15) {sprintf(c,"plot/dipi0_pau1_%s_c%d_bin%d%d.png",v,cut,i,j);}
		else if(run==2 || run==12) {sprintf(c,"plot/dipi0_pau2_%s_c%d_bin%d%d.png",v,cut,i,j);}
		else if(run==3 || run==13) {sprintf(c,"plot/dipi0_pal_%s_c%d_bin%d%d.png",v,cut,i,j);}
		else                       {sprintf(c,"plot/dipi0_%d_%s_c%d_bin%d%d.png",run,v,cut,i,j);}
		printf("Saving %s\n",c);
		cvs->SaveAs(c);	       
	    }
	}
    }
    if(bin1<0){
	cvs->cd(2);
	t = new TText(0.0, 0.85,Form("%s %s",CBEAM,CCUT[cut])); t->SetNDC(); t->SetTextSize(0.12); t->SetTextColor(1); t->Draw();
    }
    if(v=="mall"){
	t = new TText(0.05, 0.65,"M1(no mass, scaled)"); t->SetNDC(); t->SetTextSize(0.10); t->SetTextColor(1); t->Draw();
	t = new TText(0.05, 0.55,"M1(without M2)/P"); t->SetNDC(); t->SetTextSize(0.10); t->SetTextColor(6); t->Draw();
	t = new TText(0.05, 0.45,"M1(with M2)"); t->SetNDC(); t->SetTextSize(0.10); t->SetTextColor(2); t->Draw();
	t = new TText(0.05, 0.35,"M2"); t->SetNDC(); t->SetTextSize(0.10); t->SetTextColor(4); t->Draw();
    }
    if(v=="z12"){
	t = new TText(0.05, 0.65,"Zgg1"); t->SetNDC(); t->SetTextSize(0.10); t->SetTextColor(2); t->Draw();
	t = new TText(0.05, 0.55,"Zgg2"); t->SetNDC(); t->SetTextSize(0.10); t->SetTextColor(4); t->Draw();
    }
    if(cut==99) {
	t = new TText(0.0, 0.65, "Inclusive Pair");  t->SetNDC(); t->SetTextSize(0.12); t->SetTextColor(4); t->Draw();
	t = new TText(0.0, 0.45, "Exclusive Pair");  t->SetNDC(); t->SetTextSize(0.12); t->SetTextColor(2); t->Draw();
    }
    if(bin1<0){
	if     (run==1 || run==11) {sprintf(c,"plot/dipi0_pp_%s_c%d.png",v,cut);}
	else if(run==5 || run==15) {sprintf(c,"plot/dipi0_pau1_%s_c%d.png",v,cut);}
	else if(run==2 || run==12) {sprintf(c,"plot/dipi0_pau2_%s_c%d.png",v,cut);}
	else if(run==3 || run==13) {sprintf(c,"plot/dipi0_pal_%s_c%d.png",v,cut);}
	else                       {sprintf(c,"plot/dipi0_%d_%s_c%d.png",run,v,cut);}
	printf("Saving %s\n",c);
	cvs->SaveAs(c);
    }
}

void comp(int run, int pt1, int pt2, TCanvas* cvs, int fit=0){   
    int bin0=1;
    char c[100];
    char c2[100];
    readfile(OPT,run);
    gStyle->SetOptStat(0);
    gStyle->SetOptFit(0);
    gStyle->SetOptTitle(0);
    cvs->Clear();
    float ptcut[kNPtBin+1]={0.5,1.0,1.5,2.0,2.5,3.0,10.0};
    float col[NCUT+1]={0,1,4,3,4,38,6,9,8,kGreen+3,kOrange,kPink+9,kOrange+7,kGreen+3,3,2,1,2};
    memset(PAR,0,sizeof(PAR));
    TText* t;    
    int cmax=NCUT;
    if(run==20) cmax++;
    int iy=0;
    for(int cc=1; cc<cmax; cc++){
	TH1F *h,*h2,*h3,*h4;	
	if(run==11 && cc==5) continue;
	if(run==11 && cc>14) continue;
	//if(run==20 && cc!=2 && cc!=9 && cc!=10 && cc!=11 && cc!=cmax-3) continue;
	if(run==20 && cc!=2 && cc!=14  && cc!=cmax-3) continue;
	//get normalizations
	sprintf(c,"m0_%1d_c%d",pt1,cc);
	TH1F* h0 = (TH1F*)mTFile->Get(c);
	float norm0 = h0->GetEntries();
	sprintf(c,"m1_%1d%1d_c%d",pt1,pt2,cc);
	TH1F* h1 = (TH1F*)mTFile->Get(c);
	float norm1 = h1->GetEntries();
	float norm2=norm1/norm0;
	float norm3=norm2/ZggCut;
	printf("cut=%2d n0=%d n1=%d n2=%f\n",cc,norm0,norm1,norm2);
	
	sprintf(c,"dphi_%1d%1d_c%d",pt1,pt2,cc);
	h = (TH1F*)mTFile->Get(c);
	int nbin = h->GetNbinsX();
	if(cc==cmax-3){
	    h->Scale(1.0/norm0*nbin/2.0/PI);
	}else{
	    h->Scale(1.0/norm0*nbin/2.0/PI/ZggCut);
	}
	h->SetLineColor(col[cc]);
	h->SetLineWidth(2);
	h->SetMinimum(0.0);
	if(iy==0) {
	    h->SetMaximum(h->GetMaximum()*1.5);
	    h->Draw("l");
	}else{
	    h->Draw("lsame");
	}	
	if(run==20 && cc==cmax-3) cc=cmax-1;
	t = new TText(0.7, 0.85-iy*0.03,CCUT[cc]); t->SetNDC(); t->SetTextSize(0.02); t->SetTextColor(col[cc]); t->Draw();
	iy++;
    }
    float xx=0.45, yy=0.80, dy=0.03;
    t = new TText(xx, yy,    Form("pT1=%3.1f-%3.1f",ptcut[pt1],ptcut[pt1+1])); t->SetNDC(); t->SetTextSize(0.03); t->Draw();
    t = new TText(xx, yy-dy, Form("pT2=%3.1f-%3.1f",ptcut[pt2],ptcut[pt2+1])); t->SetNDC(); t->SetTextSize(0.03); t->Draw();

    if     (run==1 || run==11) {sprintf(c,"plot/dipi0_pp_compdphi.png");}
    else if(run==2 || run==12) {sprintf(c,"plot/dipi0_pau_compdphi.png");}
    else if(run==3 || run==13) {sprintf(c,"plot/dipi0_pal_compdphi.png");}
    else if(run==20)           {sprintf(c,"plot/dipi0_pythia_compdphi.png");}
    else                       {sprintf(c,"plot/dipi0_%d_compdphi.png",run);}
    printf("Saving %s\n",c);
    cvs->SaveAs(c);
}

void dipi0(int plt=0, int cut=2, int run=1, int opt=4){
    printf("plt=%d cut=%d run=%d opt=%d\n",plt,cut,run,opt);
    OPT=opt;
    RUN=run;
    openCanvas();  
    if(plt==1 || plt==0) bbc(1);
    if(plt==2 || plt==0) bbc(2);
    if(plt==3 || plt==0) bbc(3);
    if(plt==4 || plt==0) {plot("bbce",11,cut,c1); plot("bbce",13,cut,c2); plot("bbce",12,cut,c3);}
    if(plt==5 || plt==0) {plot("m0",11,cut,c1);   plot("m0",13,cut,c2);  plot("m0",12,cut,c3);}
    if(plt==6 || plt==0) {plot("m1",11,cut,c1);   plot("m1",13,cut,c2);  plot("m1",12,cut,c3);}
    if(plt==7 || plt==0) {plot("m2",11,cut,c1);   plot("m2",13,cut,c2);  plot("m2",12,cut,c3);}
    if(plt==8 || plt==0) {plot("z1",11,cut,c1);   plot("z1",13,cut,c2);  plot("z1",12,cut,c3);}
    if(plt==9 || plt==0) {plot("z2",11,cut,c1);   plot("z2",13,cut,c2);  plot("z2",12,cut,c3);}
    if(plt==10|| plt==0) {plot("phi0",11,cut,c1); plot("phi0",13,cut,c2);plot("phi0",12,cut,c3);}
    if(plt==11|| plt==0) {plot("phi1",11,cut,c1); plot("phi1",13,cut,c2);plot("phi1",12,cut,c3);}
    if(plt==12|| plt==0) {plot("phi2",11,cut,c1); plot("phi2",13,cut,c2);plot("phi2",12,cut,c3);}
    if(plt==13|| plt==0) {plot("eta1",11,cut,c1); plot("eta1",13,cut,c2);plot("eta1",12,cut,c3);}
    if(plt==14|| plt==0) {plot("eta2",11,cut,c1); plot("eta2",13,cut,c2);plot("eta2",12,cut,c3);}
    if(plt==15|| plt==0) {plot("dphi",11,cut,c1); plot("dphi",13,cut,c2);plot("dphi",12,cut,c3);}
    if(plt==16|| plt==0) {plot("phi1dphi",11,cut,c1); plot("phi1dphi",13,cut,c2);plot("phi1dphi",12,cut,c3);}
    if(plt==17|| plt==0) {plot("mix",11,cut,c1); plot("mix",13,cut,c2);plot("mix",12,cut,c3);}

    if(plt==20 || plt==0){plot("mall",11,cut,c1);   plot("mall",13,cut,c2); plot("mall",12,cut,c3);}
    if(plt==21 || plt==0){plot("z12",11,cut,c1);    plot("z12",13,cut,c2);  plot("z12",12,cut,c3);}

    if(plt==30|| plt==0){comp(11,4,2,c1);   comp(13,4,2,c2);  comp(12,4,2,c3);}
    if(plt==31|| plt==0){comp(20,2,1,c1);}

    if(plt==40 || plt==0) {plot("norm",11,cut,c1,2); plot("norm",13,cut,c2,2); plot("norm",12,cut,c3,2);}
    if(plt==41 || plt==0) {
	plot("norm",11,cut,c1,1,-1,-1,0.00,4); 
	plot("norm",13,cut,c2,1,-1,-1,0.00,4); 
	plot("norm",12,cut,c3,1,-1,-1,0.00,4);
    }
    if(plt==42 || plt==0) {
	plot("corr",11,cut,c1,1,-1,-1,0.00,4); 
	plot("corr",13,cut,c2,1,-1,-1,0.00,4); 
	plot("corr",12,cut,c3,1,-1,-1,0.00,4);
    }

    if(plt==50) mix(run,cut,3,1);
    if(plt==51) mix(11,12,4,2);
    if(plt==52) mix(12,16,2,0);
    if(plt==53) mix(11,2,3,1);
    if(plt==54) mix(12,16,3,1);
    if(plt==55) mix(13,16,3,1);
    if(plt==56) mix(13,2,2,2);
    

    if(plt==100 || plt==0){
	plot("norm",11,2,c1,1,2,0,0.1);
	plot("norm",12,2,c2,1,2,0,0.1);
	plot("norm",12,3,c2,1,2,0,0.1);
	plot("norm",12,4,c2,1,2,0,0.1);
	plot("norm",12,5,c2,1,2,0,0.1);
	plot("norm",12,15,c2,1,2,0,0.1);
	plot("norm",12,16,c2,1,2,0,0.1);
    }
    if(plt==101 || plt==0){
	plot("norm",11,2,c1,1,3,1,0.012);
	plot("norm",11,3,c1,1,3,1,0.012);
	plot("norm",11,4,c1,1,3,1,0.012);
	plot("norm",11,5,c1,1,3,1,0.012);
	plot("norm",11,15,c1,1,3,1,0.012);
	plot("norm",12,2,c2,1,3,1,0.012);
	plot("norm",12,3,c2,1,3,1,0.012);
	plot("norm",12,4,c2,1,3,1,0.012);
	plot("norm",12,5,c2,1,3,1,0.012);
	plot("norm",12,15,c2,1,3,1,0.012);
	plot("norm",12,16,c2,1,3,1,0.012);
	plot("norm",13,2,c2,1,3,1,0.012);
	plot("norm",13,3,c2,1,3,1,0.012);
	plot("norm",13,4,c2,1,3,1,0.012);
	plot("norm",13,5,c2,1,3,1,0.012);
	plot("norm",13,15,c2,1,3,1,0.012);
	plot("norm",13,16,c2,1,3,1,0.012);
    }
    if(plt==102 || plt==0){
	plot("norm",11,2,c1,1,4,2,0.002);
	plot("norm",12,2,c2,1,4,2,0.002);
	plot("norm",12,3,c2,1,4,2,0.002);
	plot("norm",12,4,c2,1,4,2,0.002);
	plot("norm",12,5,c2,1,4,2,0.002);
	plot("norm",12,15,c2,1,4,2,0.002);
	plot("norm",12,16,c2,1,4,2,0.002);
    }
    if(plt==103 || plt==0){
	plot("norm",11,2,c1,1,5,3,0.0005);
	plot("norm",12,2,c2,1,5,3,0.0005);
	plot("norm",12,3,c2,1,5,3,0.0005);
	plot("norm",12,4,c2,1,5,3,0.0005);
	plot("norm",12,5,c2,1,5,3,0.0005);
	plot("norm",12,15,c2,1,5,3,0.0005);
	plot("norm",12,16,c2,1,5,3,0.0005);
    }
    if(plt==110 || plt==0){
	plot("norm",11,2,c1,1,3,1,0.012);
	plot("norm",11,6,c2,1,3,1,0.012);
	plot("norm",12,2,c1,1,3,1,0.012);
	plot("norm",12,6,c2,1,3,1,0.012);
	plot("norm",15,2,c1,1,3,1,0.012);
	plot("norm",15,6,c2,1,3,1,0.012);
	plot("norm",11,2,c1,1,4,2,0.002);
	plot("norm",11,6,c2,1,4,2,0.002);
	plot("norm",12,2,c1,1,4,2,0.002);
	plot("norm",12,6,c2,1,4,2,0.002);
	plot("norm",15,2,c1,1,4,2,0.002);
	plot("norm",15,6,c2,1,4,2,0.002);
    }
    if(plt==111 || plt==0){
	plot("norm",11,12,c1,1,3,1,0.012);
	plot("norm",11,13,c2,1,3,1,0.012);
	plot("norm",15,12,c1,1,3,1,0.012);
	plot("norm",15,13,c2,1,3,1,0.012);
	plot("norm",12,12,c1,1,3,1,0.012);
	plot("norm",12,13,c2,1,3,1,0.012);
	plot("norm",11,12,c1,1,4,2,0.002);
	plot("norm",11,13,c2,1,4,2,0.002);
	plot("norm",15,12,c1,1,4,2,0.002);
	plot("norm",15,13,c2,1,4,2,0.002);
	plot("norm",12,12,c1,1,4,2,0.002);
	plot("norm",12,13,c2,1,4,2,0.002);
    }
    if(plt==112 || plt==0){
	plot("norm",11,9,c1,1,3,1,0.012);
	plot("norm",11,10,c2,1,3,1,0.012);
	plot("norm",11,11,c2,1,3,1,0.012);
	plot("norm",12,9,c1,1,3,1,0.012);
	plot("norm",12,10,c2,1,3,1,0.012);
	plot("norm",12,11,c2,1,3,1,0.012);
    }


    if(plt==200){
	bbc();
	for(int c=0; c<NCUT; c++){
	    plot("mall",11,c,c1);   plot("mall",12,c,c2);
	    plot("z12", 11,c,c1);   plot("z12", 12,c,c2);
	    plot("norm",11,c,c1);   plot("norm",12,c,c2);
	}
    }
}
