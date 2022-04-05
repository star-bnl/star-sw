static const int mNPREPOST=2;
static const int mNQ=4;
static const int mNL=3;
static const int mNS=21;
static const int mNID=4*3*21;

TH1F* mDataSize[2];
TH1F* mXing[mNPREPOST*2+1];
TH2F* mAdc2[2];   
TH1F* mAdc[mNID][2];

static int LOG=1;
static TCanvas *c;
static TString* FILENAME;

static float MIP[mNID];
static float SIG[mNID];

void plotSize(){  
  gStyle->SetOptStat(1110);
  gStyle->SetOptFit(0);
  gStyle->SetStatW(0.2);
  gStyle->SetStatH(0.3);
  c->Clear();
  c->Divide(1,2);
  TVirtualPad *pad;

  pad = c->cd(1); if(LOG) pad->SetLogy();
  mDataSize[0]->SetFillColor(kBlue); mDataSize[0]->Draw();  
  TText *t1= new TText(0.15,0.80,"log10(Total NData)"); t1->SetNDC(); t1->SetTextSize(0.05); t1->Draw();

  pad = c->cd(2); if(LOG) pad->SetLogy();
  mDataSize[1]->SetFillColor(kBlue); mDataSize[1]->Draw();
  TText *t2= new TText(0.15,0.80,"log10(Xing=0 NData)"); t2->SetNDC(); t2->SetTextSize(0.05); t2->Draw();

  TString f(*FILENAME);
  f.ReplaceAll(".root",".size.png");
  c->SaveAs(f.Data());
}

void plotXing(){  
  gStyle->SetOptStat(1110);
  gStyle->SetOptFit(0);
  gStyle->SetStatW(0.2);
  gStyle->SetStatH(0.5);
  c->Clear();
  c->Divide(1,mNPREPOST*2+1);
  for(int i=0; i<mNPREPOST*2+1; i++){
    TVirtualPad *pad = c->cd(i+1); pad->SetLogy();
    float mergin=0.01;
    pad->SetTopMargin(mergin);   pad->SetBottomMargin(mergin);    
    int x=i-mNPREPOST;
    mXing[i]->SetFillColor(kBlue);
    mXing[i]->Draw();
    TText *t1= new TText(0.4,0.80,Form("Xing=%d",x)); t1->SetNDC(); t1->SetTextSize(0.15); t1->Draw();
  }
  TString f(*FILENAME);
  f.ReplaceAll(".root",".xing.png");
  c->SaveAs(f.Data());
}

void plot2d(){  
  gStyle->SetOptStat(0);
  gStyle->SetOptFit(0);
  c->Clear();
  c->Divide(1,2);
  TVirtualPad *pad;
  pad = c->cd(1); if(LOG) pad->SetLogz();
  mAdc2[0]->Draw("colz");
  pad = c->cd(2); if(LOG) pad->SetLogz();
  mAdc2[1]->Draw("colz");
  TString f(*FILENAME);
  TText *t1= new TText(0.55,0.02,"SlatId=(Q-1)*3*21+(L-1)*21+S"); t1->SetNDC(); t1->SetTextSize(0.05); t1->Draw();
  f.ReplaceAll(".root",".2d.png");
  c->SaveAs(f.Data());
}

void draw(int quad=1, int layer=1, int widezoom=0, int ped=0){
  gStyle->SetOptStat(0);  
  gStyle->SetOptFit(0);  
  printf("Plotting Quad=%d Layer=%d wide/zoom=%d\n",quad,layer,widezoom);
  c->Clear();
  c->Divide(3,7);
  for(int s=1; s<=21; s++){
    if((quad==2 || quad==4) && s>19) break;
    TVirtualPad *pad = c->cd(s);    
    float mergin=0.007;
    pad->SetRightMargin(mergin); pad->SetLeftMargin(mergin);
    pad->SetTopMargin(mergin);   pad->SetBottomMargin(0.01);
    if(LOG) pad->SetLogy(); 
    pad->Draw();
    int id=(quad-1)*3*21 + (layer-1)*21 + s-1;
    TH1F* h = mAdc[id][widezoom];
    h->SetTitle("");     
    h->SetFillColor(kBlue); 
    h->SetNdivisions(205);
    h->Draw();

    //signal fit
    float mip=0,sig=0;
    TF1 *f;
    if(widezoom==1){
      f=new TF1("ga", "gaus", 30, 200);
      if(ped==0){
	f->SetParameters(0,100);
	f->SetParameters(1,100);
	f->SetParameters(2,40);
      }else{
	f->SetParameters(0,100);
	f->SetParameters(1,70);
	f->SetParameters(2,3);
      }
      int res = h->Fit("ga","0RQ");
      mip = f->GetParameter(1);
      sig = f->GetParameter(2);    
      printf("MIP=%6.3f MIPSigma=%6.3f\n",mip,sig);
      MIP[id]=mip;
      SIG[id]=sig;
      f->SetLineColor(kRed);   
      f->Draw("same");
    }

    //some text
    char tit[10]; 
    sprintf(tit,"Q%1dL%1dC%02d",quad,layer,s);
    TText *t1= new TText(0.30,0.80,tit); t1->SetNDC(); t1->SetTextSize(0.15); t1->Draw();
    if(widezoom==0){
      float mean=h->GetMean();
      float rms=h->GetRMS();
      TText *t2= new TText(0.30,0.65,Form("MEAN %4.1f RMS %4.2f",mean,rms)); t2->SetNDC(); t2->SetTextSize(0.15); t2->Draw();
    }else{
      TText *t3= new TText(0.30,0.65,Form("MIP %4.1f +- %4.2f",mip,sig)); t3->SetNDC(); t3->SetTextSize(0.15); t3->Draw();
    }
  }
  c->cd(0); 
  float min=h->GetXaxis()->GetXmin();
  float max=h->GetXaxis()->GetXmax();
  TText *t4= new TText(0.005,0.85,Form("%3.0f",min)); t4->SetNDC(); t4->SetTextSize(0.03); t4->Draw(); 
  TText *t5= new TText(0.93,0.85,Form("%3.0f",max)); t5->SetNDC(); t5->SetTextSize(0.03); t5->Draw();
  TString* name = new TString(FILENAME->Data());

  TString ff(*FILENAME);
  ff.ReplaceAll(".root",Form(".Q%1dL%1d.png",quad,layer));
  c->SaveAs(ff.Data());
}
		      
void readfile(int run){
  int year=run/1000;
  char file[100],name[100];
  sprintf(file,"%d/%d.root",year,run);
  printf("Opnening %s\n",file);
  FILENAME = new TString(file);
  TFile* tfile= new TFile(file,"old");
  mDataSize[0]=(TH1F*)tfile->Get("TotalSize");
  mDataSize[1]=(TH1F*)tfile->Get("DataSize");
  for(int i=0; i<mNPREPOST*2+1; i++){
    int x=i-mNPREPOST;
    sprintf(name,"Xing=%d",x);
    mXing[i] = (TH1F*)tfile->Get(name);
  }
  mAdc2[0] = (TH2F*)tfile->Get("Adc2");
  mAdc2[1] = (TH2F*)tfile->Get("Adc2z");  
  for(int i=0; i<mNID; i++){
    sprintf(name,"ADC%03d",i);    
    mAdc[i][0]=(TH1F*)tfile->Get(name);
    sprintf(name,"ADC%03dz",i);    
    mAdc[i][1]=(TH1F*)tfile->Get(name);
  }
}

void openCanvas(){
  c = new TCanvas("FPS","FPS",15,15,700,800);
  gStyle->SetLabelSize(0.1,"xy");
  gStyle->SetPalette(1);
  gStyle->SetStatW(0.4);
}

void writetxt(){
  TString f(*FILENAME);
  f.ReplaceAll(".root",".mip.txt");
  printf("Creating %s\n",f.Data());
  FILE *FP = fopen(f.Data(), "w");
  if(FP==NULL){ printf("Cannot open file\n"); return; }
  for(int q=1; q<=4; q++){
    for(int l=1; l<=3; l++){
      for(int s=1; s<=21; s++){
	int id=(q-1)*3*21 + (l-1)*21 + s;	
	fprintf(FP,"%3d %1d %1d %2d %12.4f %12.4f %12.4f %12.4f\n",
		id,q,l,s,MIP[id],SIG[id]);
      }    
    }
  }
  fclose(FP);
}

void plot(int run=16029002, int ped=0, int plt=0, int quad=0, int layer=0, int widezoom=-1, int log=1){  
  LOG=log;
  readfile(run);
  openCanvas();

  if(plt==1 || plt==0) plotSize();
  if(plt==2 || plt==0) plotXing();
  if(plt==3 || plt==0) plot2d();
  if(plt==4 || plt==0){
    for(int q=1; q<=4; q++){
      for(int l=1; l<=3; l++){      
	if((quad==0 && layer==0) || (quad==q && layer==l)){
	  for(int wz=0; wz<2; wz++){
	    if(widezoom==-1 || wz==widezoom){
	      draw(q,l,wz,ped);
	    }
	  }
	}
      }
    }
    if(quad==0 && layer==0) writetxt();
  }
}
