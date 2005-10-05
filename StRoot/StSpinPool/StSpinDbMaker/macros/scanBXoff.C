scanBXoff( char* Rrun    = "R6104032") {
  gStyle->SetPalette(1,0);
  gStyle->SetOptStat(11);
  
  char *tit[]={"bX7","bX48"};
  //  char *tit[]={"bX48"};
  int myOff[]={-1, -1};
  int myRms[]={-1, -1};
  int nd=2;
  
  TString fname="iter1/";  fname+=Rrun;  fname+=".hist.root";

  TFile *fd=new TFile(fname);
  //printf("open?=%d\n",fd->IsOpen());
  if(!fd->IsOpen()) {
    printf("scanBXoff %s  not exits \n",fname.Data()); 
    return;
  } 
  
  fd->ls();

  // tmp
  // TFile *fd2=new TFile("iter0/F7238.hist.root");
  //  fd2->ls();
  hI =(TH1F*) fd->Get("bXI"); assert(hI); // Ideal distribution --> the goal


  TH1F *hI2=(TH1F*)hI->Clone();

  hI2->Scale(200.); 
  hI2->SetFillColor(kYellow);
  hI2->SetLineColor(kWhite);

  TCanvas *c1=new TCanvas(Rrun,Rrun,800,700);
  c1->Divide(nd,3);
  
  
  // .....  find bXing offset for the list of spectra
  int k,i;

  for(k=0;k<nd;k++) {    
    TString tt=tit[k];
    TH1F *hc =new TH1F("ch-"+tt,"CHi2 dostribution vs. bXing offset for:"+tt,120,-0.5,119.5);
 
    TH1F *hd=(TH1F*) fd->Get(tt);
    hd->Print();
    if(hd==0) {
      printf("%s file exist but %s is missing\n",fname.Data(),tt.Data()); 
      continue;
    }
    assert(hd); 
   
    c1->cd(0*nd+k+1); hd->Draw();
    hI2->Draw("same");
    hd->Draw("same");
    hd->SetFillColor(kBlue);
 

    myRms[k]=hd->GetRMS();
    if(myRms[k]<10) continue;

    TH1F *hx=(TH1F*)hd->Clone(); // working copy
    setNorm(hx);

    int off= scanOff(hx, hI, hc);
    myOff[k]=off;
    printf("run=%s  off=%d\n",Rrun,off);
    c1->cd(1*nd+k+1); hc->Draw(); gPad->SetLogy();
    //hc->SetFillColor(kGreen);
    //hc->SetLineColor(kWhite);

    TH1F *hg=(TH1F*)hd->Clone();  // input+shift
    shift(hd,hg,off);
    c1->cd(2*nd+k+1); 
    hg->Draw();
    hI2->Draw("same");
    hg->Draw("same");
    hg->SetFillColor(kBlue);


  }
  //  hg->SetLineColor(kRed);
  printf("#scanBXoff %s  ",Rrun);

  for(k=0;k<nd;k++) {   
    printf("%s: rms=%f off=%d   ",tit[k], myRms[k], myOff[k]);
  }
  //  printf("spinBitsMean=%.1f\n", ((TH2F*) fd->Get("sBit0119"))->GetMean(2));
  printf("\n");

  //  c1->Print(fname.ReplaceAll(".root",".ps"));

}


//========================================================
//========================================================
//========================================================
void   shift(TH1F *hd,TH1F *hg, int off) {
  
  hg->Reset();
  int i;
  for(i=0;i<120;i++) {
    int j= 1+(i+120-off)%120;
    hg->SetBinContent(i+1, hd->GetBinContent(j));
    //    hg->SetBinError(i+1, hd->GetBinError(j));
  }
}
//========================================================
//========================================================
//========================================================
int   scanOff(TH1F *hx, TH1F *hp, TH1F *hc) {
  int off;
  float min=1e100;
  int k=-1;
  for(off=0;off<120;off++) {
    float chi2= getChi2(hx,hp,off);
    hc->Fill(off,chi2);
    if(min>chi2) { min=chi2; k=off;}
    // printf("off=%d chi2=%8.0f  max=%f off=%d\n",off,chi2,min,k);
	//printf(".");
  }
  if(min*3.>hc->Integral()/120.) return -2;

  return (120-k)%120;
}



//========================================================
//========================================================
//========================================================
float   getChi2(TH1F *hx, TH1F *hp, int off) {
  int nb=hx->GetNbinsX();
  assert(nb>=120);

  float * x=hx->GetArray(); 
  float * p=hp->GetArray(); 

  float sum=0;
  int i;
  for(i=0;i<120;i++) {
    int j= 1+(i+off)%120;
    float xx= (x[j] -p[i+1])/hx->GetBinError(j);
    sum+=xx*xx;
  }
  return sum;
}

//========================================================
//========================================================
//========================================================
void   setNorm(TH1F *h){
  int nb=h->GetNbinsX();
 
  float * y=h->GetArray();  
  float sum=0;
  int i;
  for(i=1;i<=nb;i++) {
    sum+=y[i];
  }
  float fac=(50*1+10*0.01 )/sum;

  for(i=1;i<=120;i++) {
    float xx=y[i];
    float val=xx*fac;
    if(xx==0) xx=1;
    float err=fac*sqrt(xx);
    h->SetBinContent(i,val);
    h->SetBinError(i,err);
  }

}


//========================================================
//========================================================
//========================================================
void draw_raw2(TFile *dirL1=0){

  assert(dirL1->IsOpen());

  TCanvas *c1=new TCanvas("raw1","My raw1",450,500);
  c1->Divide(1,2);

  char *tit[]={"bX7","bX48"};
  int nd=2;
  int k,i;

  for(k=0;k<nd;k++) {
    c1->cd(1+k);

    TH1F* h1=(TH1F*) dirL1->Get(tit[k]);
    h1->Draw();
    h1->SetLineColor(kBlue);
    //    h1->SetLineStyle(3);
    }
}



