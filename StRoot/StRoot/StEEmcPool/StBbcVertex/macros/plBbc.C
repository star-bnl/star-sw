TFile *fd=0;
TCanvas *c;
class TPad;
TPad *pad=0;


//==========================
//==========================

void plBbc( int page=21,TString fName="aa3" ) {
  int pr=0;

  TString outDir="../outBbc/";
 
  gROOT->Reset();
  gStyle->SetOptStat(1111111);
  gStyle->SetOptFit(1111111);

  gStyle->SetPalette(1,0);
  
  // Connect the input file and get the  histogram
  TString name2=outDir+fName+".hist.root";
  printf("opening '%s'\n",name2.Data());
  fd=new TFile(name2);
  if(!fd->IsOpen()) {
    printf("NOT found file=%s, STOP\n",hName.Data());
    return ;
  }
  assert(fd->IsOpen());
  //  fd->ls();
   
  TString ctit=fName+"_"; ctit+=page;
  c=new TCanvas(ctit,ctit,450,350);

#if 0  
  c->Range(0,0,1,1);
  TPad *pad0 = new TPad("pad0", "apd0",0.0,0.95,1.,1.);
  pad0->Draw();
  pad0->cd();

  TPaveText *pt = new TPaveText(0,0.,1,1,"br");
  pt->Draw();
  TDatime dt;
  TString txt2=fName+", page="+page+",  ";
  txt2+=dt.AsString();
  pt->AddText(txt2);
  txt2="--";
  pt->AddText(txt2);

  c->cd();
  pad = new TPad("pad1", "apd1",0.0,0.0,1,.95);
  pad->Draw();
#endif
  pad=c;

 //fd->ls();
  gStyle->SetOptStat(11111     );

  switch(page) {
  //..........................................
  //..........................................
  case 0: {
    pad->Divide(1,1);
    pad->cd(1);
    TH1* h= (TH1*)fd->Get("vd"); assert(h);
    h->Fit("gaus","R","",-60,60);
  }  break;
  
  //..........................................
  //..........................................
  case 1: {
    pad->Divide(2,2);
    int i;
    char *namex[]={"vk","va","vt","vv"};
    for(i=0;i<4;i++){
      pad->cd(i+1);
      TH1* h= (TH1*)fd->Get(namex[i]); assert(h);
      h->Draw("colz");
    }
  }  break;
  //..........................................
  //..........................................
  case 2: {
    pad->Divide(2,3);
    int i;
    char *namex[]={"vk","va","vt","vE","vv","vd","vW"};
    for(i=0;i<7;i++){
      pad->cd(i+1);
      TH1* h= (TH1*)fd->Get(namex[i]); assert(h);
      if(i!=5)h->Draw("box");
      else h->Fit("gaus");
    }
  }  break;
  //..........................................
  //..........................................
 case 3: {
   pad->Divide(1,1);
   pad->cd(1);
   TString tt="vv";
   TH2F*h2= (TH2F*)gROOT->FindObject(tt);
   assert(h2);
   h2->FitSlicesY();
   TH1D*hd= (TH1D*)gROOT->FindObject(tt+"_1");

   h2->Draw("colz");
   hd->SetMarkerStyle(20);
   char *cFunc="pol1";
   hd->Fit(cFunc,"R","same",220,280);
   TF1* f1=hd->GetFunction(cFunc);
   f1->SetLineColor(kYellow);
  f1->SetLineWidth(2.);
 } break;

  //..........................................
  //..........................................
  case 21: 
  case 22: 
    calT(pad,fd,page-21); // time offset
    break;
  //..........................................
  //..........................................
  case 31: 
  case 32: 
    calW(pad,fd,page-31); // time walk
    break;
  //..........................................
  //..........................................
  default: printf("page %d not defined\n",page);
    pr=0;
  }

  if(pr) {
    TString outF=fName+"_"+page+".ps";
    //TString outF=fName+".ps";

    c->Print(outF);
    outF.ReplaceAll(".ps",".gif");
    c->Print(outF);
    return;
  }

  return;
}

//==========================
//==========================
calT(TPad *pad, TFile *fd, int iew=0) {
  char cEW[2]={'E','W'};
  int pm=-1+2*iew;
  int nok=0;
  ln0=new TLine(0,0,0,200); ln0->SetLineColor(kGreen);
  pad->Divide(3,4);
  for(int ih=0;ih<16;ih++) {
    char tt1[100];
    int iT=ih;
    sprintf(tt1,"w%c%02d",cEW[iew],iT+1);
    pad->cd(ih+1);
    TH2* h2= (TH2*)fd->Get(tt1);
    if(h2==0) break;
    assert(h2);
    // h2->Draw("box");
    printf("pmt=%s neve=%.0f mean=%f\n",tt1,h2->GetEntries(),h2->GetMean(1));
    if(h2->GetEntries()<50) continue;
    TH1D *hd=h2->ProjectionY(); 
    hd->Fit("gaus");
    ln0->Draw();
    f=hd->GetFunction("gaus");
    f->SetLineColor(kRed);
    f->SetLineWidth(2.);
    nok++;
    sprintf(tt1,"bbc%c%02d",cEW[iew],iT+1);    
    printf("#2 pmt=%s dz/cm=%.1f + / - %.1f sigDZ/cm %.1f + / - %.1f\n",tt1,f->GetParameter(1),f->GetParError(1),f->GetParameter(2),f->GetParError(2));
    // output in TDC
    float fac=2;  //(1chan=2 cm)
    printf("#1 %s %.1f + / - %.1f  N=%.0f\n",tt1,pm*f->GetParameter(1)/fac,f->GetParError(1)/fac, h2->GetEntries());
  }
    printf("#1# found %d pmt in bbc%c, sign= %.1f\n",nok,cEW[iew],pm);

}

//==========================
//==========================
calW(TPad *pad, TFile *fd, int iew=0) {// time walk
  char cEW[2]={'E','W'};
   int nok=0;
  int pm=-1+2*iew;
  pad->Divide(1,1);
  
  TH2F *hs=0;
  for(int ih=0;ih<16 ;ih++) {
    char tt1[100];
    int iT=ih;
    //  if(iT<6) continue;
    sprintf(tt1,"w%c%02d",cEW[iew],iT+1);
    pad->cd(ih+1);
    TH2* h2= (TH2*)fd->Get(tt1); assert(h2);
    // h2->Draw("box");
    printf("pmt=%s neve=%.0f mean=%f\n",tt1,h2->GetEntries(),h2->GetMean(1));
    if(h2->GetEntries()<400) continue;
    twFit(h2);
    TString tt=h2->GetName();
    pad->Print(tt+".gif");
   nok++;
  }
  printf("#1# added %d pmt in bbc%c\n",nok,cEW[iew]);
}

//==========================
//==========================
twFit( TH2* h2) {

  ln0=new TLine(0,0,200,0); ln0->SetLineColor(kGreen);
  TString tt=h2->GetName();

  TF1 *f1 = new TF1(tt+"fit",timeWalkFunc,-100,120,4);
  // f1->SetParameters(-25,50,0.01 1.5);// East
   f1->SetParameters(-50,40,0.01 0.1);// East #8
  //f1->SetParameters(-80,30,1,0.1);// West
  f1->SetLineColor(kRed);
  f1->SetLineWidth(2.);
    
  h2->FitSlicesY();
  TH1D*hd= (TH1D*)gROOT->FindObject(tt+"_1");
  hd->SetMaximum(100);
  hd->SetMinimum(-100);
  hd->SetMarkerStyle(20);
  h2->Draw("colz");
  hd->Fit(tt+"fit","R","same",4,200);

  ln0->Draw();

  //........ draw +/- 1 sig error band for projection
  Stat_t er1[1000];
  memset(er1,0,sizeof(er1));

  TH1D *he= (TH1D*)gROOT->FindObject(tt+"_2");
  TH1D *yu=(TH1D*)  hd->Clone(); yu->SetLineColor(kMagenta);
  yu->Add(he);  yu->SetError(er1);  
  yu->Draw("same");
  
  TH1D *yd=(TH1D*)  hd->Clone();
  yd->Add(he,-1);  yd->SetError(er1);   yd->SetLineColor(kMagenta);
  yd->Draw("same");

  double *par=f1->GetParameters();
  double *parE=f1->GetParErrors();
  int i;

  float fac=2.16; // 1tdc ch = 2 cm
  float pm= 1; // use - for E
  printf("#1 bbc%s %g %g %g %g \n",h2->GetName()+1,pm*par[0]/fac,pm*par[1]/fac,par[2],par[3]);    
}

//==========================
//==========================

Double_t timeWalkFunc(Double_t *x, Double_t *par){
  float maxAdc=80;
  Float_t xx =x[0];
  if(xx>maxAdc) xx=maxAdc; // ADC cut-off
  Double_t f = par[0] + par[1]*exp(-par[2]*pow(xx,par[3]));
  //Double_t f = par[0] + par[1]*xx;
  return f;
}



