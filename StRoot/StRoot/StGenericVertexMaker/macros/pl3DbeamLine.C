class TFile;

TFile *fd=0;
pl3DbeamLine( int page=3,char *core="R13078011",int pl=0){ //1=ps
  
  char *inPath="out/";
  TString outPath=inPath;
  TString fullInpName=Form("%s3D_beam_%s.hist.root",inPath,core);

  fd=new TFile(fullInpName);
  if(! fd->IsOpen()) {
    printf("EROR: input histo file not found, quit\n",fullInpName.Data());
    return;
  }
  // fd->ls();
  gStyle->SetPalette(1,0);

  char *nameA[]={"trStat","trCh2","trZ","trX","trY","trP",,"trPt"};
  char *nameB[]={"fcnChiXY","fcnChiXnX","fcnChiYnY"};
  char *nameC[]={"bmSol","fcnDet","fcnDca","fcnDcaZ","fcnNtr"};
  
  switch (page) {    
  case 1:{ // general stats , input
    can=new TCanvas("aa","aa",800,800);    TPad *c=makeTitle(can,core,page);
    c->Divide(3,3);gStyle->SetOptStat(111111);
    char **nameX=nameA;
    for(int i=0;i<7;i++) { // tmp, w/o clust-finder
      printf("->%s<\n",nameX[i]);
      h=(TH1*)fd->Get(nameX[i]);  assert(h);
      c->cd(i+1); h->Draw();
    } 
    break;
  }//--------------------------------------


  case 2:{ // general stats , input
    can=new TCanvas("aa","aa",900,650);    TPad *c=makeTitle(can,core,page,0.85);
    c->Divide(3,2);gStyle->SetOptStat(0);
    char **nameX=nameB;
    for(int i=0;i<3;i++) { // tmp, w/o clust-finder
      printf("->%s<\n",nameX[i]);
      h=(TH1*)fd->Get(nameX[i]);  assert(h);
      c->cd(i+1);  gPad->SetLeftMargin(0.15);
      h->Draw("colz");
    } 
    
    c->cd(4); bmSol->Draw();
    bmSol->GetXaxis()->SetTitleOffset(0.9);

    break;
  }//--------------------------------------


  case 3:{ // explore minima
    can=new TCanvas("aa","aa",900,800);    TPad *c=makeTitle(can,core,page);
    c->Divide(3,2);gStyle->SetOptStat(111111);
    char **nameX=nameC;
    for(int i=0;i<5;i++) { // tmp, w/o clust-finder
      printf("->%s<\n",nameX[i]);
      h=(TH1*)fd->Get(nameX[i]);  assert(h);
      c->cd(i+1); h->Draw();
      if(i==3) h->Draw("colz");
    } 
    break;
  }//--------------------------------------



  default:
    printf("page=%d NOT defined\n",page);
 
  }

  char text[100];
  sprintf(text,"%s_page%02d",core,page);
  TString tit=text;
  can->SetTitle(tit);
  can->SetName(tit);
  //  c->Clear();
  
 
  if(pl) can->Print(outPath+tit+".ps");
  
}


//------------------------
TPad *makeTitle(TCanvas *c,char *core, int page,float frac=0.95) {
 
  c->Range(0,0,1,1);
  TPad *pad0 = new TPad("pad0", "apd0",0.0,frac,1.,1.);
  pad0->Draw();
  pad0->cd();

  TPaveText *pt = new TPaveText(0,0.,1,1,"br");
  pt->Draw();
  TDatime dt;
  TString txt2=core; 
  txt2+=", page=";
  txt2+=page;
  txt2+=",  ";
  txt2+=dt.AsString();
  pt->AddText(txt2);
  txt2="--";
  pt->AddText(txt2);

  c->cd();
  pad = new TPad("pad1", "apd1",0.0,0.0,1,frac);
  pad->Draw();
  return pad;
}

//============================
void  doStat(char *name0, char *name3){
  printf("->%s<\n",name0);
  h0=(TH1*)fd->Get(name0);  assert(h0);
  int nEve=h0->GetEntries()/ h0->GetNbinsX();
  printf("nEve=%d\n",nEve);

  h3=(TH1F*)fd->Get(name3);  assert(h3);
  int nb=h3->GetNbinsX();
  int i,n=0,n1=0;
  for(i=1; i<=nb;i++) {
    float y=h3->GetBinContent(i);
    if(y<=0) continue;
    n++;
    float r=y/nEve;
    float er=sqrt(y)/nEve;
    if(r>0.001){ printf("* "); n1++;}
    else continue;
      //intf("  ");

    printf("  HankCh=%d  failProb=%.3f +/- %.3f\n",  i-1,r,er);
  }
  printf("bigErr n1=%d  n=%d\n",n1,n);
}

//===================================
//==========================
//==========================
spreadFit( TH2* h2, double Ymx=370) {
 
  ln0=new TLine(0,0,450,0); ln0->SetLineColor(kBlack);ln0->SetLineWidth(2.);
  ln0->SetLineStyle(2);
  TString tt=h2->GetName();
  h2->GetListOfFunctions()->Clear(); // drop strip contur
  TString tt=h2->GetName();
  h2->FitSlicesY();
  TH1D*hd= (TH1D*)gROOT->FindObject(tt+"_1");
  h2->SetAxisRange(-Ymx,Ymx,"y");

  hd->SetMarkerStyle(20);
  h2->Draw("colz");
  hd->Draw("same");
 
  //........ draw +/- 1 sig error band for projection
  Stat_t er1[1000];
  memset(er1,0,sizeof(er1));

  TH1D *he= (TH1D*)gROOT->FindObject(tt+"_2");
  he->Fit("pol1","","same");

  TH1D *yu=(TH1D*)  hd->Clone(); yu->SetLineColor(kMagenta);
  yu->Add(he);  yu->SetError(er1);yu->SetLineWidth(2.);
  yu->Draw("same");

  TH1D *yd=(TH1D*)  hd->Clone();
  yd->Add(he,-1);  yd->SetError(er1);   yd->SetLineColor(kMagenta);
  yd->Draw("same");yd->SetLineWidth(2.);
  ln0->Draw();

}



plAll(char *core="d1") {
  int i;
  for(i=1; i<=4; i++) plFgtSS(i,core,2);
  for(i=6; i<=7; i++) plFgtSS(i,core,2);
  // for(i=101; i<=105; i++) plFgtSS(i,core,2);  // Jan's cluster finder QA
}

/*
 .L plSS.C
 plAll("minb_d1r");
 plAll("minb_d6r");
 plAll("mu2_d1r");
 plAll("mu2_d6r");
cat  minb_d1r*ps |ps2pdf - >minb_d1r.pdf
cat  minb_d6r*ps |ps2pdf - >minb_d6r.pdf
cat  mu2_d1r*ps |ps2pdf - >mu2_d1r.pdf
cat  mu2_d6r*ps |ps2pdf - >mu2_d6r.pdf


*/
