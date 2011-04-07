class TFile;
// FGT SlowSimu plots
TFile *fd=0;
plFgtSS( int page=1,char *core="d1",int pl=0){ //1=gif, 2=ps, 3=both
  //  core="R7098001a";
  char *path="./";
  TString fullInpName=path;  fullInpName+=core;
  fullInpName+=".fgt.hist.root";

  double seedThres=30;

  fd=new TFile(fullInpName);
  if(! fd->IsOpen()) {
    printf("EROR: input histo file not found, quit\n",fullInpName.Data());
    return;
  }
  // fd->ls();
  gStyle->SetPalette(1,0);

  char *nameA[]={"ss_inDE","ss_inDS","ss_inZ","ss_hitStat","ss_inXY","ss_cTof","ss_inR","ss_cPmag","ss_gXY1"};
  char *nameB[]={"fr_pairEne","fr_nPrimPair", "fr_totEne","fr_nTotPair","fr_pathL","fr_avrTPath"};
  char *nameC[]={"ss_hitStat","cl_Stat1D","ev_Stat1D"};

  
  switch (page) {    
  case 1:{ // general stats from all disks
    can=new TCanvas("aa","aa",800,500);    TPad *c=makeTitle(can,core,page);
    c->Divide(1,3);gStyle->SetOptStat(111111);
    char **nameX=nameC;
    for(int i=0;i<3;i++) {
      printf("->%s<\n",nameX[i]);
      h=(TH1*)fd->Get(nameX[i]);  assert(h);
      c->cd(i+1); h->Draw();
    } 
    break;
  }//--------------------------------------



  case 2:{ // g2t track properties
    can=new TCanvas("aa","aa",800,700);   TPad *c=makeTitle(can,core,page);
    c->Divide(3,3);
    char **nameX=nameA;
    int i;
    for(i=0;i<9;i++) {
      printf("->%s<\n",nameX[i]);
      h=(TH1*)fd->Get(nameX[i]);  assert(h);
      c->cd(i+1); h->Draw();
      if(i==4|| i==8)  h->Draw("colz"); 
      if(i==3) 	if(h->GetEntries()>0) c->GetPad(1+i)->SetLogz();      
    } 
    break;
  }//--------------------------------------

  case 3: { //  EVENT: 2D digitalization
    can=new TCanvas("aa","aa",800,420);    TPad *c=makeTitle(can,core,page);
    c->Divide(2,1);
    c->cd(1);ss_inXY->Draw("colz");
    c->cd(2);digXY->Draw("colz");
    break;
  }//--------------------------------------

  case 4: { //  EVENT: zoom in of digXY
    can=new TCanvas("aa","aa",400,420);    TPad *c=makeTitle(can,core,page);
    digXY->SetAxisRange(14.,15.,"x"); 
    digXY->SetAxisRange(35.,36.,"y");
    digXY->Draw("colz"); 
    break;
  }//--------------------------------------

  case 5: { // EVENT: projection  on rad & phi strips
    can=new TCanvas("aa","aa",800,420);    TPad *c=makeTitle(can,core,page);
    c->Divide(1,2);gStyle->SetOptStat(1001111);
    c->cd(1);digRad->Draw();
    c->cd(2);digPhi->Draw();
    break;
  }//--------------------------------------

  case 6:{ // properties of Frank's hit distribution generator
    can=new TCanvas("aa","aa",800,500);    TPad *c=makeTitle(can,core,page);
    c->Divide(3,2);
    char **nameX=nameB;
    for(int i=0;i<6;i++) {
      printf("->%s<\n",nameX[i]);
      h=(TH1*)fd->Get(nameX[i]);  assert(h);
      c->cd(i+1); h->Draw();
      if(i==5) { h->SetAxisRange(0,2);} //h->Fit("gaus");
    } 
    break;
  }//--------------------------------------

  case 7: { // R-cluster finder properties
    can=new TCanvas("aa","aa",800,550);    TPad *c=makeTitle(can,core,page);
    c->Divide(3,2);gStyle->SetOptStat(111111);
    c->cd(1);cl_RmxAmp->Draw();
    cl_RmxAmp->Fit("landau","R","",seedThres,99999);
    // c->cd(2);cl_rMul->Draw();
    c->cd(2);cl_Rwid->Draw();
    c->cd(4);cl_Rpf->Draw();
    c->cd(5);//ev_errRad->Fit("gaus","R","",-80,80);//Draw();
    ev_errRad->Rebin(3);
    ev_errRad->Fit("gaus");
    c->cd(6);ev_dRad_R->Draw("colz");
    break;
  }//--------------------------------------


  case 8: { // Phi-cluster finder properties
    can=new TCanvas("aa","aa",800,550);    TPad *c=makeTitle(can,core,page);
    c->Divide(3,2);gStyle->SetOptStat(111111);
    c->cd(1);cl_PmxAmp->Draw();cl_PmxAmp->Fit("landau","R","",seedThres,99999);
    // c->cd(2);cl_pMul->Draw();
    c->cd(2);cl_Pwid->Draw();
    c->cd(4);cl_Ppf->Draw(); // fr_Rdiff->Draw("colz");
    c->cd(5);ev_RerrPhi->Fit("gaus","R","",-100,100);//->Draw();
    c->cd(6);ev_RdPhi_R->Draw("colz");
    break;
  }//--------------------------------------


  case 9: { // Rad vs. Phi-cluster finder properties
   can=new TCanvas("aa","aa",500,280);    TPad *c=makeTitle(can,core,page);
    c->Divide(2,1);gStyle->SetOptStat(111111);
    c->cd(1);cl_rpMul->Draw("colz");
   
     break;
  }//--------------------------------------


  case 10: { // Rad err vs. R
    can=new TCanvas("aa","aa",800,420);    TPad *c=makeTitle(can,core,page);
    c->Divide(2,1);gStyle->SetOptStat(0);gStyle->SetOptFit(1);
    int nReb=4;
    c->cd(1);spreadFit(ev_dRad_R->RebinX(nReb,"dRreb")); 
    c->cd(2);spreadFit(ev_RdPhi_R->RebinX(nReb,"RdPreb"));
     break;
  }//--------------------------------------


  case 101: { // tmp1
    can=new TCanvas("aa","aa",1100,850);    TPad *c=makeTitle(can,core,page);
    c->Divide(3,2);gStyle->SetOptStat(111111);
    c->cd(1);cl_rpMul->Draw("colz");
    c->cd(4);ev_Stat1D->Draw(); ev_Stat1D->SetAxisRange(0,6);
    float yy=ev_Stat1D->GetMaximum();
    ev_Stat1D->SetMinimum(yy*.75);
    c->cd(2);ev_errRad->Fit("gaus","R","",-200,200);//Draw();
    c->cd(3);ev_dRad_R->Draw("colz");

    c->cd(5);ev_RerrPhi->Fit("gaus","R","",-60,60);//->Draw();
    c->cd(6);ev_RdPhi_R->Draw("colz");

    break;
  }//--------------------------------------

  case 102: { // tmp1
    can=new TCanvas("aa","aa",500,280);    TPad *c=makeTitle(can,core,page);
    c->Divide(2,1);
    c->cd(1);cl_RmxAmp->Fit("landau","R","",seedThres,99999);
    c->cd(2);cl_PmxAmp->Fit("landau","R","",seedThres,99999);

    break;
  }//--------------------------------------

  case 103: { // tmp1
    can=new TCanvas("aa","aa",1100,400);    TPad *c=makeTitle(can,core,page);
    c->Divide(3,1);gStyle->SetOptStat(1111);
    c->cd(1);ev_Stat1D->Draw(); ev_Stat1D->SetAxisRange(0,6);
    float yy=ev_Stat1D->GetMaximum();
    ev_Stat1D->SetMinimum(yy*.75);
    c->cd(2);ev_errRad->Fit("gaus","R","",-200,200);//Draw();
    c->cd(3);ev_RerrPhi->Fit("gaus","R","",-60,60);//->Draw();

    break;
  }//--------------------------------------

  case 104: { // tmp1
    can=new TCanvas("aa","aa",900,750);    TPad *c=makeTitle(can,core,page);
    c->Divide(3,2);gStyle->SetOptStat(1111);
    c->cd(1);cl_RmxAmp->Fit("landau","R","",seedThres,99999);
    c->cd(2);cl_Rpf->Draw();
    c->cd(3);ev_errRad->Fit("gaus","R","",-200,200);

    c->cd(4);cl_PmxAmp->Fit("landau","R","",seedThres,99999);
    c->cd(5);cl_Ppf->Draw();
    c->cd(6);ev_RerrPhi->Fit("gaus","R","",-80,80);

    break;
  }//--------------------------------------

  case 105: { // tmp1
    can=new TCanvas("aa","aa",1100,800);    TPad *c=makeTitle(can,core,page);
    c->Divide(3,2);gStyle->SetOptStat(111111);
    c->cd(4);ev_RokXY->Draw("colz"); //  ev_RokXY->Rebin2D(2,2);
 
    c->cd(1);ev_Stat1D->Draw(); ev_Stat1D->SetAxisRange(0,6);
    float yy=ev_Stat1D->GetMaximum();
    ev_Stat1D->SetMinimum(yy*.75);

    int nReb=4;
    c->cd(2);ev_RerrPhi->Fit("gaus","R","",-100,100);//->Draw();
    // ev_RdPhi_R->RebinY(2);
    c->cd(3);spreadFit(ev_RdPhi_R->RebinX(nReb,"RdPreb"),370);

    c->cd(5);ev_errRad->Fit("gaus","R","",-300,300);//Draw();
    c->cd(6);spreadFit(ev_dRad_R->RebinX(nReb,"dRreb"),370); 

    break;
  }//--------------------------------------


  default:
    printf("page=%d NOT defined\n",page);
 
  }

  char text[100];
  sprintf(text,"%s_page%03d",core,page);
  TString tit=text;
  can->SetTitle(tit);
  can->SetName(tit);
  //  c->Clear();
  
  if(pl&1) can->Print(tit+".gif");
  if(pl&2) can->Print(tit+".ps");
  
}


//------------------------
TPad *makeTitle(TCanvas *c,char *core, int page) {
 
  c->Range(0,0,1,1);
  TPad *pad0 = new TPad("pad0", "apd0",0.0,0.95,1.,1.);
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
  pad = new TPad("pad1", "apd1",0.0,0.0,1,.95);
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
  for(i=1; i<=3; i++) plFgtSS(i,core,2);
  for(i=6; i<=10; i++) plFgtSS(i,core,2);
  for(i=101; i<=105; i++) plFgtSS(i,core,2);
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
