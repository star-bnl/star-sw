TFile *fd=0;
TCanvas *c;
class TPad;
TPad *pad=0;

//==========================
//==========================

void plPi0Solo( int page=1,TString fName="R7098001" ) {
  fName="sumP";
 int pr=0;

  TString outDir="./out1/"; 
 
  gROOT->Reset();
  gStyle->SetPalette(1,0);
  
  // Connect the input file and get the  histogram
  TString name2=outDir+fName+".hist.root";
  printf("opening '%s'\n",name2.Data());
  TFile *fd=new TFile(name2);
  if(!fd->IsOpen()) {
    printf("NOT found file=%s, STOP\n",hName.Data());
    return ;
  }
  assert(fd->IsOpen());
  //  fd->ls();
  
  float xLo=.05, xHi=.22;
  //    float xLo=.4, xHi=.7;

  
  TString ctit="pi0-"; ctit+=page;
  if(page==3 || page==6) 
    c=new TCanvas(ctit,ctit,900,950);
  else
    c=new TCanvas(ctit,ctit,900,650);
  
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

  gStyle->SetOptStat(11111     );

  switch(page) {
  //..........................................
  //..........................................
  case 1: {
    pad->Divide(1,1);
    pad->cd(1);
    printYield(xLo,xHi);
    plotMix("invm", xLo,xHi, fd);
  }  break;
  //..........................................
  //..........................................
  case 2: {
    char *name[]={"ctbSum","tE","cE","cSh","sE","cN"};
    int log[]={  1,1,1,0,1,0};
    int i;
    pad->Divide(4,2);
    for(i=0;i<6;i++){
      pad->cd(i+1);
      TH1* h= (TH1*)fd->Get(name[i]); assert(h);
      h->Draw();
      if(log[i]) gPad->SetLogy();
    }
    
    pad->cd(7);
    plotMix("invm", xLo,xHi, fd);
  }  break;
  //..........................................
  //..........................................
  case 3: {
    char *name[]={"xyL","xyH","0eta","eta12","0xyH","invm","0ener","0phi","0Z"};
    int i;
    pad->Divide(3,3);
    for(i=0;i<9;i++){
      pad->cd(i+1);
      TH1* h= (TH1*)fd->Get(name[i]); assert(h);
      if(i<6 && i%3<2 ) { h->Draw("colz"); continue; }
      plotMix(name[i], xLo,xHi, fd);	
    }    
  }  break;

  //..........................................
  //..........................................
  case 4: {
    pad->Divide(2,3);
    int i;
    char *namex[]={"0ener","0eta","0phi","0pt","0Z","0Ang"};
    for(i=0;i<6;i++){
      pad->cd(i+1);
      plotMix(namex[i], xLo,xHi, fd);
    }
  }  break;

  //..........................................
  //..........................................
  case 5: {
    pad->Divide(1,2);
    pad->cd(1);
    TH1* h= (TH1*)fd->Get("ytw"); assert(h);
    h->Draw();

    pad->cd(2);
    plotMix("0ytw", xLo,xHi, fd);

  }  break;

  //..........................................
  //..........................................
  case 6: {
    gStyle->SetOptStat(1);
    TGraphErrors*  gr1=new TGraphErrors;
    gr1->SetMarkerStyle(21);
    gr1->SetName(ctit+"eM"); // 
    gr1->SetTitle("Pi0 mass vs. Eta; eta bin; inv mass (MeV);");

    TGraphErrors*  gr2=new TGraphErrors;
    gr2->SetMarkerStyle(20);
    gr2->SetName(ctit+"eS"); // 
    gr2->SetTitle("Pi0 mass width vs. Eta; eta bin; sigma (MeV);");

    TGraphErrors*  gr3=new TGraphErrors;
    gr3->SetMarkerStyle(19);
    gr3->SetName(ctit+"eN"); // 
    gr3->SetTitle("Pi0 Yield vs. Eta; eta bin; yield, err=nBckg/10;");

    pad->Divide(3,4);
    int i;
    for(i=0;i<12;i++) {
      char t1[100];
      sprintf(t1,"invm%02d",i+1);
      pad->cd(1+i);
      plotMix(t1, xLo,xHi, fd,gr1,gr2,gr3);
    }
    printYield(xLo,xHi);

    c2=new TCanvas(ctit+"e",ctit+"e",400,700);
    c2->Divide(1,3);
    c2->cd(1);    gr1->Draw("AP");
    gr1->SetMinimum(100);gr1->SetMaximum(180);
    TLine *ln=new TLine(0,135,13,135);
    ln->SetLineColor(kBlue); ln->Draw();
    c2->cd(2);    gr2->Draw("AP");
    gr2->SetMinimum(20);gr2->SetMaximum(60);
    c2->cd(3);    gr3->Draw("AP");
  }  break;
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


//---------------------------------------------
//---------------------------------------------
//---------------------------------------------
  void printYield(float xLo, float xHi) {
  ax=invm->GetXaxis();
  int bLo=ax->FindBin(xLo);
  int bHi=ax->FindBin(xHi);
  int bin1=ax->FindBin(0.6);
  int bin2=ax->FindBin(1.0);

  float nReal=invm->Integral(bin1,bin2);
  float nMix=Xinvm->Integral(bin1,bin2);
  float fac=-1;
  if(nMix>0) fac=nReal/nMix;
  printf("nReal=%d nMix=%d fac=%f\n",nReal,nMix,fac);


  float s1=invm->Integral(bLo,bHi);
  float s2=Xinvm->Integral(bLo,bHi);
  float x=s1-s2*fac;
  float ex=sqrt(s1+s2*fac*fac);
  printf("s1=%f s2=%f\n",s1,s2);
  float s2b=-1;
  if(s2>0) s2b=x/s2/fac; // signal to background 
  //error calculated  assuming s1 & s2 are independent
  float es2b=-999;
  if(s1>0 && s2>0 ) es2b=s2b*sqrt(1/s1+1/s2/fac/fac);
  printf("  Nrec=%.0f Nmix=%.0f  for invm=[%.2f, %.2f] , Npi0=%.0f + / - %.0f \n s2b=%.2f + / - %.3f \n",s1,s2,xLo,xHi, x,ex,s2b,es2b);
  printf("#1  Nrec=%.0f, Nmix=%.0f  mixFac=%.3f<br>   Npi0=%.0f + / - %.0f <br>  s2b=%.2f + / - %.3f \n",s1,s2, fac,x,ex,s2b,es2b);
  }


//---------------------------------------------
//---------------------------------------------
//---------------------------------------------
void  plotMix(TString hname="m1",float xLo, float xHi,TFile *fd,
	      TGraphErrors*  grM=0, TGraphErrors*  grS=0, TGraphErrors*  grN=0) {
  hx= (TH1*)fd->Get("invm"); assert(hx);
  int bin1=hx->FindBin(0.6);
  int bin2=hx->FindBin(1.0);
  float nReal=hx->Integral(bin1,bin2);
  hx= (TH1*)fd->Get("Xinvm"); assert(hx); 
  float nMix=hx->Integral(bin1,bin2);
  float fac=nReal/nMix;

  printf("total: nReal=%d nMix=%d fac=%f \n",nReal,nMix,fac);

  int i=1;
  TH1* hr= (TH1*)fd->Get(hname); assert(hr);
  TH1* hm= (TH1*)fd->Get("X"+hname); assert(hm);
  hm->Sumw2();
  hm->Scale(fac);

  hm->SetLineColor(kGreen);
  //  hm->SetLineStyle(2);
  TH1F* hD=(TH1F*)hr->Clone();
  hD->Sumw2();
  hD->SetTitle(hname+" Real-Mixed");
  hD->Add(hm,-1);
  hD->SetLineColor(kBlue);
  hD->Draw();
  hD->SetMinimum(-0.2 *hr->GetMaximum());
  hD->SetMaximum(1.1*hr->GetMaximum());
  hr->Draw("same");
  hm->Draw("same");

  //  gStyle->SetOptStat(11);
  mzer=new TLine(-10.,0,10,0.);
  mzer->Draw();

  //  printf("total: nReal=%d nMix=%d fac=%f name='%s'\n",nReal,nMix,fac,hname.Data());

  if(strstr(hname.Data(),"invm")==0) return;

  hD->SetTitle(" Real-Mixed; inv mass GeV");

  hD->Fit("gaus","R","same",xLo,xHi);
  hD->Draw("same");
  mLo=new TLine(xLo,0,xLo,3500);
  mLo->SetLineColor(kMagenta);
  mHi=new TLine(xHi,0,xHi,3500);
  mHi->SetLineColor(kMagenta);
  mLo->Draw();
  mHi->Draw();

  f=hD->GetFunction("gaus");
  f->SetLineColor(kRed);
  f->SetLineWidth(1.);

  double *par=f->GetParameters();
  double *epar=f->GetParErrors();
  float   pi0m=par[1]*1000; // in MeV
  float   epi0m=epar[1]*1000; // in MeV
  float   pi0s=par[2]*1000; // in MeV
  float   epi0s=epar[2]*1000; // in MeV

  printf("#2  mass/MeV=%.0f + / - %.0f , <br>sig/MeV %.0f + / - %.0f\n",pi0m,epi0m,pi0s,epi0s);


  if(grM==0) return;
  int etaB=atoi(hname.Data()+4);

  int n=grM->GetN();
  grM->SetPoint(n,etaB,pi0m);
  grM->SetPointError(n,0.5,epi0m);

  n=grS->GetN();
  grS->SetPoint(n,etaB,pi0s);
  grS->SetPointError(n,0.5,epi0s);

  ax=invm->GetXaxis();
  int bLo=ax->FindBin(xLo);
  int bHi=ax->FindBin(xHi);
  float nPi0=hD->Integral(bLo,bHi);
  float nBckg=hr->Integral(bLo,bHi) - nPi0;
  printf("etaB=%d nPi0=%.1f nBckg=%.1f\n",etaB,nPi0,nBckg);
  n=grN->GetN();
  grN->SetPoint(n,etaB,nPi0);
   grN->SetPointError(n,0.5,0.  );
  return;
  // TH1F* hR=(TH1F*)hr->Clone();
}

