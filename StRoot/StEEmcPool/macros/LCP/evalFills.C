TFile *outH;
TH1F *hs[6];


evalFills(char *fillL="F2201", TString wrkDir="./") {
  wrkDir="/star/data04/sim/balewski/LcpRun2/maxEta1.4/";
  //fillL=" F2053  F2075  F2076  F2083 ";

  fillL=" F2053  F2075  F2076  F2083  F2095  F2102  F2105  F2110  F2116  F2127  F2132  F2134  F2135  F2136  F2147  F2153  F2161  F2162  F2175  F2178  F2181  F2185  F2187  F2192  F2193  F2196  F2201  F2207  F2208  F2212  F2216  F2222  F2235  F2246  F2251  F2257  F2258  F2266  F2269  F2275  F2277  F2281  F2289  F2290  F2301  F2303  F2304";

  gStyle->SetPalette(1,0);
  gStyle->SetOptStat(1000110);
  gROOT->LoadMacro("getPol.C");

  createHist("asyVer1.hist.root");

  outH->ls();
  
  char *fill=strtok(fillL," "); // init 'strtok'
  int nFill=0;
  do {
    printf("\n\n process fill %02d '%s' \n",nFill,fill);
    int xFill=atoi(fill+1);
    assert(xFill);
    if(xFill<2189) continue;

    //.................. access R1-All histos
    TString fname=wrkDir+"r"+fill+".hist.root";
    TFile * inpH=new TFile(fname);
    assert(inpH->IsOpen());
    if(!inpH->IsOpen()) {
      printf("#fail %s does not open, skip\n",fname.Data());
      continue;
    }
    //inpH->ls();
    nFill++;
    projectYield(inpH,xFill);
    inpH->Close();
    
  }while(fill=strtok(0," ")); 
  
  char *cut="evalFill";
    c=new TCanvas(cut,cut,400,500);
  //c=new TCanvas();
  c->Divide(1,3);

  c->cd(1);  
  plotPair(hs[1],hs[0]);
  //return;

  c->cd(2);  
  plotPair(hs[3],hs[2]);

  c->cd(3);  
  plotPair(hs[5],hs[4]);

  
  //  outH->Write();
  //outH->ls();
  
  return;
}


//==========================================
//==========================================
void projectYield(TFile *inp,int xFill){
  assert(inp->IsOpen());

  float P,Q,eP,eQ;
  getPol(xFill,P,Q,eP,eQ);
  float PQ, ePQ;
  PQ=P*Q;
  ePQ=PQ*sqrt(eP*eP/P/P +eQ*eQ/Q/Q);
  printf("%d--> P*Q=%f /- %f\n",xFill,PQ,ePQ);
  
  TH1F * h=(TH1F *)inp->Get("r1*All");
  assert(h);

  char *tit=h->GetTitle();
  int nEve=atoi(strstr(tit,"Neve=")+5);
  printf("tit='%s' %d\n",tit,nEve);

  // take module
  P=fabs(P);
  Q=fabs(Q);
  PQ=fabs(PQ);
  
  hs[0]->Fill(P,nEve);
  hs[1]->Fill(P,nEve*P*P);

  hs[2]->Fill(Q,nEve);
  hs[3]->Fill(Q,nEve*Q*Q);

  hs[4]->Fill(PQ,nEve);
  hs[5]->Fill(PQ,nEve*PQ*PQ);

  float minPol=0.07, minPol2=0.01;

  printf("#fill F%d %d %d %d %d\n",xFill,nEve, P>minPol, Q>minPol,PQ>minPol2);
}


//==========================================
//==========================================
void createHist(TString fname) {
  outH=new TFile(fname,"RECREATE");
  assert(outH->IsOpen());
  printf("save outH -->%s\n", fname.Data());

  //.......... N*P
  hs[0]=new TH1F("NvP","yiled vs. P",30,0,0.3);  
  hs[0]->SetLineColor(kBlue);
  hs[0]->SetLineStyle(2);

  hs[1]=new TH1F("NPPvP","",30,0,0.3); 
  hs[1]->SetXTitle("Magnitude of Polarization in Blue");
  hs[1]->SetYTitle("FOM = LCP yield * P * P");

  //........... N*Q
  hs[2]=new TH1F("NvQ","yield vs. Q",30,0,0.3);
  hs[2]->SetLineColor(kMagenta);
  hs[2]->SetLineStyle(2);
 
  hs[3]=new TH1F("NQQvQ","",30,0,0.3);  
  hs[3]->SetXTitle("Magnitude of Polarization in Yellow");
  hs[3]->SetYTitle("FOM = LCP yield * Q * Q");

  //........... N*PQ
  hs[4]=new TH1F("NvPQ","yield vs. P*Q",30,0,0.06);
  hs[4]->SetLineColor(kGreen);
  hs[4]->SetLineStyle(2);
 
  hs[5]=new TH1F("NPPQQvPQ","",30,0,0.06);  
  hs[5]->SetXTitle("Magnitude of Polarization Product  Blue*Yellow");
  hs[5]->SetYTitle("FOM = LCP yield * P * P * Q * Q");
 }


//==========================================
//==========================================
void plotPair(TH1F *h1,TH1F *h0 ) {
  h1->Draw();

  // edit fonts/sizes
  TAxis *ax =h1->GetYaxis(); 
  float ss=ax->GetTitleSize();
  //printf("ss=%f\n",ss);
  ax->SetTitleSize(2*ss);
  ax->SetTitleOffset(0.5);
  ax =h1->GetXaxis(); 
 ax->SetTitleSize(1.5*ss);
 ax->SetLabelSize(1.5*ss);
 ax->SetTitleOffset(0.7);
 

  // edit fonts/sizes DONE



  gPad->Update();
  //scale hint1 to the pad coordinates
  Float_t rightmax = 1.1*h0->GetMaximum();
  Float_t scale = gPad->GetUymax()/rightmax;
  h0->Scale(scale);
  h0->Draw("same");
   
  //draw an axis on the right side
  TGaxis *axis = new TGaxis(gPad->GetUxmax(),gPad->GetUymin(),
	      gPad->GetUxmax(), gPad->GetUymax(),0,rightmax,510,"-R");
  int col=h0->GetLineColor();
  axis->SetLineColor(col);
  
  axis->SetTextColor(col);
  axis->SetLabelColor(col);
  axis ->SetTitle("LCP yield");
 axis->SetTitleSize(2*ss);
 axis->SetTitleOffset(.5);

  axis->Draw();
  
  
  TPaveStats *st =( TPaveStats *)gPad->GetPrimitive("stats");
  st->SetX1NDC(0.35);
  st->SetX2NDC(0.5);
  st->SetY1NDC(0.7);
  st->SetY2NDC(1.);
}





