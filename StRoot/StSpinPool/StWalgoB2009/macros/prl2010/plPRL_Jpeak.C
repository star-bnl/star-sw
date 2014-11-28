float thresET=25; // cut of for computations of yields & backg
TH1F *hRaw[5];
TH1F *hFinW[5];
int stage=3;

void plPRL_Jpeak(TString cCore="WAL_PRL_Jpeak") {
  TString  iPath="joeBackgMar18/";
  TString fullInpName=iPath+"bkgd_histos_pos_final.root";
  fdP=new TFile(fullInpName); assert(fdP->IsOpen());
  TString fullInpName=iPath+"bkgd_histos_neg_final.root";
  fdN=new TFile(fullInpName); assert(fdN->IsOpen());
  //fdP->ls();
  //fdN->ls();

    const Char_t *title = "W boson signal";
    c=new TCanvas(cCore,title,400,450); 
    setPadPaperSize(245.0); // column width, approx. 3.4 in = 245 pt
    c->Divide(1,2,0,0);

    c->cd(1);
    gPad->SetPad(0,(((245.0*c->GetWh()/c->GetWw())-26.0)*(0.5 + 0.5*(10.0/170.0)) + 26.0)/(245.0*c->GetWh()/c->GetWw()),1,1);
    setPadMarginWidthPoint(gPad, 32, 1, 1, 0, gPad);
    setPadLineWidthPoint(gPad, 0.5);

    c->cd(2);
    gPad->SetPad(0,0,1,(((245.0*c->GetWh()/c->GetWw())-26.0)*(0.5 + 0.5*(10.0/170.0)) + 26.0)/(245.0*c->GetWh()/c->GetWw()));
    setPadMarginWidthPoint(gPad, 32, 0, 1, (2.0*12.0) + 2, gPad);
    setPadLineWidthPoint(gPad, 0.5);

    c->cd(1);
    doJob1(fdN,"W^{-}",1); // W-
    c->cd(2);
    doJob1(fdP,"W^{+}",0); // W+
}

//===================
void doJob1(TFile *fd, TString charge, int q) {
  gStyle->SetPalette(1,0);
  gStyle->SetEndErrorSize(0);
  gStyle->SetFillStyle(0); 
  //float fs=0.06;
  //if(q==1)fs=0.07;

  TString fileEnd;
  if(q==1) fileEnd="_r";

  //error histograms
  TH1F *hRaw2= (TH1F*) fd->Get(Form("raw_stat_err2%s",fileEnd.Data())); assert(hRaw2);
  TH1F *hEemcBack2= (TH1F*) fd->Get(Form("eemc_stat_err2%s",fileEnd.Data())); assert(hEemcBack2);
  TH1F *hTauBack2= (TH1F*) fd->Get(Form("tau_stat_err2%s",fileEnd.Data())); assert(hTauBack2);
  TH1F *hQcdBack2= (TH1F*) fd->Get(Form("QCD_stat_err2%s",fileEnd.Data())); assert(hQcdBack2);

  hRaw[q]= (TH1F*) fd->Get(Form("signal2%s",fileEnd.Data())); assert(hRaw[q]);
  hFinW[q]= (TH1F*) fd->Get(Form("signal_final3%s",fileEnd.Data())); assert(hFinW[q]);
  hRaw[q]->SetAxisRange(8,74);
  if(q==0){
    hRaw[q]->SetMaximum(79.999);
    hRaw[q]->SetMinimum(-9.999);
  }
  else {
    hRaw[q]->SetMaximum(69.999);
    hRaw[q]->SetMinimum(-9.999);
  }
  hRaw[q]->SetStats(false);
  hRaw[q]->Draw("h"); 
  zeroline = new TLine(hRaw[q]->GetXaxis()->GetBinLowEdge(hRaw[q]->GetXaxis()->GetFirst()), 0, hRaw[q]->GetXaxis()->GetBinUpEdge(hRaw[q]->GetXaxis()->GetLast()), 0);
  zeroline->SetLineColor(kBlack);
  setLineWidthPoint(zeroline, 0.5);
  zeroline->Draw();
  if(stage < 3) {
    setLineWidthPoint(hRaw[q], 0.5);
  }
  h=hRaw[q];

        h->UseCurrentStyle();
        stripCmdHist(h);
        setHistFontSize(h, 10.0);
        setLineWidthPoint(h, 0.5);
        setHistTickLength(h, 4.0, 4.0);
        setHistTitleLabelOffset(h, 2.0*12.0 - 3.0, 23, 12.0 - 2.0, 4.5 - 2.0);


  TH1F * hBack=(TH1F*) hRaw[q]->Clone(); hBack->SetNameTitle("totBackg", "totBackg");

  hBack->Add(hFinW[q],-1.);
  setLineWidthPoint(hBack, 0.5);
  setMarkerSizePoint(hBack, 3.5);
  //  hx=(TH1F*) hBack->Clone();
  hBack->SetLineColor(kBlue);
  hBack->SetMarkerColor(kBlue);
    

  //draw histograms
  hFinW[q]->SetMarkerStyle(8);
  hFinW[q]->SetLineColor(kBlack);
  hFinW[q]->SetFillColor(kYellow);
  setMarkerSizePoint(hFinW[q], 3.5);
  setLineWidthPoint(hFinW[q], 0.5);
  if(stage > 2)  hFinW[q]->Draw("same h");
  float startLine=9;
  if(q==1) startLine=3;
 
  if(stage > 1) {
    hBack->Draw("h  same");
  }

  //  hx->Draw("e same");
  // ET-cut lines
  float yy=50;
  if(q)yy=38;

  ln=new TLine(25,0.,25,yy);  ln->SetLineColor(kRed);  ln->SetLineStyle(2);
  setLineWidthPoint(ln, 1);  ln->Draw();

  ln=new TLine(50,0.,50,yy);
  ln->SetLineColor(kRed);
  ln->SetLineStyle(2);
  setLineWidthPoint(ln, 1);
  ln->Draw();

  //set stat errors
  int startbin=8;
  if(q==1) startbin=4;
  float xpos[50],ypos[50],err[50];
  for(int ibin=startbin;ibin<hRaw[q]->GetNbinsX();ibin++){
    float eemcBack2=hEemcBack2->GetBinContent(ibin);
    float tauBack2=hTauBack2->GetBinContent(ibin);
    float qcdBack2=hQcdBack2->GetBinContent(ibin);
    float rawErr2=hRaw[q]->GetBinContent(ibin);
    float backToterr=sqrt(eemcBack2+tauBack2+qcdBack2);
    float finToterr=sqrt(eemcBack2+tauBack2+qcdBack2+rawErr2);
    hBack->SetBinError(ibin,backToterr);
    xpos[ibin-startbin]=hBack->GetBinCenter(ibin);
    ypos[ibin-startbin]=hFinW[q]->GetBinContent(ibin);
    err[ibin-startbin]=finToterr;
  }
  TGraphErrors *errors = new TGraphErrors(50,xpos,ypos,0,err);
  errors->SetMarkerStyle(8);

  setMarkerSizePoint(errors, 3.5);
  errors->SetLineColor(kBlack);
  if(stage > 2) errors->Draw("p");


  lg=new TLegend(0.72,0.35,0.98,0.82); // top
  if(!q) lg=new TLegend(0.72,0.50,0.98,0.82); // top
  if(q==0) lg->SetHeader(" Positron #font[122]{|}_{_{ }}#eta_{_{ }}#font[122]{|}_{ }_{_{ }}<_{ }1");
  else     lg->SetHeader(" Electron #font[122]{|}_{_{ }}#eta_{_{ }}#font[122]{|}_{ }_{_{ }}<_{ }1");
  if(q) {
    lg->AddEntry(hRaw[q],"Candidates","lf");
    lg->AddEntry(hBack,"Backg. est.","l e");
    lg->AddEntry(hFinW[q],"Signal","lfep");
  }
  lg->SetFillColor(kWhite);
  lg->SetLineColor(kWhite);
  lg->SetShadowColor(kWhite);
  lg->Draw();
  stripCmdLegend(lg);
  setTextFontSize(lg, 10.0);
  

  if(q==0){
    TLatex *lat2 = new TLatex(0.36,0.88,SYMBOL_pplusp + "_{ }#rightarrow_{ }#font[12]{W}^{#font[122]{+}}^{}#rightarrow_{ }#font[12]{e}^{#font[122]{+}}_{ }#font[122]{+}_{ }#nu_{#font[12]{e}}");
    stripCmdLatex(lat2);
    setTextFontSize(lat2, 10.0);
  }
  else{
    TLatex *lat2 = new TLatex(0.36,0.88,SYMBOL_pplusp + "_{ }#rightarrow_{ }#font[12]{W}^{#font[122]{-}}^{}#rightarrow_{ }#font[12]{e}^{#font[122]{-}}_{ }#font[122]{+}_{ }#bar{#nu}_{#font[12]{e}}");
    setLineWidthPoint(lat2, 0.5);
    stripCmdLatex(lat2);
    setTextFontSize(lat2, 10.0);
  }

  lat2->SetNDC(); lat2->Draw("same");


  if(q){ 
    tx=new TLatex(14,3,"Software threshold");     tx->Draw();
    tx->SetTextAngle(90);  stripCmdLatex(tx);  setTextFontSize(tx, 10.0);
  }

  if(q){ 
    tx=new TLatex(58,61,"STAR 2009");     tx->Draw();
    stripCmdLatex(tx);  setTextFontSize(tx, 10.0);
  }

  if (q==0) {
    hRaw[q]->SetTitle("; E^{e}_{T} (" + SYMBOL_GeV + ");Counts");
  } else
    hRaw[q]->SetTitle(";;Counts");
  hRaw[q]->GetXaxis()->CenterTitle(true);
  hRaw[q]->GetYaxis()->CenterTitle(true);


  ln=new TLine(7.5,0.,75,0);    setLineWidthPoint(ln, 0.5);  ln->Draw();

    
}

//------------------
float getSum(TH1F* h) {
  h->SetAxisRange(thresET,100);
  float sum=h->Integral();
   h->SetAxisRange(0,100);
   return sum;
}
