float thresET=25; // cut of for computations of yields & backg
TH1F *hRaw[5];
TH1F *hFinW[5];
int stage=3;

void plWjacob_APS() {
  
  gStyle->SetOptDate(0);

  TString  iPath="";
  TString fullInpName=iPath+"bkgd_histos_pos_output4.root";
  fdP=new TFile(fullInpName); assert(fdP->IsOpen());
  TString fullInpName=iPath+"bkgd_histos_neg_4GeV_output4.root";
  fdN=new TFile(fullInpName); assert(fdN->IsOpen());
  //fdP->ls();
  //fdN->ls();

  c=new TCanvas("aa","aa",900,450);
  c->Divide(2,1);
  c->SetFillColor(kWhite);

  c->cd(2); doJob1(fdP,"W^{#scale[1.5]{+}}",0); // W+
  c->cd(1); doJob1(fdN,"W^{#scale[1.5]{-}}",1); // W-

}

//===================
void doJob1(TFile *fd, TString charge, int q) {
  gStyle->SetPalette(1,0);
  gStyle->SetEndErrorSize(0);
    
  gPad->SetGridy(false);
  gPad->SetGridx(false);

  TString fileEnd;
  if(q==1) fileEnd="_r";

  //error histograms
  TH1F *hRaw2= (TH1F*) fd->Get(Form("raw_stat_err2%s",fileEnd.Data())); assert(hRaw2);
  TH1F *hEemcBack2= (TH1F*) fd->Get(Form("eemc_stat_err2%s",fileEnd.Data())); assert(hEemcBack2);
  TH1F *hTauBack2= (TH1F*) fd->Get(Form("tau_stat_err2%s",fileEnd.Data())); assert(hTauBack2);
  TH1F *hQcdBack2= (TH1F*) fd->Get(Form("QCD_stat_err2%s",fileEnd.Data())); assert(hQcdBack2);

  hRaw[q]= (TH1F*) fd->Get(Form("signal2%s",fileEnd.Data())); assert(hRaw[q]);
  hFinW[q]= (TH1F*) fd->Get(Form("signal_final3%s",fileEnd.Data())); assert(hFinW[q]);
  hRaw[q]->SetAxisRange(0,70);
  if(q==0){
    hRaw[q]->SetMaximum(99.9);
    hRaw[q]->SetMinimum(-10);
  }
  else {
    hRaw[q]->SetMaximum(99.9);
    hRaw[q]->SetMinimum(-10);
  }
  hRaw[q]->SetStats(false);
  hRaw[q]->Draw("h");
  if(stage < 3) hRaw[q]->SetLineWidth(2);
    
  TH1F * hBack=(TH1F*) hRaw[q]->Clone(); hBack->SetNameTitle("totBackg", "totBackg");
  hBack->Add(hFinW[q],-1.);
  hBack->SetLineColor(kBlue);
  hBack->SetLineWidth(2);
  hBack->SetMarkerStyle(8);
  hBack->SetMarkerColor(kBlue);
  hBack->SetMarkerSize(0.8);
    
  //draw histograms
  hFinW[q]->SetMarkerStyle(8); hFinW[q]->SetMarkerSize(0.8); hFinW[q]->SetLineColor(kBlack);
  hFinW[q]->SetFillColor(kYellow);
  hFinW[q]->SetLineWidth(2);
  if(stage > 2)  hFinW[q]->Draw("same h");
  float startLine=1;
  if(q==1) startLine=3;
  ln=new TLine(startLine,0,71,0); ln->Draw();
  if(stage > 1) {
    hBack->Draw("h e same");
    ln2=new TLine(startLine,0,15,0); ln2->SetLineWidth(2); ln2->Draw();
  }
  
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
  errors->SetMarkerSize(0.8); errors->SetLineColor(kBlack);
  if(stage > 2) errors->Draw("p");

  lg=new TLegend(0.6,0.62,0.88,0.78); // top
  //TDatime dt;lg->SetHeader(dt.AsString());
  if(q==0) lg->SetHeader("      positron |#eta|_{ }<_{ }1");
  else lg->SetHeader("      electron |#eta|_{ }<_{ }1");
  lg->AddEntry(hRaw[q],Form("#font[52]{%s} candidates",charge.Data()),"l");
  lg->AddEntry(hBack,"Backg. est.","lfep");
  lg->AddEntry(hFinW[q],Form("Backg. subtr. #font[52]{%s} ",charge.Data()),"lfep");
  lg->SetFillColor(kWhite);
  lg->SetLineColor(kWhite);
  lg->Draw();

  TLatex *lat1 = new TLatex(0.15,0.84,"Run 9 STAR Preliminary  #sqrt{#font[72]{s}}_{ }=_{ }500 GeV");
  lat1->SetNDC();  lat1->SetTextSize(0.045); lat1->Draw("same");
  if(q==0){
    TLatex *lat2 = new TLatex(0.48,0.79,"#font[52]{p+p #rightarrow W^{#scale[1.2]{+}} #rightarrow e^{#scale[1.2]{+}} + #nu_{e}}");
    lat2->SetNDC(); lat2->Draw("same");
  }
  else{
    TLatex *lat2 = new TLatex(0.48,0.79,"#font[52]{p+p #rightarrow W^{#scale[1.2]{-}} #rightarrow e^{#scale[1.2]{-}} + #bar{#nu}_{e}}");
    lat2->SetNDC(); lat2->Draw("same");
  }

  if(max==100) tx=new TText(14,20,"software threshold"); 
  else tx=new TText(14,10,"software threshold");
  tx->SetTextAngle(90);
  tx->SetTextSize(0.03);//tx->SetTextColor(kMagenta);
  tx->Draw();

  char tt[100]; sprintf(tt,"; EMC cluster  #font[72]{E_{T}} (GeV)       ;Counts",charge.Data());
  hRaw[q]->GetYaxis()->SetTitleOffset(1.25);
  hRaw[q]->SetTitle(tt);
  //hRaw[q]->SetLineWidth(2); 

  // .... compute integrals
  //float rawSum=getSum(hRaw[q]);
  //float signSum=getSum(hFinW[q]);
  //float backSum=getSum(hBack);
  //printf(" sum ET>25 : raw=%.0f signal=%.0f  back=%.0f  S+B/S=%.2f\n", rawSum,signSum, backSum,rawSum/signSum);
    
}

//------------------
float getSum(TH1F* h) {
  h->SetAxisRange(thresET,100);
  float sum=h->Integral();
   h->SetAxisRange(0,100);
   return sum;
}
