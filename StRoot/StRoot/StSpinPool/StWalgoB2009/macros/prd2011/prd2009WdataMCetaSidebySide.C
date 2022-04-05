TH1F *hBase;
TCanvas *c;

bool colorPlot;
TString iPathMC,iPathData;

void prd2009WdataMCetaSidebySide(bool x=false,TString y="/star/u/stevens4/wAnalysis/efficXsec/outEmb/gainUp2/",TString z="/star/data01/pwg/stevens4/wAnalysis/xSecPaper/sl11b/data/",float canvasScale=1.0){

  colorPlot=x;
  iPathMC=y;
  iPathData=z;

  c=new TCanvas("aa","bb",500*canvasScale,300*canvasScale);
  if(!colorPlot) c->SetGrayscale();

  TPad *cT = new TPad("padT","padT",0,0,0.5,1.); cT->Draw();
  TPad *cB = new TPad("padB","padB",0.5,0,1.,1.0); cB->Draw();
  //c->Divide(1,2); 
  
  //both charges
  cT->cd();
  prd2009WdataMCetaX("Wplus","pos",128.6,270.0,1,canvasScale); 
  cB->cd();
  prd2009WdataMCetaX("Wminus","neg",385.0,85.0,2,canvasScale); 
  
  if(colorPlot) {
    c->Print("/star/u/stevens4/wAnalysis/xSecPaper/plots/color/wEtaColor.eps");
    c->Print("/star/u/stevens4/wAnalysis/xSecPaper/plots/color/wEtaColor.png");
  }
  else {
    c->Print("/star/u/stevens4/wAnalysis/xSecPaper/plots/bw/wEtaBW.eps");
    c->Print("/star/u/stevens4/wAnalysis/xSecPaper/plots/bw/wEtaBW.png");
  }

  return;

}

void prd2009WdataMCetaX(string charge, string charge2,float wlumi,float max,int pad,float canvasScale) {

  gStyle->SetOptStat(0); 
  gStyle->SetOptDate(0);
  
  float lumi[4] = {13.18,wlumi,96.2,531.9};

  TFile *f[4]; TH2F *h[4];
  TString name[4]={"run9setABCD",Form("%s",charge),"Wtau","Ze+e-Interf"};
  TString path[4]={iPathData,iPathMC,iPathMC,iPathMC};
  for(int i=0; i<4; i++){
    f[i]=new TFile(Form("%s%s.wana.hist.root",path[i].Data(),name[i].Data())); assert(f[i]->IsOpen());
    h[i]=(TH2F*)f[i]->Get(Form("%s_muclustpTbal_wE_etaBin",charge2));
    h[i]->Scale(13.18/lumi[i]);
  }
  
  //c->cd(pad);
  if(pad==1){
    gPad->SetTopMargin(0.01); gPad->SetBottomMargin(0.15);
    gPad->SetLeftMargin(0.2); gPad->SetRightMargin(0.03);
  }
  if(pad==2){
    gPad->SetTopMargin(0.01); gPad->SetBottomMargin(0.15);
    gPad->SetLeftMargin(0.2); gPad->SetRightMargin(0.03);
  }
  gPad->SetGridy(false); gPad->SetGridx(false);
  initHisto();
  hBase->SetMinimum(0.0); hBase->SetMaximum(max);
  //if(pad==1) hBase->SetMinimum(0.001);
  hBase->GetXaxis()->SetTitleSize(0.07);
  hBase->GetXaxis()->SetTitleOffset(0.98);
  hBase->GetXaxis()->CenterTitle();
  hBase->GetXaxis()->SetNdivisions(4);
  hBase->GetXaxis()->SetLabelSize(0.06);
  hBase->GetYaxis()->CenterTitle();
  hBase->GetYaxis()->SetTitleSize(0.07);
  hBase->GetYaxis()->SetTitleOffset(1.3);
  hBase->GetYaxis()->SetLabelSize(0.06);
  hBase->GetYaxis()->SetLabelOffset(0.012);
  hBase->GetYaxis()->SetNdivisions(6);
  if(pad==2) {
    hBase->GetYaxis()->SetTitleOffset(1.2);
  }
  hBase->Draw();
  THStack *hs = new THStack("hs","test");
  
  //write histos with yields in each bin
  TH1F* hEta[6];

  //define QCD piece by hand from background macro output
  hEta[4]=(TH1F*)hBase->Clone(); hEta[4]->SetName("bin4");
  hEta[5]=(TH1F*)hBase->Clone(); hEta[5]->SetName("bin5");
  if(charge=="Wplus") {
    //second EEMC
    hEta[4]->SetBinContent(1,0.13); hEta[4]->SetBinContent(2,2.71);
    hEta[4]->SetBinContent(3,0.88); hEta[4]->SetBinContent(4,5.35);
    //QCD
    hEta[5]->SetBinContent(1,2.28); hEta[5]->SetBinContent(2,3.02);
    hEta[5]->SetBinContent(3,0.0);  hEta[5]->SetBinContent(4,2.00); 
  }
  else if(charge=="Wminus"){
    //second EEMC
    hEta[4]->SetBinContent(1,3.31);  hEta[4]->SetBinContent(2,3.28);
    hEta[4]->SetBinContent(3,0.21);  hEta[4]->SetBinContent(4,2.45);
    //QCD
    hEta[5]->SetBinContent(1,2.27);  hEta[5]->SetBinContent(2,2.34);
    hEta[5]->SetBinContent(3,0.82);  hEta[5]->SetBinContent(4,0.68);
  }
  
  //int style[5]={1,1001,3144,1001};
  if(colorPlot) { 
    int color[6]={1,kMagenta,kGreen,kBlue,kRed,kOrange}; 
    hEta[4]->SetLineColor(color[4]);
    hEta[4]->SetFillColor(color[4]);
    hEta[5]->SetLineColor(color[5]);
    hEta[5]->SetFillColor(color[5]);
  }
  else { 
    int color[6]={1,kMagenta,kYellow+2,kYellow+4,kYellow}; 
    hEta[4]->SetLineColor(1);    hEta[4]->SetFillColor(color[4]);//color[5]); 
    hEta[5]->SetFillStyle(3344); hEta[5]->SetLineColor(1); hEta[5]->SetFillColor(1);
  }
  
  
  float low[4]={-.99,-.49,0.01,0.51};
  float high[4]={-.51,-.01,0.49,0.99};
  for(int j=3; j>=0; j--) { //loop over samples
    hEta[j]=(TH1F*)hBase->Clone(); hEta[j]->SetName(Form("bin%d",j));
    for(int ieta=0; ieta<4; ieta++) { //loop over eta bins
      int etaLow=h[j]->GetXaxis()->FindBin(low[ieta]);
      int etaHigh=h[j]->GetXaxis()->FindBin(high[ieta]);
      float yield=h[j]->Integral(etaLow,etaHigh,26,100);
      //cout<<"etaLow="<<etaLow<<" etaHigh="<<etaHigh<<" yield="<<yield<<endl;
      hEta[j]->SetBinContent(ieta+1,yield);
      if(j==0) hEta[j]->SetBinError(ieta+1,sqrt(yield));
    }
    
    hEta[j]->SetLineColor(color[j]);
    if(j==1) hEta[j]->SetLineStyle(2);
    if(j>1) {
      hEta[j]->SetFillColor(color[j]); //don't fill data
      //hEta[j]->SetFillStyle(style[j]);
    }
    else
      hEta[j]->SetLineWidth(2);

    if(j>0) hs->Add(hEta[j]);
    
    if(j==2) {
      hs->Add(hEta[4]); //add EEMC on top of Z and Wtau
      hs->Add(hEta[5]); //add QCD on top of EEMC
    }
  }
  hs->Draw("same");
  hEta[0]->Draw("same e");

  //make legend
  float textSize=0.047; if(canvasScale>1.0) textSize=0.044;
  if(charge=="Wplus") {
    TLegend *leg = new TLegend(0.25,0.75,0.8,0.97);
    leg->SetEntrySeparation(0.01);
    leg->SetMargin(0.2);
    leg->SetFillColor(kWhite);
    leg->SetTextSize(textSize);
    leg->SetLineColor(kWhite);
    leg->SetHeader("#scale[1.25]{Positron E^{e}_{T}>25 GeV}");
    leg->AddEntry(hEta[0]," STAR 2009 Data","l");
    leg->AddEntry(hEta[1]," W #rightarrow e #nu MC","l");
    leg->AddEntry(hEta[5]," Data-driven QCD","f");
    leg->Draw();
  }
  else {
    TLegend *leg = new TLegend(0.25,0.75,0.8,0.97);
    leg->SetEntrySeparation(0.01);
    leg->SetMargin(0.2);
    leg->SetFillColor(kWhite);
    leg->SetTextSize(textSize);
    leg->SetLineColor(kWhite);
    leg->SetHeader("#scale[1.25]{Electron E^{e}_{T}>25 GeV}");
    leg->AddEntry(hEta[4]," Second EEMC","f");
    leg->AddEntry(hEta[2]," W #rightarrow #tau #nu MC","f");
    leg->AddEntry(hEta[3]," Z #rightarrow ee MC","f");
    leg->Draw();
  }

  return;

  

}

//------------------------------
void initHisto() {
  char  *txt0="h";
  hBase=new TH1F(txt0, "; #eta_{e}; Counts", 4, -1.,1.);
}
