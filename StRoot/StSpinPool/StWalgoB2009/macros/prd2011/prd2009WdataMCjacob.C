
bool colorPlot;
TString iPathMC,iPathBkgd;

void prd2009WdataMCjacob(bool x=false,TString y="/star/u/stevens4/wAnalysis/efficXsec/outEmb/gainUp2/",TString z="/star/u/stevens4/wAnalysis/xSecPaper/background/",float canvasScale=1.0) {
  
  colorPlot=x;
  iPathMC=y;
  iPathBkgd=z;

  TCanvas *c=new TCanvas("aa","bb",500*canvasScale,500*canvasScale);
  if(!colorPlot) c->SetGrayscale();  
  
  TPad *cT = new TPad("padT","padT",0,0.5,1.,1.); cT->Draw();
  TPad *cB = new TPad("padB","padB",0,0,1.,0.5); cB->Draw();
    
  //both charges
  cT->cd();
  prd2009WdataMCjacobX("Wplus","pos",128.6,147.0,1,canvasScale); 
  cB->cd();
  prd2009WdataMCjacobX("Wminus","neg",385.0,74.0,2,canvasScale); 

  if(colorPlot) {
    c->Print("/star/u/stevens4/wAnalysis/xSecPaper/plots//color/wJacobColor.eps");
    c->Print("/star/u/stevens4/wAnalysis/xSecPaper/plots/color/wJacobColor.png");
  }
  else {
    c->Print("/star/u/stevens4/wAnalysis/xSecPaper/plots/bw/wJacobBW.eps");
    c->Print("/star/u/stevens4/wAnalysis/xSecPaper/plots/bw/wJacobBW.png");
  }
  return;

}

void prd2009WdataMCjacobX(string charge, string charge2,float wlumi,float max,int pad,float canvasScale) {

  gStyle->SetOptStat(00000); 
  gStyle->SetOptDate(0);
    
  //init files
  TFile *f; 
  f=new TFile(Form("%sbkgd_histos_%s_final.root",iPathBkgd.Data(),charge2)); assert(f->IsOpen());
  
  TFile *fmc = new TFile(Form("%s%s.wana.hist.root",iPathMC.Data(),charge)); assert(fmc->IsOpen());

  TH1F* hTau; 
  TH1F* hZ;     
  TH1F* hEemc;
  TH1F* hQcd;
  TH1F* hData;
  
#if 0
  if(charge=="Wplus"){
    hTau = (TH1F*)f->Get("mcclustPtBal_2");
    hZ   = (TH1F*)f->Get("zsig_bkgd2");     
    hEemc= (TH1F*)f->Get("eemc_bkgd2");     
    hQcd = (TH1F*)f->Get("new_bkgd");       
    hData= (TH1F*)f->Get("signal2");        
  } else if(charge=="Wminus"){
#endif
    
  hTau = (TH1F*)f->Get("tauhist_r");
  hZ   = (TH1F*)f->Get("zsig_bkgd2_r");     
  hEemc= (TH1F*)f->Get("eemc_bkgd2_r");     
  hQcd = (TH1F*)f->Get("new_bkgd_r");       
  hData= (TH1F*)f->Get("signal2_r");
  //}
  
  if(colorPlot) { int color[4]={kGreen,kBlue,kRed,kOrange}; }
  else { int color[4]={kYellow+2,kYellow+4,kYellow,kYellow+3}; }
  
  hTau->SetFillColor(color[0]); hTau->SetLineColor(color[0]);
  hZ->SetFillColor(color[1]);   hZ->SetLineColor(color[1]);
  hEemc->SetFillColor(color[2]); hEemc->SetLineColor(color[2]);
  hQcd->SetFillColor(color[3]);  hQcd->SetLineColor(color[3]);
  if(!colorPlot) { 
    hEemc->SetLineColor(color[3]); 
    hQcd->SetFillStyle(3344); hQcd->SetLineColor(1);
  }
  hData->SetLineWidth(2); 

  TH1F* hW   = fmc->Get("muclustPtBal"); hW->Scale(13.18/wlumi);
  TH1F *hW2  = new TH1F("muclustPtBal_2","; E_{T}^{e} (GeV); Counts",49,1,99);
  TH1F *hW4  = new TH1F("muclustPtBal_4","; E_{T}^{e} (GeV); Counts",24,3,99);
  //set axis range and offsets
  hW2->GetYaxis()->SetTitleOffset(1.25);
  hW2->GetXaxis()->SetRangeUser(10,60);
  hW4->GetYaxis()->SetTitleOffset(1.25);
  hW4->GetXaxis()->SetRangeUser(10,60);

  for (int i=1; i<=49; i++) { //repack in 2 GeV bins
    hW2->SetBinContent(i,hW->GetBinContent(2*i)+hW->GetBinContent(2*i+1));
  }
  for (int i=1; i<=24; i++) { //repack in 4 GeV bins
    hW4->SetBinContent(i,hW2->GetBinContent(2*i)+hW2->GetBinContent(2*i+1));
  }
  hW2->SetLineColor(6); hW2->SetLineStyle(2); hW2->SetLineWidth(2);
  hW4->SetLineColor(6); hW4->SetLineStyle(2); hW4->SetLineWidth(2);

  //plot everything
  if(pad==1){
    gPad->SetTopMargin(0.05); gPad->SetBottomMargin(0.0);
    gPad->SetLeftMargin(0.1); gPad->SetRightMargin(0.03);
  }
  if(pad==2){
    gPad->SetTopMargin(0.0); gPad->SetBottomMargin(0.15);
    gPad->SetLeftMargin(0.1); gPad->SetRightMargin(0.03);
  }
  gPad->SetGridy(false); gPad->SetGridx(false);
  THStack *hs = new THStack("hs",";E_{T}^{e} (GeV);Counts");
  hs->Add(hZ); hs->Add(hTau);  //electroweak background
  hs->Add(hEemc); hs->Add(hQcd); //sum together as "QCD"
  //if(charge=="Wplus") hs->Add(hW2); //W signal histo
  //else if(charge=="Wminus") 
  hs->Add(hW4); //W signal histo
 
  if(pad==1) hs->SetMinimum(0.001);
  hs->SetMaximum(max);
  hs->Draw();
  hs->GetXaxis()->SetTitleOffset(0.9);
  hs->GetXaxis()->SetRangeUser(10,60);
  hs->GetXaxis()->SetTitleSize(0.07);
  hs->GetXaxis()->SetLabelSize(0.06);
  hs->GetXaxis()->CenterTitle();
  hs->GetYaxis()->SetTitleOffset(0.65);
  hs->GetYaxis()->SetTitleSize(0.07);
  hs->GetYaxis()->SetLabelSize(0.06);
  hs->GetYaxis()->CenterTitle();
  
  hData->GetXaxis()->SetLabelSize(0.06);
  hData->GetYaxis()->SetLabelSize(0.06);
  hData->Draw("h e same");
  
  //hData->Draw();
  //hs->Draw("h e same");

#if 0
  gPad->SetGridy(false); gPad->SetGridx(false);
  TH1F* hDataBkgdSub;
  //if(charge=="Wplus") {
  //  hW2->Draw();
  //  hDataBkgdSub= (TH1F*)f->Get("signal_final3");
  //}
  //else if(charge=="Wminus") {
  hW4->Draw();
  hDataBkgdSub= (TH1F*)f->Get("signal_final3_r"); 
  hDataBkgdSub->SetLineWidth(2);
  //}
  hDataBkgdSub->Draw("h e same");
#endif
  
  //make legend
  float textSize=0.055; if(canvasScale>1.0) textSize=0.05;
  if(charge=="Wplus"){
    TLegend *leg = new TLegend(0.7,0.63,0.95,0.9);
    leg->SetEntrySeparation(0.01);
    leg->SetMargin(0.2);
    leg->SetFillColor(0);
    leg->SetTextSize(textSize);
    leg->SetLineColor(kWhite);
    leg->SetHeader("#scale[1.3]{Positron |#eta_{e}| < 1}");
    leg->AddEntry(hData," STAR 2009 Data","l");
    leg->AddEntry(hW2," W #rightarrow e #nu MC","l");
    leg->AddEntry(hQcd," Data-driven QCD","f");
    leg->Draw();
  }
  else {
    TLegend *leg = new TLegend(0.7,0.66,0.95,0.93);
    leg->SetEntrySeparation(0.01);
    leg->SetMargin(0.2);
    leg->SetFillColor(0);
    leg->SetTextSize(textSize);
    leg->SetLineColor(kWhite);
    leg->SetHeader("#scale[1.25]{Electron |#eta_{e}| < 1}");
    leg->AddEntry(hEemc," Second EEMC","f");
    leg->AddEntry(hTau," W #rightarrow #tau #nu MC","f");
    leg->AddEntry(hZ," Z #rightarrow ee MC","f");
    leg->Draw();
  }

  return;

  //insert text
  TLatex *lat0 = new TLatex(0.13,0.945,"Run 9 STAR #font[52]{p+p}  #sqrt{#font[72]{s}}_{ }=_{ }500 GeV");
  lat0->SetNDC(); lat0->SetTextSize(0.04); lat0->Draw("same");
  TLatex *lat2 = new TLatex(0.6,0.86,"#sigma(#font[52]{W^{#scale[1.2]{#pm}} #rightarrow e^{#scale[1.2]{#pm}} + #nu_{e}})");
  //lat2->SetNDC(); lat2->SetTextSize(0.042); lat2->Draw("same");
  
}

