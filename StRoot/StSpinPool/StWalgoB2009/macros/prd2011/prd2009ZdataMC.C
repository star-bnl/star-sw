
bool colorPlot;
TString iPathMC,iPathData;

void prd2009ZdataMC(bool x=false,TString y="/star/u/stevens4/wAnalysis/efficXsec/outEmb/gainUp2/",TString z="/star/data01/pwg/stevens4/wAnalysis/xSecPaper/sl11b/data/",float canvasScale=1.0){

  colorPlot=x;
  iPathMC=y;
  iPathData=z;

  char* core0="Ze+e-Interf";

  gStyle->SetOptStat(0);
  gStyle->SetOptDate(0);
  
  //load file
  TString fullInpName=iPathMC;  fullInpName+=core0;
  fullInpName+=".wana.hist.root";
  fd=new TFile(fullInpName);
  if(!fd->IsOpen()) {
    printf("ERROR: input histo file not found, quit\n",fullInpName.Data());
    return;
  } else {
    printf("Opened: %s\n",fullInpName.Data());
  }
 
  //Data MC comparison plot
  fdata=new TFile(Form("%srun9setABCD.wana.hist.root",iPathData.Data()));
  TCanvas *c=new TCanvas("zData/MC","zData/MC",500*canvasScale,300*canvasScale);
  c->SetTopMargin(0.05); c->SetBottomMargin(0.13);
  c->SetRightMargin(0.04); c->SetLeftMargin(0.08);
  gPad->SetGridy(false); gPad->SetGridx(false);
  if(!colorPlot) c->SetGrayscale();
  TH1F* data = (TH1F*)fdata->Get("_Z_ZmassUnlike");
  TH1F* mc = (TH1F*)fd->Get("_Z_ZmassUnlike"); mc->Sumw2();
  mc->Scale(13.18/531.9);
  
  //remove default lines (need to improve for final plot)
  TList *LxMC; TList *LxData;
  LxMC=mc->GetListOfFunctions(); LxMC->Delete();
  LxData=data->GetListOfFunctions(); LxData->Delete();
  
  mc->SetTitle(";m_{e^{#font[52]{+}}e^{#font[52]{#scale[1.2]{-}}}} (GeV/c^{2})      ; Counts");
  mc->SetLineStyle(2); mc->SetLineColor(kMagenta); 
  mc->SetLineWidth(2); mc->SetFillColor(0);
  mc->Rebin(4); mc->SetMaximum(13.99.); 
  mc->GetXaxis()->SetTitleOffset(0.9);
  mc->GetXaxis()->CenterTitle();
  mc->GetXaxis()->SetRangeUser(30,120);
  mc->GetXaxis()->SetTitleSize(0.06);
  mc->GetXaxis()->SetLabelSize(0.05);
  mc->GetYaxis()->SetTitleOffset(0.6);
  mc->GetYaxis()->CenterTitle();
  mc->GetYaxis()->SetTitleSize(0.06);
  mc->GetYaxis()->SetLabelSize(0.05);
  mc->Draw("h e");
  data->SetFillColor(0); data->SetLineWidth(2);
  data->Rebin(4);
  data->GetXaxis()->SetRangeUser(30,120);
  data->GetXaxis()->SetLabelSize(0.06);
  data->GetYaxis()->SetLabelSize(0.06);
  data->Draw("e same");

  TLegend *leg = new TLegend(0.2,0.72,0.4,0.87);
  leg->SetEntrySeparation(0.01);
  leg->SetMargin(0.2);
  leg->SetFillColor(0);
  leg->SetTextSize(0.05);
  leg->SetLineColor(0);
  leg->AddEntry(data," STAR 2009 Data","l");
  leg->AddEntry(mc," Z #rightarrow ee MC","l");
  leg->Draw();

  if(colorPlot) {
    c->Print("/star/u/stevens4/wAnalysis/xSecPaper/plots/color/zDataMCcolor.eps");
    c->Print("/star/u/stevens4/wAnalysis/xSecPaper/plots/color/zDataMCcolor.png");  
  }
  else {
    c->Print("/star/u/stevens4/wAnalysis/xSecPaper/plots/bw/zDataMCbw.eps");
    c->Print("/star/u/stevens4/wAnalysis/xSecPaper/plots/bw/zDataMCbw.png");
  }
  return;
}

