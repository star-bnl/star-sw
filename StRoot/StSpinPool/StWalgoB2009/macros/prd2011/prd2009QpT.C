
bool colorPlot;
TString iPathData;

void  prd2009QpT(bool x=false,TString y="/star/data01/pwg/stevens4/wAnalysis/xSecPaper/sl11b/data/",float canvasScale=1.0){

  TString cCore="Wprd_QPT";
  
  colorPlot=x;
  iPathData=y;

  TString dataFinalRoot=iPathData+"run9setABCD.wana.hist.root";
  TFile* fd=0;
  fd=new TFile(dataFinalRoot); assert(fd->IsOpen());
  
  gStyle->SetFillStyle(0);
  gStyle->SetPalette(1,0);
  gStyle->SetOptStat(0);
  gStyle->SetOptDate(0);
  const Char_t *title = "High-energy electron charge separation with TPC";
  c=new TCanvas(cCore,title,500*canvasScale,300*canvasScale);
  c->SetTopMargin(0.05); c->SetBottomMargin(0.15);
  c->SetRightMargin(0.04); c->SetLeftMargin(0.08);
  gPad->SetGridy(false); gPad->SetGridx(false);
  if(!colorPlot) c->SetGrayscale();

  h=pubchRecPNp;
  //h->Draw("colz");

  TH1F* qPt_1D = (TH1F*)h->ProjectionY("name",26,100)->Clone(); 
  qPt_1D->Rebin(); qPt_1D->SetLineWidth(2); 
  qPt_1D->SetTitle("; Lepton charge sign #times 1/p_{T} (c/GeV); Counts");
  qPt_1D->SetMinimum(0.001);
  qPt_1D->GetXaxis()->CenterTitle();
  qPt_1D->GetXaxis()->SetTitleOffset(1.1);
  qPt_1D->GetXaxis()->SetTitleSize(0.06);
  qPt_1D->GetXaxis()->SetLabelSize(0.05);
  qPt_1D->GetYaxis()->CenterTitle();
  qPt_1D->GetYaxis()->SetTitleOffset(0.65);
  qPt_1D->GetYaxis()->SetTitleSize(0.06);
  qPt_1D->GetYaxis()->SetLabelSize(0.05);
  qPt_1D->Draw();

  //add hatched histo for opposite charges
  TH1F* minusHatch = (TH1F*)qPt_1D->Clone();
  TH1F* plusHatch = (TH1F*)qPt_1D->Clone();
  minusHatch->GetXaxis()->SetRangeUser(-0.1,-0.001);
  minusHatch->SetFillStyle(3554); minusHatch->SetFillColor(kBlue);
  minusHatch->Draw("same");
  plusHatch->GetXaxis()->SetRangeUser(0.001,0.1);
  plusHatch->SetFillStyle(3545); plusHatch->SetFillColor(kGreen);
  plusHatch->Draw("same");
  
    
  TLegend *leg = new TLegend(0.15,0.68,0.35,0.83);
  leg->SetEntrySeparation(0.01);
  leg->SetMargin(0.2);
  leg->SetFillColor(0);
  leg->SetTextSize(0.05);
  leg->SetLineColor(0);
  string extra=""; if(canvasScale>1.0) extra=" ";
  leg->AddEntry(qPt_1D,Form("#splitline{    STAR 2009 Data}{ E_{T}^{e}>25 GeV and |%s#eta_{e}|<1}",extra),"");
  leg->Draw("same");

  if(colorPlot) {
    c->Print("/star/u/stevens4/wAnalysis/xSecPaper/plots/color/qPTcolor.eps");
    c->Print("/star/u/stevens4/wAnalysis/xSecPaper/plots/color/qPTcolor.png");
  }  
  else {
    c->Print("/star/u/stevens4/wAnalysis/xSecPaper/plots/bw/qPTBW.eps");
    c->Print("/star/u/stevens4/wAnalysis/xSecPaper/plots/bw/qPTBW.png");
  }
  return;
    
}


