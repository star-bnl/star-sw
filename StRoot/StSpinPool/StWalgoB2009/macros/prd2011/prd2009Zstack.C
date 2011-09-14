
bool colorPlot;
TString iPathData;

void prd2009Zstack(bool x=false,TString y="/star/data01/pwg/stevens4/wAnalysis/xSecPaper/sl11b/data/", float canvasScale=1.0){

  colorPlot=x;
  iPathData=y;
  
  gStyle->SetOptStat(00000); 
  gStyle->SetOptDate(0);
  
  //init files
  TFile *f; 
  f=new TFile(Form("%srun9setABCD.wana.hist.root",iPathData.Data())); assert(f->IsOpen());

  //mc file for testing
  //f=new TFile("/star/u/stevens4/wAnalysis/efficXsec/wana/sl11b/Ze+e-Interf.wana.hist.root"); assert(f->IsOpen());
  
  const int mxHist=4;
  TH1F* h[mxHist];
  
  if(colorPlot){
    int lineColor[mxHist]={kBlack,kGreen,kRed,kBlue};
    int fillColor[mxHist]={0,kGreen,kRed,kBlue};
  }
  else {
    int lineColor[mxHist]={1,kGray+2,kGray,kGray+3};
    int fillColor[mxHist]={0,kGray+2,kGray,kGray+3};
  }

  for(int i=0; i<mxHist; i++){
    int j=i; 
    if(i==1) j=2; if(i==2) j=1; //remove after re-run data
    h[i]=(TH1F*)f->Get(Form("_Z_ZmassUnlike%d",j)); 
    //h[i]=(TH1F*)f->Get(Form("_Z_ZmassLike%d",j)); 
    h[i]->Rebin();
    h[i]->SetTitle("; m_{#scale[1.3]{e^{#font[52]{+}}e^{#font[52]{#scale[1.2]{-}}}}} (GeV/c^{2}); Counts      ");

    h[i]->GetXaxis()->SetRangeUser(25,125);
    h[i]->GetXaxis()->CenterTitle();
    h[i]->SetFillColor(fillColor[j]); h[i]->SetLineColor(lineColor[j]);
    h[i]->GetYaxis()->SetTitleOffset(0.9);
    h[i]->GetYaxis()->CenterTitle();
  }
  
  TCanvas *c=new TCanvas("aa","bb",500*canvasScale,300*canvasScale);
  c->SetTopMargin(0.05); c->SetBottomMargin(0.13); 
  c->SetLeftMargin(0.08); c->SetRightMargin(0.04);
  gPad->SetGridy(false); gPad->SetGridx(false);
  if(!colorPlot) c->SetGrayscale();
  gPad->SetLogy();
  h[0]->Draw();
  h[0]->GetXaxis()->SetTitleOffset(0.9);
  h[0]->GetXaxis()->CenterTitle();
  h[0]->GetXaxis()->SetTitleSize(0.06);
  h[0]->GetXaxis()->SetLabelSize(0.05);
  h[0]->GetYaxis()->SetTitleOffset(0.6);
  h[0]->GetYaxis()->CenterTitle();
  h[0]->GetYaxis()->SetTitleSize(0.06);
  h[0]->GetYaxis()->SetLabelSize(0.05);
  for(int i=1; i<mxHist; i++){
    h[i]->GetXaxis()->SetLabelSize(0.06);
    h[i]->GetYaxis()->SetLabelSize(0.06);
    h[i]->Draw("same");
  }

  TLegend *leg = new TLegend(0.55,0.6,0.90,0.93);
  leg->SetEntrySeparation(0.01);
  leg->SetMargin(0.2);
  leg->SetFillColor(0);
  leg->SetTextSize(0.045);
  leg->SetLineColor(0);
  leg->AddEntry(h[0]," #splitline{Candidate track and BEMC}{   cluster reconstructed}","l");
  leg->AddEntry(h[1]," E_{T}^{e} > 15 GeV and |#Deltar| < 0.7 cm","f");
  leg->AddEntry(h[2]," E_{T}^{e}/E_{T}^{4#times4} > 0.95","f");
  leg->AddEntry(h[3]," E_{T}^{e}/E_{T}^{#DeltaR<0.7} > 0.88","f");
  leg->Draw();

  string like="";
  if(colorPlot) {
    c->Print(Form("/star/u/stevens4/wAnalysis/xSecPaper/plots/color/zStack%sColor.eps",like));
    c->Print(Form("/star/u/stevens4/wAnalysis/xSecPaper/plots/color/zStack%sColor.png",like));
  }
  else {
    c->Print(Form("/star/u/stevens4/wAnalysis/xSecPaper/plots/bw/zStack%sBW.eps",like));
    c->Print(Form("/star/u/stevens4/wAnalysis/xSecPaper/plots/bw/zStack%sBW.png",like));
  }

  return;

}

