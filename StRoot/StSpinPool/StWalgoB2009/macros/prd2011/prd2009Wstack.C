
bool colorPlot;
TString iPathData;

void prd2009Wstack(bool x=false,TString y="/star/data01/pwg/stevens4/wAnalysis/xSecPaper/sl11b/data/",float canvasScale=1.0){

  colorPlot=x;
  iPathData=y;
  
  gStyle->SetOptStat(00000); 
  gStyle->SetOptDate(0);
  
  //init files
  TFile *f; 
  f=new TFile(Form("%srun9setABCD.wana.hist.root",iPathData.Data())); assert(f->IsOpen());
  
  const int mxHist=5;
  TH1F* h[mxHist];
  TH1F *h2[mxHist]; 
  
  if(colorPlot){
    int lineColor[mxHist]={kBlack,kOrange,kRed,kGreen,kBlue};
    int fillColor[mxHist]={0,kOrange,kRed,kGreen,kBlue};
  }
  else {
    int lineColor[mxHist]={1,kGray,kGray+1,kGray+2,kGray+3};
    int fillColor[mxHist]={0,kGray,kGray+1,kGray+2,kGray+3};
  }

  for(int i=0; i<mxHist; i++){
    h[i]=(TH1F*)f->Get(Form("muETlive%d",i)); 
    
    //rebin histos
    h2[i]=new TH1F(Form("muETliveRebin%d",i),"; E_{T}^{e} (GeV); Counts      ",49,1,99);
    for (int j=1; j<=49; j++) //2 GeV bins
      h2[i]->SetBinContent(j,h[i]->GetBinContent(2*j)+h[i]->GetBinContent(2*j+1));
    
    h[i]->GetXaxis()->SetRangeUser(10,70);
    h[i]->GetXaxis()->CenterTitle();
    h[i]->SetFillColor(fillColor[i]); h[i]->SetLineColor(lineColor[i]);
    h[i]->GetYaxis()->SetTitleOffset(0.9);
    h[i]->GetYaxis()->CenterTitle();
    
    h2[i]->GetXaxis()->SetRangeUser(10,70);
    h2[i]->GetXaxis()->CenterTitle();
    h2[i]->SetFillColor(fillColor[i]); h2[i]->SetLineColor(lineColor[i]);
    h2[i]->GetYaxis()->SetTitleOffset(0.9);
    h2[i]->GetYaxis()->CenterTitle();
    
  }
  
  TCanvas *c=new TCanvas("aa","bb",500*canvasScale,300*canvasScale);
  c->SetTopMargin(0.05); c->SetBottomMargin(0.13); 
  c->SetLeftMargin(0.09); c->SetRightMargin(0.04);
  gPad->SetGridy(false); gPad->SetGridx(false);
  if(!colorPlot) c->SetGrayscale();
  gPad->SetLogy();
  h2[0]->Draw();
  h2[0]->SetMaximum(70000.);
  h2[0]->GetXaxis()->SetTitleOffset(0.9);
  h2[0]->GetXaxis()->CenterTitle();
  h2[0]->GetXaxis()->SetTitleSize(0.06);
  h2[0]->GetXaxis()->SetLabelSize(0.05);
  h2[0]->GetYaxis()->SetTitleOffset(0.67);
  h2[0]->GetYaxis()->CenterTitle();
  h2[0]->GetYaxis()->SetTitleSize(0.06);
  h2[0]->GetYaxis()->SetLabelSize(0.05);
  for(int i=1; i<mxHist; i++){
    h2[i]->GetXaxis()->SetLabelSize(0.06);
    h2[i]->GetYaxis()->SetLabelSize(0.06);
    h2[i]->Draw("same");
  }

  TLegend *leg = new TLegend(0.55,0.56,0.90,0.93);
  leg->SetEntrySeparation(0.04);
  leg->SetMargin(0.2);
  leg->SetFillColor(0);
  leg->SetTextSize(0.045);
  leg->SetLineColor(0);
  leg->AddEntry(h[0]," #splitline{Candidate track and BEMC}{   cluster reconstructed}","l"); 
  leg->AddEntry(h[1]," E_{T}^{e} > 15 GeV and |#Deltar| < 0.7 cm","f");
  leg->AddEntry(h[2]," E_{T}^{e}/E_{T}^{4#times4} > 0.95","f");
  leg->AddEntry(h[3]," E_{T}^{e}/E_{T}^{#DeltaR<0.7} > 0.88","f");
  leg->AddEntry(h[4]," Signed P_{T}- balance > 15 GeV/c","f");
  leg->Draw();

  if(colorPlot) {
    c->Print("/star/u/stevens4/wAnalysis/xSecPaper/plots/color/wStackColor.eps");
    c->Print("/star/u/stevens4/wAnalysis/xSecPaper/plots/color/wStackColor.png");
  }
  else {
    c->Print("/star/u/stevens4/wAnalysis/xSecPaper/plots/bw/wStackBW.eps");
    c->Print("/star/u/stevens4/wAnalysis/xSecPaper/plots/bw/wStackBW.png");
  }

  return;

}

