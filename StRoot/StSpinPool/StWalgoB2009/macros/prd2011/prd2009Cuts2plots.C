
bool colorPlot;
TString iPathMC,iPathData;

void prd2009Cuts2plots(bool x=false,TString y="/star/u/stevens4/wAnalysis/efficXsec/outEmb/gainUp2/",TString z="/star/data01/pwg/stevens4/wAnalysis/xSecPaper/sl11b/data/",float canvasScale=1.0){

  colorPlot=x;
  iPathMC=y;
  iPathData=z;
   
  TString cCore="WprdCuts";
  TString dataFinalRoot=iPathData+"run9setABCD.wana.hist.root";
  TString mcFinalRoot=iPathMC+"Wplus.wana.hist.root";
  TFile* fd=0;
  fd=new TFile(dataFinalRoot); assert(fd->IsOpen()); assert(fd->IsOpen());
  fdmc=new TFile(mcFinalRoot); assert(fd->IsOpen());  assert(fdmc->IsOpen());
  //gStyle->SetFillStyle(0);
  gStyle->SetPalette(1,0);
  gStyle->SetOptStat(0); 
  gStyle->SetOptDate(0);

  const Char_t *title = "Selection of W events";

  c=new TCanvas(cCore,title,500*canvasScale,300*canvasScale);
  if(!colorPlot) {
    c->SetGrayscale();
    //define new color pallete 
    const Int_t Number = 2;
    Double_t Red[Number]    = { 0.9, 0.00};
    Double_t Green[Number]  = { 0.9, 0.00};
    Double_t Blue[Number]   = { 0.9, 0.00};
    Double_t Length[Number] = { 0.00, 1.00};
    Int_t nb=50;
    TColor::CreateGradientColorTable(Number,Length,Red,Green,Blue,nb);
  }
    

  float xx1=0.5175; 
  TPad *cL = new TPad("padD", "apdD",0,0,xx1,1.);  cL->Draw();  
  TPad *cR = new TPad("padD", "apdD",xx1,0,1.,1.);  cR->Draw();
  
  // signed Pt-balance
  cL->cd(); 
  gPad->SetGridy(false); gPad->SetGridx(false);
  gPad->SetRightMargin(0.0); gPad->SetTopMargin(0.01);
  gPad->SetBottomMargin(0.12);
  gPad->SetLeftMargin(0.18);
  h2=(TH2F*)fd->Get("musPtBalance_clust");
  //h2->Draw("box");
  //h2->Draw("cont1");
  h2->Draw("col");
  h2->Rebin2D(); //h2->SetMaximum(100);
  h2->SetAxisRange(8,70,"x");h2->SetAxisRange(-40,60,"y");
  h2->GetXaxis()->SetNdivisions(5);  
  h2->SetTitle("; E^{e}_{T} (GeV); Signed P_{T}- balance (GeV/c)    ");
  h2->GetXaxis()->CenterTitle(true); 
  h2->GetXaxis()->SetLabelSize(0.05);
  h2->GetXaxis()->SetTitleSize(0.06); 
  h2->GetXaxis()->SetTitleOffset(0.9);
  h2->GetYaxis()->CenterTitle(true);
  h2->GetYaxis()->SetLabelSize(0.05);
  h2->GetYaxis()->SetTitleSize(0.06);
  h2->GetYaxis()->SetTitleOffset(1.2);

  TList *list=h2->GetListOfFunctions(); // to capture old
  TLine* ln = (TLine*)list->At(0);
  ln->SetX1(8);ln->SetX2(72); ln->SetLineStyle(2); ln->SetLineWidth(1.5);  
  tx=new TLatex(54,-33,"STAR 2009 Data");   tx->Draw();
  tx->SetTextAlign(21);  

  cR->cd(); 
  gPad->SetGridy(false); gPad->SetGridx(false);
  gPad->SetRightMargin(0.12); gPad->SetTopMargin(0.01);
  gPad->SetBottomMargin(0.12);
  gPad->SetLeftMargin(0.0);
  h2mc=(TH2F*)fdmc->Get("musPtBalance_clust");
  //h2mc->Draw("box");
  //h2mc->Draw("cont1");
  h2mc->Draw("colz");
  h2mc->Rebin2D(); h2mc->Scale(h2->GetMaximum()/h2mc->GetMaximum());
  h2mc->SetAxisRange(8,70,"x");h2mc->SetAxisRange(-40,60,"y");
  h2mc->GetXaxis()->SetNdivisions(5);  
  h2mc->SetTitle("; E^{e}_{T} (GeV);");

  h2mc->GetXaxis()->CenterTitle(true); 
  h2mc->GetXaxis()->SetLabelSize(0.0518);
  h2mc->GetXaxis()->SetTitleSize(0.062); 
  h2mc->GetXaxis()->SetTitleOffset(0.9);
  h2mc->GetYaxis()->CenterTitle(true);
  h2mc->GetYaxis()->SetLabelSize(0);
  h2mc->GetYaxis()->SetLabelColor(0);
  h2mc->GetYaxis()->SetTitleSize(0.062);
  h2mc->GetYaxis()->SetTitleOffset(1.15);
  
  TList *list=h2mc->GetListOfFunctions(); // to capture old
  TLine* ln = (TLine*)list->At(0);
  ln->SetX1(8);ln->SetX2(72); ln->SetLineStyle(2); ln->SetLineWidth(1.5); 
  tx=new TLatex(58,-33,"W #rightarrow e#nu MC");   tx->Draw();
  tx->SetTextAlign(21);
  
  if(colorPlot) {
    c->Print("/star/u/stevens4/wAnalysis/xSecPaper/plots/color/sPtBalcutColor.eps");
    c->Print("/star/u/stevens4/wAnalysis/xSecPaper/plots/color/sPtBalcutColor.png");
  }
  else {
    c->Print("/star/u/stevens4/wAnalysis/xSecPaper/plots/bw/sPtBalcutBW.eps");
    c->Print("/star/u/stevens4/wAnalysis/xSecPaper/plots/bw/sPtBalcutBW.png");
  }

  
  c2=new TCanvas("isoCuts","isoCuts",500*canvasScale,250*canvasScale);
  if(!colorPlot) c2->SetGrayscale();
  
  float xx2=0.51;
  TPad *cL2 = new TPad("padL2", "padL2",0,0,xx2,1.);    
  TPad *cR2 = new TPad("padR2", "padR2",xx2,0,1,1.); 
  cL2->Draw();
  cR2->Draw();

  //======================  2x2/4x4
  cL2->cd();
  gPad->SetGridy(false); gPad->SetGridx(false);
  gPad->SetBottomMargin(0.16);gPad->SetTopMargin(0.02);
  gPad->SetLeftMargin(0.2); gPad->SetRightMargin(0.00);
  h=(TH1F*)fd->Get("muBclET24R"); h->Draw();
  h->SetLineWidth(1.);
  hmc=(TH1F*)fdmc->Get("muBclET24R"); hmc->Draw("same");
  hmc->SetFillColor(93);  hmc->Scale(0.7); hmc->SetLineColor(93);
  
  h->SetTitle(";E^{e}_{T} / E^{4#times4}_{T} ;Counts  ");
  h->GetYaxis()->SetNdivisions(5);   
  h->GetYaxis()->CenterTitle(true); 
  h->GetYaxis()->SetTitleSize(0.07); 
  h->GetYaxis()->SetTitleOffset(1.4);
  h->GetYaxis()->SetLabelSize(0.06);
  h->GetXaxis()->SetNdivisions(3);
  h->GetXaxis()->CenterTitle(true); 
  h->GetXaxis()->SetTitleSize(0.07);
  h->GetXaxis()->SetTitleOffset(1.0);
  h->GetXaxis()->SetRangeUser(0.02,1.1);
  h->GetXaxis()->SetLabelSize(0.06);
      
  list=h->GetListOfFunctions(); // to capture old
  hmc->GetListOfFunctions()->Delete();
  ln = (TLine*)list->At(0);
  ln->SetY2(h->GetMaximum()*1.02);ln->SetLineStyle(2); ln->SetLineWidth(1.5); 
  lnclone = (TLine*)ln->Clone(); lnclone->Draw("same");
  TLine *lnAxis = new TLine(0,0,1.1,0);
  lnAxis->Draw("same");

  TLegend *leg = new TLegend(0.3,0.68,0.6,0.85);
  leg->SetEntrySeparation(0.01);
  //leg->SetMargin(0.2);
  leg->SetFillColor(0);
  leg->SetTextSize(0.058);
  leg->SetLineColor(0);
  leg->AddEntry(h," STAR 2009 Data","l");
  leg->AddEntry(hmc," W #rightarrow e#nu MC","f");
  leg->Draw();

  //======================  2x2/near
  cR2->cd(); 
  gPad->SetGridy(false); gPad->SetGridx(false);
  gPad->SetBottomMargin(0.16); gPad->SetTopMargin(0.02);
  gPad->SetLeftMargin(0.0);  gPad->SetRightMargin(0.01);
  h=(TH1F*)fd->Get("muBjetETR"); h->Draw();
  h->SetLineWidth(1.);
  hmc=(TH1F*)fdmc->Get("muBjetETR"); hmc->Draw("same");
  hmc->SetFillColor(93);  hmc->Scale(.2);  hmc->SetLineColor(93);
 
  h->SetTitle(";E^{e}_{T} / E^{#DeltaR<0.7}_{T} ");
  h->GetXaxis()->SetNdivisions(3);   
  h->GetXaxis()->CenterTitle(true);
  h->GetXaxis()->SetTitleSize(0.07); 
  h->GetXaxis()->SetTitleOffset(1.0);
  h->GetXaxis()->SetLabelSize(0.06); 
  h->GetXaxis()->SetRangeUser(0.02,1.1);
  h->GetYaxis()->SetNdivisions(4);   
  h->GetYaxis()->SetLabelSize(0.06);
  h->GetYaxis()->SetLabelOffset(-0.13);
  h->SetMinimum(0.02);
  
  list=h->GetListOfFunctions(); // to capture old
  hmc->GetListOfFunctions()->Delete();
  ln = (TLine*)list->At(0);
  ln->SetY2(h->GetMaximum()*1.02); ln->SetLineStyle(2); ln->SetLineWidth(1.5);  
  lnclone = (TLine*)ln->Clone(); lnclone->Draw("same");
  lnAxis->Draw("same");

  if(colorPlot) {
    c2->Print("/star/u/stevens4/wAnalysis/xSecPaper/plots/color/isoCutsColor.eps");
    c2->Print("/star/u/stevens4/wAnalysis/xSecPaper/plots/color/isoCutsColor.png");
  }
  else {
    c2->Print("/star/u/stevens4/wAnalysis/xSecPaper/plots/bw/isoCutsBW.eps");
    c2->Print("/star/u/stevens4/wAnalysis/xSecPaper/plots/bw/isoCutsBW.png");
  }

  return;

  float yy1=0.57;
  TPad *cT = new TPad("padT", "apdT",0,yy1,1,1.);    
  TPad *cB = new TPad("padB", "apdB",0,0.0,1,yy1); 
  cB->Draw();
  cT->Draw();

  //======================  2x2/4x4
  float xx3=0.1;
  cT->cd();
  gPad->SetGridy(false); gPad->SetGridx(false);
  gPad->SetBottomMargin(0.0);gPad->SetTopMargin(0.02);
  gPad->SetLeftMargin(xx3); gPad->SetRightMargin(0.01);
  h=(TH1F*)fd->Get("muBclET24R"); h->Draw();
  hmc=(TH1F*)fdmc->Get("muBclET24R"); hmc->Draw("same");
  hmc->SetFillColor(93);  hmc->Scale(0.85);
  
  h->SetTitle("; ;Counts  ");
  h->GetYaxis()->SetNdivisions(0);   
  h->GetYaxis()->CenterTitle(true); 
  h->GetYaxis()->SetTitleSize(0.085); 
  h->GetYaxis()->SetTitleOffset(0.55);
  h->GetXaxis()->SetNdivisions(3);
      
  list=h->GetListOfFunctions(); // to capture old
  hmc->GetListOfFunctions()->Delete();
  ln = (TLine*)list->At(0);
  ln->SetY2(h->GetMaximum()*1.02);ln->SetLineStyle(2); ln->SetLineWidth(1.5);  

  tx=new TLatex(0.4,500," E^{e}_{T} / E^{4#times4}_{T} ");   tx->SetTextSize(0.09);  tx->Draw();
  tx->SetTextAlign(21);   

  tx=new TLatex(-0.02,15,"0"); tx->SetTextSize(0.07);    tx->Draw();
  tx->SetTextAlign(21);    

  tx=new TLatex(-0.05,3000,"3000");  tx->SetTextSize(0.07);  tx->Draw();
  tx->SetTextAlign(21);    

  tx=new TLatex(0.125,2960,"a)");   tx->SetTextSize(0.09); tx->Draw();
  tx->SetTextAlign(21);    
  
  //======================  2x2/near
  cB->cd(); 
  gPad->SetGridy(false); gPad->SetGridx(false);
  gPad->SetBottomMargin(0.15); gPad->SetTopMargin(0.00);
  gPad->SetLeftMargin(xx3);  gPad->SetRightMargin(0.01);
  h=(TH1F*)fd->Get("muBjetETR"); h->Draw();
  hmc=(TH1F*)fdmc->Get("muBjetETR"); hmc->Draw("same");
  hmc->SetFillColor(93);  hmc->Scale(.2);
 
  h->SetTitle("; E^{e}_{T} / E^{#DeltaR<0.7}_{T}  ;Counts");
  h->GetXaxis()->SetNdivisions(3);   h->GetXaxis()->CenterTitle(true);
  h->GetXaxis()->SetTitleSize(0.06); h->GetXaxis()->SetTitleOffset(0.9);
  h->GetYaxis()->SetNdivisions(4);   h->GetYaxis()->CenterTitle(true);
  h->GetYaxis()->SetTitleSize(0.06); h->GetYaxis()->SetTitleOffset(0.8);
  h->GetYaxis()->SetLabelSize(0.055); h->GetXaxis()->SetLabelSize(0.055);

  list=h->GetListOfFunctions(); // to capture old
  hmc->GetListOfFunctions()->Delete();
  ln = (TLine*)list->At(0);
  ln->SetY2(h->GetMaximum()*1.02); ln->SetLineStyle(2); ln->SetLineWidth(1.5);  

  tx=new TLatex(0.125,275,"b)");   tx->SetTextSize(0.07); tx->Draw();
  tx->SetTextAlign(30);    
  
  if(colorPlot) {
    c2->Print("/star/u/stevens4/wAnalysis/xSecPaper/plots/color/isoCutsColor.eps");
    c2->Print("/star/u/stevens4/wAnalysis/xSecPaper/plots/color/isoCutsColor.png");
  }
  else {
    c2->Print("/star/u/stevens4/wAnalysis/xSecPaper/plots/bw/isoCutsBW.eps");
    c2->Print("/star/u/stevens4/wAnalysis/xSecPaper/plots/bw/isoCutsBW.png");
  }

  return;

}

