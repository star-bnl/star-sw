TString dataPath="sortMarch22/";
TString mcPath="MC-setC-5.9.10/";

TString dataFinalRoot=dataPath+"run9setP1234.wana.hist.root";
TString mcFinalRoot=mcPath+"rcn10010.wana.hist.root";

TFile* fd=0;

//============================================
//============================================
void   plPRL_Cuts(TString cCore="WAL_PRL_Cuts"){
  gStyle->SetFillStyle(0);
  fd=new TFile( dataFinalRoot); assert(fd->IsOpen()); assert(fd->IsOpen());
  fdmc=new TFile( mcFinalRoot); assert(fd->IsOpen());  assert(fdmc->IsOpen());
  gStyle->SetPalette(1,0);
  gStyle->SetOptStat(0);  
  const Char_t *title = "Selection of W events";
  c=new TCanvas(cCore,title,500,300);
  setPadPaperSize(245.0); // column width, approx. 3.4 in = 245 pt

  TPad *cL = new TPad("padD", "apdD",0.55,0,1.,1.);  cL->Draw();  
  float yy1=0.60;
  TPad *cT = new TPad("padT", "apdT",0,yy1,0.55,1.);  cT->Draw();  
  TPad *cB = new TPad("padB", "apdB",0,0.0,0.55,yy1);  cB->Draw();  
  
  cL->cd(); 
  gPad->SetRightMargin(0.01); gPad->SetTopMargin(0.01);
  gPad->SetBottomMargin(0.19);
  gPad->SetLeftMargin(0.24);
  
  
  h2=(TH2F*)fd->Get("musPtBalance_clust");
  h2->Draw("col");
  h2->Rebin2D();
  h2->SetAxisRange(8,70,"x");h2->SetAxisRange(-40,60,"y");
  h2->GetXaxis()->SetNdivisions(5);  
  h2->SetTitle("; E^{e}_{T} (GeV); signed P_{T} balance (GeV)    ");
  h2->GetXaxis()->CenterTitle(true);
  h2->GetYaxis()->CenterTitle(true);

  stripCmdHist(h2);
  setHistFontSize(h2, 10.0);
  setLineWidthPoint(h2, 0.5);
  setMarkerSizePoint(h2, 3.5);
  setHistTickLength(h2, 4.0, 4.0);
  setHistTitleLabelOffset(h2, 2.0*12.0 - 2.0, 18, 15.0 - 2.0, 4.5 - 2.0);
  TList *list=h2->GetListOfFunctions(); // to capture old
  TLine* ln = (TLine*)list->At(0);
  ln->SetX1(8);ln->SetX2(72); ln->SetLineColor(45); setLineWidthPoint(ln, 1);

  ln=new TLine(25,15.,25,63);  ln->SetLineColor(kRed);  ln->SetLineStyle(2);
  setLineWidthPoint(ln, 1);  ln->Draw();

  ln=new TLine(50,15.,50,63);  ln->SetLineColor(kRed);  ln->SetLineStyle(2);
  setLineWidthPoint(ln, 1);  ln->Draw();

  ln=new TLine(25,15.,50,15);  ln->SetLineColor(kRed);  ln->SetLineStyle(2);
  setLineWidthPoint(ln, 1);  ln->Draw();

  tx=new TLatex(16,55,"c)");   tx->Draw();
  tx->SetTextAlign(21);    stripCmdLatex(tx);    setTextFontSize(tx, 10);


  //======================  2x2/4x4
  float xx3=0.25;
  cT->cd(); 
  gPad->SetBottomMargin(0.);gPad->SetTopMargin(0.02);
  gPad->SetLeftMargin(xx3); gPad->SetRightMargin(0.01);
  h=(TH1F*)fd->Get("muBclET24R"); h->Draw();
  hmc=(TH1F*)fdmc->Get("muBclET24R"); hmc->Draw("same");
  hmc->SetFillColor(93);  hmc->Scale(3.);
  
  h->SetTitle("; ;Counts  ");
  h->GetYaxis()->SetNdivisions(0);   h->GetYaxis()->CenterTitle(true);
  h->GetXaxis()->SetNdivisions(3);  
  stripCmdHist(h);
  setHistFontSize(h, 10.0);
  setLineWidthPoint(h, 0.5);
  setMarkerSizePoint(h, 3.5);
  setHistTickLength(h, 4.0, 4.0);
  setHistTitleLabelOffset(h, 2.0*12.0 - 2.0, 23, 15.0 - 2.0, 4.5 - 2.0);
  list=h->GetListOfFunctions(); // to capture old
  ln = (TLine*)list->At(0);
  ln->SetY2(3500);ln->SetLineStyle(2);  setLineWidthPoint(ln, 1); 

  tx=new TLatex(0.4,500," E^{e}_{T} / E^{4#times 4}_{T} ");     tx->Draw();
  tx->SetTextAlign(21);    stripCmdLatex(tx);    setTextFontSize(tx, 10);

  tx=new TLatex(-0.04,15,"0");     tx->Draw();
  tx->SetTextAlign(21);    stripCmdLatex(tx);    setTextFontSize(tx, 10);

  tx=new TLatex(-0.13,3000,"3000");     tx->Draw();
  tx->SetTextAlign(21);    stripCmdLatex(tx);    setTextFontSize(tx, 10);

  tx=new TLatex(0.125,2960,"a)");   tx->Draw();
  tx->SetTextAlign(21);    stripCmdLatex(tx);    setTextFontSize(tx, 10);
  
  //======================  2x2/near
  cB->cd(); 
  gPad->SetBottomMargin(0.35); gPad->SetTopMargin(0.02);
  gPad->SetLeftMargin(xx3);  gPad->SetRightMargin(0.01);
  h=(TH1F*)fd->Get("muBjetETR"); h->Draw();
  hmc=(TH1F*)fdmc->Get("muBjetETR"); hmc->Draw("same");
  hmc->SetFillColor(93);  hmc->Scale(.35);
 
  h->SetTitle("; E^{e}_{T} / E^{R<0.7}_{T}  ;Counts");
  h->GetXaxis()->SetNdivisions(3);   h->GetXaxis()->CenterTitle(true);
  h->GetYaxis()->SetNdivisions(4);   h->GetYaxis()->CenterTitle(true);

  stripCmdHist(h);
  setHistFontSize(h, 10.0);
  setLineWidthPoint(h, 0.5);
  setMarkerSizePoint(h, 3.5);
  setHistTickLength(h, 4.0, 4.0);
  setHistTitleLabelOffset(h, 2.0*12.0 - 2.0, 22, 15.0 - 2.0, 4.5 - 2.0);
  list=h->GetListOfFunctions(); // to capture old
  ln = (TLine*)list->At(0);
  ln->SetY2(320);ln->SetLineStyle(2);  setLineWidthPoint(ln, 1); 

  tx=new TLatex(0.125,275,"b)");   tx->Draw();
  tx->SetTextAlign(21);    stripCmdLatex(tx);    setTextFontSize(tx, 10);
  
}

