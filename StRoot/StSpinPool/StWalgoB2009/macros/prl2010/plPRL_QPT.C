TString dataPath="sortMarch22/";
TString dataFinalRoot=dataPath+"run9setP1234.wana.hist.root";
TFile* fd=0;

//============================================
//============================================
void   plPRL_QPT(TString cCore="WAL_PRL_QPT"){
  gStyle->SetFillStyle(0);
  fd=new TFile( dataFinalRoot); assert(fd->IsOpen());
  gStyle->SetPalette(1,0);
  gStyle->SetOptStat(0);  
  const Char_t *title = "High-energy electron charge separation with TPC";
  c=new TCanvas(cCore,title,500,260);
    setPadPaperSize(245.0); // column width, approx. 3.4 in = 245 pt
    setPadMarginWidthPoint(gPad, 27, 5, 25, (2.0*12.0) + 2, gPad);
    setPadLineWidthPoint(gPad, 0.5);

    h=AspinQpT2;
    h->Rebin2D(2,2);

    // h->Draw("colz");  return;
    
  // rotated X-Y axis
  TAxis *aX=h->GetXaxis(),*aY=h->GetYaxis();
  TH2F* h2=new TH2F("aa",";Lepton charge sign_{ }#times 1#font[122]{/}" + SYMBOL_pT + " (#font[12]{c}#font[122]{/}_{_{ }}" + SYMBOL_GeV + ");  E^{e}_{T} (" + SYMBOL_GeV + ")     ",aY->GetNbins(),aY->GetXmin()+0.0001,aY->GetXmax()-0.0001,
		    aX->GetNbins(),aX->GetXmin(),aX->GetXmax());

  for(int bx=1;bx<=aX->GetNbins();bx++) 
    for(int by=1;by<=aY->GetNbins();by++)
      h2->SetBinContent(by,bx,h->GetBinContent(bx,by));

    stripCmdHist(h2);
    setHistFontSize(h2, 10.0);
    setLineWidthPoint(h2, 0.5);
    setMarkerSizePoint(h2, 3.5);
    setHistTickLength(h2, 4.0, 4.0);
    setHistTitleLabelOffset(h2, 2.0*12.0 - 2.0, 18, 12.0 - 2.0, 4.5 - 2.0);
    h2->GetZaxis()->SetLabelSize(h2->GetYaxis()->GetLabelSize());
    h2->GetZaxis()->SetLabelOffset(h2->GetYaxis()->GetLabelOffset());
    h2->GetZaxis()->SetTickLength(h2->GetYaxis()->GetTickLength());
    h2->GetXaxis()->CenterTitle(true);
    h2->GetYaxis()->CenterTitle(true);

    h2->Draw("colz");


    h2->SetAxisRange(15,67,"y");
    h2->GetXaxis()->SetNdivisions(5);  
    h2->GetYaxis()->SetNdivisions(4);  
    h2->GetZaxis()->SetNdivisions(4);  

  //Q+, Q-
  float y0=61.5;
  ln=new TLine(0,15,0,68); ln->Draw();
  tx=new TLatex(0.05,y0,"positive");
    tx->SetTextAlign(21);
    stripCmdLatex(tx);
    setTextFontSize(tx, 10);
    tx->Draw();
  tx=new TLatex(-0.05,y0,"negative");
    tx->SetTextAlign(21);
    stripCmdLatex(tx);
    setTextFontSize(tx, 10);
    tx->Draw();

  //ET cut lines
  int ic=kBlack;
    ln=new TLine(-.1,25,0.08,25); ln->Draw(); 
    ln->SetLineColor(kRed);  
    ln->SetLineStyle(2);  
    setLineWidthPoint(ln, 1);
    ln=new TLine(-.1,50,0.05,50); 
    ln->Draw(); 
    ln->SetLineColor(kRed);  
    ln->SetLineStyle(2);  
    setLineWidthPoint(ln, 1);


  // Q/PT cut lines
  TList *list=h->GetListOfFunctions(); // to capture old slanted lines
  for(int i=0;i<2;i++) {
    float x0=0.01; if(i)x0=-x0;
    ln=new TLine(x0,25,x0,50); 
    ln->Draw(); 
    ln->SetLineColor(ic);    
    setLineWidthPoint(ln, 1);
    TLine* lr = (TLine*)list->At(i+4);
    ln=new TLine(lr->GetY1(),lr->GetX1(),lr->GetY2(),lr->GetX2());  
    ln->Draw(); 
    ln->SetLineColor(ic);    
    setLineWidthPoint(ln, 1);
  }


}


