class TCanvas;
TCanvas *c1 = 0;
// DrawH("mu:y","MuR","i&&j","prof","","FitP",0)
void DrawH(const Char_t *opt  ="ProfileX",
	   const Char_t *titleC = "",
	   const Char_t *HistName="MultiplicityPI") {
  TList *files = gROOT->GetListOfFiles();
  if (! files) return;
  Int_t nn = files->GetSize();
  if (! nn) return;
  TIter next(files);
  TLegend *leg = new TLegend(0.5,0.6,0.9,0.9,"");
  Double_t xmin = 1e10;
  Double_t xmax = -1e10;
  Double_t ymin = 1e10;
  Double_t ymax = -1e10;
  TH1 **hist  = new TH1*[nn];
  TH1 *h = 0;
  TH1 *hr = 0;
  Int_t n = 0;
  Int_t Marker = 20;
  Int_t color = 1;
  TString Opt(opt);
  TString TitleC(titleC);
  cout << opt << "\t" << Opt << endl;
  Double_t width = 2;
  Double_t linetype = 1;
  while ( (f = (TFile *) next()) ) { 
    f->cd();
    hr = (TH2 *) f->Get(HistName);
    if (! hr) continue;
    TString Title(gSystem->BaseName(f->GetName()));
    Title.ReplaceAll(".root","");
    Title.ReplaceAll("Hist","");
//     Int_t Index = Title.Index("_");
//     if (Index > 0)  Title = TString(Title.Data(),Index);
    //    cout << Title << endl;
    if (Opt == "") {h = hr;}
    else {
      if (Opt.Contains("ProfileX",TString::kIgnoreCase)) {TH2 *h2 = (TH2 *) hr; h = h2->ProfileX(); h->Draw();}
      if (Opt.Contains("ProfileY",TString::kIgnoreCase)) {TH2 *h2 = (TH2 *) hr; h = h2->ProfileY();}
      if (Opt.Contains("ProjectionX",TString::kIgnoreCase)) {TH2 *h2 = (TH2 *) hr; h = h2->ProjectionX();}
      if (Opt.Contains("ProjectionY",TString::kIgnoreCase)) {TH2 *h2 = (TH2 *) hr; h = h2->ProjectionY();}
      h->SetName(Form("%s_%i",h->GetName(),n));
    }
    //    cout << Opt << "\t" << h->GetName() << endl;
    h->SetMarkerStyle(Marker);
    h->SetLineColor(color);
    h->SetLineWidth(width);
    h->SetLineStyle(linetype);
    h->SetMarkerColor(color);
    if (h) {
      color++;
      if (color >= 9) {color = 1; Marker++; linetype++;}
      hist[n++] = h;
      leg->AddEntry(h,Title.Data());
      if (h->GetXaxis()->GetXmin() < xmin) xmin = h->GetXaxis()->GetXmin();
      if (h->GetXaxis()->GetXmax() > xmax) xmax = h->GetXaxis()->GetXmax();
      if (h->GetMinimum() < ymin) ymin = h->GetMinimum();
      if (h->GetMaximum() > ymax) ymax = h->GetMaximum();
      //      cout << "x min/max\t" << xmin << "/\t" << xmax << "\ty min/max\t" << ymin << "/\t" << ymax << endl;
      if (TString(TitleC) == "") TitleC = h->GetTitle(); 
    }
  }
  if (!c1)  c1 = new TCanvas("c1",TitleC.Data());
  TH1F *frame = c1->DrawFrame(xmin-0.01,ymin-0.01,xmax+0.01,ymax+0.01);
  //  c1->SetLogy(1);
  frame->SetTitle(TitleC.Data());
//   TString XA(draw);
//   Int_t s = XA.Index(":");
//   TString YA("");
//   if (s) {
//     YA = TString(XA.Data(),s);
//     XA = TString(XA.Data()+s+1);
//   }
//   frame->SetXTitle(XA.Data());
//   frame->SetYTitle(YA.Data());
  for (int i = 0; i < n; i++) {
    hist[i]->Draw("same");
    //    cout << "i/p" << i << "\t" << hist[i] << endl;
  }
  leg->Draw();
}
