class TCanvas;
TCanvas *c1 = 0;
// DrawNt("mu:y","MuR","i&&j","prof","","FitP",0)
void DrawNt(const Char_t *draw="dX:sector+12*(charge+1)",
	    const Char_t *Out  ="dX",
	    const Char_t *select="abs(dX)<2&&abs(abs(charge*pT*rI/(3e-4*field))-1)<2&&abs(abs(charge*pT*rO/(3e-4*field))-1)<2",
	    const Char_t *opt  ="prof",
	    const Char_t *TitleC = "",
	    const Char_t *NtName="TpcResNtuple", Int_t cut = 1) {
  cout << "DrawNt(\""<< draw << "\",\"" << Out << "\",\"" <<  select << "\",\"" << opt << "\",\"" <<  TitleC << "\",\"" << NtName << "\"," << cut << ")" << endl;
  TList *files = gROOT->GetListOfFiles();
  if (! files) return;
  Int_t nn = files->GetSize();
  if (! nn) return;
  TIter next(files);
  TLegend *leg = new TLegend(0.75,0.6,0.9,0.9,"");
  TString ttl(draw);
  ttl += " for ";
  ttl += select;
  Double_t xmin = 1e10;
  Double_t xmax = -1e10;
  Double_t ymin = 1e10;
  Double_t ymax = -1e10;
  TH1 **hist  = new TH1*[nn];
  Int_t n = 0;
  Int_t Marker = 20;
  Int_t color = 1;
  while ( (f = (TFile *) next()) ) { 
    f->cd();
    TNtuple *tuple = (TNtuple *) f->Get(NtName);
    if (! tuple) continue;
    TString Title(gSystem->BaseName(f->GetName()));
    Title.ReplaceAll(".root","");
    Title.ReplaceAll("TpcAligner","");
    if (cut) {
      Int_t Index = Title.Index("_");
      if (Index > 0)  Title = TString(Title.Data(),Index);
    }
    //    cout << Title << endl;
    TString Draw(draw);
    Draw += ">>";
    Draw += Out;
    tuple->SetMarkerStyle(Marker);
    tuple->SetLineColor(color);
    tuple->SetMarkerColor(color);
    TString Opt(opt);
    Opt += "goff";
    //    cout << "Draw(\"" << Draw << "\",\"" << select << "\",\"" << Opt << "\")" << endl;
    tuple->Draw(Draw.Data(),select,Opt.Data());
    TH1 *h = (TH1*) f->Get(Out);
    if (h) {
      color++;
      if (color >= 9) {color = 1; Marker++;}
      hist[n++] = h;
      leg->AddEntry(h,Title.Data());
      if (h->GetXaxis()->GetXmin() < xmin) xmin = h->GetXaxis()->GetXmin();
      if (h->GetXaxis()->GetXmax() > xmax) xmax = h->GetXaxis()->GetXmax();
      if (h->GetMinimum() < ymin) ymin = h->GetMinimum();
      if (h->GetMaximum() > ymax) ymax = h->GetMaximum();
      //      cout << "x min/max\t" << xmin << "/\t" << xmax << "\ty min/max\t" << ymin << "/\t" << ymax << endl;
    }
    else { cout << Out << " has not been found" << endl;}
  }
  if (!c1) c1 = new TCanvas();
  TH1F *frame = c1->DrawFrame(xmin-0.01,ymin-0.01,xmax+0.01,ymax+0.01);
  TString XA(draw);
  Int_t s = XA.Index(":");
  TString YA("");
  if (s) {
    YA = TString(XA.Data(),s);
    XA = TString(XA.Data()+s+1);
  }
  frame->SetXTitle(XA.Data());
  frame->SetYTitle(YA.Data());
  for (int i = 0; i < n; i++) {
    hist[i]->Draw("same");
  }
  leg->Draw();
}
