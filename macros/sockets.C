void sockets(Int_t sector=1, Int_t socket=0, const Char_t *draw="curr:day", const Char_t *plot="") {
  TNtuple *FitP = (TNtuple *) gDirectory->Get("FitP");
  if (! FitP) return;
  FitP->SetMarkerStyle(1);
  Int_t color = 1;
  FitP->SetMarkerColor(color);
  TString Out("all");
  TString Draw(draw);
  TString Plot(Draw);
  Plot += ">>"; Plot += Out; Plot += plot;
  cout << "Plot: " << Plot << endl;
  FitP->Draw(Plot);
  TLegend *leg = new TLegend(0.1,0.1,0.4,0.4);
  TH2 *o = (TH2 *)gDirectory->Get(Out);
  if (o) leg->AddEntry(o, Out);
  Int_t secMin = 1;
  Int_t secMax = 24;
  if (sector>0) {secMin = secMax = sector;}
  Int_t socMin = 1;
  Int_t socMax = 8;
  if (socket > 0) {socMin = socMax = socket;}
  for (Int_t sec = secMin; sec <= secMax; sec++) {
    FitP->SetMarkerStyle(sec+19);
    for (Int_t soc = socMin; soc <= socMax; soc++) {
      FitP->SetMarkerColor(soc);
      TString Cond(Form("sector==%i&&socket==%i",sec,soc));
      Out = Form("S%iC%i",sec,soc);
      Plot = Draw; Plot += ">>"; Plot += Out; Plot += plot;
      cout << "Plot: " << Plot << endl;
      FitP->Draw(Plot,Cond,"same");
      o = (TH2 *)gDirectory->Get(Out);
      if (o) leg->AddEntry(o, Out);
    }
  }
  leg->Draw();
}
