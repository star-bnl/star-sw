void DrawEta(Int_t rank = 0, Double_t xrange = 0, Int_t log = 1) {
  TString fName = gSystem->BaseName(gDirectory->GetName());
  fName.ReplaceAll(".root","");
  TString Rank("All");
  if (rank > 0) {Rank = "Rank "; Rank += rank;}
  if (xrange > 0) {Rank += xrange;}
  if (log == 0) {Rank += "LinScale";}
  cout << "Draw " << Rank.Data() << endl;
  TCanvas *c1 = new TCanvas(Form("%s %s",fName.Data(),Rank.Data()),Form("%s %s",fName.Data(),Rank.Data()));
  c1->Divide(3,1);
  const Char_t *Names[3] = {"eta","y","pT"};
  const Char_t *Cuts[3] = {"NfpLT15","NfpGE25","NfpGE15"};
  TLegend *leg = 0;
  for (Int_t t = 0; t < 3; t++) {
    c1->cd(t+1)->SetLogy(log);
    if (t < 2) 
      leg = new TLegend(0.2,0.15,0.9,0.3,"");
    else 
      leg = new TLegend(0.3,0.7,0.9,0.85,"");
    leg->SetBorderSize(0);
    leg->SetFillColor(0);
    leg->SetTextSize(0.022);
    Int_t color = 1;
    TString Title;
    Int_t n = 0;
    Int_t marker = 20;
    //    for (Int_t flag = 0; flag < 10; flag++) {
    for (Int_t flag = 0; flag < 1; flag++) {
      for (Int_t c = 2; c >= 0; c--) {
	TString name(Form("%sF%iR%i%s",Names[t],flag,rank,Cuts[c]));
	//	cout << "Hist: " << name.Data() << endl;
	TH1D *h = (TH1D *) gDirectory->Get(name);
	if (! h) continue;
	h->SetStats(0);
	h->SetMarkerStyle(marker);
	h->SetMarkerSize(0.5);
	h->SetMarkerColor(color++);
	if (color >= 7) {
	  color = 1;
	  marker++;
	}
	if (t == 0) h->SetXTitle("#eta");
	if (t == 1) h->SetXTitle("Y");
	if (t == 2) h->SetXTitle("p_{T}");
	if (n == 0) {
	  if (xrange > 0) {
	    if (t < 2) h->SetAxisRange(-xrange, xrange);
	    else       h->SetAxisRange(0, xrange);
	  }
	  h->Draw("e");
	}
	else        h->Draw("samee");
	n++;
	Title = h->GetTitle();
	Title.ReplaceAll("pseudo rapidity for","");
	Title.ReplaceAll("rapidity for","");
	Title.ReplaceAll("pT for","");
	Title.ReplaceAll(" and Rank","");
	if (h->GetEntries() > 100) leg->AddEntry(h,Title.Data());
      }
    }
    //    if (t == 2) 
    leg->Draw();
  }
}
