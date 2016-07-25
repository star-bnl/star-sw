void DrawpT() {
  const Char_t *Field[2] = {"FF","RFF"};
  const Char_t *Align[3] = {"eval0","eval","P11id"};
  const Char_t *AlignName[3] = {"y2001 Default","y2011 Cosmic","P11id"};
  TFile *f[2][3];
  for (Int_t field = 0; field < 2; field++) {
    for (Int_t align = 0; align < 3; align++) {
      TString FileN(Align[align]);
      FileN += "/";
      FileN += Field[field];
      //      FileN += "/MupT.root";
      FileN += "/MupTCut.root";
      f[field][align] = new TFile(FileN);
      if (! f[field][align]) continue;
      TH2D *ratio = (TH2D *) f[field][align]->Get("N2PratioPr");
      if (! ratio) {
	delete f[field][align]; f[field][align] = 0;
      }
      //      if (! f[field][align]) return;
    }
  }
  TCanvas *c1 = new TCanvas();
  c1->Divide(3,2);
  for (Int_t field = 0; field < 2; field++) {
    for (Int_t align = 0; align < 3; align++) {
      Int_t k = 3*field+align+1;
      c1->cd(k)->SetLogx(1);
      if (! f[field][align]) continue;
      f[field][align]->cd();
      cout << "k = " << k << "\t" << gDirectory->GetName() << endl;
      //      TH2D *ratio = (TH2D *) gDirectory->Get("N2PratioGl");
      TH2D *ratio = (TH2D *) gDirectory->Get("N2PratioPr");
      if (! ratio) continue;
      TAxis *y = ratio->GetYaxis();
      Int_t ny = y->GetNbins();
      TLegend *leg = new TLegend(0.6,0.6,0.85,0.85);
      TString Title = ratio->GetTitle();
      Title += Form(" %s %s",Field[field],AlignName[align]);
      cout << "Title = " << Title.Data() << endl;
      TString same("");
      for (Int_t j = 1; j <= 4; j += 3) {
      //      for (Int_t j = 2; j <= 3; j++) {
	TH1D *proj = ratio->ProjectionX(Form("%s%sbin%i",Field[field],Align[align],j),j,j);
	proj->SetStats(0);
	proj->SetMarkerColor(j);
	proj->SetTitle(Title);
	proj->SetMaximum(1.2);
	proj->SetMinimum(0.8);
	proj->Draw(same);
	same = "same";
	leg->AddEntry(proj,Form("#eta [%4.1f,%4.1f]",y->GetBinLowEdge(j),y->GetBinUpEdge(j)));
      }
      leg->Draw();
    }
  }  
}
