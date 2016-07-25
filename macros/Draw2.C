TFile *files[2] = {_file0, _file1};
void Draw2(const Char_t *name="n0I", const Char_t *proj="x") {
  if (! files[0] || ! files[1]) {cout << "files have not been set" << endl; return;}
  TString Proj(proj);
  TString same("");
  TLegend *l = new TLegend(0.6,0.2,1.0,0.3);
  for (Int_t k = 0; k < 2; k++) {
    files[k]->cd(); cout << gDirectory->GetName() << endl;
    TH1 *h = (TH1 *) gDirectory->Get(name);
    if (! h) continue;
    cout << h->GetName() << "\tfrom directoty\t" << h->GetDirectory()->GetName() << endl;
    if (h->IsA()->InheritsFrom( "TH2" )) {
      TH2 *h2 = (TH2 *) h;
      if (Proj.Contains("x",TString::kIgnoreCase)) h = h2->ProfileX(Form("%s_pfx%i",h2->GetName(),k));
      else                                         h = h2->ProfileY(Form("%s_pfy%i",h2->GetName(),k));
      cout << h->GetName() << "\tfrom directoty\t" << h->GetDirectory()->GetName() << endl;
      //      h->SetDirectory(files[k]);
    }
    h->SetMarkerColor(k+1);
    h->SetLineColor(k+1);
    l->AddEntry(h,gDirectory->GetName());
    h->Draw(same);
    same = "sames";
  }
  l->Draw();
}
