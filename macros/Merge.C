void Merge() {
  TH1F *h1 = new TH1F("h1","h1",110,-110,0);
  TH1F *h2 = new TH1F("h2","h2",220,0,110);
  TH1F *h3 = new TH1F("h3","h3",330,-55,55);
  TRandom r;
  for (Int_t i=0;i<10000;i++) {
    h1->Fill(r.Gaus(-55,10));
    h2->Fill(r.Gaus(55,10));
    h3->Fill(r.Gaus(0,10));
  }
  
  TList *list = new TList;
      list->Add(h1);
      list->Add(h2);
      list->Add(h3);
      TH1F *h = (TH1F*)h1->Clone("h");
      h->Reset();
      h->Merge(list);
      h->Draw();
}
