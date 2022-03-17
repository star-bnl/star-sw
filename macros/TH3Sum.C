TH3F *TH3Sum(const Char_t *name = "EO", Int_t i1 = 1, Int_t i2 = 13) {
  TH3F *sum = 0;
  for (Int_t i = i1; i <= i2; i++) {
    TH3F *h3 = (TH3F *) gDirectory->Get(Form("%s_%i",name,i));
    if (h3) {
      if (! sum) {
	sum = new TH3F(*h3);
        sum->SetName(name);
      } else  
	sum->Add(h3);
    }
  }
  return sum;
}
