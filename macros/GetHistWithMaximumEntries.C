TH1 *GetHistWithMaximumEntries(const Char_t *pattern="DV", Int_t N = 7) {
  TString patt(pattern);
  TList *listOfKeys = gDirectory->GetListOfKeys();
  TList *listOfObjects = gDirectory->GetList();
  if (! listOfObjects && ! listOfKeys) return;
  TIter nextobj(listOfObjects); 
  TObject *obj = 0;
  Double_t maxEntries = 0;
  TH1 *histMax = 0;
  while ((obj = nextobj())) {
    TString Name(obj->GetName());
    if (patt == "" || Name.Contains(pattern) && (N == 0 || Name.Length() == N)) {
      if ( obj->IsA()->InheritsFrom( "TH1" ) ) {
	TH1 *hist = (TH1 *) obj;
	if (hist->GetEntries() > maxEntries) {
	  maxEntries = hist->GetEntries();
	  histMax = hist;
	}
      }
    }
  }
  TKey *key = 0;
  TIter nextkey(listOfKeys); 
  while ((key = (TKey*) nextkey())) {
    TString Name(key->GetName());
    if (patt == "" || Name.Contains(pattern) && (N == 0 || Name.Length() == N)) {
      obj = key->ReadObj();
      if (! obj) continue;
      if ( obj->IsA()->InheritsFrom( "TH1" ) ) {
	TH1 *hist = (TH1 *) obj;
	if (hist->GetEntries() > maxEntries) {
	  maxEntries = hist->GetEntries();
	  histMax = hist;
	}
      }
    }
  }
  if (histMax) {
    cout << "Found hitogram " << histMax->GetName() << " with mximum number of entries " << histMax->GetEntries() << endl;
  }
  return histMax;
}
