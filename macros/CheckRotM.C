void CheckRotM(const Char_t *opt="R7") {
  TList *keys = gDirectory->GetListOfKeys();
  if (! keys) return;
  TIter next(keys);
  TKey *key = 0;
  while ((key = (TKey*) next())) {
    TString Name(key->GetName());// cout << Name << endl;
    if (Name.BeginsWith(opt)) {
      TGeoHMatrix *comb = (TGeoHMatrix *) gDirectory->Get(Name);
      if (! comb) continue;
      comb->Print(); 
      TGeoRotation rot(*comb);
      cout << "=== Determinant " << rot.Determinant() << endl;
    }
  }
}
