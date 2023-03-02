void reg(const TString reg = "TdEdx*") { 
  TPRegexp re(reg); 
  TIter next(gDirectory->GetListOfKeys()); 
  TKey *key; while ((key= (TKey*)next())) { 
    TString st = key->GetName(); 
    if (st.Index(re) == kNPOS) continue;
    key->Print(); 
  } 
}
