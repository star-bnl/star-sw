void CheckHistos() {
  TIter nextkey( gDirectory->GetListOfKeys() );
  TKey *key = 0;
  while ((key = (TKey*) nextkey())) {
    TObject *obj = key->ReadObj();
    if ( obj->IsA()->InheritsFrom( "TH1" ) ) {
      TH1 *h1 = (TH1*)obj;
      cout << "Found histogram " << h1->GetName() << " with " << h1->GetEntries() << " entries" << endl;
      
    }
  }
}
