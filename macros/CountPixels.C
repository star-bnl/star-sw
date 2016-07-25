void CountPixels() {
  TDataSet *set = chain->FindByName("fcfPixATop");
  TDataSetIter next(set);
  Int_t N = 0;
  St_fcfPixel *pix = 0;
  TDataSet *det = 0;
  while ((det = next())) {
    TString name(det->GetName());
    if (! name.Contains("fcfPixA")) continue;
    pix = (St_fcfPixel *) det;
    N += pix->GetNRows();
  }
  cout << "No. of Pixels = " << N << endl;
}
