void PxlSector() {
  Double_t phi[10] = {0,324,288,252,216,180,144,108,72,36};
  for (Int_t i = 0; i < 10; i++) {
    TGeoHMatrix R;
    R.RotateZ(phi[i]); 
    cout << "Id = " << i+1 << "\t";; R.Print();
  }
}
