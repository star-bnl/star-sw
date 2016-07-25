void coilz() {
  const Int_t N = 12;
  Double_t zMin[N] = {20.129, 38.829, 52.846, 75.721,  98.596, 121.471, 146.158, 169.033, 191.908, 214.783, 237.671, 247.500};
  Double_t zMax[N] = {37.941, 47.769, 70.658, 93.533, 116.408, 139.283, 163.970, 186.845, 209.720, 232.595, 246.611, 265.312};
  Double_t zAv = 0;
  for (Int_t i = 0; i < N; i++) {
    Double_t dz = zMax[i] - zMin[i];
    Double_t z  = (zMax[i] + zMin[i])/2 -142.72042;
    cout << i << "\t" << zMin[i] << "\t" << zMax[i] << "\t" << dz << "\t" << Form("%8.2f",z) << endl;
    zAv = zAv + z;
  }
  zAv /= N;
  cout << "zAv " << zAv << endl;
}
