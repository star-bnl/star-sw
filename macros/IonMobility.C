//________________________________________________________________________________
TGraph *IonMobility(Char_t *FileName="$STAR/garfield/Data/IonMobility_Ar+_Ar.txt") {
  static TGraph *gr = 0;
  if (! gr) {
    FILE *fp = fopen(gSystem->ExpandPathName(FileName),"r");
    if (! fp) {
      cout << "Can't open" << FileName << endl;
      return 0;
    }
    Char_t line[121];
    Float_t x[200], y[200];
    Int_t nP = 0;
    while (fgets(&line[0],120,fp)) {
      if (line[0] == '#') continue;
      Int_t n = sscanf(&line[0],"%f %f",&x[nP],&y[nP]);
      if (n != 2) continue;
      nP++;
    }
    gr = new TGraph(nP,x,y);
  }
  return gr;
}
//________________________________________________________________________________
Double_t tofunc(Double_t *x, Double_t *p) {
  Double_t E0 = p[0];
  Double_t ra = p[1];
  Double_t r  = x[0];
  Double_t E = E0*ra/r;
  Double_t drdt = IonMobility()->Eval(E)*E;
  return 1./drdt;
}
//________________________________________________________________________________
TF1 *Tof() { // dT/dr 
  Double_t ra = 1e-3; 
  Double_t rc = 0.306245;
  TF1 *func = new TF1("Tof",tofunc, ra, rc, 5);
  func->SetParName(0, "E0 (V/cm)");  func->SetParameter(0, 1509.18);
  func->SetParName(1, "r_{A} (cm)"); func->SetParameter(1, ra);
  return func;
}
