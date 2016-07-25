void TestRotations(Double_t alpha = 0.1, Double_t beta  = 0.2, Double_t gamma = 0.3) {
  TGeoHMatrix orig;
  orig.RotateX(TMath::RadToDeg()*alpha); cout << "X\t"; orig.Print();
  Double_t ca = TMath::Cos(alpha); Double_t sa = TMath::Sin(alpha);
  Double_t cb = TMath::Cos(beta) ; Double_t sb = TMath::Sin(beta); 
  Double_t cg = TMath::Cos(gamma); Double_t sg = TMath::Sin(gamma);
  TGeoHMatrix RX, R;
  Double_t rotsX[9] = {
    1., 0., 0.,
    0., ca,-sa,
    0., sa, ca
  };
  RX.SetRotation(rotsX); cout << "RX\t"; RX.Print();
  R.SetRotation(rotsX); cout << "R\t"; R.Print();
  Double_t rotsY[9] = {
    cb ,  0,  sb,
    0  ,  1,   0,
    -sb,  0,  cb    
  };
  TGeoHMatrix RY;
  RY.RotateY(TMath::RadToDeg()*beta);  cout << "Y\t"; RY.Print();
  RY *= RX;  cout << "Y*X\t"; RY.Print();
  R.SetRotation(rotsY);                R.Print();
  orig.RotateY(TMath::RadToDeg()*beta);  cout << "XY\t"; orig.Print();
  Double_t rotsXY[9] = {
    cb,   sb*sa,   sb*ca,
    0 ,      ca,  -sa   ,
    -sb,  cb*sa,   cb*ca        
  };
  R.SetRotation(rotsXY); R.Print();
  orig.RotateZ(TMath::RadToDeg()*gamma); cout << "XYZ\t"; orig.Print();
  Double_t rots[9] = {
    cg*cb,  cg*sb*sa-sg*ca, cg*sb*ca+sg*sa,
    sg*cb,  sg*sb*sa   +ca, sg*sb*ca   -sa,
    -sb  ,     cb*sa      ,    cb*ca      };

  TGeoHMatrix R;
  R.SetRotation(rots); R.Print();
  Double_t *rotx = R.GetRotationMatrix();
  Double_t beta1  = - TMath::ASin(rotx[6]);
  Double_t gamma1 =   TMath::ATan2(rotx[3],rotx[0]);
  Double_t alpha1 =   TMath::ATan2(rotx[7],rotx[8]);
  cout << "alpha = " << alpha1 << "\tbeta = " << beta1 << "\tgamma = " << gamma1 << endl; 
  TGeoRotation RM(R); cout << "RM\t"; RM.Print();
  RM.GetAngles(alpha1,beta1,gamma1); 
  cout << "alpha = " << TMath::DegToRad()*alpha1 << "\tbeta = " << TMath::DegToRad()*beta1 << "\tgamma = " << TMath::DegToRad()*gamma1 << endl; 
  TGeoRotation RN; RN.SetAngles(alpha1,beta1,gamma1); cout << "RN\t"; RN.Print();
}
