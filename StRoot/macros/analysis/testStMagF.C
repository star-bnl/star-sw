{
//testB.C macro to read in and run over the magnetic field map grid. 
  gROOT->Reset();
  gSystem->Load("StMagF");
//  .L StMagF.so;
  // make a field object at reversed polarity;
  f = new StMagFCM("Star Full Reversed Field","bfp112.map",2,-1.0);
  f->ReadField();
  Float_t pt[3] = new Float_t;
  Float_t B[3] = new Float_t;
    pt[2] = -275.0;
  for(int i=0;i<10;i++){
    pt[0] = 8.0*TMath::Sin(i*TMath::Pi()/12.);
    pt[1] = 8.0*TMath::Cos(i*TMath::Pi()/12.);
    f->Field(&pt,&B);
    printf(" xyz = %7.2f %7.2f %7.2f Bxyz= %7.4f %7.4f %7.4f \n", pt[0],pt[1],pt[2],B[0],B[1],B[2]);
  }
}
