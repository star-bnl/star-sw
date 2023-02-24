#include "Riostream.h"
#include "TGeoMatrix.h"
#include "TMath.h"
//________________________________________________________________________________
void Beta(Double_t dbeta = 0) {
  Double_t alpha =  0; // 0.00010; // 2013 Tpc
  Double_t beta  = -0.00048;
  Double_t gamma =  0; // 0.00036;
  Double_t x0    = -0.2383;
  Double_t y0    = -0.1732;
  Double_t z0    = -0.1957;
  Double_t zWheel =  229.71; // zWheel = 229.71 cm (90.4375 inch)
  Double_t X = x0 - zWheel*beta;
  Double_t Z = z0 - zWheel*(1 - TMath::Cos(beta));
  TGeoHMatrix TpcR("TpcR");
  TpcR.RotateX(TMath::RadToDeg()*alpha);
  TpcR.RotateY(TMath::RadToDeg()*beta);
  TpcR.RotateZ(TMath::RadToDeg()*gamma); TpcR.Print();
  Double_t rWheel = 220;
  Double_t XWheel = rWheel;
  const Char_t *Names[5] = {"EN", "WN", "ES", "WS", "CT"};
  Double_t local[5][3] = {
    {-rWheel,  0, -zWheel}, //localEN[3] = East North, fixed
    {-rWheel,  0,  zWheel}, //localWN[3] = West Norhh, floating in Z direction
    { rWheel,  0, -zWheel}, //localES[3] = East South, floating in X direction
    { rWheel,  0,  zWheel}, //localWS[3] = West South, floating in X & Z diections
    {      0,  0,       0}  //localCT[3] = TPC center
  };
  TGeoHMatrix ToEN("ToEN"); ToEN.SetTranslation(local[0]); ToEN.Print();
  TGeoHMatrix ToENI("ToENI"); ToENI = ToEN.Inverse();    ToENI.Print();
  TGeoHMatrix RR = ToEN * TpcR * ToENI; RR.SetName("RR"); RR.Print();
#if 1
  //  Double_t xyz0[3] = {x0, y0, z0};
  Double_t xyz0[3] = {-0.1282,    -0.1732,    -0.3012};
  for (Int_t j = 0; j < 4; j++) {
    for (Int_t i = 0; i < 3; i++) 
    local[j][i] += xyz0[i];
  }
#else
  Double_t xyzC[3] = {x0, y0, z0};
  Double_t xyz0[3];
  RR.MasterToLocal(xyzC, xyz0);
  cout << Form("\txyzC = %10.4f %10.4f %10.4f",xyzC[0],xyzC[1],xyzC[2]);
  cout << Form("\txyz0 = %10.4f %10.4f %10.4f",xyz0[0],xyz0[1],xyz0[2]) << endl;
#endif
  Double_t global[3];
  for (Int_t i = 0; i < 5; i++) {
    RR.LocalToMaster(local[i],global);
    cout << Names[i];
    cout << Form("\tlocal = %10.4f %10.4f %10.4f",local[i][0],local[i][1],local[i][2]);
    cout << Form("\tglobal = %10.4f %10.4f %10.4f",global[0],global[1],global[2]);
    cout << Form("\tdiff = %10.4f %10.4f %10.4f",global[0]-local[i][0],global[1]-local[i][1],global[2]-local[i][2]) << endl;
  }
}
