#include "Riostream.h"
#include "TMath.h"
#include "TDatabasePDG.h"
TDataSet *CreateTable() {
  StarPrimaryMaker *maker = StarPrimaryMaker::instance();
  if (! maker) return 0;
  Double_t sigma_X = 0.010;
  Double_t sigma_Y = 0.010;
  Double_t sigma_Z = 30.000;
  Double_t Z_min   = -5;
  Double_t Z_max   =  5;
  cout << "ZCut3cm: set sigma_X = " << sigma_X << "\tsigma_Y = " << sigma_Y << "\tsigma_Z = " << sigma_Z 
       << "\tZ_min = " << Z_min << "\tZ_max = " << Z_max
       << endl;
  maker->SetSigma(sigma_X, sigma_Y, sigma_Z);
  //  maker->SetCuts(0,-1, 0, -1,  0, -1, Z_min, Z_max);
  maker->SetZvertexRange(Z_min, Z_max);
  TDataSet *tableSet = new TDataSet("ZCut5cm");
  return (TDataSet *)tableSet;
}
