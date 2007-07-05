#include "LaserBeams.h"
#include "LASERINO.h"
void CheckLasers() {
  cout << "NoLocals " << NoLocals << "\tNoBeams " << NoBeams << endl;
  const Int_t numSectors = 24;
  Double_t beta = 0;
  Double_t dBeta = 720./numSectors;
  Int_t sector = 0;
  Int_t half   = 0;
  TGeoHMatrix TpcHalf[2];
  Double_t rotHalfs[18] = {
    0, -1, 0, -1, 0, 0, 0, 0, -1,// sector  1-12
    0,  1, 0, -1, 0, 0, 0, 0,  1 // sector 13-24
  };
  for (half = 0; half <2; half++) TpcHalf[half].SetRotation(&rotHalfs[9*half]);
  TGeoHMatrix RotSec[24];
  for (sector = 1; sector <= numSectors; sector++) {
    if (sector > 12) beta = (numSectors-sector)*dBeta;
    else             beta = sector*dBeta;
    RotSec[sector-1].RotateZ(-beta);
  }
  
  for (Int_t raft = 1; raft <= 14; raft++) {
  //  for (Int_t raft = 9; raft <= 9; raft++) {
    TRVector mX;
    TRMatrix A(0,6);
    for (Int_t bundle = 1; bundle <= 6; bundle++) {
      for (Int_t mirror = 1; mirror <= 7; mirror++) {
	Int_t RBM = 100*raft + 10*bundle + mirror;
	if (RBM == 121 || // +1
	    RBM == 122 || // +1
	    RBM == 124 || // +1
	    RBM == 126 || // +1
	    RBM == 131 || // +1
	    RBM == 132 )  // +1
	continue;
	if (RBM == 211 || // +1
	    RBM == 221 || // +1
	    RBM == 225 || // +1
	    RBM == 231 || // +1
	    RBM == 244 || // +1
	    RBM == 246 || // +1
	    RBM == 265)   // +1
	continue;
	if (RBM == 311 || // +1
	    RBM == 331 || // +1
// 	    RBM == 335 || // -1 
	    RBM == 337 )  // +1
	    continue;
	if (raft ==  5) continue;
	Int_t l = -1; 
	for (Int_t local = 0; local < NoLocals; local++) {
	  if (Locals[local].Raft == raft && Locals[local].Bundle == bundle && Locals[local].Mirror == mirror) {l = local; break;}
	}
	Int_t k = -1;
	for (Int_t global = 0; global < NoBeams; global++) {
	  if (LaserBeams[global].Raft == raft && LaserBeams[global].Bundle == bundle && LaserBeams[global].Mirror == mirror) {k = global; break;}
	} 
	if (l < 0 || k < 0) continue;
	//	cout << "Found match for sector " << LaserBeams[k].Sector << " raft " << raft << " bundle " << bundle << " mirror " << mirror << endl;
	Double_t *xg = &LaserBeams[k].X;// TRVector XG(3,xg); cout << " XG " << XG << endl;
	Double_t xG[3];
	sector = LaserBeams[k].Sector;
	half = 0;
	if (sector > 12) half = 1;
	TGeoHMatrix RotM = RotSec[sector-1] * TpcHalf[half];
	//	TGeoHMatrix RotM = TpcHalf[half] *  RotSec[sector-1];
	//	cout << " Raft " << raft << " Sector " << sector << " Bundle " << bundle << " Mirror " << mirror << endl; 
	//	RotM.Print();               //TRVector xgT(3,xg); cout << "xg " << xgT;
	RotM.MasterToLocal(xg,xG);  //TRVector xGT(3,xG); cout << "xG " << xGT;
	Double_t *xl = &Locals[l].X;
	Double_t xL[3] = {0.1*xl[0], 0.1*xl[1], 0.1*xl[2]};// TRVector XL(3,xL); cout << "XL " << XL;
	/*
	        (     1 -gamma  beta )
	  DRT = ( gamma      1 -alpha); 
	        ( -beta  alpha     1 )
	*/
	Double_t a[18] = {
	      0.,  xL[2], -xL[1],  1., 0., 0.,
	  -xL[2],     0.,  xL[0],  0., 1., 0.,
	   xL[1], -xL[0],     0.,  0., 0., 1.
	};
	for (Int_t i = 0; i < 3; i++) {
	  Double_t x = xG[i] - xL[i];
	  mX.AddRow(&x);
	  A.AddRow(&a[6*i]);
	}
      }
    }
    //    cout << "X "; mX.Print();
    //    cout << "A "; A.Print();
    if (A.GetNrows() < 1) {
      cout << "Raft " << raft << " is empty" << endl; 
      continue;
    } 
    cout << "Raft " << raft << " Sector = " << sector << "   =========================" << endl;  
    TRVector AmX(A,TRArray::kATxB,mX);      // cout << "AmX\t" << AmX << endl; 
    TRSymMatrix S(A,TRArray::kATxA);        // cout << "S\t" << S << endl;
    TRSymMatrix SInv(S,TRArray::kInverted); // cout << "SInv " << SInv << endl;
    TRVector  X(SInv,TRArray::kSxA,AmX);    // cout << "X " << X << endl;
    TGeoHMatrix T(Form("Raft%0i",raft));
    // normolize
    Double_t *xx = X.GetArray();
    T.RotateX(xx[0]*180/TMath::Pi());
    T.RotateY(xx[1]*180/TMath::Pi());
    T.RotateZ(xx[2]*180/TMath::Pi());
    T.SetTranslation(&xx[3]);
    cout << Form("{%i,%i",raft,sector);
    for (Int_t i = 0; i < 6; i++) cout << Form(",%15.5f",xx[i]);
    cout << "}," << endl;
    // cout << "Determinant " << T.Determinant() << endl;
    T.Print();
    TGeoHMatrix RotM = RotSec[sector-1] * TpcHalf[half] * T;
    //    TGeoHMatrix RotM = TpcHalf[half] * RotSec[sector-1] *  T;
    RotM.Print();
    // Check table
    for (Int_t bundle = 1; bundle <= 6; bundle++) {
      for (Int_t mirror = 1; mirror <= 7; mirror++) {
	Int_t l = -1; 
	for (Int_t local = 0; local < NoLocals; local++) {
	  if (Locals[local].Raft == raft && Locals[local].Bundle == bundle && Locals[local].Mirror == mirror) {l = local; break;}
	}
	Int_t k = -1;
	for (Int_t global = 0; global < NoBeams; global++) {
	  if (LaserBeams[global].Raft == raft && LaserBeams[global].Bundle == bundle && LaserBeams[global].Mirror == mirror) {k = global; break;}
	} 
	if (l < 0 || k < 0) continue;
	sector = LaserBeams[k].Sector;
	//	cout << "Found match for raft " << raft << " bundle " << bundle << " mirror " << mirror << endl;
	Double_t *xg = &LaserBeams[k].X; // TRVector XG(3,xg); cout << " XG " << XG;
	Double_t *xl = &Locals[l].X;     
	Double_t xL[3] = {0.1*xl[0],0.1*xl[1],0.1*xl[2]}; // TRVector XL(3,xL); cout << " xL " << XL;
	Double_t xG[3];
	
	RotM.LocalToMaster(xL,xG);     //  TRVector XGG(3,xG); cout << " xG " << XGG;
	Double_t dev = 0;
	for (Int_t i = 0; i < 3; i++) {
	  Double_t dif = (xG[i] - xg[i]);
	  dev += dif*dif;
	}
	dev = TMath::Sqrt(dev);
	if (dev > 0.0000) {
	  cout << Form("S%02iR%02iB%iM%i dev = %f",sector,raft,bundle,mirror,dev);
	  cout << Form(" dX/dY/dZ = %f/%f/%f",xG[0] - xg[0],xG[1] - xg[1],xG[2] - xg[2]);
	  if (dev > 0.005) cout << " ===========";
	  cout << endl;
	}
      }
    }
  }
}
