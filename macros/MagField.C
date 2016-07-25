/*
  root.exe 'Load.C("StarMagField")' MagField.C+
 */
#include "Riostream.h"
#include "Ask.h"
#include "TSystem.h"
#include "StarMagField.h"
#include "TGeoMatrix.h"
#include "TMath.h"
#include "TVirtualFitter.h"
#include "TH2.h"
#include "TProfile2D.h"
#include "TDirectory.h"
#include "Fint.h"
using namespace std;
TProfile2D *BphiH = 0;
TProfile2D *BrhoH = 0;
TProfile2D *BzH = 0;
Int_t NP = 0;
Double_t fitpar[6];
Double_t fitper[6];
const Int_t Narg = 3;
Int_t     *NoPhZRhoEnt = 0;
Double_t *PhiZRhoentries = 0;
StThreeVectorD *BLxyzTable = 0;
TGeoHMatrix rot;
//________________________________________________________________________________
void InitFint() {
  NoPhZRhoEnt = new Int_t[Narg];
  NoPhZRhoEnt[0] = StarMagField::Instance()->nPhi;
  NoPhZRhoEnt[1] = StarMagField::Instance()->nZ;
  NoPhZRhoEnt[2] = StarMagField::Instance()->nR;
  Int_t N = NoPhZRhoEnt[0] + NoPhZRhoEnt[1] + NoPhZRhoEnt[2];
  PhiZRhoentries = new Double_t[N];
  Int_t m = 0;
  for (Int_t i = 0; i < StarMagField::Instance()->nPhi; i++, m++) {
    PhiZRhoentries[m] = StarMagField::Instance()->Phi3D[i];
  }
  for (Int_t j = 0; j < StarMagField::Instance()->nZ; j++, m++) {
    PhiZRhoentries[m] = StarMagField::Instance()->Z3D[j];
  }
  for (Int_t k = 0; k < StarMagField::Instance()->nR; k++, m++) {
    PhiZRhoentries[m] = StarMagField::Instance()->R3D[k];
  }
  N = NoPhZRhoEnt[0]*NoPhZRhoEnt[1]*NoPhZRhoEnt[2];
  BLxyzTable = new StThreeVectorD[N];
  for (Int_t i = 0; i < StarMagField::Instance()->nPhi; i++) {
    for (Int_t j = 0; j < StarMagField::Instance()->nZ; j++) {
      for (Int_t k = 0; k < StarMagField::Instance()->nR; k++) {
	Float_t Bp = StarMagField::Instance()->Bphi3D[i][j][k];
	Float_t Bz = StarMagField::Instance()->Bz3D[i][j][k];
	Float_t Br = StarMagField::Instance()->Br3D[i][j][k];
	/* 
	   B_x = B_rho * cos(phi) - B_phi * sin(phi) 
	   B_y = B_rho * sin(phi) + B_phi * cos(phi)
	   B_z = B_z
	*/
	//	Double_t phi = PhiZRhoentries[i];
	Int_t ijk = i + NoPhZRhoEnt[0]*(j + NoPhZRhoEnt[1]*k);
	BLxyzTable[ijk] = StThreeVectorD(Bp,Bz,Br);
#if 0
	cout 
	  << " Rho " << Form("%8.3f",PhiZRhoentries[NoPhZRhoEnt[0]+NoPhZRhoEnt[1]+k])
	  << " Z " << Form("%8.3f",PhiZRhoentries[NoPhZRhoEnt[0]+j])
	  << " Phi " << Form("%8.3f",TMath::RadToDeg()*PhiZRhoentries[i])
	  << " Brho " << Form("%8.3f",BLxyzTable[ijk].z())
	  << " Bz " << Form("%8.3f",BLxyzTable[ijk].y())
	  << " Bphi " << Form("%8.3f",BLxyzTable[ijk].x())
	  << endl;
#endif
      }
    }
  }  
}
#if 0
//________________________________________________________________________________
StThreeVectorD Bpzr(Double_t *pzr) {
  return Fint(Narg, pzr, NoPhZRhoEnt, PhiZRhoentries, BLxyzTable);
}
#endif
//________________________________________________________________________________
StThreeVectorD Bxyz(StThreeVectorD xyz) {
  Double_t xyzG[3] = {xyz.x(), xyz.y(), xyz.z()};
  Double_t xyzL[3];
  rot.MasterToLocal(xyzG,xyzL);
  Double_t phi = TMath::ATan2(xyzL[1],xyzL[0]);
  Double_t rho =  TMath::Sqrt(xyzL[0]*xyzL[0] + xyzL[1]*xyzL[1]);
  Double_t xyzLpzr[3] = {phi, xyzL[2], rho};
  StThreeVectorD Bpzr = Fint(Narg, xyzLpzr, NoPhZRhoEnt, PhiZRhoentries, BLxyzTable);
  Double_t Bp = Bpzr.x();
  Double_t Bz = Bpzr.y();
  Double_t Br = Bpzr.z();
  Double_t BL[3] = {Br*TMath::Cos(phi) - Bp*TMath::Sin(phi), 
		    Br*TMath::Sin(phi) + Bp*TMath::Cos(phi), 
		    Bz};
  Double_t BG[3];
  rot.LocalToMasterVect(BL,BG);
  return StThreeVectorD(BG[0],BG[1],BG[2]);
}
//________________________________________________________________________________
void BphiSq(Int_t &/* npar */, Double_t */* gin */, Double_t &f, Double_t *p, Int_t /* flag */) {
  rot = TGeoHMatrix();
  rot.RotateX(1e-3*TMath::RadToDeg()*p[0]);
  rot.RotateY(1e-3*TMath::RadToDeg()*p[1]);
  rot.RotateZ(1e-3*TMath::RadToDeg()*p[2]);
  rot.SetTranslation(&p[3]);
  f = 0;
  BphiH->Reset();
  TAxis *x = BphiH->GetXaxis();
  Int_t nbinx = x->GetNbins();
  TAxis *y = BphiH->GetYaxis();
  Int_t nbiny = y->GetNbins();
  for (Int_t binx = 1; binx <= nbinx; binx++) {
    Double_t z = x->GetBinCenter(binx);
    for (Int_t biny = 1; biny <= nbiny; biny++) {
      Double_t r = y->GetBinCenter(biny);
      //      for (Int_t i = 0; i < StarMagField::Instance()->nPhi; i++) {
      for (Int_t i = 0; i < 18; i++) {
	//	Float_t phi = StarMagField::Instance()->Phi3D[i];
	Float_t phi = (10*i+5)*TMath::DegToRad();
	Double_t xyzG[3] = {r*TMath::Cos(phi), r*TMath::Sin(phi),z};
	StThreeVectorD xyz(xyzG[0],xyzG[1],xyzG[2]);
	StThreeVectorD BG = Bxyz(xyz);
	/* 
	   B_x = B_rho * cos(phi) - B_phi * sin(phi) 
	   B_y = B_rho * sin(phi) + B_phi * cos(phi)
	   B_z = B_z

           B_rho =   B_x * cos(phi) + B_y * sin(phi);
	   B_phi = - B_x * sin(phi) + B_y * cos(phi);
	   
	 */
	Double_t Brho =   BG.x() * TMath::Cos(phi) + BG.y() * TMath::Sin(phi);
	Double_t Bphi = - BG.x() * TMath::Sin(phi) + BG.y() * TMath::Cos(phi);
	Double_t Bz   =   BG.z();
	BphiH->Fill(z,r,Bphi);
	BrhoH->Fill(z,r,Brho);
	BzH->Fill(z,r,Bz);
	f += Bphi*Bphi;
	if (r == 0.) break;
      }
    }
#if 0
    StThreeVectorD xyz(0.,0.,z);
    StThreeVectorD BG = Bxyz(xyz);
    Double_t Brho2 =   BG.XYvector().Mod2(); 
    f += Brho2;
#endif
  }
}
//________________________________________________________________________________
void Fit(Float_t scale = 1.0, Int_t Nr = 21, Double_t rMax = 210, Double_t Nz = 20, Double_t zMax = 200) {
  TString FFlag("");
  if (scale > 0.8)       FFlag += "FF";
  else if (scale > 0.2)  FFlag += "FHF";
  else if (scale > -0.8) FFlag += "RHF";
  else                   FFlag += "RFF";
  const Char_t *Names[3] = {"BphiH","BrhoH","BzH"};
  TProfile2D *h = 0;
  for (Int_t k = 0; k < 3; k++) {
    TString Name(Names[k]);
    Name += FFlag;
    h = (TProfile2D *) gDirectory->Get(Name);
    if (h) delete h;
    Double_t dr = rMax/Nr;
    h = new TProfile2D(Name,Form("%s versus z and R",Name.Data()),Nz,-zMax,zMax,Nr,-dr/2,rMax-dr/2);
    if (k == 0)      BphiH = h;
    else if (k == 1) BrhoH = h;
    else             BzH   = h;
  }
  if (StarMagField::Instance()) {
    delete [] NoPhZRhoEnt;    NoPhZRhoEnt    = 0;
    delete [] PhiZRhoentries; PhiZRhoentries = 0;
    delete [] BLxyzTable;   BLxyzTable   = 0;
    delete StarMagField::Instance();
  }
  new StarMagField(StarMagField::kMapped, scale);
  InitFint();
  //Fit a circle to the graph points
  TVirtualFitter::SetDefaultFitter("Minuit");  //default is Minuit
  TVirtualFitter *fitter = TVirtualFitter::Fitter(0, 6);
  fitter->SetFCN(BphiSq);
  const Char_t *names[6] = {"alpha","beta","gamma","x0","y0","z0"};
  Double_t params[6] = { 0, 0, 0, 0, 0, 0};
  for (Int_t i = 0; i < 6; i++) {
    fitter->ReleaseParameter(i);
    fitter->SetParameter(i, names[i], params[i], 0.1, -2., 2.);
  }
  //  fitter->FixParameter(0);
  //  fitter->FixParameter(1);
  fitter->FixParameter(2);
  //  fitter->FixParameter(3);
  //  fitter->FixParameter(4);
  fitter->FixParameter(5);
#if 0
  Double_t f;
  Int_t n = 6;
  BphiSq(n,0,f,params,0);
  return;
#endif
  Double_t arglist[1] = {0};
  //  fitter->ExecuteCommand("MIGRAD", arglist, 0);
  fitter->ExecuteCommand("MINImize", arglist, 0);
  //fitter->ExecuteCommand("CALL fcn", arglist, 0);
  if (NP == 0) {memset(fitpar, 0, sizeof(fitpar)); memset(fitper, 0, sizeof(fitper));}
  for (Int_t i = 0; i < 6; i++) {
    Double_t par = fitter->GetParameter(i);
    Double_t err = fitter->GetParError(i);
    if (TMath::Abs(err) < 1e-10) continue;
    if (i >= 3) 
      cout << Form(" %6s(mkm)  = %6.1f +/- %5.1f",names[i],1e4*par, 1e4*err);
    else 
      cout << Form(" %6s(mrad)  = %7.3f +/- %6.3f",names[i],par, err);
    fitper[i] += 1./(err*err);
    fitpar[i] += par/(err*err);
  }
  cout << endl;
  NP++;
  if (NP > 1) {
    for (Int_t i = 0; i < 6; i++) {
      Double_t par = fitpar[i];
      Double_t err = fitper[i];
      if (TMath::Abs(err) < 1e-10) continue;
      par = par/err;
      err = 1./TMath::Sqrt(err);
      if (i >= 3) 
	cout << Form("<%6s(mkm)> = %6.1f +/- %5.1f",names[i],1e4*par, 1e4*err);
      else 
	cout << Form("<%6s(mrad)> = %7.3f +/- %6.3f",names[i],par, err);
    }
    cout << endl;
  }
}
//________________________________________________________________________________
 void MagField() {
  Fit();
}
/*
  ================================================================================
Fint
 FF:  alpha(mrad)  =  -0.037 +/-  0.002   beta(mrad)  =  -0.026 +/-  0.002     x0(mkm)  = -1122.3 +/-  99.8     y0(mkm)  = -1310.0 +/-  78.6
      alpha(mrad)  =  -0.011 +/-  0.000   beta(mrad)  =  -0.049 +/-  0.000     x0(mkm)  = -1199.6 +/-  98.3     y0(mkm)  = -1357.5 +/-  76.0 Bpzr
      alpha(mrad)  =  -0.001 +/-  0.000   beta(mrad)  =   0.004 +/-  0.000     x0(mkm)  = -413669.2 +/- 161.9   y0(mkm)  = -435005.8 +/-   1.1 Bxyz
      alpha(mrad)  =   0.000 +/-  0.000   beta(mrad)  =   0.000 +/-  0.000     x0(mkm)  =   -0.1 +/-   0.0      y0(mkm)  =   -0.2 +/-   0.0 + Br => 0 @ r = 0
FF:   alpha(mrad)  =  -0.004 +/-  0.003   beta(mrad)  =  -0.023 +/-  0.003     x0(mkm)  =  355.5 +/- 1225.0     y0(mkm)  =  -25.4 +/-  85.
FHF:  alpha(mrad)  =  -0.009 +/-  0.006   beta(mrad)  =  -0.026 +/-  0.007     x0(mkm)  =  398.7 +/- 3509.6     y0(mkm)  =  -59.1 +/- 306.7
RHF:  alpha(mrad)  =  -0.009 +/-  0.006   beta(mrad)  =  -0.026 +/-  0.007     x0(mkm)  =  398.7 +/- 3509.6     y0(mkm)  =  -59.1 +/- 306.7
RFF:  alpha(mrad)  =  -0.003 +/-  0.002   beta(mrad)  =  -0.025 +/-  0.001     x0(mkm)  =  518.9 +/- 83647.6    y0(mkm)  =  -21.1 +/- 144.9
    < alpha(mrad)> =  -0.004 +/-  0.002<  beta(mrad)> =  -0.025 +/-  0.001<    x0(mkm)> =  364.0 +/- 1098.3<    y0(mkm)> =  -27.9 +/-  69.9
====== 08/29/12 =====
Only Bphi: Int_t Nr = 21, Double_t rMax = 210, Double_t Nz = 20, Double_t zMax = 200

 FF: alpha(mrad)  =   0.009 +/-  0.003   beta(mrad)  =  -0.027 +/-  0.003     x0(mkm)  =    27.2 +/-    9.3     y0(mkm)  =   -5.2 +/-     4.1
FHF: alpha(mrad)  =   0.012 +/-  0.007   beta(mrad)  =  -0.025 +/-  0.007     x0(mkm)  = -1183.8 +/- 6632.5     y0(mkm)  = -109.5 +/-   973.1
RHF: alpha(mrad)  =   0.012 +/-  0.007   beta(mrad)  =  -0.025 +/-  0.007     x0(mkm)  = -1183.8 +/- 6632.5     y0(mkm)  = -109.5 +/-   973.1
RFF: alpha(mrad)  =   0.012 +/-  0.003   beta(mrad)  =  -0.026 +/-  0.003     x0(mkm)  = -2733.6 +/- 3224.2     y0(mkm)  = -1101.8 +/- 1549.7
   < alpha(mrad)> =   0.010 +/-  0.002<  beta(mrad)> =  -0.026 +/-  0.002<    x0(mkm)> =    27.2 +/-    9.3<    y0(mkm)> =    -5.2 +/-    4.1

 */ 
