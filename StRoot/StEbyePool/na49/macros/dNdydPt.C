///////////////////////////////////////////////////////////////////////////////
//
// $Id: dNdydPt.C,v 1.4 2001/11/06 18:02:40 posk Exp $
//
// Author:       Glenn Cooper with help from Art Poskanzer, May '00
// Description:  Calculates dNdydPt .
//
//
///////////////////////////////////////////////////////////////////////////////
//
// $Log: dNdydPt.C,v $
// Revision 1.4  2001/11/06 18:02:40  posk
// 40 GeV compatability.
//
// Revision 1.3  2001/05/14 23:22:38  posk
// Minor changes.
//
// Revision 1.2  2001/03/06 17:32:53  posk
// All macros now work.
//
//
// Revision 1.1  2001/02/23 00:58:19  posk
// NA49 version of STAR software.
//
///////////////////////////////////////////////////////////////////////////////

enum Pid_t { kPionPlus, kPionMinus, kKaonPlus, kKaonMinus, kProton, kPbar };
enum Charge_t { kplus=0, kMinus=1 };
Char_t *PidName[] = { "pi+", "pi-", "K+", "K-", "pr", "pbar" };
Double_t Mass[6]  = { 0.140, 0.140, 0.494, 0.494, 0.938, 0.938 };
Int_t Charge[6]   = { kplus, kMinus, kplus, kMinus, kplus, kMinus };
Double_t YMax[6]  = { 3.8, 3.8, 3.2, 3.2, 2.91, 2.91 };
Int_t eBeamSav = 0;
//Double_t NPart[6] = { 366, 309, 242, 178, 132, 85 };

void dNdydPt()
 //help
{
  cout  << endl << "// At the point y,Pt: " << endl << "Double_t dNdydPt(Int_t pid, Double_t y, Double_t pt, Int_t cent, Int_t eBeam)" << endl << endl;
  cout << "// Integrated from ptmin to ptmax: " << endl << "Double_t dNdy(Int_t pid, Double_t ptmin, Double_t ptmax, Double_t y, Int_t cent, Int_t eBeam)" << endl << endl;
  cout << "// Averaged between ptman and ptmax: " << endl << "Double_t dNdydPt(Int_t pid, Double_t ptmin, Double_t ptmax, Double_t y, Int_t cent, Int_t eBeam)" << endl << endl;
  cout << "// Integrated between ymin-ymax and ptmin-ptmax: " << endl << "Double_t dN(Int_t pid, Double_t ymin, Double_t ymax, Double_t ptmin, Double_t ptmax, Int_t cent, Int_t eBeam)" << endl << endl;
  cout << "// Averaged between ymin-ymax and ptmin-ptmax: " << endl << "Double_t dNdydPt(Int_t pid, Double_t ymin, Double_t ymax, Double_t ptmin, Double_t ptmax, Int_t cent, Int_t eBeam)" << endl << endl;
}

Double_t dNdydPt(Int_t pid, Double_t y, Double_t pt, Int_t cent, Int_t eBeam = 158)
  // At the point y,Pt
{
  if (CheckInput(pid, y, pt, cent, eBeam)) {
    Double_t dNdy    = dNdy(y, pid, cent);
    Double_t dNdPt   = dNdPt(y, pt, pid, cent);
    Double_t dNdydPt = dNdy * dNdPt;
//     printf("dNdydPt= %.2f per GeV/c for %4s, y= %.2f, Pt= %.2f GeV, Cent= %d:\n", 
// 	   dNdydPt, PidName[pid], y, pt, cent);
    return dNdydPt;
  } else {
    return 0.;
  }
}

Double_t dNdy(Int_t pid, Double_t ptmin, Double_t ptmax, Double_t y, Int_t cent,
	      Int_t eBeam = 158)
  // Integrated from ptmin to ptmax
{
  if (CheckInput(pid, y, ptmin, cent, eBeam) && 
      CheckInput(pid, y, ptmax, cent, eBeam)) {
    TF1* fdndPt = (TF1*)gROOT->FindObject("fdNdPt");
    if (!fdndPt) fdndPt = new TF1("fdNdPt", fdNdPt, 0., 2., 3);
    fdndPt->SetParameter(0, pid);
    fdndPt->SetParameter(1, cent);
    fdndPt->SetParameter(2, y);
    Double_t dNdPt   = fdndPt->Integral(ptmin, ptmax);
    Double_t dNdy    = dNdy(y, pid, cent); 
    Double_t dNdydPt = dNdy * dNdPt;
//     printf("dNdy= %.2f for %4s, PtMin= %.2f, PtMax= %.2f, y= %.2f GeV, Cent= %d:\n",
// 	   dNdydPt, PidName[pid], ptmin, ptmax, y, cent);
    return dNdydPt;
  } else {
    return 0.;
  }
}

Double_t dNdydPt(Int_t pid, Double_t ptmin, Double_t ptmax, Double_t y, Int_t cent,
		 Int_t eBeam = 158)
  // Averaged between ptman and ptmax
{
  if (ptmax<=ptmin) return 0.; 
  Double_t dNdydPt   = dNdy(pid, ptmin, ptmax, y, cent, eBeam) / (ptmax - ptmin);
  printf("dNdydPt= %.2f per GeV/c for %4s, PtMin= %.2f, PtMax= %.2f, 
    y= %.2f GeV, Cent= %d:\n", dNdydPt, PidName[pid], ptmin, ptmax, y, cent);

  return dNdydPt;
}

Double_t dN(Int_t pid, Double_t ymin, Double_t ymax, Double_t ptmin,
		 Double_t ptmax, Int_t cent, Int_t eBeam = 158)
  // Integrated between ymin-ymax and ptmin-ptmax
{
  if (CheckInput(pid, ymin, ptmin, cent, eBeam) && 
      CheckInput(pid, ymax, ptmax, cent, eBeam)) {
    TF2* fdndydPt = (TF2*)gROOT->FindObject("fdNdydPt");
    if (!fdndydPt) fdndydPt = new TF2("fdNdydPt", fdNdydPt, -YMax[pid], YMax[pid],
				      0., 2., 3);
    fdndydPt->SetParameter(0, pid);
    fdndydPt->SetParameter(1, cent);
    Double_t dN = fdndydPt->Integral(ymin, ymax, ptmin, ptmax);
    printf("dN= %.2f for %4s, ymin= %.2f, ymax= %.2f, 
    ptmin= %.2f GeV, ptmax= %.2f GeV, Cent= %d:\n", dN, PidName[pid], 
	   ymin, ymax, ptmin, ptmax, cent);
    return dN;
  } else {
    return 0.;
  }
}

Double_t dNdydPt(Int_t pid, Double_t ymin, Double_t ymax, Double_t ptmin,
		 Double_t ptmax, Int_t cent, Int_t eBeam = 158)
  // Averaged between ymin-ymax and ptmin-ptmax
{
  if (ptmax<=ptmin || ymax<=ymin) return 0.; 
  Double_t dNdydPt = dN(pid, ymin, ymax, ptmin, ptmax, cent, eBeam) /
    ((ymax - ymin) * (ptmax - ptmin));
  printf("dNdydPt= %.2f per GeV/c for %4s, ymin= %.2f, ymax= %.2f, 
    ptmin= %.2f GeV, ptmax= %.2f GeV, Cent= %d:\n", dNdydPt, 
	 PidName[pid], ymin, ymax, ptmin, ptmax, cent);

  return dNdydPt;
}

/////////////////////////////////////////////////////////////////

Bool_t CheckInput(Int_t pid, Double_t y, Double_t pt, Int_t cent, Int_t eBeam)
{
  if (pid < 0 || pid > 5) {
    printf("parameter pid= %d out of range\n", pid);
    return kFALSE;
  } else if (y < -YMax[pid] || y > YMax[pid]) {
    printf("parameter y= %.2f out of range\n", y);
    return kFALSE;
  } else if (pt < 0. || pt > 2.) {
    printf("parameter pt= %.2f out of range\n", pt);
    return kFALSE;
  } else if (cent < 1 || cent > 6) {
    printf("parameter cent= %d out of range\n", cent);
    return kFALSE;
  } else if (eBeam != 158 && eBeam != 40) {
    printf("parameter eBeam= %d out of range\n", eBeam);
    return kFALSE;
  } else {
    if (eBeam != eBeamSav) {
      SetParameters(eBeam);
      eBeamSav = eBeam;
    }
    return kTRUE;
  }
}

Double_t T1(Double_t y, Int_t pid, Int_t cent)
{
  switch(pid) {
  case kPionPlus:
  case kPionMinus:
    return piT10[Charge[pid]][cent-1]/
      TMath::CosH(y/piyT1[Charge[pid]][cent-1]);
    break;
  case kKaonPlus:
  case kKaonMinus:
    return kT0[Charge[pid]][cent-1]/
      TMath::CosH(y/kyT[Charge[pid]][cent-1]);
    break;
  case kProton:
    return prT10[cent-1]/
      TMath::CosH(y/pryT1[cent-1]);
    break;
  case kPbar:
    return pbarT0[cent-1]/
      TMath::CosH(y/pbaryT[cent-1]);
    break;
  }
}

Double_t T2(Double_t y, Int_t pid, Int_t cent)
{
  switch(pid) {
  case kPionPlus:
  case kPionMinus:
    return piT20[Charge[pid]][cent-1];
    break;
  case kKaonPlus:
  case kKaonMinus:
    return 0.;
    break;
  case kProton:
    return prT20[cent-1]/TMath::CosH(y/pryT2[cent-1]);
    break;
  case kPbar:
    return 0.;
    break;
  }
}

Double_t Xi2(Double_t y, Int_t pid, Int_t cent)
{
  switch(pid) {
  case kPionPlus:
  case kPionMinus:
    return pixi20[Charge[pid]][cent-1];
    break;
  case kKaonPlus:
  case kKaonMinus:
    return 0.;
    break;
  case kProton:
    return -prxi20[cent-1]/TMath::CosH(y/pryxi2[cent-1]);
    break;
  case kPbar:
    return 0.;
    break;
  }
}

Double_t dNdPtTerm(Double_t pt, Double_t mass, Double_t T)
{
  double mPerp = TMath::Sqrt(mass*mass + pt*pt);
  return pt/(T*(mass+T)) * TMath::Exp(-(mPerp-mass)/T);
}

Double_t dNdPt(Double_t y, Double_t pt, Int_t pid, Int_t cent)
  // Really (1/N)dNdPt, a normalized function
{
  double m0   = Mass[pid];
  double t1   = T1(y, pid, cent);
  double pt_1 = dNdPtTerm(pt, m0, t1);
  double xi2  = Xi2(y, pid, cent);
  if (xi2==0.) return pt_1;
  if (pid==4) xi2 *= -1;
  double t2   = T2(y, pid, cent);
  double pt_2 = dNdPtTerm(pt, m0, t2);
  return (1-xi2)*pt_1 + xi2*pt_2;
}

Double_t fdNdPt(Double_t *x, Double_t *par)
{
  // x[0]   - Pt
  // par[0] - pid
  // par[1] - cent
  // par[2] - y
  return dNdPt(par[2]+0.5, x[0], par[0]+0.5, par[1]+0.5);
}

Double_t dNdy(Double_t y, Int_t pid, Int_t cent)
{
  double sigy, muy, ydev, ydev1;
  switch(pid) {
  case kPionPlus:
  case kPionMinus:
    sigy = pisigy[Charge[pid]][cent-1];
    ydev = y/sigy;
    return piA[Charge[pid]][cent-1]/
      TMath::Sqrt(2*TMath::Pi()*sigy*sigy)*
      TMath::Exp(-ydev*ydev/2);
    break;
  case kKaonPlus:
  case kKaonMinus:
    sigy  = ksigy[Charge[pid]][cent-1];
    muy   = kmuy[Charge[pid]][cent-1];
    ydev  = (y-muy)/sigy;
    ydev1 = (y+muy)/sigy;
    return kA[Charge[pid]][cent-1]/
      TMath::Sqrt(8*TMath::Pi()*sigy*sigy)*
      (TMath::Exp(-ydev*ydev/2)+TMath::Exp(-ydev1*ydev1/2));
    break;
  case kProton:
    sigy  = prsigy[cent-1];
    muy   = prmuy[cent-1];
    ydev  = (y-muy)/sigy;
    ydev1 = (y+muy)/sigy;
    return prA[cent-1]/
      TMath::Sqrt(8*TMath::Pi()*sigy*sigy)*
      (TMath::Exp(-ydev*ydev/2)+TMath::Exp(-ydev1*ydev1/2));
    break;
  case kPbar:
    sigy = pbarsigy[cent-1];
    ydev = y/sigy;
    return pbarA[cent-1]/
      TMath::Sqrt(2*TMath::Pi()*sigy*sigy)*
      TMath::Exp(-ydev*ydev/2);
    break;
  }
}

Double_t dndydPt(Int_t pid, Double_t y, Double_t pt, Int_t cent)
{
  Double_t dNdy    = dNdy(y, pid, cent);
  Double_t dNdPt   = dNdPt(y, pt, pid, cent);

  return dNdy * dNdPt;
}

Double_t fdNdydPt(Double_t *x, Double_t *par)
{
  // x[0]   - y
  // x[1]   - Pt
  // par[0] - pid
  // par[1] - cent
  return dndydPt(par[0]+0.5, x[0], x[1], par[1]+0.5);
}

const Int_t nCens = 6;
const Int_t nChars = 2;
typedef Double_t par_charge_cent[nChars][nCens];
typedef Double_t par_cent[nCens];

par_charge_cent piA;
par_charge_cent pisigy;
par_charge_cent piT10;
par_charge_cent piyT1;
par_charge_cent piT20;
par_charge_cent pixi20;

par_charge_cent kA;
par_charge_cent kmuy;
par_charge_cent ksigy;
par_charge_cent kT0;
par_charge_cent kyT;

par_cent prA;
par_cent prmuy;
par_cent prsigy;
par_cent prT10;
par_cent pryT1;
par_cent prxi20;
par_cent pryxi2;
par_cent prT20;
par_cent pryT2;

par_cent pbarA;
par_cent pbarsigy;
par_cent pbarT0;
par_cent pbaryT;

void SetParameters(Int_t eBeam) {
  // part, plus/minus, cent

  // pipl1
  piA[0][0] =  563.7 ;
  pisigy[0][0] = 1.47 ;
  piT10[0][0] = 0.208 ;
  piyT1[0][0] = 3.99 ;
  piT20[0][0] = 0.089 ;
  pixi20[0][0] = 0.246 ;
  
  // pipl2
  piA[0][1] =  466.1 ;
  pisigy[0][1] = 1.51 ;
  piT10[0][1] = 0.211 ;
  piyT1[0][1] = 3.38 ;
  piT20[0][1] = 0.089 ;
  pixi20[0][1] = 0.262 ;
  
  // pipl3  
  piA[0][2] =  345.6 ;
  pisigy[0][2] = 1.54 ;
  piT10[0][2] = 0.207 ;
  piyT1[0][2] = 3.83 ;
  piT20[0][2] = 0.084 ;
  pixi20[0][2] = 0.255 ;
  
  // pipl4
  piA[0][3] =  243.2 ;
  pisigy[0][3] = 1.58 ;
  piT10[0][3] = 0.207 ;
  piyT1[0][3] = 3.46 ;
  piT20[0][3] = 0.084 ;
  pixi20[0][3] = 0.266 ;
  
  // pipl5
  piA[0][4] =  168.6 ;
  pisigy[0][4] = 1.62 ;
  piT10[0][4] = 0.207 ;
  piyT1[0][4] = 3.33 ;
  piT20[0][4] = 0.083 ;
  pixi20[0][4] = 0.265 ;
  
  // pipl6
  piA[0][5] =  109.1 ;
  pisigy[0][5] = 1.72 ;
  piT10[0][5] = 0.198 ;
  piyT1[0][5] = -4.0 ;
  piT20[0][5] = 0.081 ;
  pixi20[0][5] = 0.244 ;
  
  // pimi1
  piA[1][0] = 606.6;
  pisigy[1][0] = 1.41;
  piT10[1][0] = 0.213 ;
  piyT1[1][0] = 2.73 ;
  piT20[1][0] = 0.081 ;
  pixi20[1][0] = 0.261 ;
  
  // pimi2
  piA[1][1] =  495.8 ;
  pisigy[1][1] = 1.46 ;
  piT10[1][1] = 0.212 ;
  piyT1[1][1] = 2.92 ;
  piT20[1][1] = 0.080 ;
  pixi20[1][1] = 0.274 ;
  
  // pimi3
  piA[1][2] =  370.0 ;
  pisigy[1][2] = 1.49 ;
  piT10[1][2] = 0.211 ;
  piyT1[1][2] = 2.88 ;
  piT20[1][2] = 0.075 ;
  pixi20[1][2] = 0.282 ;
  
  // pimi4
  piA[1][3] =  256.5 ;
  pisigy[1][3] = 1.51 ;
  piT10[1][3] = 0.209 ;
  piyT1[1][3] = 2.93 ;
  piT20[1][3] = 0.079 ;
  pixi20[1][3] = 0.279 ;

  // pimi5
  piA[1][4] =  179.8 ;
  pisigy[1][4] = 1.58 ;
  piT10[1][4] = 0.204 ;
  piyT1[1][4] = 3.03 ;
  piT20[1][4] = 0.078 ;
  pixi20[1][4] = 0.272 ;
  
  // pimi6
  piA[1][5] =  116.67 ;
  pisigy[1][5] = 1.67 ;
  piT10[1][5] = 0.199 ;
  piyT1[1][5] = 3.03 ;
  piT20[1][5] = 0.077 ;
  pixi20[1][5] = 0.260 ;
  
  // kpl1
  kA[0][0] =  102.1 ;
  kmuy[0][0] =  0.77 ;
  ksigy[0][0] =  0.85 ;
  kT0[0][0] =  0.251 ;
  kyT[0][0] =  2.37 ;
  
  // kpl2
  kA[0][1] =   81.1 ;
  kmuy[0][1] =  0.79 ;
  ksigy[0][1] =  0.87 ;
  kT0[0][1] =  0.246 ;
  kyT[0][1] =  2.85 ;
  
  // kpl3
  kA[0][2] =   58.4 ;
  kmuy[0][2] =  0.80 ;
  ksigy[0][2] =  0.88 ;
  kT0[0][2] =  0.243 ;
  kyT[0][2] =  2.59 ;
  
  // kpl4
  kA[0][3] =   38.25 ;
  kmuy[0][3] =  0.80 ;
  ksigy[0][3] =  0.88 ;
  kT0[0][3] =  0.237 ;
  kyT[0][3] =  2.48 ;
  
  // kpl5
  kA[0][4] =   24.71 ;
  kmuy[0][4] =  0.83 ;
  ksigy[0][4] =  0.88 ;
  kT0[0][4] =  0.219 ;
  kyT[0][4] =  3.6 ;
  
  // kpl6
  kA[0][5] =   14.38 ;
  kmuy[0][5] =  0.87 ;
  ksigy[0][5] =  0.88 ;
  kT0[0][5] =  0.214 ;
  kyT[0][5] =  3.01 ;
  
  // kmi1
  kA[1][0] =  45.68 ;
  kmuy[1][0] = 0.72 ;
  ksigy[1][0] = 0.80 ;
  kT0[1][0] = 0.233 ;
  kyT[1][0] = 2.25 ;
  
  // kmi2[1][0] =;
  kA[1][1] =  36.46 ;
  kmuy[1][1] = 0.71 ;
  ksigy[1][1] = 0.83 ;
  kT0[1][1] = 0.232 ;
  kyT[1][1] = 2.25 ;
  
  // kmi3
  kA[1][2] =  26.33 ;
  kmuy[1][2] = 0.73 ;
  ksigy[1][2] = 0.81 ;
  kT0[1][2] = 0.229 ;
  kyT[1][2] = 2.28 ;
  
  // kmi4
  kA[1][3] =  17.30 ;
  kmuy[1][3] = 0.71 ;
  ksigy[1][3] = 0.87 ;
  kT0[1][3] = 0.218 ;
  kyT[1][3] = 2.45 ;

  // kmi5
  kA[1][4] =  11.31 ;
  kmuy[1][4] = 0.70 ;
  ksigy[1][4] = 0.87 ;
  kT0[1][4] = 0.214 ;
  kyT[1][4] = 2.33 ;
  
  // kmi6
  kA[1][5] =  6.48 ;
  kmuy[1][5] = 0.67 ;
  ksigy[1][5] = 0.91 ;
  kT0[1][5] = 0.195 ;
  kyT[1][5] = 2.77 ;
  
  // proton1
  prA[0] =  137.1 ;
  prmuy[0] = 1.435 ;
  prsigy[0] = 0.976 ;
  prT10[0] = 0.308 ;
  pryT1[0] = 2.08 ;
  prxi20[0] = 0.09 ;
  pryxi2[0] = 100 ;
  prT20[0] = 0.109 ;
  pryT2[0] = 2.6 ;
  
  // proton2
  prA[1] =  126.2 ;
  prmuy[1] = 1.581 ;
  prsigy[1] = 1.057 ;
  prT10[1] = 0.299 ;
  pryT1[1] = 2.23 ;
  prxi20[1] = 0.10 ;
  pryxi2[1] = 2.2 ;
  prT20[1] = 0.115 ;
  pryT2[1] = 2.1 ;
  
  // proton3
  prA[2] =  112.3 ;
  prmuy[2] = 1.825 ;
  prsigy[2] = 1.164 ;
  prT10[2] = 0.295 ;
  pryT1[2] = 2.32 ;
  prxi20[2] = 0.09 ;
  pryxi2[2] = 1.47 ;
  prT20[2] = 0.110 ;
  pryT2[2] = 2.1 ;
  
  // proton4
  prA[3] =  98.5 ;
  prmuy[3] = 2.12 ;
  prsigy[3] = 1.264 ;
  prT10[3] = 0.283 ;
  pryT1[3] = 2.44 ;
  prxi20[3] = 0.04 ;
  pryxi2[3] = -1.8 ;
  prT20[3] = 0.115 ;
  pryT2[3] = 1.24 ;
  
  // proton5
  prA[4] =  87 ;
  prmuy[4] = 2.46 ;
  prsigy[4] = 1.36 ;
  prT10[4] = 0.284 ;
  pryT1[4] = 2.14 ;
  prxi20[4] = 0.02 ;
  pryxi2[4] = 100 ;
  prT20[4] = 0.097 ;
  pryT2[4] = -1.4 ;
  
  // proton6
  prA[5] =  105 ;
  prmuy[5] = 3.43 ;
  prsigy[5] = 1.64 ;
  prT10[5] = 0.277 ;
  pryT1[5] = 2.06 ;
  prxi20[5] = 0.05 ;
  pryxi2[5] = 100.0 ;
  prT20[5] = 0.12 ;
  pryT2[5] = 0.12 ;
  
  // pbar1
  pbarA[0] = 4.90 ;
  pbarsigy[0] =1.60 ;
  pbarT0[0] =0.360 ;
  pbaryT[0] =1.9 ;
  
  // pbar2
  pbarA[1] = 3.62 ;
  pbarsigy[1] =1.55 ;
  pbarT0[1] =0.328 ;
  pbaryT[1] =1.88 ;
  
  // pbar3
  pbarA[2] = 2.88 ;
  pbarsigy[2] =1.52 ;
  pbarT0[2] =0.369 ;
  pbaryT[2] =1.42 ;
  
  // pbar4
  pbarA[3] = 1.95 ;
  pbarsigy[3] =1.47 ;
  pbarT0[3] =0.329 ;
  pbaryT[3] =1.47 ;
  
  // pbar5
  pbarA[4] = 1.35 ;
  pbarsigy[4] =1.44 ;
  pbarT0[4] =0.302 ;
  pbaryT[4] =4.3 ;
  
  // pbar6
  pbarA[5] = 0.84 ;
  pbarsigy[5] =1.40 ;
  pbarT0[5] =0.31 ;
  pbaryT[5] =2.8 ;
  
  // 40 GeV changes
  if (eBeam==40) {
    Float_t piA40  = 0.55;               // pion total yield
    Float_t sigy40 = 0.8;                // pion and kaon gaussian widths
    Float_t y40    = 2.24/2.92;          // ratio of yCM values
    Float_t KAm    = 0.4;                // K- total yield
    Float_t KAp    = 0.52;               // K+ total yield
    Float_t pBar   = 0.5;                // p bar total yield
    for (int n = 0; n < nCens; n++) {
      for (int c = 0; c < nChars; c++) {
	piA[c][n]    *= piA40;
	pisigy[c][n] *= sigy40;
	kA[c][n]     *= (c) ? KAm : KAp;
	ksigy[c][n]  *= sigy40;
	kmuy[c][n]   *= y40;             // separation between the 2 gaussians
      }
      prmuy[n]     *= y40;               // separation between the 2 gaussians
      pbarA[n]     *= pBar;
    }
  }
  
}
