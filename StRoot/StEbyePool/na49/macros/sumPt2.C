// Calculates the sum of Pt square for all the particles.
// Glenn Cooper April 00
#include <stdio.h>

enum Pid_t { kPionPlus, kPionMinus, kKaonPlus, kKaonMinus, kProton, kPbar };
Char_t *PidName[] = { "pi+", "pi-", "K+", "K-", "pr", "pbar" };
enum Charge_t { kPlus=0, kMinus=1 };

Double_t Mass[6] = { 0.140, 0.140, 0.494, 0.494, 0.938, 0.938 };
Int_t Charge[6] = { kPlus, kMinus, kPlus, kMinus, kPlus, kMinus };
Double_t YMax[6] = { 3.8, 3.8, 3.2, 3.2, 2.91, 2.91 };

Double_t NPart[6] = { 366, 309, 242, 178, 132, 85 };

Double_t GetMass(Int_t ipid)
{
  return Mass[ipid];
}

Double_t GetT1(Double_t y, Int_t ipid, Int_t cent)
{
  switch(ipid) {
  case kPionPlus:
  case kPionMinus:
    return piT10[Charge[ipid]][cent-1]/
      TMath::CosH(y/piyT1[Charge[ipid]][cent-1]);
    break;
  case kKaonPlus:
  case kKaonMinus:
    return kT0[Charge[ipid]][cent-1]/
      TMath::CosH(y/kyT[Charge[ipid]][cent-1]);
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

Double_t GetT2(Double_t y, Int_t ipid, Int_t cent)
{
  switch(ipid) {
  case kPionPlus:
  case kPionMinus:
    return piT20[Charge[ipid]][cent-1];
    break;
  case kKaonPlus:
  case kKaonMinus:
    return 0;
    break;
  case kProton:
    return prT20[cent-1]/TMath::CosH(y/pryT2[cent-1]);
    break;
  case kPbar:
    return 0;
    break;
  }
}

Double_t GetXi2(Double_t y, Int_t ipid, Int_t cent)
{
  switch(ipid) {
  case kPionPlus:
  case kPionMinus:
    return pixi20[Charge[ipid]][cent-1];
    break;
  case kKaonPlus:
  case kKaonMinus:
    return 0;
    break;
  case kProton:
    return -prxi20[cent-1]/TMath::CosH(y/pryxi2[cent-1]);
    break;
  case kPbar:
    return 0;
    break;
  }
}

Double_t GetMeanPt2_1(Double_t mass, Double_t T)
{
  double tm = T/mass;
  return 2*mass*T*(1+3*tm+3*tm*tm)/(1+tm);
}

Double_t GetMeanPt2(Double_t y, Int_t ipid, Int_t cent)
{
  double m = GetMass(ipid);
  double t1 = GetT1(y, ipid, cent);
  double mpt2_1 = GetMeanPt2_1(m,t1);
  double t2 = GetT2(y, ipid, cent);
  double mpt2_2 = GetMeanPt2_1(m,t2);
  double xi2 = GetXi2(y, ipid, cent);
  return (1-xi2)*mpt2_1 + xi2*mpt2_2;
}

Double_t GetDndy(Double_t y, Int_t ipid, Int_t cent)
{
  double sigy, muy, ydev, ydev1;
  switch(ipid) {
  case kPionPlus:
  case kPionMinus:
    sigy = pisigy[Charge[ipid]][cent-1];
    ydev = y/sigy;
    return piA[Charge[ipid]][cent-1]/
      TMath::Sqrt(2*TMath::Pi()*sigy*sigy)*
      TMath::Exp(-ydev*ydev/2);
    break;
  case kKaonPlus:
  case kKaonMinus:
    sigy = ksigy[Charge[ipid]][cent-1];
    muy = kmuy[Charge[ipid]][cent-1];
    ydev = (y-muy)/sigy;
    ydev1 = (y+muy)/sigy;
    return kA[Charge[ipid]][cent-1]/
      TMath::Sqrt(8*TMath::Pi()*sigy*sigy)*
      (TMath::Exp(-ydev*ydev/2)+TMath::Exp(-ydev1*ydev1/2));
    break;
  case kProton:
    sigy = prsigy[cent-1];
    muy = prmuy[cent-1];
    ydev = (y-muy)/sigy;
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

Double_t fDndy(Double_t *x, Double_t *par)
{
  // x[0] - y
  // par[0] - ipid
  // par[1] - cent
  return GetDndy(x[0], par[0]+0.5, par[1]+0.5);
}

Double_t GetSumPt2(Double_t y, Int_t ipid, Int_t cent)
{
  return GetDndy(y,ipid,cent)*GetMeanPt2(y,ipid,cent);
}

Double_t fSumPt2(Double_t *x, Double_t *par)
{
  // x[0] - y
  // par[0] - ipid
  // par[1] - cent
  return GetSumPt2(x[0], par[0]+0.5, par[1]+0.5);

}

void Run(Int_t cent)
{
  TF1* fdndy = (TF1*)gROOT->FindObject("fdndy");
  if(!fdndy) fdndy = new TF1("fdndy",(void*)fDndy,-2.8,2.8,2);
  TF1* fsumpt2 = (TF1*)gROOT->FindObject("fsumpt2");
  if(!fsumpt2) fsumpt2 = new TF1("fsumpt2",(void*)fSumPt2,-2.8,2.8,2);
  fdndy->SetParameter(1,cent);
  fsumpt2->SetParameter(1,cent);
  FILE *f = fopen("sumpt2.txt","a");
  printf("Sum of pt^2 for Cent %d:\n", cent);
  fprintf(f,"Sum of pt^2 for Cent %d:\n", cent);
  Double_t N[6];
  Double_t SumPt2[6];
  for(int ipid=0; ipid<6; ipid++) {
    fdndy->SetParameter(0,ipid);
    fsumpt2->SetParameter(0,ipid);
    N[ipid] = fdndy->Integral(-YMax[ipid],YMax[ipid]);
    SumPt2[ipid] = fsumpt2->Integral(-YMax[ipid],YMax[ipid]);
    printf("  %4s:  %.2f\n", PidName[ipid], SumPt2[ipid]);
    fprintf(f,"  %4s:  %.2f\n", PidName[ipid], SumPt2[ipid]);
  }
  fprintf(f,"\n");
  printf("   All:  %.2f\n", 3.0*(SumPt2[kPionPlus]+SumPt2[kPionMinus])/2 + 
	 2.0*(SumPt2[kKaonPlus]+SumPt2[kKaonMinus]) +
	 NPart[cent-1]*SumPt2[kProton]/N[kProton]);
  fprintf(f,"   All:  %.2f\n", 3.0*(SumPt2[kPionPlus]+SumPt2[kPionMinus])/2 + 
	  2.0*(SumPt2[kKaonPlus]+SumPt2[kKaonMinus]) +
	  NPart[cent-1]*SumPt2[kProton]/N[kProton]);
  printf("\n");
}

typedef Double_t par_charge_cent[2][6];
typedef Double_t par_cent[6];

par_charge_cent piA;
par_charge_cent pisigy;
par_charge_cent piT10;
par_charge_cent piyT1;
par_charge_cent piT20;
par_charge_cent pixi20;

// part-plus/minus-cent

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

piA[1][0] =  606.6 ;
pisigy[1][0] = 1.41 ;
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

par_charge_cent kA;
par_charge_cent kmuy;
par_charge_cent ksigy;
par_charge_cent kT0;
par_charge_cent kyT;

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

par_cent prA;
par_cent prmuy;
par_cent prsigy;
par_cent prT10;
par_cent pryT1;
par_cent prxi20;
par_cent pryxi2;
par_cent prT20;
par_cent pryT2;

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

par_cent pbarA;
par_cent pbarsigy;
par_cent pbarT0;
par_cent pbaryT;

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
