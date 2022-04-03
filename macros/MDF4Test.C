#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Rtypes.h"
#include "TAxis.h"
#include "TF1.h"
#endif
/* 
c1 = new TCanvas("c1","Inner");
c1->Clear()
c1->Divide(2,2)
c1->cd(1)
FitP->SetMarkerColor(2);FitP->Draw("mu-0.4972:z0>>tmbk","(j==1&&chisq<200&&dmu<0.01)","profs")
FitP->SetMarkerColor(1); FitP->Draw("mu-tb1(z0)-pad1(z1)-z1(z2)-adc1(z3):z0>>tmbC","j==1&&chisq<200&&dmu<0.01","profssame")
c1->cd(2)
FitP->SetMarkerColor(2);FitP->Draw("mu-0.4972:z1>>pad","(j==1&&chisq<200&&dmu<0.01)","profs")
FitP->SetMarkerColor(1); FitP->Draw("mu-tb1(z0)-pad1(z1)-z1(z2)-adc1(z3):z1>>padC","j==1&&chisq<200&&dmu<0.01","profssame")
c1->cd(3)
FitP->SetMarkerColor(2);FitP->Draw("mu-0.4972:z2>>z","(j==1&&chisq<200&&dmu<0.01)","profs")
FitP->SetMarkerColor(1); FitP->Draw("mu-tb1(z0)-pad1(z1)-z1(z2)-adc1(z3):z2>>zC","j==1&&chisq<200&&dmu<0.01","profssame")
c1->cd(4)
FitP->SetMarkerColor(2);FitP->Draw("mu-0.4972:z3>>adcL","(j==1&&chisq<200&&dmu<0.01)","profs")
FitP->SetMarkerColor(1); FitP->Draw("mu-tb1(z0)-pad1(z1)-z1(z2)-adc1(z3):z3>>adcLC","j==1&&chisq<200&&dmu<0.01","profssame")

c2 = new TCanvas("c2","Outer");
c2->Clear()
c2->Divide(2,2)
c2->cd(1)
FitP->SetMarkerColor(2);FitP->Draw("mu-0.4972:z0","(j==2&&chisq<200&&dmu<0.01)","profs")
FitP->SetMarkerColor(1); FitP->Draw("mu-tb1(z0)-pad1(z1)-z1(z2)-adc1(z3):z0","j==2&&chisq<200&&dmu<0.01","profssame")
c2->cd(2)
FitP->SetMarkerColor(2);FitP->Draw("mu-0.4972:z1","(j==2&&chisq<200&&dmu<0.01)","profs")
FitP->SetMarkerColor(1); FitP->Draw("mu-tb1(z0)-pad1(z1)-z1(z2)-adc1(z3):z1","j==2&&chisq<200&&dmu<0.01","profssame")
c2->cd(3)
FitP->SetMarkerColor(2);FitP->Draw("mu-0.4972:z2","(j==2&&chisq<200&&dmu<0.01)","profs")
FitP->SetMarkerColor(1); FitP->Draw("mu-tb1(z0)-pad1(z1)-z1(z2)-adc1(z3):z2","j==2&&chisq<200&&dmu<0.01","profssame")
c2->cd(4)
FitP->SetMarkerColor(2);FitP->Draw("mu-0.4972:z3","(j==2&&chisq<200&&dmu<0.01)","profs")
FitP->SetMarkerColor(1); FitP->Draw("mu-tb1(z0)-pad1(z1)-z1(z2)-adc1(z3):z3","j==2&&chisq<200&&dmu<0.01","profssame")
================================================================================
TCanvas *cI = new TCanvas("cI","Inner");
cI->Divide(2,2);
cI->cd(1);
FitP->SetLineColor(1); FitP->SetMarkerColor(1); FitP->Draw("mu-0.4972:z0>>tmbkI","j==1&&chisq<200&&dmu<0.01","profs")
FitP->SetLineColor(2); FitP->SetMarkerColor(2); FitP->Draw("mu-mdf0(z0,z1,z2,z3):z0>>tmbkIC","j==1&&chisq<200&&dmu<0.01","profssame")
cI->cd(2);
FitP->SetLineColor(1); FitP->SetMarkerColor(1); FitP->Draw("mu-0.4972:z1>>padI","j==1&&chisq<200&&dmu<0.01","profs")
FitP->SetLineColor(2); FitP->SetMarkerColor(2); FitP->Draw("mu-mdf0(z0,z1,z2,z3):z1>>padIC","j==1&&chisq<200&&dmu<0.01","profssame")
cI->cd(3);
FitP->SetLineColor(1); FitP->SetMarkerColor(1); FitP->Draw("mu-0.4972:z2>>zI","j==1&&chisq<200&&dmu<0.01","profs")
FitP->SetLineColor(2); FitP->SetMarkerColor(2); FitP->Draw("mu-mdf0(z0,z1,z2,z3):z2>>zIC","j==1&&chisq<200&&dmu<0.01","profssame")
cI->cd(4);
FitP->SetLineColor(1); FitP->SetMarkerColor(1); FitP->Draw("mu-0.4972:z3>>adcLI","j==1&&chisq<200&&dmu<0.01","profs")
FitP->SetLineColor(2); FitP->SetMarkerColor(2); FitP->Draw("mu-mdf0(z0,z1,z2,z3):z3>>adcLIC","j==1&&chisq<200&&dmu<0.01","profssame")

TCanvas *cO = new TCanvas("cO","Outer");
cO->Divide(2,2);
cO->cd(1);
FitP->SetLineColor(1); FitP->SetMarkerColor(1); FitP->Draw("mu-0.4972:z0>>tmbkO","j==2&&chisq<200&&dmu<0.01","profs")
FitP->SetLineColor(2); FitP->SetMarkerColor(2); FitP->Draw("mu-mdf1(z0,z1,z2,z3):z0>>tmbkOC","j==2&&chisq<200&&dmu<0.01","profssame")
cO->cd(2);
FitP->SetLineColor(1); FitP->SetMarkerColor(1); FitP->Draw("mu-0.4972:z1>>padO","j==2&&chisq<200&&dmu<0.01","profs")
FitP->SetLineColor(2); FitP->SetMarkerColor(2); FitP->Draw("mu-mdf1(z0,z1,z2,z3):z1>>padOC","j==2&&chisq<200&&dmu<0.01","profssame")
cO->cd(3);
FitP->SetLineColor(1); FitP->SetMarkerColor(1); FitP->Draw("mu-0.4972:z2>>zO","j==2&&chisq<200&&dmu<0.01","profs")
FitP->SetLineColor(2); FitP->SetMarkerColor(2); FitP->Draw("mu-mdf1(z0,z1,z2,z3):z2>>zOC","j==2&&chisq<200&&dmu<0.01","profssame")
cO->cd(4);
FitP->SetLineColor(1); FitP->SetMarkerColor(1); FitP->Draw("mu-0.4972:z3>>adcLO","j==2&&chisq<200&&dmu<0.01","profs")
FitP->SetLineColor(2); FitP->SetMarkerColor(2); FitP->Draw("mu-mdf1(z0,z1,z2,z3):z3>>adcLOC","j==2&&chisq<200&&dmu<0.01","profssame")
================================================================================
TCanvas *cI = new TCanvas("cI","Inner");
cI->Divide(2,2);
cI->cd(1);
FitP->SetLineColor(1); FitP->SetMarkerColor(1); FitP->Draw("mu-0.4972:z0>>tmbkI","j==1&&chisq<200&&dmu<0.01","profs")
FitP->SetLineColor(2); FitP->SetMarkerColor(2); FitP->Draw("mu-tmdf0(z0,z1,z2,z3):z0>>tmbkIC","j==1&&chisq<200&&dmu<0.01","profssame")
cI->cd(2);
FitP->SetLineColor(1); FitP->SetMarkerColor(1); FitP->Draw("mu-0.4972:z1>>padI","j==1&&chisq<200&&dmu<0.01","profs")
FitP->SetLineColor(2); FitP->SetMarkerColor(2); FitP->Draw("mu-tmdf0(z0,z1,z2,z3):z1>>padIC","j==1&&chisq<200&&dmu<0.01","profssame")
cI->cd(3);
FitP->SetLineColor(1); FitP->SetMarkerColor(1); FitP->Draw("mu-0.4972:z2>>zI","j==1&&chisq<200&&dmu<0.01","profs")
FitP->SetLineColor(2); FitP->SetMarkerColor(2); FitP->Draw("mu-tmdf0(z0,z1,z2,z3):z2>>zIC","j==1&&chisq<200&&dmu<0.01","profssame")
cI->cd(4);
FitP->SetLineColor(1); FitP->SetMarkerColor(1); FitP->Draw("mu-0.4972:z3>>adcLI","j==1&&chisq<200&&dmu<0.01","profs")
FitP->SetLineColor(2); FitP->SetMarkerColor(2); FitP->Draw("mu-tmdf0(z0,z1,z2,z3):z3>>adcLIC","j==1&&chisq<200&&dmu<0.01","profssame")

TCanvas *cO = new TCanvas("cO","Outer");
cO->Divide(2,2);
cO->cd(1);
FitP->SetLineColor(1); FitP->SetMarkerColor(1); FitP->Draw("mu-0.4972:z0>>tmbkO","j==2&&chisq<200&&dmu<0.01","profs")
FitP->SetLineColor(2); FitP->SetMarkerColor(2); FitP->Draw("mu-tmdf1(z0,z1,z2,z3):z0>>tmbkOC","j==2&&chisq<200&&dmu<0.01","profssame")
cO->cd(2);
FitP->SetLineColor(1); FitP->SetMarkerColor(1); FitP->Draw("mu-0.4972:z1>>padO","j==2&&chisq<200&&dmu<0.01","profs")
FitP->SetLineColor(2); FitP->SetMarkerColor(2); FitP->Draw("mu-tmdf1(z0,z1,z2,z3):z1>>padOC","j==2&&chisq<200&&dmu<0.01","profssame")
cO->cd(3);
FitP->SetLineColor(1); FitP->SetMarkerColor(1); FitP->Draw("mu-0.4972:z2>>zO","j==2&&chisq<200&&dmu<0.01","profs")
FitP->SetLineColor(2); FitP->SetMarkerColor(2); FitP->Draw("mu-tmdf1(z0,z1,z2,z3):z2>>zOC","j==2&&chisq<200&&dmu<0.01","profssame")
cO->cd(4);
FitP->SetLineColor(1); FitP->SetMarkerColor(1); FitP->Draw("mu-0.4972:z3>>adcLO","j==2&&chisq<200&&dmu<0.01","profs")
FitP->SetLineColor(2); FitP->SetMarkerColor(2); FitP->Draw("mu-tmdf1(z0,z1,z2,z3):z3>>adcLOC","j==2&&chisq<200&&dmu<0.01","profssame")
*/
Double_t tb1(Double_t x) {
//========= Macro generated from object: TB/pol3
//========= by ROOT version5.34/39
  static TF1 *TB = 0;
  if (! TB) {
    TB = new TF1("TB","pol3",0,25);
    TB->SetFillColor(19);
    TB->SetFillStyle(0);
    TB->SetMarkerStyle(20);
    TB->SetLineWidth(3);
    TB->SetChisquare(223.681);
    TB->SetNDF(3);
    TB->GetXaxis()->SetTitleOffset(1.2);
    TB->SetParameter(0,0.811028);
    TB->SetParError(0,0.00228576);
    TB->SetParLimits(0,0,0);
    TB->SetParameter(1,-0.0734656);
    TB->SetParError(1,0.000644001);
    TB->SetParLimits(1,0,0);
    TB->SetParameter(2,0.00448237);
    TB->SetParError(2,5.34402e-05);
    TB->SetParLimits(2,0,0);
    TB->SetParameter(3,-9.6498e-05);
    TB->SetParError(3,1.31125e-06);
    TB->SetParLimits(3,0,0);
  }
  return TB->Eval(x);
}
Double_t pad1(Double_t x) 
{
//========= Macro generated from object: pol3/pol3
//========= by ROOT version5.34/39
   
  static TF1 *PAD = 0;
  if (! PAD) {
   PAD = new TF1("PAD","pol3",-1,1);
   PAD->SetFillColor(19);
   PAD->SetFillStyle(0);
   PAD->SetMarkerStyle(20);
   PAD->SetLineWidth(3);
   PAD->SetChisquare(1711.79);
   PAD->SetNDF(2);
   PAD->GetXaxis()->SetTitleOffset(1.2);
   PAD->SetParameter(0,0.292961);
   PAD->SetParError(0,0.00156549);
   PAD->SetParLimits(0,0,0);
   PAD->SetParameter(1,-0.163953);
   PAD->SetParError(1,0.000983758);
   PAD->SetParLimits(1,0,0);
   PAD->SetParameter(2,0.0251774);
   PAD->SetParError(2,0.000184146);
   PAD->SetParLimits(2,0,0);
   PAD->SetParameter(3,-0.00112136);
   PAD->SetParError(3,9.51956e-06);
   PAD->SetParLimits(3,0,0);
  }
  return PAD->Eval(x);
}
Double_t z1(Double_t x)
{
//========= Macro generated from object: Z/pol5
//========= by ROOT version5.34/39
   
  static TF1 *Z = 0;
  if (! Z) {
    Z = new TF1("Z","pol5",-1,1);
   Z->SetFillColor(19);
   Z->SetFillStyle(0);
   Z->SetMarkerStyle(20);
   Z->SetLineWidth(3);
   Z->SetChisquare(41.4035);
   Z->SetNDF(87);
   Z->GetXaxis()->SetTitleOffset(1.2);
   Z->SetParameter(0,0.00750877);
   Z->SetParError(0,0.000854567);
   Z->SetParLimits(0,0,0);
   Z->SetParameter(1,-0.000182975);
   Z->SetParError(1,8.92008e-05);
   Z->SetParLimits(1,0,0);
   Z->SetParameter(2,-2.9037e-06);
   Z->SetParError(2,2.90536e-06);
   Z->SetParLimits(2,0,0);
   Z->SetParameter(3,8.73229e-08);
   Z->SetParError(3,3.93064e-08);
   Z->SetParLimits(3,0,0);
   Z->SetParameter(4,-7.15764e-10);
   Z->SetParError(4,2.32685e-10);
   Z->SetParLimits(4,0,0);
   Z->SetParameter(5,1.94037e-12);
   Z->SetParError(5,4.9828e-13);
   Z->SetParLimits(5,0,0);
   //   Z->Draw("");
  }
  return Z->Eval(x);
}

Double_t adc1(Double_t x){
//========= Macro generated from object: A/pol5
//========= by ROOT version5.34/39
   
  static TF1 *A = 0;
  if (! A) {
   A = new TF1("A","pol5",-1,1);
   A->SetFillColor(19);
   A->SetFillStyle(0);
   A->SetMarkerStyle(20);
   A->SetLineWidth(3);
   A->SetChisquare(519.884);
   A->SetNDF(36);
   A->GetXaxis()->SetTitleOffset(1.2);
   A->SetParameter(0,-6.22461);
   A->SetParError(0,0.160586);
   A->SetParLimits(0,0,0);
   A->SetParameter(1,5.94442);
   A->SetParError(1,0.136042);
   A->SetParLimits(1,0,0);
   A->SetParameter(2,-2.11211);
   A->SetParError(2,0.0451857);
   A->SetParLimits(2,0,0);
   A->SetParameter(3,0.355167);
   A->SetParError(3,0.00735072);
   A->SetParLimits(3,0,0);
   A->SetParameter(4,-0.0285716);
   A->SetParError(4,0.000585509);
   A->SetParLimits(4,0,0);
   A->SetParameter(5,0.00088602);
   A->SetParError(5,1.82708e-05);
   A->SetParLimits(5,0,0);
   //   A->Draw("");
  }
  return A->Eval(x);
}
Double_t tb2(Double_t x) {
//========= Macro generated from object: TB/pol3
//========= by ROOT version5.34/39
  static TF1 *TB = 0;
  if (! TB) {
  }
  return TB->Eval(x);
}
Double_t pad2(Double_t x) 
{
//========= Macro generated from object: pol3/pol3
//========= by ROOT version5.34/39
   
  static TF1 *PAD = 0;
  if (! PAD) {
  }
  return PAD->Eval(x);
}
Double_t z2(Double_t x)
{
//========= Macro generated from object: Z/pol5
//========= by ROOT version5.34/39
   
  static TF1 *Z = 0;
  if (! Z) {
  }
  return Z->Eval(x);
}

Double_t adc2(Double_t x){
//========= Macro generated from object: A/pol5
//========= by ROOT version5.34/39
   
  static TF1 *A = 0;
  if (! A) {
  }
  return A->Eval(x);
}
//================================================================================
// -*- mode: c++ -*-
// 
// File MDF1MDF.cxx generated by TMultiDimFit::MakeRealCode
// on Tue Mar 29 14:57:55 2022
// ROOT version 5.34/39
//
// This file contains the function 
//
//    double  MDF1::MDF(double *x); 
//
// For evaluating the parameterization obtained
// from TMultiDimFit and the point x
// 
// See TMultiDimFit class documentation for more information 
// 
//#include "MDF1.h"
namespace MDF1 {
//
// Static data variables
//
int    gNVariables    = 4;
int    gNCoefficients = 20;
double gDMean         = 0.481485;
// Assignment to mean vector.
double gXMean[] = {
  -0.521274, -0.719245, -0.017131, -0.365224 };

// Assignment to minimum vector.
double gXMin[] = {
  3.5, 2, 2, 3.57 };

// Assignment to maximum vector.
double gXMax[] = {
  23, 12.5, 186, 9.415 };

// Assignment to coefficients vector.
double gCoefficient[] = {
  -0.0731407,
  -0.037936,
  0.00505927,
  -0.00053619,
  -0.0833821,
  -0.0522851,
  -0.0398875,
  0.198457,
  0.0227179,
  -0.015714,
  0.0171056,
  -0.0407999,
  -0.0559183,
  -0.068934,
  0.0194381,
  0.0208321,
  -0.00527045,
  0.0438592,
  -0.00771385,
  -0.0485459
 };

// Assignment to error coefficients vector.
double gCoefficientRMS[] = {
  9.68575e-11,
  5.2671e-10,
  1.28004e-09,
  3.93473e-10,
  2.67332e-09,
  2.34933e-10,
  6.94905e-10,
  1.29099e-09,
  4.19806e-10,
  2.7707e-10,
  5.59378e-10,
  3.41016e-09,
  3.26467e-10,
  1.73363e-09,
  1.31688e-10,
  2.04048e-10,
  2.44538e-10,
  3.17021e-09,
  1.20419e-10,
  9.53393e-09
 };

// Assignment to powers vector.
// The powers are stored row-wise, that is
//  p_ij = gPower[i * NVariables + j];
int    gPower[] = {
  1,  1,  1,  1,
  1,  1,  1,  2,
  1,  1,  1,  3,
  1,  3,  1,  1,
  2,  1,  1,  3,
  2,  1,  1,  1,
  1,  3,  1,  2,
  1,  4,  1,  2,
  1,  2,  1,  1,
  4,  1,  1,  1,
  4,  1,  2,  1,
  4,  2,  1,  2,
  2,  2,  1,  1,
  4,  1,  2,  2,
  1,  1,  2,  1,
  1,  2,  2,  1,
  2,  1,  2,  1,
  1,  1,  2,  3,
  1,  1,  4,  1,
  1,  2,  1,  4
};

// 
// The method   double MDF(double *x)
// 
double MDF(double *x) {
  double returnValue = gDMean;
  int    i = 0, j = 0, k = 0;
  for (i = 0; i < gNCoefficients ; i++) {
    // Evaluate the ith term in the expansion
    double term = gCoefficient[i];
    for (j = 0; j < gNVariables; j++) {
      // Evaluate the polynomial in the jth variable.
      int power = gPower[gNVariables * i + j]; 
      double p1 = 1, p2 = 0, p3 = 0, r = 0;
      double v =  1 + 2. / (gXMax[j] - gXMin[j]) * (x[j] - gXMax[j]);
      // what is the power to use!
      switch(power) {
      case 1: r = 1; break; 
      case 2: r = v; break; 
      default: 
        p2 = v; 
        for (k = 3; k <= power; k++) { 
          p3 = p2 * v;
          p1 = p2; p2 = p3; 
        }
        r = p3;
      }
      // multiply this term by the poly in the jth var
      term *= r; 
    }
    // Add this term to the final result
    returnValue += term;
  }
  return returnValue;
}
// EOF for MDF1MDF.cxx

}
namespace MDF0 {
// -*- mode: c++ -*-
// 
// File MDF0MDF.cxx generated by TMultiDimFit::MakeRealCode
// on Tue Mar 29 15:04:40 2022
// ROOT version 5.34/39
//
// This file contains the function 
//
//    double  MDF(double *x); 
//
// For evaluating the parameterization obtained
// from TMultiDimFit and the point x
// 
// See TMultiDimFit class documentation for more information 
// 
//#include "MDF0.h"
//
// Static data variables
//
int    gNVariables    = 4;
int    gNCoefficients = 20;
double gDMean         = 0.494821;
// Assignment to mean vector.
double gXMean[] = {
  -0.659328, -0.788945, -0.207504, -0.344367 };

// Assignment to minimum vector.
double gXMin[] = {
  3.5, 2, 2, 3.57 };

// Assignment to maximum vector.
double gXMax[] = {
  23, 12.5, 182, 9.415 };

// Assignment to coefficients vector.
double gCoefficient[] = {
  -0.0952562,
  -0.0533766,
  -0.0383996,
  -0.105157,
  -0.0145849,
  -0.0066764,
  0.282457,
  0.0292169,
  -0.0614018,
  -0.0269236,
  0.0533472,
  0.0389513,
  0.0291081,
  0.0336723,
  -0.10576,
  0.0117346,
  0.0415272,
  -0.00539696,
  -0.0284979,
  0.0175389
 };

// Assignment to error coefficients vector.
double gCoefficientRMS[] = {
  6.48535e-11,
  5.75211e-10,
  1.55092e-09,
  1.84015e-09,
  1.77301e-10,
  1.08025e-10,
  6.43095e-09,
  6.01137e-10,
  2.60316e-09,
  3.58339e-10,
  4.71817e-10,
  4.54563e-10,
  6.58254e-10,
  2.63002e-10,
  1.52119e-08,
  2.77272e-10,
  2.44558e-10,
  2.7589e-10,
  2.19598e-09,
  3.62564e-10
 };

// Assignment to powers vector.
// The powers are stored row-wise, that is
//  p_ij = gPower[i * NVariables + j];
int    gPower[] = {
  1,  1,  1,  1,
  1,  1,  1,  2,
  1,  1,  1,  3,
  1,  3,  1,  2,
  2,  1,  1,  1,
  1,  1,  2,  1,
  1,  3,  1,  3,
  1,  1,  2,  2,
  1,  1,  1,  4,
  4,  1,  1,  1,
  1,  2,  3,  1,
  3,  1,  3,  1,
  1,  3,  3,  1,
  1,  4,  2,  1,
  1,  4,  1,  4,
  3,  2,  1,  1,
  2,  2,  2,  1,
  2,  1,  3,  1,
  1,  1,  3,  3,
  4,  1,  2,  1
};

// 
// The method   double MDF(double *x)
// 
double MDF(double *x) {
  double returnValue = gDMean;
  int    i = 0, j = 0, k = 0;
  for (i = 0; i < gNCoefficients ; i++) {
    // Evaluate the ith term in the expansion
    double term = gCoefficient[i];
    for (j = 0; j < gNVariables; j++) {
      // Evaluate the polynomial in the jth variable.
      int power = gPower[gNVariables * i + j]; 
      double p1 = 1, p2 = 0, p3 = 0, r = 0;
      double v =  1 + 2. / (gXMax[j] - gXMin[j]) * (x[j] - gXMax[j]);
      // what is the power to use!
      switch(power) {
      case 1: r = 1; break; 
      case 2: r = v; break; 
      default: 
        p2 = v; 
        for (k = 3; k <= power; k++) { 
          p3 = p2 * v;
          p1 = p2; p2 = p3; 
        }
        r = p3;
      }
      // multiply this term by the poly in the jth var
      term *= r; 
    }
    // Add this term to the final result
    returnValue += term;
  }
  return returnValue;
}

// EOF for MDF0MDF.cxx
}
//================================================================================
Double_t mdf0(Double_t z0, Double_t z1, Double_t z2, Double_t z3) {
  Double_t x[4] = {z0,z1,z2,z3};
  return MDF0::MDF(x);
} 
//================================================================================
Double_t mdf1(Double_t z0, Double_t z1, Double_t z2, Double_t z3) {
  Double_t x[4] = {z0,z1,z2,z3};
  return MDF1::MDF(x);
} 
namespace TMDF0 {
// -*- mode: c++ -*-
// 
// File TMDF0.C generated by TMultiDimFit::MakeRealCode
// on Tue Mar 29 16:14:54 2022
// ROOT version 5.34/39
//
// This file contains the function 
//
//    double  MDF(double *x); 
//
// For evaluating the parameterization obtained
// from TMultiDimFit and the point x
// 
// See TMultiDimFit class documentation for more information 
// 
//
// Static data variables
//
static int    gNVariables    = 4;
static int    gNCoefficients = 20;
static double gDMean         = 0.494821;
// Assignment to mean vector.
static double gXMean[] = {
  -0.659328, -0.788945, -0.207504, -0.344367 };

// Assignment to minimum vector.
static double gXMin[] = {
  3.5, 2, 2, 3.57 };

// Assignment to maximum vector.
static double gXMax[] = {
  23, 12.5, 182, 9.415 };

// Assignment to coefficients vector.
static double gCoefficient[] = {
  -0.0394429,
  -0.152496,
  0.0467203,
  0.0152774,
  -0.0629196,
  -0.0155602,
  0.0321839,
  0.0188931,
  0.0238748,
  0.0149726,
  0.0103656,
  -0.00880762,
  -0.0116933,
  0.00474391,
  -0.00613028,
  -0.0146692,
  0.00900925,
  0.0303581,
  -0.0146244,
  0.00653847
 };

// Assignment to error coefficients vector.
static double gCoefficientRMS[] = {
  7.8046e-10,
  1.55061e-09,
  8.29851e-10,
  5.49881e-10,
  2.36284e-10,
  9.5589e-11,
  4.167e-10,
  2.36689e-10,
  5.40459e-10,
  1.42839e-10,
  2.02876e-10,
  2.3627e-10,
  8.5455e-11,
  9.95794e-11,
  4.39475e-10,
  4.89651e-10,
  1.90245e-10,
  1.40194e-09,
  2.068e-10,
  2.03428e-10
 };

// Assignment to powers vector.
// The powers are stored row-wise, that is
//  p_ij = gPower[i * NVariables + j];
static int    gPower[] = {
  1,  1,  1,  1,
  1,  1,  1,  2,
  1,  1,  1,  3,
  1,  3,  1,  2,
  2,  1,  1,  1,
  1,  1,  2,  1,
  2,  2,  1,  3,
  2,  4,  1,  1,
  1,  1,  2,  2,
  1,  4,  2,  1,
  2,  1,  2,  4,
  1,  4,  1,  4,
  4,  1,  1,  4,
  4,  2,  3,  1,
  1,  1,  1,  4,
  1,  2,  1,  3,
  3,  2,  1,  1,
  1,  2,  2,  2,
  1,  4,  1,  1,
  1,  3,  1,  3
};

// 
// The function   double MDF(double *x)
// 
double MDF(double *x) {
  double returnValue = gDMean;
  int    i = 0, j = 0, k = 0;
  for (i = 0; i < gNCoefficients ; i++) {
    // Evaluate the ith term in the expansion
    double term = gCoefficient[i];
    for (j = 0; j < gNVariables; j++) {
      // Evaluate the polynomial in the jth variable.
      int power = gPower[gNVariables * i + j]; 
      double p1 = 1, p2 = 0, p3 = 0, r = 0;
      double v =  1 + 2. / (gXMax[j] - gXMin[j]) * (x[j] - gXMax[j]);
      // what is the power to use!
      switch(power) {
      case 1: r = 1; break; 
      case 2: r = v; break; 
      default: 
        p2 = v; 
        for (k = 3; k <= power; k++) { 
          p3 = p2 * v;
          p3 = 2 * v * p2 - p1; 
          p1 = p2; p2 = p3; 
        }
        r = p3;
      }
      // multiply this term by the poly in the jth var
      term *= r; 
    }
    // Add this term to the final result
    returnValue += term;
  }
  return returnValue;
}

// EOF for TMDF0.C
}
namespace TMDF1 {
// -*- mode: c++ -*-
// 
// File TMDF1.C generated by TMultiDimFit::MakeRealCode
// on Tue Mar 29 16:22:12 2022
// ROOT version 5.34/39
//
// This file contains the function 
//
//    double  MDF(double *x); 
//
// For evaluating the parameterization obtained
// from TMultiDimFit and the point x
// 
// See TMultiDimFit class documentation for more information 
// 
//
// Static data variables
//
static int    gNVariables    = 4;
static int    gNCoefficients = 20;
static double gDMean         = 0.481485;
// Assignment to mean vector.
static double gXMean[] = {
  -0.521274, -0.719245, -0.017131, -0.365224 };

// Assignment to minimum vector.
static double gXMin[] = {
  3.5, 2, 2, 3.57 };

// Assignment to maximum vector.
static double gXMax[] = {
  23, 12.5, 186, 9.415 };

// Assignment to coefficients vector.
static double gCoefficient[] = {
  -0.0152368,
  -0.219536,
  0.0746264,
  -0.0462639,
  0.0371691,
  0.0188879,
  0.0122579,
  -0.0144629,
  -0.009095,
  0.0342711,
  0.024188,
  -0.0195241,
  -0.0374876,
  -0.0306134,
  0.0142725,
  0.00470251,
  -0.0293143,
  -0.000148832,
  0.00869617,
  -0.00452649
 };

// Assignment to error coefficients vector.
static double gCoefficientRMS[] = {
  2.089e-09,
  9.68286e-10,
  5.16228e-10,
  4.50078e-09,
  3.40049e-10,
  8.74768e-10,
  1.59898e-09,
  2.86876e-10,
  1.56299e-10,
  1.76683e-09,
  6.22434e-10,
  1.22144e-10,
  1.66459e-09,
  1.93332e-09,
  1.28616e-09,
  1.78433e-10,
  3.7926e-09,
  2.71594e-10,
  1.48843e-10,
  6.76769e-11
 };

// Assignment to powers vector.
// The powers are stored row-wise, that is
//  p_ij = gPower[i * NVariables + j];
static int    gPower[] = {
  1,  1,  1,  1,
  1,  1,  1,  2,
  1,  4,  1,  2,
  3,  1,  1,  2,
  1,  2,  1,  1,
  3,  1,  2,  2,
  3,  1,  1,  4,
  1,  3,  2,  4,
  2,  1,  1,  1,
  1,  1,  1,  3,
  1,  3,  1,  2,
  1,  4,  1,  1,
  2,  3,  1,  2,
  4,  2,  1,  2,
  3,  2,  3,  2,
  1,  3,  1,  1,
  2,  1,  1,  2,
  3,  1,  1,  1,
  4,  1,  1,  1,
  3,  1,  2,  1
};

// 
// The function   double MDF(double *x)
// 
double MDF(double *x) {
  double returnValue = gDMean;
  int    i = 0, j = 0, k = 0;
  for (i = 0; i < gNCoefficients ; i++) {
    // Evaluate the ith term in the expansion
    double term = gCoefficient[i];
    for (j = 0; j < gNVariables; j++) {
      // Evaluate the polynomial in the jth variable.
      int power = gPower[gNVariables * i + j]; 
      double p1 = 1, p2 = 0, p3 = 0, r = 0;
      double v =  1 + 2. / (gXMax[j] - gXMin[j]) * (x[j] - gXMax[j]);
      // what is the power to use!
      switch(power) {
      case 1: r = 1; break; 
      case 2: r = v; break; 
      default: 
        p2 = v; 
        for (k = 3; k <= power; k++) { 
          p3 = p2 * v;
          p3 = 2 * v * p2 - p1; 
          p1 = p2; p2 = p3; 
        }
        r = p3;
      }
      // multiply this term by the poly in the jth var
      term *= r; 
    }
    // Add this term to the final result
    returnValue += term;
  }
  return returnValue;
}

// EOF for TMDF1.C
}
//================================================================================
Double_t tmdf0(Double_t z0, Double_t z1, Double_t z2, Double_t z3) {
  Double_t x[4] = {z0,z1,z2,z3};
  return TMDF0::MDF(x);
} 
//================================================================================
Double_t tmdf1(Double_t z0, Double_t z1, Double_t z2, Double_t z3) {
  Double_t x[4] = {z0,z1,z2,z3};
  return TMDF1::MDF(x);
} 
#ifdef __CINT__
#pragma link C++ namespace MDF0;
#pragma link C++ namespace MDF1;
#pragma link C++ namespace TMDF0;
#pragma link C++ namespace TMDF1;
#endif
