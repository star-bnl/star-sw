/*
  cd ~/work/dEdx/RunXIX68
  foreach f ( `ls -1d   *19/2*.root *20/2*.root` )  
    set b = `basename ${f} .root`; root.exe -q -b ${f} CheckPads.C+ >& ${b}.list
  end
  #grep Dead *.list > DeadFEE.list
  #remove Dead  string >  DeadFEE2.list

#  grep Dead *.list | awk -F\{ '{print "{"$2"}, /""* Dead  *""/"}' > DeadFEE2.list
  grep Dead *.list | awk -F\{ '{print "{"$2}' > DeadFEE2.list
  sort DeadFEE2.list > DeadFEE.listSorted
  MergeDeadFee.pl DeadFEE.listSorted | tee DeadFeeRuns
  sort DeadFeeRuns | tee DeadFeeRuns.sorted
                         add Dead
#  grep Alive *.list > AliveFEE.list
#  remove Alive  string >  AliveFEE2.list

  grep Alive 2*.list | awk -F\{ '{print "{"$2}' > AliveFEE2.list
  sort AliveFEE2.list > AliveFEE.sorted
  MergeDeadFee.pl AliveFEE.sorted  | tee AliveFeeRuns
  sort AliveFeeRuns | tee AliveFeeRuns.sorted
                          add Alive
  
  cat *Runs.sorted | sort | tee DeadOrAlived_Runx_XIX_XX.sorted
*/
//   foreach f ( `ls -1d   */2*.root` )
//________________________________________________________________________________
#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include <stdio.h>
#include "TROOT.h"
#include "TSystem.h"
#include "TMath.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TProfile3D.h"
#include "TLegend.h"
#include "TCanvas.h"
#endif
struct rowpadFEEmap_t {
  Int_t row, padMin, padMax, fee;
} rowpadFEE[] = {
  { 1,  1, 26, 54},
  { 1, 27, 52, 55},
  { 2,  1, 27, 54},
  { 2, 28, 54, 55},
  { 3,  1, 22, 52},
  { 3, 23, 28, 54},
  { 3, 29, 34, 55},
  { 3, 35, 56, 53},
  { 4,  1, 24, 52},
  { 4, 25, 29, 54},
  { 4, 30, 34, 55},
  { 4, 35, 58, 53},
  { 5,  1, 10, 52},
  { 5, 11, 20, 49},
  { 5, 21, 40, 50},
  { 5, 41, 50, 51},
  { 5, 51, 60, 53},
  { 6,  1,  8, 52},
  { 6, 22, 41, 50},
  { 6, 42, 54, 51},
  { 6, 55, 62, 53},
  { 6,  9, 21, 49},
  { 7,  1,  3, 46},
  { 7, 26, 37, 50},
  { 7, 38, 59, 51},
  { 7,  4, 25, 49},
  { 7, 60, 62, 48},
  { 8,  1,  7, 46},
  { 8, 27, 38, 50},
  { 8, 39, 57, 51},
  { 8, 58, 64, 48},
  { 8,  8, 26, 49},
  { 9,  1, 25, 46},
  { 9, 26, 41, 47},
  { 9, 42, 66, 48},
  {10,  1, 25, 46},
  {10, 26, 43, 47},
  {10, 44, 68, 48},
  {11,  1, 27, 43},
  {11, 28, 43, 47},
  {11, 44, 70, 45},
  {12,  1, 29, 43},
  {12, 30, 43, 47},
  {12, 44, 72, 45},
  {13,  1,  4, 43},
  {13, 23, 52, 44},
  {13,  5, 22, 40},
  {13, 53, 70, 42},
  {13, 71, 74, 45},
  {14,  1, 22, 40},
  {14, 23, 52, 44},
  {14, 53, 74, 42},
  {15,  1,  8, 36},
  {15, 23, 54, 41},
  {15, 55, 68, 42},
  {15, 69, 76, 39},
  {15,  9, 22, 40},
  {16,  1, 13, 36},
  {16, 14, 23, 40},
  {16, 24, 55, 41},
  {16, 56, 65, 42},
  {16, 66, 78, 39},
  {17,  1, 24, 36},
  {17, 25, 40, 37},
  {17, 41, 56, 38},
  {17, 57, 80, 39},
  {18,  1, 19, 36},
  {18, 20, 41, 37},
  {18, 42, 63, 38},
  {18, 64, 82, 39},
  {19,  1, 26, 32},
  {19, 27, 42, 37},
  {19, 43, 58, 38},
  {19, 59, 84, 35},
  {20,  1, 22, 32},
  {20, 23, 33, 33},
  {20, 34, 43, 37},
  {20, 44, 53, 38},
  {20, 54, 64, 34},
  {20, 65, 86, 35},
  {21,  1,  4, 32},
  {21, 25, 43, 33},
  {21, 44, 62, 34},
  {21,  5, 24, 28},
  {21, 63, 82, 31},
  {21, 83, 86, 35},
  {22,  1,  8, 32},
  {22, 26, 44, 33},
  {22, 45, 63, 34},
  {22, 64, 80, 31},
  {22, 81, 88, 35},
  {22,  9, 25, 28},
  {23,  1, 10, 28},
  {23, 11, 18, 24},
  {23, 19, 36, 29},
  {23, 37, 45, 33},
  {23, 46, 54, 34},
  {23, 55, 72, 30},
  {23, 73, 80, 27},
  {23, 81, 90, 31},
  {24,  1, 15, 28},
  {24, 16, 26, 24},
  {24, 27, 44, 29},
  {24, 45, 46, 33},
  {24, 47, 48, 34},
  {24, 49, 66, 30},
  {24, 67, 77, 27},
  {24, 78, 92, 31},
  {25,  1, 20, 24},
  {25, 21, 29, 29},
  {25, 30, 47, 25},
  {25, 48, 65, 26},
  {25, 66, 74, 30},
  {25, 75, 94, 27},
  {26,  1, 21, 24},
  {26, 22, 35, 29},
  {26, 36, 48, 25},
  {26, 49, 61, 26},
  {26, 62, 75, 30},
  {26, 76, 96, 27},
  {27,  1, 24, 19},
  {27, 25, 32, 20},
  {27, 33, 42, 25},
  {27, 43, 56, 21},
  {27, 57, 66, 26},
  {27, 67, 74, 22},
  {27, 75, 98, 23},
  {28,  1, 16, 19},
  {28, 17, 24, 20},
  {28, 25, 42, 25},
  {28, 43, 56, 21},
  {28, 57, 74, 26},
  {28, 75, 82, 22},
  {28, 83, 98, 23},
  {29,  1, 12, 17},
  {29, 13, 20, 19},
  {29, 21, 42, 20},
  {29, 43, 58, 21},
  {29, 59, 80, 22},
  {29, 81, 88, 23},
  {29, 89,100, 18},
  {30,  1,  5, 17},
  {30, 22, 43, 20},
  {30, 44, 59, 21},
  {30, 60, 81, 22},
  {30,  6, 21, 19},
  {30, 82, 97, 23},
  {30, 98,102, 18},
  {31,  1, 20, 17},
  {31, 21, 40, 13},
  {31, 41, 42, 20},
  {31, 43, 62, 14},
  {31, 63, 64, 22},
  {31, 65, 84, 15},
  {31, 85,104, 18},
  {32,  1, 27, 17},
  {32, 28, 43, 13},
  {32, 44, 63, 14},
  {32, 64, 79, 15},
  {32, 80,106, 18},
  {33,  1, 32, 12},
  {33, 33, 43, 13},
  {33, 44, 49, 14},
  {33, 50, 59,  9},
  {33, 60, 65, 14},
  {33, 66, 76, 15},
  {33, 77,108, 16},
  {34,  1, 28, 12},
  {34, 29, 44, 13},
  {34, 45, 49, 14},
  {34, 50, 61,  9},
  {34, 62, 66, 14},
  {34, 67, 82, 15},
  {34, 83,110, 16},
  {35,  1, 19,  7},
  {35, 20, 44,  8},
  {35, 45, 66,  9},
  {35, 67, 91, 10},
  {35, 92,110, 11},
  {36,  1, 20,  7},
  {36, 21, 24, 12},
  {36, 25, 46,  8},
  {36, 47, 66,  9},
  {36, 67, 88, 10},
  {36, 89, 92, 16},
  {36, 93,112, 11},
  {37,102,114,  6},
  {37,  1, 13,  1},
  {37, 14, 26,  7},
  {37, 27, 38,  2},
  {37, 39, 45,  8},
  {37, 46, 57,  3},
  {37, 58, 69,  4},
  {37, 70, 76, 10},
  {37, 77, 88,  5},
  {37, 89,101, 11},
  {38,105,116,  6},
  {38,  1, 12,  1},
  {38, 13, 24,  7},
  {38, 25, 36,  2},
  {38, 37, 46,  8},
  {38, 47, 58,  3},
  {38, 59, 70,  4},
  {38, 71, 80, 10},
  {38, 81, 92,  5},
  {38, 93,104, 11},
  {39,100,118,  6},
  {39,  1, 19,  1},
  {39, 20, 39,  2},
  {39, 40, 59,  3},
  {39, 60, 79,  4},
  {39, 80, 99,  5},
  {40,101,120,  6},
  {40,  1, 20,  1},
  {40, 21, 40,  2},
  {40, 41, 60,  3},
  {40, 61, 80,  4},
  {40, 81,100,  5}
};
Int_t NC = sizeof(rowpadFEE)/sizeof(rowpadFEEmap_t);
//________________________________________________________________________________
Int_t FEE(Int_t row, Int_t pad) {
  Int_t fee = 0;
  if (row > 0 && row <= 40 && pad > 0) {
    for (Int_t i = 0; i < NC; i++) {
      if (row != rowpadFEE[i].row) continue;  
      if (rowpadFEE[i].padMin <= pad && pad <= rowpadFEE[i].padMax) {
	fee = rowpadFEE[i].fee;
	return fee;
      }
    }
    static Int_t ibreak;
    ibreak++;
  }
  return fee;
}
//________________________________________________________________________________
void CheckPads(Int_t sector = 0) {
  Int_t nPads[72] = { 
    52, 54, 56, 58, 60, 62, 62, 64, 66, 68,
    70, 72, 74, 74, 76, 78, 80, 82, 84, 86,
    86, 88, 90, 92, 94, 96, 98, 98,100,102,
    104,106,108,110,110,112,114,116,118,120,
    98,100,102,104,106,106,108,110,112,112,
    114,116,118,120,122,122,124,126,128,128,
    130,132,134,136,138,138,140,142,144,144,
    144,144};
  TH3F *AlivePads = (TH3F *) gDirectory->Get("AlivePads");
  if (! AlivePads) {cout << "AlivePads is missing" << endl; return;}
  TProfile3D *ActivePads = (TProfile3D *) gDirectory->Get("ActivePads");
  if (! ActivePads) {cout << "ActivePads is missing" << endl; return;}
  TString Dir(gSystem->BaseName(gDirectory->GetName()));
  Int_t index = Dir.Index("_");
  TString Run(Dir,index); // cout << "Run = " << Run.Data() << endl;
  Run.Prepend(", /* ");
  Run += " */";
  Int_t nx = AlivePads->GetXaxis()->GetNbins();
  Int_t ny = AlivePads->GetYaxis()->GetNbins();
  Int_t nz = AlivePads->GetZaxis()->GetNbins();
  //  cout << gDirectory->GetName() << endl;
  TCanvas *c1 = 0;
  if (sector > 0) {
    c1 = new TCanvas("c1","c1",800,1200);
    c1->Divide(1,2);
  }
  //  for (Int_t s = 1; s <= nx; s++) {
  Int_t s1 = 1, s2 = nx;
  if (sector > 0) {s1 = s2 = sector;}
  for (Int_t s = s1; s <= s2; s++) {
    for (Int_t r = 1; r <= ny; r++) {
      TString  Dead(Form("/*Dead: */ {%2i,%3i,",s,r));
      TString  Alive(Form("/*Alive:*/ {%2i,%3i,",s,r));
      Int_t p1d = 0, p2d = -1;
      Int_t p1a = 0, p2a = -1;
      Int_t edge1 = 1;
      if (r > 40) edge1 = 2;
      Int_t edge2 = 2;
      Int_t feeG = 0;
      Int_t feeA = 0;
      for (Int_t p = edge1 + 1; p < nPads[r-1] - edge2; p++) {
	Double_t g = AlivePads->GetBinContent(s,r,p);
	Double_t a = ActivePads->GetBinContent(s,r,p);
	if (g >  0.0 && a >  0.0) continue;
	Double_t g0 = AlivePads->GetBinContent(s,r,p-1);
	Double_t g1 = AlivePads->GetBinContent(s,r,p+1);
	if ((g <= 0.0 || g0 <= 0.0 || g1 <= 0.0) && a <= 0.0) continue; // All Dead
	Int_t fee = FEE(r,p);
	if (g >  0.0){ 
	  if (p1d == 0) {
	    p1d = p;
	    p2d = p;
	    feeG = fee;
	  } else {
	    if (fee == feeG && p == p2d + 1) p2d = p;
	    else {
	      if (p1d < p2d) cout << Dead.Data() << Form("%3i,%3i,%3i}",p1d, p2d,feeG) << Run.Data() << endl;
	      p1d = p2d = p;
	      feeG = fee;
	    }
	  }
	} else {// alive
	  if (p1a == 0) {
	    p1a = p;
	    p2a = p;
	    feeA = fee;
	  } else {
	    if (fee == feeA && p == p2a + 1) p2a = p;
	    else {
	      if (p1a < p2a) cout << Alive.Data() << Form("%3i,%3i,%3i}",p1a, p2a, feeA) << Run.Data() << endl;
	      p1a = p2a = p;
	      feeA = fee;
	    }
	  }
	}
      }
      if (p1d < p2d) cout << Dead.Data()  << Form("%3i,%3i,%3i}",p1d, p2d, feeG) << Run.Data() << endl;
      if (p1a < p2a) cout << Alive.Data() << Form("%3i,%3i,%3i}",p1a, p2a, feeA) << Run.Data() << endl;
    }
    if (c1) {
      c1->cd(1); ActivePads->GetXaxis()->SetRange(s,s); ActivePads->Project3D(Form("yz_%i",s))->Draw("colz");
      c1->cd(2); AlivePads->GetXaxis()->SetRange(s,s); AlivePads->Project3D(Form("yz_%i",s))->Draw("colz");
      c1->Update();
    }
  }
}
