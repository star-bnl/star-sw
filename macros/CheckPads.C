/*
foreach d (`ls -1d *GeV*`)
  cd ${d}
  set list = `ls -1d 2*.root | awk -F_ '{print $1}' | sort -u | xargs`
  cd -
  echo "${list}"
  foreach f (${list})
    if (-r ${f}.list) continue;
    echo "${f}"
    root.exe -q -b ${d}/*${f}*.root CheckPads.C+ >& ${f}.list
  end
end
  mkdir empty
  mv `grep 'Pvxyz entries' 2*.list | awk '{if ($4 < 1000) print $0}' | awk -F: '{print $1}'` empty
  grep Dead 2*.list  | awk -F\{ '{print "{"$2}' | tee  DeadFEE2.list
  sort -u DeadFEE2.list | tee  DeadFEE.listSorted
  MergeDeadFee.pl DeadFEE.listSorted | tee DeadFeeRuns
  sort DeadFeeRuns | tee DeadFeeRuns.sorted

  grep Alive 2*.list | awk -F\{ '{print "{"$2}' | tee AliveFEE2.list
  sort -u AliveFEE2.list | tee AliveFEE.sorted
  MergeDeadFee.pl AliveFEE.sorted  | tee AliveFeeRuns
  sort AliveFeeRuns | tee AliveFeeRuns.sorted
  cat *Runs.sorted | sort -u | tee DeadOrAlived_Runs_XIX_XXII.sorted
*/
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
#include "TFile.h"
#include "TProfile3D.h"
#include "TLegend.h"
#include "TCanvas.h"
#include "TIterator.h"
#endif
struct rowpadFEEmap_t {// FEE & RDO map for iTPC
  Int_t row, padMin, padMax, fee, rdo;
} rowpadFEE[] = {
  { 1,  1, 26, 54, 1},
  { 1, 27, 52, 55, 1},
  { 2,  1, 27, 54, 1},
  { 2, 28, 54, 55, 1},
  { 3,  1, 22, 52, 1},
  { 3, 23, 28, 54, 1},
  { 3, 29, 34, 55, 1},
  { 3, 35, 56, 53, 1},
  { 4,  1, 24, 52, 1},
  { 4, 25, 29, 54, 1},
  { 4, 30, 34, 55, 1},
  { 4, 35, 58, 53, 1},
  { 5,  1, 10, 52, 1},
  { 5, 11, 20, 49, 1},
  { 5, 21, 40, 50, 1},
  { 5, 41, 50, 51, 1},
  { 5, 51, 60, 53, 1},
  { 6,  1,  8, 52, 1},
  { 6, 22, 41, 50, 1},
  { 6, 42, 54, 51, 1},
  { 6, 55, 62, 53, 1},
  { 6,  9, 21, 49, 1},
  { 7,  1,  3, 46, 1},
  { 7, 26, 37, 50, 1},
  { 7, 38, 59, 51, 1},
  { 7,  4, 25, 49, 1},
  { 7, 60, 62, 48, 1},
  { 8,  1,  7, 46, 1},
  { 8, 27, 38, 50, 1},
  { 8, 39, 57, 51, 1},
  { 8, 58, 64, 48, 1},
  { 8,  8, 26, 49, 1},
  { 9,  1, 25, 46, 1},
  { 9, 26, 41, 47, 1},
  { 9, 42, 66, 48, 1},
  {10,  1, 25, 46, 1},
  {10, 26, 43, 47, 1},
  {10, 44, 68, 48, 1},

  {11,  1, 27, 43, 2},
  {11, 28, 43, 47, 1},
  {11, 44, 70, 45, 2},

  {12,  1, 29, 43, 2}, // ? correction from Irakli, 03/09/2020: 29 => 28 ?
  {12, 30, 43, 47, 1},
  {12, 44, 72, 45, 2},

  {13,  1,  4, 43, 2},
  {13, 23, 52, 44, 2},
  {13,  5, 22, 40, 2},
  {13, 53, 70, 42, 2},
  {13, 71, 74, 45, 2},
  {14,  1, 22, 40, 2},
  {14, 23, 52, 44, 2},
  {14, 53, 74, 42, 2},
  {15,  1,  8, 36, 2},
  {15, 23, 54, 41, 2},
  {15, 55, 68, 42, 2},
  {15, 69, 76, 39, 2},
  {15,  9, 22, 40, 2},
  {16,  1, 13, 36, 2},
  {16, 14, 23, 40, 2},
  {16, 24, 55, 41, 2},
  {16, 56, 65, 42, 2},
  {16, 66, 78, 39, 2},
  {17,  1, 24, 36, 2},
  {17, 25, 40, 37, 2},
  {17, 41, 56, 38, 2},
  {17, 57, 80, 39, 2},
  {18,  1, 19, 36, 2},
  {18, 20, 41, 37, 2},
  {18, 42, 63, 38, 2},
  {18, 64, 82, 39, 2},
  {19,  1, 26, 32, 2},
  {19, 27, 42, 37, 2},
  {19, 43, 58, 38, 2},
  {19, 59, 84, 35, 2},
  {20,  1, 22, 32, 2},
  {20, 23, 33, 33, 2},
  {20, 34, 43, 37, 2},
  {20, 44, 53, 38, 2},
  {20, 54, 64, 34, 2},
  {20, 65, 86, 35, 2},

  {21,  1,  4, 32, 2},
  {21, 25, 43, 33, 2},
  {21, 44, 62, 34, 2},
  {21,  5, 24, 28, 3},
  {21, 63, 82, 31, 4},
  {21, 83, 86, 35, 2},

  {22,  1,  8, 32, 2},
  {22, 26, 44, 33, 2},
  {22, 45, 63, 34, 2},
  {22, 64, 80, 31, 4},
  {22, 81, 88, 35, 2},
  {22,  9, 25, 28, 3},

  {23,  1, 10, 28, 3},
  {23, 11, 18, 24, 3},
  {23, 19, 36, 29, 3},
  {23, 37, 45, 33, 2},
  {23, 46, 54, 34, 2},
  {23, 55, 72, 30, 4},
  {23, 73, 80, 27, 4},
  {23, 81, 90, 31, 4},

  {24,  1, 15, 28, 3},
  {24, 16, 26, 24, 3},
  {24, 27, 44, 29, 3},
  {24, 45, 46, 33, 2},
  {24, 47, 48, 34, 2},
  {24, 49, 66, 30, 4},
  {24, 67, 77, 27, 4},
  {24, 78, 92, 31, 4},

  {25,  1, 20, 24, 3},
  {25, 21, 29, 29, 3},
  {25, 30, 47, 25, 3},
  {25, 48, 65, 26, 4},
  {25, 66, 74, 30, 4},
  {25, 75, 94, 27, 4},

  {26,  1, 21, 24, 3},
  {26, 22, 35, 29, 3},
  {26, 36, 48, 25, 3},
  {26, 49, 61, 26, 4},
  {26, 62, 75, 30, 4},
  {26, 76, 96, 27, 4},

  {27,  1, 24, 19, 3},
  {27, 25, 32, 20, 3},
  {27, 33, 42, 25, 3},
  {27, 43, 56, 21, 3},
  {27, 57, 66, 26, 4},
  {27, 67, 74, 22, 4},
  {27, 75, 98, 23, 4},

  {28,  1, 16, 19, 3},
  {28, 17, 24, 20, 3},
  {28, 25, 42, 25, 3},
  {28, 43, 56, 21, 3},
  {28, 57, 74, 26, 4},
  {28, 75, 82, 22, 4},
  {28, 83, 98, 23, 4},

  {29,  1, 12, 17, 3},
  {29, 13, 20, 19, 3},
  {29, 21, 42, 20, 3},
  {29, 43, 58, 21, 3},
  {29, 59, 80, 22, 4},
  {29, 81, 88, 23, 4},
  {29, 89,100, 18, 4},

  {30,  1,  5, 17, 3},
  {30, 22, 43, 20, 3},
  {30, 44, 59, 21, 3},
  {30, 60, 81, 22, 4},
  {30,  6, 21, 19, 3},
  {30, 82, 97, 23, 4},
  {30, 98,102, 18, 4},

  {31,  1, 20, 17, 3},
  {31, 21, 40, 13, 3},
  {31, 41, 42, 20, 3},
  {31, 43, 62, 14, 4},
  {31, 63, 64, 22, 4},
  {31, 65, 84, 15, 4},
  {31, 85,104, 18, 4},

  {32,  1, 27, 17, 3},
  {32, 28, 43, 13, 3},
  {32, 44, 63, 14, 4},
  {32, 64, 79, 15, 4},
  {32, 80,106, 18, 4},

  {33,  1, 32, 12, 3},
  {33, 33, 43, 13, 3},
  {33, 44, 49, 14, 4},
  {33, 50, 59,  9, 4},
  {33, 60, 65, 14, 4},
  {33, 66, 76, 15, 4},
  {33, 77,108, 16, 4},

  {34,  1, 28, 12, 3},
  {34, 29, 44, 13, 3},
  {34, 45, 49, 14, 4},
  {34, 50, 61,  9, 4},
  {34, 62, 66, 14, 4},
  {34, 67, 82, 15, 4},
  {34, 83,110, 16, 4},

  {35,  1, 19,  7, 3},
  {35, 20, 44,  8, 3},
  {35, 45, 66,  9, 4},
  {35, 67, 91, 10, 4},
  {35, 92,110, 11, 4},

  {36,  1, 20,  7, 3},
  {36, 21, 24, 12, 3},
  {36, 25, 46,  8, 3},
  {36, 47, 66,  9, 4},
  {36, 67, 88, 10, 4},
  {36, 89, 92, 16, 4},
  {36, 93,112, 11, 4},

  {37,102,114,  6, 4},
  {37,  1, 13,  1, 3},
  {37, 14, 26,  7, 3},
  {37, 27, 38,  2, 3},
  {37, 39, 45,  8, 3},
  {37, 46, 57,  3, 3},
  {37, 58, 69,  4, 4},
  {37, 70, 76, 10, 4},
  {37, 77, 88,  5, 4},
  {37, 89,101, 11, 4},

  {38,105,116,  6, 4},
  {38,  1, 12,  1, 3},
  {38, 13, 24,  7, 3},
  {38, 25, 36,  2, 3},
  {38, 37, 46,  8, 3},
  {38, 47, 58,  3, 3},
  {38, 59, 70,  4, 4},
  {38, 71, 80, 10, 4},
  {38, 81, 92,  5, 4},
  {38, 93,104, 11, 4},

  {39,100,118,  6, 4},
  {39,  1, 19,  1, 3},
  {39, 20, 39,  2, 3},
  {39, 40, 59,  3, 3},
  {39, 60, 79,  4, 4},
  {39, 80, 99,  5, 4},

  {40,101,120,  6, 4},
  {40,  1, 20,  1, 3},
  {40, 21, 40,  2, 3},
  {40, 41, 60,  3, 3},
  {40, 61, 80,  4, 4},
  {40, 81,100,  5, 4},

};
Int_t NC = sizeof(rowpadFEE)/sizeof(rowpadFEEmap_t);
//________________________________________________________________________________
Int_t FEE(Int_t row, Int_t pad, Int_t &rdo) {
  Int_t fee = 0;
  rdo = 0;
  if (row > 0 && row <= 40 && pad > 0) {
    for (Int_t i = 0; i < NC; i++) {
      if (row != rowpadFEE[i].row) continue;  
      if (rowpadFEE[i].padMin <= pad && pad <= rowpadFEE[i].padMax) {
	fee = rowpadFEE[i].fee;
	rdo = rowpadFEE[i].rdo;
	return fee;
      }
    }
    static Int_t ibreak;
    ibreak++;
  } else if (row > 40 && row <= 72 && pad > 0) {
    Int_t rold = row - 27;
    Int_t rrdo = 0;
    if      (rold < 22) rrdo = 3;
    else if (rold < 30) rrdo = 4;
    else if (rold < 38) rrdo = 5;
    else                rrdo = 6;
    rdo = rrdo + 2;
  }
  return fee;
}
//________________________________________________________________________________
void Row4FEE(Int_t fee) {
  for (Int_t i = 0; i < NC; i++) {
    if (fee != rowpadFEE[i].fee) continue;  
    cout << Form("%3i. %3i, %3i, %1i, %3i", rowpadFEE[i].row, rowpadFEE[i].padMin, rowpadFEE[i].padMax, rowpadFEE[i].rdo, fee) << endl;
  }
}
//________________________________________________________________________________
void Row4RDO(Int_t rdo) {
  for (Int_t i = 0; i < NC; i++) {
    if (rdo != rowpadFEE[i].rdo) continue;  
    cout << Form("%3i. %3i, %3i, %1i, %3i", rowpadFEE[i].row, rowpadFEE[i].padMin, rowpadFEE[i].padMax, rowpadFEE[i].rdo,  rowpadFEE[i].fee) << endl;
  }
}
//________________________________________________________________________________
void PrintPads(TString Dead, Int_t r, Int_t p1d, Int_t p2d, Int_t rdoG, Int_t feeG, TString RunC) {
  //  r = p1d = p2d = -1;
  cout << Dead.Data() << Form(",%3i,%3i,%3i,%2i,%3i}", r, p1d, p2d, rdoG, feeG) << RunC.Data() << endl;
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
  // Merge files if any
  TH3F *AlivePads = 0;
  TProfile3D *ActivePads = 0;
  TH3F *PVxyz = 0;
  TSeqCollection *files = gROOT->GetListOfFiles();
  Int_t nn = files->GetSize();
  if (nn > 1) {
    cout << "Merge " << nn << " files" << endl;
  }
  TDirectory *savedir = gDirectory;
  TFile *f = 0;
  Int_t i = 0; 
  TIter next(files);
  while ( (f = (TFile *) next()) ) { 
    i++;
    TH3F       *alivePads  = (TH3F *) f->Get("AlivePads");
    TProfile3D *activePads = (TProfile3D *) f->Get("ActivePads");
    TH3F       *pvxyz      = (TH3F *) f->Get("PVxyz");
    if (alivePads) {
      if (! AlivePads) AlivePads = alivePads;
      else             AlivePads->Add(alivePads);
    }
    if (activePads) {
      if (! ActivePads) ActivePads = activePads;
      else              ActivePads->Add(activePads);
    }
    if (pvxyz) {
      if (! PVxyz) PVxyz = pvxyz;
      else         PVxyz->Add(pvxyz);
    }
  }
  if (! AlivePads)  { cout << "AlivePads is missing" << endl; return;}
  if (! ActivePads) { cout << "ActivePads is missing" << endl; return;}
  if (! PVxyz)      { cout << "PVxyz is missing" << endl; return;}
  Double_t entries = PVxyz->GetEntries();
  cout << "Pvxyz entries = " << entries <<   endl; 
  //  if (entries < 20) return;
  if (entries < 2000) return;
  TString Dir(gSystem->BaseName(gDirectory->GetName()));
  Dir.ReplaceAll("hlt_","");
  Int_t index = Dir.Index("_");
  TString Run;
  if (index > 0)  {
    Run = TString(Dir,index); // cout << "Run = " << Run.Data() << endl;
  }
  TString RunC(Run);
  RunC.Prepend(", /* ");
  RunC += " */";
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
  Int_t minpaddiff = 5;
  for (Int_t s = s1; s <= s2; s++) {
    TString  Dead(Form("/*Dead: */ {%2i",s));
    TString  Alive(Form("/*Alive:*/ {%2i",s));
    for (Int_t r = 1; r <= ny; r++) {
      //     TString  Dead(Form("/*Dead: */ {%2i,%3i,",s,r));
      //      TString  Alive(Form("/*Alive:*/ {%2i,%3i,",s,r));
      Int_t p1d = 0, p2d = -1;
      Int_t p1a = 0, p2a = -1;
      Int_t edge1 = 1;
      if (r > 40) edge1 = 2;
      Int_t edge2 = 2;
      Int_t feeG = 0;
      Int_t feeA = 0;
      Int_t rdoG = 0;
      Int_t rdoA = 0;
      Int_t rdo  = 0;
      for (Int_t p = edge1 + 1; p < nPads[r-1] - edge2; p++) {
	Double_t g = AlivePads->GetBinContent(s,r,p);
	Double_t a = ActivePads->GetBinContent(s,r,p);
	if (g >  0.0 && a >  0.0) continue;
	Double_t g0 = AlivePads->GetBinContent(s,r,p-1);
	Double_t g1 = AlivePads->GetBinContent(s,r,p+1);
	if ((g <= 0.0 || g0 <= 0.0 || g1 <= 0.0) && a <= 0.0) continue; // All Dead
	Int_t fee = FEE(r,p, rdo);
	if (g >  0.0){ 
	  if (p1d == 0) {
	    p1d = p;
	    p2d = p;
	    feeG = fee;
	    rdoG = rdo;
	  } else {
	    if (fee == feeG && p == p2d + 1) p2d = p;
	    else {
	      //	      if (p1d + minpaddiff < p2d) cout << Dead.Data() << Form("%3i,%3i,%2i,%3i}",p1d, p2d, rdoG, feeG) << RunC.Data() << endl;
	      if (p1d + minpaddiff < p2d) PrintPads(Dead, r,p1d, p2d, rdoG, feeG, RunC);
	      p1d = p2d = p;
	      feeG = fee;
	      rdoG = rdo;
	    }
	  }
	} else {// alive
	  if (p1a == 0) {
	    p1a = p;
	    p2a = p;
	    feeA = fee;
	    rdoA = rdo;
	  } else {
	    if (fee == feeA && p == p2a + 1) p2a = p;
	    else {
	      //	      if (p1a + minpaddiff < p2a) cout << Alive.Data() << Form("%3i,%3i,%2i,%3i}",p1a, p2a, rdoA, feeA) << RunC.Data() << endl;
	      if (p1a + minpaddiff < p2a) PrintPads(Alive, r, p1a, p2a, rdoA, feeA, RunC);
	      p1a = p2a = p;
	      feeA = fee;
	      rdoA = rdo;
	    }
	  }
	}
      }
      //      if (p1d + minpaddiff < p2d) cout << Dead.Data()  << Form("%3i,%3i,%2i,%3i}",p1d, p2d, rdoG, feeG) << RunC.Data() << endl;
      //      if (p1a + minpaddiff< p2a) cout << Alive.Data() << Form("%3i,%3i,%2i,%3i}",p1a, p2a, rdoA, feeA) << RunC.Data() << endl;
      if (p1d + minpaddiff < p2d) PrintPads(Dead, r, p1d, p2d, rdoG, feeG, RunC);
      if (p1a + minpaddiff < p2a) PrintPads(Alive, r, p1a, p2a, rdoA, feeA, RunC);
    }
    if (c1) {
      c1->cd(1); ActivePads->GetXaxis()->SetRange(s,s); ActivePads->Project3D(Form("yz_%i",s))->Draw("colz");
      c1->cd(2); AlivePads->GetXaxis()->SetRange(s,s); AlivePads->Project3D(Form("yz_%i",s))->Draw("colz");
      c1->Update();
      c1->SaveAs(Form("%s_s%i.png",Run.Data(),sector));
    }
  }
}
