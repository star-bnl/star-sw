/*
  root.exe lDb.C TpcRotations.C
 */

#if !defined(__CINT__)
// code that should be seen ONLY by the compiler
#else
#if !defined(__CINT__) || defined(__MAKECINT__)
// code that should be seen by the compiler AND rootcint
#else
// code that should always be seen
#endif
#endif
//________________________________________________________________________________
#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include "TROOT.h"
#include "TSystem.h"
#include "TMath.h"
#include "TH1.h"
#include "TStyle.h"
#include "TGraph.h"
#include "TNtuple.h"
#include "TCanvas.h"
#include "TString.h"
#include "TLegend.h"
#include "TFile.h"
#include "TNamed.h"
#include "TGeoMatrix.h"
#include "StChain.h"
#include "tables/St_Survey_Table.h"
#include "TGeoMatrix.h"
#include "StTpcDb/StTpcDb.h"
#include "StEvent/StEnumerations.h"
#include "StDetectorDbMaker/StTpcSurveyC.h"
#include "St_db_Maker/St_db_Maker.h"
#endif
//________________________________________________________________________________
void TpcRotations(Int_t date = 20180101, Int_t time = 0) {
  StMaker *chain = StChain::GetChain();
  St_db_Maker *dbMk = (St_db_Maker *) chain->Maker("db");
  if (! dbMk) return;
  dbMk->SetDebug(1);
  Int_t D = date;
  Int_t T = time;
  if (D <= 0) {
    TDatime dt;
    Int_t i = dt.Convert(kTRUE); // to GMT
    dt.Set(i);
    D = dt.GetDate();
    T = dt.GetTime();
    cout << "Set GMT Date " << D << " Time " << T << endl;
  }
  dbMk->SetDateTime(D,T); 
  TFile *fOut = new TFile(Form("TpcRotation.%08i.%06i.root",D,T),"recreate");
  chain->Init();
  chain->Make();
  fOut->cd();
  TGeoHMatrix rot = StTpcDb::instance()->Tpc2GlobalMatrix(); rot.Print("");  rot.Write();
  for (Int_t sector = 1; sector <= 24; sector++) {
    rot = StTpcDb::instance()->SupS2Tpc(sector );       rot.Print(""); rot.Write(); 
    rot = StTpcDb::instance()->SubSInner2SupS(sector ); rot.Print(""); rot.Write(); 
    rot = StTpcDb::instance()->SubSOuter2SupS(sector ); rot.Print(""); rot.Write(); 
    rot = StTpcDb::instance()->SubSInner2Tpc(sector );  rot.Print(""); rot.Write(); 
    rot = StTpcDb::instance()->SubSOuter2Tpc(sector );  rot.Print(""); rot.Write(); 
    rot = StTpcSuperSectorPosition::instance()->GetMatrix4Id(sector); rot.Print(""); rot.Write(); 
    rot = StTpcOuterSectorPosition::instance()->GetMatrix4Id(sector); rot.Print(""); rot.Write(); 
    rot = StTpcInnerSectorPosition::instance()->GetMatrix4Id(sector); rot.Print(""); rot.Write(); 
  }
  delete fOut;
}
//________________________________________________________________________________
void MakeSurveyTable(TGeoHMatrix rot[24], const Char_t *tableName =" TpcSuperSectorPositionB", Int_t date = 20190101, Int_t time = 300) {
  TString fOut =  Form("%s.%8i.%06i.C", tableName, date, time);
  ofstream out;
  cout << "Create " << fOut.Data() << endl;
  out.open(fOut.Data());
  out << "TDataSet *CreateTable() {" << endl;
  out << "  if (!gROOT->GetClass(\"St_Survey\")) return 0;" << endl;
  out << "  Survey_st row[24] = {" << endl; 
  out << "    //             -gamma     beta    gamma            -alpha    -beta    alpha                x0       y0       z0" << endl;
  for (Int_t i = 0; i < 24; i++) {
    out << "    {" << Form("%2i",i+1) << ","; 
    Double_t *r = rot[i].GetRotationMatrix();
    Double_t *t = rot[i].GetTranslation();
    //    cout << " ";
    for (Int_t j =  0; j <  9; j++) out << Form("%8.5f,",r[j]);
    //    cout << " ";
    for (Int_t j =  0; j <  3; j++) out << Form("%8.4f,",t[j]); 
    cout << " ";
    Double_t zero = 0;
    for (Int_t j = 12; j < 18; j++) out << Form("%8.5f,",zero);
    out << "\"Recalculate\"}";
    if (i != 23) out << ",";
    out << endl;
  }
  cout << endl;
  out << "  };" << endl;
  out << Form("  St_Survey *tableSet = new St_Survey(\"%s\",24);",tableName) << endl; 
  out << "  for (Int_t i = 0; i < 24; i++) tableSet->AddAt(&row[i].Id);" << endl;
  out << "  return (TDataSet *)tableSet;" << endl;
  out << "}" << endl;
  out.close(); 
}
//________________________________________________________________________________
void Compare(TFile *file0 = 0, TFile *file1 = 0) {
  static TGeoHMatrix I("Indentity"); I.SetBit(TGeoMatrix::kGeoGenTrans); I.SetBit(TGeoMatrix::kGeoRotation);
  if (! file0 || ! file1) return;
  const Char_t *Names[9] = {"Tpc2Glob", "SupS_%02dtoTpc","SubS_%02dInner2SupS","SubS_%02dOuter2SupS","SubS_%02dInner2Tpc","SubS_%02dOuter2Tpc","TpcSuperSectorPositionB_%d","TpcOuterSectorPositionB_%d","TpcInnerSectorPositionB_%d"};
  TGeoHMatrix *rot[9][24][2] = {0};
  TFile *files[2] = {file0, file1};
  enum { kSuperSector = 6, kInnerSector = 8, kOuterSector = 7};
  /*
	kTpc2GlobalMatrix := StTpcPosition
	kSupS2Tpc       :=                 (Shift(half) * StTpcHalfPosition(half)) * (rotmS(sector,iPhi) * StTpcSuperSectorPosition)
        kSubSInner2SupS := Flip * StTpcInnerSectorPosition
        kSubSOuter2SupS := Flip * StTpcOuterSectorPosition
        kSubSInner2Tpc  := kSupS2Tpc * kSubSInner2SupS
        kSubSOuter2Tpc  := kSupS2Tpc * kSubSOuter2SupS

   */
  TString Type;
  for (Int_t i = 0; i < 2; i++) {
    rot[0][0][i] = (TGeoHMatrix *) files[i]->Get(Names[0]);
    if (! rot[0][0][i]) return;
    rot[0][0][i]->Print();
    for (Int_t k = 1; k < 9; k++) {
      for (Int_t sec = 0; sec < 24; sec++) {
	Int_t sector = sec + 1;
	Type = Form(Names[k],sector);
	rot[k][sec][i] = (TGeoHMatrix *) files[i]->Get(Type);
	cout << "rot[" << k << "][" << sec << "][" << i << "]:"; rot[k][sec][i]->Print();
      }
    }
  }
  /*  S = TpcSuperSectorPositionB; I = TpcInnerSectorPositionB; O = TpcOuterSectorPositionB;
    P_old_I = S_old * I_old
    P_old_O = S_old * O_old 
    P_new_I = S_new * I_new 
    P_new_O = S_new * O_new 
    S_new = S_old * D;  
    P_new_O  = S_old * D * O_new = P_old_O = S_old * O_old;
    D = O_old * O_new^-1xs
    S_new = S_old * D = S_old * O_old * O_new^-1;
    
    P_new_I = S_new * I_new = S_old * O_old * O_new^-1 * I_new

    P_new_I  = S_new * I_new = S_old * D * I_new; I_new =

    P_old_O == P_new_O => S_old * O_old = S_new * O_new; S_new = S_old * O_old * O_new^-1;
    P_old_I = S_old * I_old = S_new * I_new; I_new = S_new^-1 * S_old * I_old ;

    S_new = S_old * O_old * O_new^-1;
    I_new = S_new^-1 * S_old * I_old;

   */
  TGeoHMatrix Super[24];
  TGeoHMatrix Inner[24];
  for (Int_t sector = 1; sector <= 24; sector++) {
    for (Int_t k = 1; k < 9; k++) {
      if (k != kSuperSector && k != kInnerSector && k != kOuterSector) continue;
      TGeoHMatrix *rot0 = rot[k][sector-1][0];
      TGeoHMatrix *rot1 = rot[k][sector-1][1];
      if (! rot0 || ! rot1) {
	cout << "Matrices are missing" << endl;
	continue;
      }
      TGeoHMatrix diff = (*rot0) * (rot1->Inverse()); 
      if (diff == I) {
	cout << rot0->GetName() << " are the same" << endl;
	continue;
      }
      cout << "rot0:"; rot0->Print();
      cout << "rot1:"; rot1->Print();
      cout << "diff:"; diff.Print();
      cout << "================================================================================" << endl;
    }
    TGeoHMatrix O_old(*rot[kOuterSector][sector-1][0]); cout << "O_old:"; O_old.Print();
    TGeoHMatrix O_new(*rot[kOuterSector][sector-1][1]); cout << "O_new:"; O_new.Print();
    TGeoHMatrix I_old(*rot[kInnerSector][sector-1][0]); cout << "I_old:"; I_old.Print();
    TGeoHMatrix I_new(*rot[kInnerSector][sector-1][1]); cout << "I_new:"; I_new.Print();
    TGeoHMatrix S_old(*rot[kSuperSector][sector-1][0]); cout << "S_old:"; S_old.Print();
    TGeoHMatrix S_new(*rot[kSuperSector][sector-1][1]); cout << "S_new:"; S_new.Print();
    cout << "Update" << endl;
    Super[sector-1] = S_old * O_old * O_new.Inverse(); cout << "Super[" << sector-1 << "]:"; Super[sector-1].Print();
    Inner[sector-1] = Super[sector-1].Inverse() * S_old * I_old; cout << "Inner[" << sector-1 <<"]:"; Inner[sector-1].Print();
    cout << "Done with sector " << sector <<" ================================================================================" << endl;
  }
  MakeSurveyTable(Super,"TpcSuperSectorPositionB",20190101,300);
  MakeSurveyTable(Inner,"TpcInnerSectorPositionB",20190101,300);
}
