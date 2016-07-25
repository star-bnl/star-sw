void tpcSectorPosition2Survey(Int_t date = 20101220, Int_t time = 1) {
  gROOT->LoadMacro("bfc.C");
  TString C("mysql,tpcDb");
  C += ",nodefault";
  bfc(-1,C);
  //  Create the makers to be called by the current chain
  St_db_Maker *dbMk = (St_db_Maker *) chain->Maker("db");
  dbMk->SetDebug();
  StMaker *tpcDbMk = chain->Maker("tpcDB");
  tpcDbMk->SetDebug();
  //  dbMk->SetDateTime(20090701,10000);
  //  dbMk->SetDateTime(20090328,164000);
  //  dbMk->SetDateTime(20100107,132403);
  dbMk->SetDateTime(date,time);
  //  dbMk->SetDateTime("y2010");
  chain->SetDebug();
  chain->Init();
  chain->Make();
  TGeoHMatrix SupS2TpcE[24];
  TGeoHMatrix Outer2Inner[24];
  TGeoHMatrix flip = gStTpcDb->Flip();
  TGeoHMatrix flipI = flip.Inverse();
  for (Int_t sector = 1; sector <= 24; sector++) {
    cout << "kPadInner2Tpc[" << sector-1 << "]"; gStTpcDb->TpcRot(sector,StTpcDb::kPadInner2Tpc).Print();
    cout << "kPadOuter2Tpc[" << sector-1 << "]"; gStTpcDb->TpcRot(sector,StTpcDb::kPadOuter2Tpc).Print();
    TGeoHMatrix SubSInner2SupS = gStTpcDb->TpcRot(sector,StTpcDb::kSubSInner2SupS); cout << "SubSInner2SupS[" << sector-1 << "]\t"; SubSInner2SupS.Print();
    SupS2TpcE[sector-1] =  flip * SubSInner2SupS * flipI; cout << "SupS2TpcE[" << sector-1 << "]\t"; SupS2TpcE[sector-1].Print();
    TGeoHMatrix SupS2TpcEI = SupS2TpcE[sector-1].Inverse(); cout << "SupS2TpcEI\t"; SupS2TpcEI.Print();
    TGeoHMatrix SubSOuter2SupS = gStTpcDb->TpcRot(sector,StTpcDb::kSubSOuter2SupS); cout << "SubSOuter2SupS[" << sector-1 << "]\t"; SubSOuter2SupS.Print();
    Outer2Inner[sector-1] = flipI * SupS2TpcEI * flip * SubSOuter2SupS;; cout << "Outer2Inner[" << sector -1 << "]\t"; Outer2Inner[sector-1].Print();
    cout  << "========================" << endl;
  }
  const Char_t *Names[2] = {"TpcSuperSectorPosition","TpcOuterSectorPosition"};
  Int_t NoSectors = 24;
  Double_t zero = 0;
  for (Int_t k = 0; k < 2; k++) {
    TGeoHMatrix *rot  = &SupS2TpcE[0];
    if (k) rot = &Outer2Inner[0];
    TString fOut =  Form("%s.%8i.%06i.C",Names[k],date,time);
    ofstream out;
    cout << "Create " << fOut << endl;
    out.open(fOut.Data());
    out << "TDataSet *CreateTable() {" << endl;
    out << "  if (!gROOT->GetClass(\"St_Survey\")) return 0;" << endl;
    out << "  Survey_st row[" << NoSectors << "] = {" << endl; 
    for (Int_t i = 0; i < NoSectors; i++) { 
      TString lineC;
      lineC = "    {"; lineC += Form("%2i",i+1); 
      const Double_t *r = rot[i].GetRotationMatrix();
      const Double_t *t = rot[i].GetTranslation();
      for (Int_t j =  0; j <  9; j++) lineC += Form(",%9.6f",r[j]);
      for (Int_t j =  0; j <  3; j++) lineC += Form(",%8.4f",t[j]);
      lineC += ",0,0,0,0,0,0";
      lineC += ",\""; lineC +=  Form("T%8i.%06i",date,time); lineC += "\"}";
      if (i != NoSectors - 1) lineC += ",";
      cout << lineC.Data() << endl;
      out  << lineC.Data() << endl;
    } 
    out << "  };" << endl;
    out << "  St_Survey *tableSet = new St_Survey(\"" << Names[k] << "\"," << NoSectors << ");" << endl; 
    out << "  for (Int_t i = 0; i < " << NoSectors << "; i++) tableSet->AddAt(&row[i].Id, i);" << endl; 
    out << "  return (TDataSet *)tableSet;" << endl;
    out << "}" << endl;
    out.close(); 
  }
}
/* 
StWarning: St_tpcSectorPositionC::instance found table tpcSectorPosition with NRows = 1 in db
StWarning: Validity:20000501/5 -----   20010501/0

 ---------------------------------------------------------------------------------------
 bfcChain/.make/db/.const/StarDb/Geometry/tpc/Sector_01/.tpcSectorPosition/tpcSectorPosition  Allocated rows: 1  Used rows: 1    Row size: 32 bytes
 Table: tpcSectorPosition_st         [0] :
 ======================================================================================
float   innerSectorLocalxShift         0 : cm : shift in local x coord.  
float   innerSectorLocalyShift         0 : cm : shift in local y coord.  
float   innerSectorRotationAngle        -0.01667 : degrees : clockwise rotation  
float   innerSectorCovMatrix           0 : 0  
float   outerSectorLocalxShift    0.0048 : cm : shift in local x coord.  
float   outerSectorLocalyShift         0 : cm : shift in local y coord.  
float   outerSectorRotationAngle         0.01568 : degrees : clockwise rotation  
float   outerSectorCovMatrix           0 : 0  
---------------------------------------------------------------------------------------
matrix SubS_01Inner2SupS - tr=1  rot=1  refl=0  scl=0
  1.000000    0.000291    0.000000    Tx =  -0.035786
 -0.000291    1.000000    0.000000    Ty =   0.000005
  0.000000   -0.000000    1.000000    Tz =   0.000000
matrix SubS_01Outer2SupS - tr=1  rot=1  refl=0  scl=0
  1.000000   -0.000274    0.000000    Tx =   0.038461
  0.000274    1.000000    0.000000    Ty =   0.000006
  0.000000   -0.000000    1.000000    Tz =   0.000000

*/

