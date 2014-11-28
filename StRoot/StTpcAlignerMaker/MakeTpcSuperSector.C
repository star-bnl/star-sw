class St_db_Maker;
class TTable;
St_db_Maker *dbMk = 0;
TTable *table = 0;
//________________________________________________________________________________
void MakeTpcSuperSector(const Char_t *opt = 0){
  gROOT->LoadMacro("SuperSectorPar.h+");
  Int_t kPass = NP - 1;
  if (opt) {
    kPass = -1;
    for (Int_t k = 0; k < NP; k++) {
      TString Name(Passes[k].PassName);
      if (! Name.Contains(opt,TString::kIgnoreCase)) continue;
      kPass = k; 
      break;
    }
    if (kPass < 0) {
      cout << "Pass for " << opt << " has not been found" << endl;
      return;
    }
  }
  cout << "Pass for " << kPass << " has been found" << endl;
  Passes[kPass].Print();
  gROOT->LoadMacro("bfc.C");
  bfc(0,"mysql,tpcDb,nodefault");
  StMaker *dbMk = chain->Maker("db");
  if (! dbMk) return;
  dbMk->SetDebug(1);
  StEvtHddr *header = chain->GetEvtHddr();
  header->SetRunNumber(1);
  dbMk->SetDateTime(Passes[kPass].date,Passes[kPass].time); 
  header->SetDateTime(Passes[kPass].date,Passes[kPass].time);
  chain->MakeEvent();
  dbMk->SetDebug(2);
  dbMk->SetDateTime(Passes[kPass].date,Passes[kPass].time); 
  // Outer sector in Inner sector coordinate system
  St_SurveyC                 *TpcSuperSectorPositionOld = StTpcSuperSectorPosition::instance();
  if (! (TpcSuperSectorPositionOld)) return;
  Int_t NoSectors = TpcSuperSectorPositionOld->GetNRows();
  St_Survey      *TpcSuperSectorPosition = new St_Survey("TpcSuperSectorPosition",NoSectors);
  for (Int_t s = 0; s < NoSectors; s++) {
    TGeoHMatrix LSold, LS, dR, dRI;
    LSold = TpcSuperSectorPositionOld->GetMatrix(s);   cout << "===================== Sector \t" << s+1 << endl; cout << "\tLSold\t"; LSold.Print();
    Int_t i = -1; 
    for (Int_t k = 0; k < N; k++) {
      if (TpcSuperSectorPositionOld->Id(s) == Passes[kPass].Data[k].sector) {i = k; break;}
    }
    if (i < 0) {
      cout << "Correction for " << TpcSuperSectorPositionOld->Id(s) << " is not found" << endl;
    } else {
      cout << "Sector " << TpcSuperSectorPositionOld->Id(s) << "\ti " << i 
	   << "\talpha " <<  Passes[kPass].Data[i].alpha << "+/-" << Passes[kPass].Data[i].Dalpha
	   << "\tbeta "  <<  Passes[kPass].Data[i].beta  << "+/-" << Passes[kPass].Data[i].Dbeta
	   << "\tgamma " <<  Passes[kPass].Data[i].gamma << "+/-" << Passes[kPass].Data[i].Dgamma
	   << "\tx " <<  Passes[kPass].Data[i].x << "+/-" << Passes[kPass].Data[i].Dx
	   << "\ty " <<  Passes[kPass].Data[i].y << "+/-" << Passes[kPass].Data[i].Dy
	   << "\tz " <<  Passes[kPass].Data[i].z << "+/-" << Passes[kPass].Data[i].Dz << endl;
      Double_t xyz[3] = {0, 0, 0};
      if (Passes[kPass].Data[i].Dalpha >= 0) dR.RotateX(TMath::RadToDeg()*Passes[kPass].Data[i].alpha*1e-3);
#if 1
      if (Passes[kPass].Data[i].Dbeta  >= 0) dR.RotateY(TMath::RadToDeg()*Passes[kPass].Data[i].beta *1e-3);
#endif
      if (Passes[kPass].Data[i].Dgamma >= 0) dR.RotateZ(TMath::RadToDeg()*Passes[kPass].Data[i].gamma*1e-3);
      if (Passes[kPass].Data[i].Dx >= 0) xyz[0] =  1e-4*Passes[kPass].Data[i].x;
      if (Passes[kPass].Data[i].Dy >= 0) xyz[1] =  1e-4*Passes[kPass].Data[i].y;
      if (Passes[kPass].Data[i].Dz >= 0) xyz[2] =  1e-4*Passes[kPass].Data[i].z;
      dR.SetTranslation(xyz);
      cout << "dR\t"; dR.Print();
    }
#if 0
#if 1 /* 10/03/11 */    
    LS = dR * LSold; cout << "LS_new\t"; LS.Print();
#else
    LS = LSold * dR; cout << "LS_new\t"; LS.Print();
#endif
    LS = LSold * dR; cout << "LS_new\t"; LS.Print();
#else
    dRI = dR.Inverse();
    LS = LSold * dRI; cout << "LS_new\t"; LS.Print();
#endif
    Survey_st row; memset (&row, 0, sizeof(Survey_st));
    Double_t *r = LS.GetRotationMatrix();
    memcpy(&row.r00, r, 9*sizeof(Double_t));
    row.sigmaRotX = Passes[kPass].Data[i].Dalpha;
    row.sigmaRotY = Passes[kPass].Data[i].Dbeta;
    row.sigmaRotZ = Passes[kPass].Data[i].Dgamma;
    row.sigmaTrX  = Passes[kPass].Data[i].Dx;
    row.sigmaTrY  = Passes[kPass].Data[i].Dy;
    row.sigmaTrZ  = Passes[kPass].Data[i].Dz;
    row.Id = TpcSuperSectorPositionOld->Id(s);
    Double_t *t = LS.GetTranslation();
    memcpy(&row.t0, t, 3*sizeof(Double_t));
    TpcSuperSectorPosition->AddAt(&row);
  }
  TString fOut =  Form("%s.%8i.%06i.C",TpcSuperSectorPosition->GetName(),Passes[kPass].date,Passes[kPass].time);
  ofstream out;
  cout << "Create " << fOut << endl;
  out.open(fOut.Data());
  out << "TDataSet *CreateTable() {" << endl;
  out << "  if (!gROOT->GetClass(\"St_Survey\")) return 0;" << endl;
  out << "  Survey_st row[" << NoSectors << "] = {" << endl; 
  Survey_st *OuterSectorPositions = TpcSuperSectorPosition->GetTable(); 
  for (Int_t i = 0; i < NoSectors; i++, OuterSectorPositions++) { 
    out << "    {" << Form("%2i",TpcSuperSectorPositionOld->Id(i)); 
    Double_t *r = &(OuterSectorPositions->r00);
    for (Int_t j =  0; j <  9; j++) out << Form(",%9.6f",r[j]);
    for (Int_t j =  9; j < 12; j++) out << Form(",%8.4f",r[j]);
    for (Int_t j = 12; j < 18; j++) out << Form(",%5.2f",TMath::Min(99.99,r[j]));
    out << ",\"" << Passes[kPass].PassName << "\"}";
    if (i != NoSectors - 1) out << ",";
    out << endl;
  } 
  out << "  };" << endl;
  out << "  St_Survey *tableSet = new St_Survey(\"" << TpcSuperSectorPosition->GetName() << "\"," << NoSectors << ");" << endl; 
  out << "  for (Int_t i = 0; i < " << NoSectors << "; i++) tableSet->AddAt(&row[i].Id, i);" << endl; 
  out << "  return (TDataSet *)tableSet;" << endl;
  out << "}" << endl;
  out.close(); 
}
