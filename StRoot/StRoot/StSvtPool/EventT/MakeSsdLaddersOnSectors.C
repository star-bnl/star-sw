class St_db_Maker;
class TTable;
St_db_Maker *dbMk = 0;
TTable *table = 0;
struct data_t {
  Int_t barrel, layer, ladder, wafer, type;
  Double_t u, Du, v, Dv, w, Dw, alpha, Dalpha, beta, Dbeta, gamma, Dgamma;
  Char_t *Comment;
};
const Int_t N = 20;
Int_t date = 20070321;
Int_t time =      207; // 
#define CutSTEP
const Char_t *Pass = "Pass217 FFL";
data_t Data[N] = {
        {4,7, 1, 0,-1,  -0.05, 0.06,  13.74, 1.79,  -5.07, 1.28,  -0.31, 0.03,   0.34, 0.20,   0.00, 0.00,"Average"},
        {4,7, 2, 0,-1,  -1.66, 0.15,   4.90, 2.75,  27.89, 2.38,  -0.20, 0.07,  -0.38, 0.40,  -0.03, 0.00,"Average"},
        {4,7, 3, 0,-1,  -0.20, 0.18, -36.62, 5.37,  -5.33, 3.05,   0.61, 0.12,  -0.64, 0.55,   0.02, 0.00,"Average"},
        {4,7, 4, 0,-1,   0.72, 0.05,   0.89, 1.51, -30.16, 1.17,  -0.02, 0.02,  -0.01, 0.17,   0.00, 0.00,"Average"},
        {4,7, 5, 0,-1,   0.27, 0.05,   0.57, 1.54,  13.57, 1.43,   0.01, 0.02,   0.72, 0.16,   0.02, 0.00,"Average"},
        {4,7, 6, 0,-1,      0,-9.99,      0,-9.99,      0,-9.99,      0,-9.99,      0,-9.99,      0,-9.99,"Average"},
        {4,7, 7, 0,-1,      0,-9.99,      0,-9.99,      0,-9.99,      0,-9.99,      0,-9.99,      0,-9.99,"Average"},
        {4,7, 8, 0,-1,      0,-9.99,      0,-9.99,      0,-9.99,      0,-9.99,      0,-9.99,      0,-9.99,"Average"},
        {4,7, 9, 0,-1,  -0.70, 0.05,   3.01, 1.81,  -4.55, 1.11,   0.23, 0.03,  -0.14, 0.18,  -0.00, 0.00,"Average"},
        {4,7,10, 0,-1,   2.49, 0.08, -11.15, 1.66,   4.10, 1.19,   0.10, 0.05,   0.17, 0.24,  -0.01, 0.00,"Average"},
        {4,7,11, 0,-1,  -0.62, 0.10,   5.41, 3.32,   5.69, 1.74,   0.56, 0.08,  -2.49, 0.56,  -0.01, 0.00,"Average"},
        {4,7,12, 0,-1,  -1.01, 0.06,   6.73, 1.34,  -8.84, 1.82,  -0.24, 0.02,  -0.04, 0.16,  -0.03, 0.00,"Average"},
        {4,7,13, 0,-1,  -0.08, 0.09,  -4.46, 1.63,  10.86, 1.33,  -0.23, 0.04,  -0.88, 0.18,  -0.00, 0.00,"Average"},
        {4,7,14, 0,-1,   2.62, 0.22,  70.40, 2.86,  -4.58, 4.03,   0.73, 0.09,  10.31, 0.70,   0.00, 0.00,"Average"},
        {4,7,15, 0,-1,   1.65, 0.09,   4.86, 2.93, -19.02, 1.79,   0.44, 0.06,   1.41, 0.50,   0.02, 0.00,"Average"},
        {4,7,16, 0,-1,   0.27, 0.04,  -9.50, 1.69,  -2.12, 0.94,   0.21, 0.03,  -0.05, 0.20,   0.00, 0.00,"Average"},
        {4,7,17, 0,-1,  -0.78, 0.06,  -0.75, 1.59, -18.17, 1.27,  -0.19, 0.03,  -0.26, 0.18,   0.01, 0.00,"Average"},
        {4,7,18, 0,-1,  -2.06, 0.06,  -8.53, 1.43,   8.99, 1.44,   0.08, 0.03,  -0.57, 0.15,  -0.02, 0.00,"Average"},
        {4,7,19, 0,-1,   2.25, 0.06,  -0.35, 1.72,  11.66, 1.65,   0.20, 0.03,   0.27, 0.18,   0.00, 0.00,"Average"},
        {4,7,20, 0,-1,  -1.61, 0.07,   5.94, 1.66, -33.09, 1.27,  -0.40, 0.04,  -1.73, 0.20,  -0.02, 0.00,"Average"},
};
#endif
//________________________________________________________________________________
void MakeSsdLaddersOnSectors(){//, Int_t time = 38 ){ // combine SsdBarrelOnGlobal and SsdSectorsOnBarrel into SsdSectorsOnGlobal
  gROOT->LoadMacro("bfc.C");
  bfc(0,"mysql,db,nodefault");
  StMaker *dbMk = chain->Maker("db");
  if (! dbMk) return;
  dbMk->SetDebug(1);
  StEvtHddr *header = chain->GetEvtHddr();
  header->SetRunNumber(1);
  dbMk->SetDateTime(date,time); 
  header->SetDateTime(date,time);
  chain->MakeEvent();
#ifdef CutSTEP
  cout << "============================ CutSTEP =========================" << endl;
#endif  
  St_Survey *SsdLaddersOnSectorsOld = (St_Survey *) dbMk->GetDataBase("Geometry/ssd/SsdLaddersOnSectors");  // sectors in the SSD barrel coordinate system
  if (! (SsdLaddersOnSectorsOld)) return;
  Survey_st *LaddersOnSectors = SsdLaddersOnSectorsOld->GetTable();  // sectors in the SSD barrel coordinate system
  Int_t NoLadders = SsdLaddersOnSectorsOld->GetNRows();
  St_Survey *SsdLaddersOnSectors = new St_Survey("SsdLaddersOnSectors",NoLadders);
  for (Int_t s = 0; s < NoLadders; s++, LaddersOnSectors++) {
    TGeoHMatrix LSold, LS, dR;
    //    SsdLaddersOnSectorsOld->Print(s,1);
    Int_t i = LaddersOnSectors->Id%100 - 1;
    cout << "Ladder " << LaddersOnSectors->Id << "\ti " << i 
	 << "\talpha " <<  Data[i].alpha << "\tbeta " <<  Data[i].beta << "\tgamma " <<  Data[i].gamma << endl;
    cout << "dR\t"; dR.Print();
    //    if (Data[i].Dalpha < 1 && Data[i].Dbeta  < 1 && Data[i].Dgamma < 1) {
#ifndef CutSTEP
      dR.RotateX(-180./TMath::Pi()*Data[i].alpha*1e-3);
      dR.RotateZ(-180./TMath::Pi()*Data[i].beta*1e-3);
      dR.RotateY(-180./TMath::Pi()*Data[i].gamma*1e-3);
      Double_t xyz[3] = {1e-4*Data[i].u,  1e-4*Data[i].w, 1e-4*Data[i].v};
#else
      dR.RotateX(-180./TMath::Pi()*Data[i].alpha*0.5e-3);
      dR.RotateZ(-180./TMath::Pi()*Data[i].beta*0.5e-3);
      dR.RotateY(-180./TMath::Pi()*Data[i].gamma*0.5e-3);
      Double_t xyz[3] = {0.5e-4*Data[i].u,  0.5e-4*Data[i].w, 0.5e-4*Data[i].v};
#endif
      dR.SetTranslation(xyz);
      //    }
    cout << "dR\t"; dR.Print();
    LSold.SetRotation(&LaddersOnSectors->r00);
    LSold.SetTranslation(&LaddersOnSectors->t0); cout << "Ladder\t" << s+1 << endl; cout << "\tLSold\t"; LSold.Print();

    LS = LSold * dR; cout << "LS\t"; LS.Print();
    Survey_st row = *LaddersOnSectors;
    Double_t *r = LS.GetRotationMatrix();
    memcpy(&row.r00, r, 9*sizeof(Double_t));
    Double_t *t = LS.GetTranslation();
    memcpy(&row.t0, t, 3*sizeof(Double_t));
    SsdLaddersOnSectors->AddAt(&row);
    //    SsdLaddersOnSectors->Print(SsdLaddersOnSectors->GetNRows()-1,1);
  }
  TString fOut =  Form("%s.%8i.%06i.C",SsdLaddersOnSectors->GetName(),date,time);
  ofstream out;
  cout << "Create " << fOut << endl;
  out.open(fOut.Data());
  out << "TDataSet *CreateTable() {" << endl;
  out << "  if (!gROOT->GetClass(\"St_Survey\")) return 0;" << endl;
  out << "  Survey_st row[" << NoLadders << "] = {" << endl; 
  Survey_st *LaddersOnSectors = SsdLaddersOnSectors->GetTable(); 
  for (Int_t i = 0; i < NoLadders; i++, LaddersOnSectors++) { 
    out << "    {" << Form("%1i",LaddersOnSectors->Id); 
    Double_t *r = &(LaddersOnSectors->r00);
    for (Int_t j = 0; j < 9; j++) out << Form(",%8.5f",r[j]);
    for (Int_t j = 9; j < 12; j++) out << Form(",%8.4f",r[j]);
    for (Int_t j = 12; j < 18; j++) out << Form(",%3.1f",r[j]);
    out << ",\"" << Pass << "\"}";
    if (i != NoLadders - 1) out << ",";
    out << endl;
  } 
  out << "  };" << endl;
  out << "  St_Survey *tableSet = new St_Survey(\"" << SsdLaddersOnSectors->GetName() << "\"," << NoLadders << ");" << endl; 
  out << "  for (Int_t i = 0; i < " << NoLadders << "; i++) tableSet->AddAt(&row[i].Id, i);" << endl; 
  out << "  return (TDataSet *)tableSet;" << endl;
  out << "}" << endl;
  out.close(); 
}
