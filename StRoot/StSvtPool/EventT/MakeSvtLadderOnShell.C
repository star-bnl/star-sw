class St_db_Maker;
class TTable;
St_db_Maker *dbMk = 0;
TTable *table = 0;
struct data_t {
  Int_t barrel, layer, ladder, wafer, type;
  Double_t u, Du, v, Dv, w, Dw, alpha, Dalpha, beta, Dbeta, gamma, Dgamma;
  Char_t *Comment;
};
const Int_t N = 36;
Int_t date = 20070321;
Int_t time =      206; // 
#define CutSTEP
const Char_t *Pass = "Pass217 FFK"; 
data_t Data[N] = {
  {1,2, 1, 0,-1,  -9.48, 0.54,  -0.72, 0.90,  -3.00, 3.55,  -0.16, 0.03,   0.02, 0.11,   0.01, 0.01,"Average"},
  {1,1, 2, 0,-1,   1.27, 0.39,   4.36, 0.61, -21.58, 2.39,   0.17, 0.03,   0.20, 0.08,   0.08, 0.01,"Average"},
  {1,2, 3, 0,-1,  15.49, 0.67,  92.52, 1.06,   1.50, 4.56,   0.59, 0.06,   0.33, 0.14,  -0.33, 0.02,"Average"},
  {1,1, 4, 0,-1,  -6.76, 0.76, 124.26, 1.04, -44.76, 4.58,   0.00, 0.10,  -9.65, 0.14,   0.05, 0.03,"Average"},
  {1,2, 5, 0,-1,   0.34, 0.44,  10.93, 0.93,   8.23, 2.12,   0.10, 0.03,   0.15, 0.12,   0.05, 0.01,"Average"},
  {1,1, 6, 0,-1,  -2.25, 0.38,  -1.03, 0.80, -25.70, 3.12,  -0.01, 0.03,   0.10, 0.11,   0.02, 0.01,"Average"},
  {1,2, 7, 0,-1,  -4.12, 0.43,  -5.81, 0.81, -48.57, 3.19,   0.02, 0.04,   0.08, 0.12,   0.06, 0.01,"Average"},
  {1,1, 8, 0,-1,   1.11, 0.54,  -6.15, 0.95, -22.24, 3.63,   0.00, 0.08,  -0.10, 0.12,  -0.08, 0.02,"Average"},
  {2,4, 1, 0,-1,      0,-9.99,      0,-9.99,      0,-9.99,      0,-9.99,      0,-9.99,      0,-9.99,"Average"},
  {2,3, 2, 0,-1,   5.14, 0.40,  -3.33, 0.81,  -8.41, 1.74,  -0.04, 0.03,   0.14, 0.14,   0.05, 0.01,"Average"},
  {2,4, 3, 0,-1,  -6.41, 0.48,  -0.78, 0.71,   5.24, 1.73,  -0.03, 0.03,   0.28, 0.13,   0.01, 0.01,"Average"},
  {2,3, 4, 0,-1,   6.07, 0.50,  14.45, 1.16,   8.63, 2.04,   0.08, 0.02,  -0.44, 0.13,  -0.00, 0.01,"Average"},
  {2,4, 5, 0,-1,  -0.43, 0.46,  -7.11, 0.94,  -1.55, 2.10,   0.23, 0.04,  -0.41, 0.16,   0.06, 0.01,"Average"},
  {2,3, 6, 0,-1,  -2.56, 0.50,  17.87, 0.85,  -8.84, 2.10,   0.07, 0.03,  -0.19, 0.15,   0.02, 0.01,"Average"},
  {2,4, 7, 0,-1,  -3.78, 0.42,  10.61, 0.86,   7.32, 2.07,  -0.01, 0.03,  -0.95, 0.18,   0.02, 0.01,"Average"},
  {2,3, 8, 0,-1, -24.64, 1.25,   9.69, 1.81, -17.04, 3.93,   0.20, 0.07,  -0.04, 0.24,   0.20, 0.03,"Average"},
  {2,4, 9, 0,-1,  -1.06, 0.49,   1.26, 1.09, -22.13, 2.56,   0.12, 0.04,  -0.43, 0.23,   0.05, 0.01,"Average"},
  {2,3,10, 0,-1,  -7.14, 0.39,  -0.95, 0.94,  -2.87, 2.22,  -0.16, 0.05,   0.16, 0.16,   0.05, 0.01,"Average"},
  {2,4,11, 0,-1,      0,-9.99,      0,-9.99,      0,-9.99,      0,-9.99,      0,-9.99,      0,-9.99,"Average"},
  {2,3,12, 0,-1, -10.84, 0.54,  -8.63, 1.02,  11.48, 2.72,   0.02, 0.05,   0.38, 0.18,   0.03, 0.01,"Average"},
  {3,6, 1, 0,-1,  -1.07, 0.62,  -3.47, 1.24,   7.32, 3.47,  -0.02, 0.03,   0.08, 0.18,   0.05, 0.01,"Average"},
  {3,5, 2, 0,-1,  -9.15, 0.64,  -3.33, 1.10,  -8.51, 2.81,   0.09, 0.04,  -0.57, 0.18,   0.00, 0.01,"Average"},
  {3,6, 3, 0,-1,   0.35, 0.29,  -2.25, 0.92, -10.24, 2.06,  -0.16, 0.03,   0.10, 0.16,   0.01, 0.00,"Average"},
  {3,5, 4, 0,-1,   1.25, 0.50,  -9.99, 0.74,   0.58, 1.92,   0.01, 0.03,   0.10, 0.16,  -0.03, 0.01,"Average"},
  {3,6, 5, 0,-1,  -3.02, 0.89,  -0.41, 1.32,  21.32, 3.31,   0.02, 0.03,  -0.12, 0.18,   0.03, 0.01,"Average"},
  {3,5, 6, 0,-1,  -0.52, 0.51,   1.77, 1.02,   9.28, 2.60,   0.08, 0.03,   0.17, 0.18,  -0.02, 0.01,"Average"},
  {3,6, 7, 0,-1,   0.90, 0.86, -11.78, 1.93, -26.40, 4.59,  -0.27, 0.14,  -0.84, 0.40,   0.08, 0.03,"Average"},
  {3,5, 8, 0,-1, -12.29, 0.76,  14.79, 1.47,   9.28, 3.55,   0.03, 0.04,  -0.46, 0.24,   0.01, 0.01,"Average"},
  {3,6, 9, 0,-1,   1.04, 0.43,   8.78, 1.18, -13.74, 3.00,   0.08, 0.04,  -0.23, 0.10,   0.04, 0.01,"Average"},
  {3,5,10, 0,-1,   1.38, 0.54,  -0.63, 1.22,  10.68, 3.29,   0.02, 0.03,   0.18, 0.18,   0.02, 0.01,"Average"},
  {3,6,11, 0,-1,  -0.19, 0.97,   5.35, 2.00,  22.98, 5.27,  -0.07, 0.10,   0.12, 0.31,  -0.06, 0.03,"Average"},
  {3,5,12, 0,-1,  -0.97, 0.54,  -2.64, 1.27, -12.72, 3.48,   0.09, 0.05,   0.51, 0.27,   0.06, 0.01,"Average"},
  {3,6,13, 0,-1,   2.09, 0.31,  13.83, 1.19,   5.42, 2.59,  -0.42, 0.04,   0.65, 0.19,   0.01, 0.00,"Average"},
  {3,5,14, 0,-1,  -2.63, 0.67,  -0.55, 1.76, -40.93, 3.56,  -0.00, 0.04,   1.63, 0.27,  -0.01, 0.01,"Average"},
  {3,6,15, 0,-1,  -9.77, 0.73, -12.88, 1.73, -11.78, 3.90,   0.25, 0.09,  -0.44, 0.29,  -0.03, 0.02,"Average"},
  {3,5,16, 0,-1, -15.85, 0.72, -15.55, 1.47, -24.82, 1.84,  -0.41, 0.09,  -0.02, 0.29,   0.21, 0.02,"Average"}
};
//________________________________________________________________________________
void MakeSvtLadderOnShell(){//, Int_t time = 38 ){ // combine SvtBarrelOnGlobal and SvtShellsOnBarrel into SvtShellsOnGlobal
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
  dbMk->SetDebug(2);
  dbMk->SetDateTime(date,time); 
  St_Survey *LadderOnShellOld = (St_Survey *) dbMk->GetDataBase("Geometry/svt/LadderOnShell");  // shells in the SVT barrel coordinate system
  if (! (LadderOnShellOld)) return;
  Survey_st *LaddersOnShells = LadderOnShellOld->GetTable();  // shells in the SVT barrel coordinate system
  Int_t NoLadders = LadderOnShellOld->GetNRows();
  St_Survey *LadderOnShell = new St_Survey("LadderOnShell",NoLadders);
#ifdef CutSTEP
  cout << "============================ CutSTEP =========================" << endl;
#endif
  TGeoHMatrix T;
  Double_t tr[3] = {0,0, 23.525};
  // Double_t tr[3] = {0,0, -23.525};
  T.SetTranslation(tr);
  TGeoHMatrix F;
  Double_t flip[9] = {
    1, 0, 0,
    0, 0, 1,
    0, 1, 0
  };
  F.SetRotation(flip);
  TGeoHMatrix TInv = T.Inverse();
  for (Int_t s = 0; s < NoLadders; s++, LaddersOnShells++) {
    TGeoHMatrix LSold, LS, dR, ddR, dddR;
    //    LadderOnShellOld->Print(s,1);
    LSold.SetRotation(&LaddersOnShells->r00);
    LSold.SetTranslation(&LaddersOnShells->t0); cout << "===================== Ladder \t" << s+1 << endl; cout << "\tLSold\t"; LSold.Print();
    Int_t i = -1; 
    for (Int_t k = 0; k < N; k++) {
      if (LaddersOnShells->Id == 1000*Data[k].barrel + Data[k].ladder) {i = k; break;}
    }
    if (i < 0) {
      cout << "Correction for " << LaddersOnShells->Id << " is not found" << endl;
    } else {
      cout << "Ladder " << LaddersOnShells->Id << "\ti " << i 
	   << "\talpha " <<  Data[i].alpha << "+/-" << Data[i].Dalpha
	   << "\tbeta "  <<  Data[i].beta  << "+/-" << Data[i].Dbeta
	   << "\tgamma " <<  Data[i].gamma << "+/-" << Data[i].Dgamma
	   << "\tu " << Data[i].u << "\tv " << Data[i].v << "\tw " << Data[i].w << endl; 
      Double_t xyz[3] = {0, 0, 0};
      //      if (Data[i].Dalpha < 2 && Data[i].Dbeta  < 2 && Data[i].Dgamma < 2) {
#ifndef CutSTEP
	if (Data[i].Dalpha > 0) dR.RotateX(-180./TMath::Pi()*Data[i].alpha*1e-3);
	if (Data[i].Dbeta  > 0) dR.RotateZ(-180./TMath::Pi()*Data[i].beta*1e-3);
	if (Data[i].Dgamma > 0) dR.RotateY(-180./TMath::Pi()*Data[i].gamma*1e-3);
	if (Data[i].Du > 0) xyz[0] =  1e-4*Data[i].u;
	if (Data[i].Dv > 0) xyz[2] =  1e-4*Data[i].v;
	if (Data[i].Dw > 0) xyz[1] =  1e-4*Data[i].w;
#else
	if (Data[i].Dalpha > 0) dR.RotateX(-180./TMath::Pi()*Data[i].alpha*0.5e-3);
	if (Data[i].Dbeta  > 0) dR.RotateZ(-180./TMath::Pi()*Data[i].beta*0.5e-3);
	if (Data[i].Dgamma > 0) dR.RotateY(-180./TMath::Pi()*Data[i].gamma*0.5e-3);
	if (Data[i].Du > 0) xyz[0] =  0.5e-4*Data[i].u;
	if (Data[i].Dv > 0) xyz[2] =  0.5e-4*Data[i].v;
	if (Data[i].Dw > 0) xyz[1] =  0.5e-4*Data[i].w;
#endif
	//      }
      dR.SetTranslation(xyz);
      cout << "dR\t"; dR.Print();
    }
    // 	shellOnGlobal *	ladderOnShell * T * dR * T**-1 * ladderOnSurvey * waferOnLadder 
    // 	shellOnGlobal * ( ladderOnShell * F * T * dR * T**-1 * F ) * ladderOnSurvey * waferOnLadder 
    ddR = T * dR * TInv; cout << "ddR\t" << ddR.Print();
    dddR = F * T * dR * TInv * F; cout << "dddR\t" << dddR.Print();
    //    LS = LSold * dR; cout << "LS_old\t"; LS.Print();
    //    LS = LSold * dddR; cout << "LS_new\t"; LS.Print();
    LS = LSold * ddR; cout << "LS_new\t"; LS.Print();
    Survey_st row = *LaddersOnShells;
    Double_t *r = LS.GetRotationMatrix();
    memcpy(&row.r00, r, 9*sizeof(Double_t));
    Double_t *t = LS.GetTranslation();
    memcpy(&row.t0, t, 3*sizeof(Double_t));
    LadderOnShell->AddAt(&row);
    //    LadderOnShell->Print(LadderOnShell->GetNRows()-1,1);
  }
  TString fOut =  Form("%s.%8i.%06i.C",LadderOnShell->GetName(),date,time);
  ofstream out;
  cout << "Create " << fOut << endl;
  out.open(fOut.Data());
  out << "TDataSet *CreateTable() {" << endl;
  out << "  if (!gROOT->GetClass(\"St_Survey\")) return 0;" << endl;
  out << "  Survey_st row[" << NoLadders << "] = {" << endl; 
  Survey_st *LaddersOnShells = LadderOnShell->GetTable(); 
  for (Int_t i = 0; i < NoLadders; i++, LaddersOnShells++) { 
    out << "    {" << Form("%1i",LaddersOnShells->Id); 
    Double_t *r = &(LaddersOnShells->r00);
    for (Int_t j = 0; j < 9; j++) out << Form(",%8.5f",r[j]);
    for (Int_t j = 9; j < 12; j++) out << Form(",%8.4f",r[j]);
    for (Int_t j = 12; j < 18; j++) out << Form(",%3.1f",r[j]);
    out << ",\"" << Pass << "\"}";
    if (i != NoLadders - 1) out << ",";
    out << endl;
  } 
  out << "  };" << endl;
  out << "  St_Survey *tableSet = new St_Survey(\"" << LadderOnShell->GetName() << "\"," << NoLadders << ");" << endl; 
  out << "  for (Int_t i = 0; i < " << NoLadders << "; i++) tableSet->AddAt(&row[i].Id, i);" << endl; 
  out << "  return (TDataSet *)tableSet;" << endl;
  out << "}" << endl;
  out.close(); 
}
