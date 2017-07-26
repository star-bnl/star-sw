/* 
   root.exe lDb.C pxlSectorOnHalfCheck.C
 */
void pxlSectorOnHalfCheck() {
  TString command = ".L  pxlSectorOnHalf.20151210.000001.C";
  TInterpreter::EErrorCode ee;
  gInterpreter->ProcessLine(command,&ee);
  St_Survey *ideal = (St_Survey *) gInterpreter->Calc("CreateTable()",&ee);
  command.ReplaceAll(".L ",".U ");
  gInterpreter->ProcessLine(command,&ee);
  TString command2 = ".L  pxlSectorOnHalf.20160101.000004.C";
  gInterpreter->ProcessLine(command2,&ee);
  St_Survey *real = (St_Survey *) gInterpreter->Calc("CreateTable()",&ee);
  command2.ReplaceAll(".L ",".U ");
  gInterpreter->ProcessLine(command2,&ee);
  if (! ideal || ! real) return;
  ideal->Print(0,10);
  real->Print(0,10);
  if (ideal->GetNRows() != real->GetNRows()) return;
  for (Int_t i = 0; i < real->GetNRows(); i++) {
    Survey_st *Ideal = ideal->GetTable() + i; 
    TGeoHMatrix I; I.SetRotation(&Ideal->r00); I.SetTranslation(&Ideal->t0);
    Survey_st *Real  = real->GetTable()  + i; 
    TGeoHMatrix R; R.SetRotation(&Real->r00); R.SetTranslation(&Real->t0);
    TGeoHMatrix P = R * I.Inverse();
    cout << "i = " << i << "\t"; P.Print();
  }
}
