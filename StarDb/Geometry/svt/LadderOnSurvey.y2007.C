TDataSet *CreateTable() { 
// -----------------------------------------------------------------
//
// These are the rotation+translation survey matrices on a local
// system. The translation numbers are for precision point A in
// "Shell". These matrices 'act' on WaferOnLadder from the left
// and being acted from the left with LadderOnShell in order to
// be globally rotated into the Clamshell system.    
//
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_Survey")) return 0;
  Survey_st row = {0, 1,0,0, 0,1,0, 0,0,1, 0,0,0, 1e-5,1e-5,1e-4,4e-3,4e-3,4e-3,"Ideal"};
  Int_t n = 36;
  St_Survey *tableSet = new St_Survey("LadderOnSurvey",n);
  Int_t NB = 3;
  Double_t rad_offset = 0.015;
  Double_t radii[6] = {  6.37,  7.38, 10.38, 11.27, 14.19, 15.13};
  for (Int_t barrel = 1; barrel <= NB; barrel++) {
    Int_t NL = 8;
    if (barrel == 2) NL = 12;
    if (barrel == 3) NL = 16;
    for (Int_t ladder = 1; ladder <= NL; ladder++) {
      row.Id = 1000*barrel + ladder;
      Int_t layer = 2*barrel - 1 + ladder%2;
      row.t1 = radii[layer-1] + rad_offset;
      tableSet->AddAt(&row.Id);
    }
  }
  return (TDataSet *)tableSet;
}
