TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// 
// These are the rotation and translation matrices for placing the
// SVT Wafers on their ladders from survey data. In calculating
// the normal [y] position of the wafer 150 um were subtracted in
// to bring the point from the surface[survey] to the wafer center.
// All translations are relative to the Ladder survery precision
// point A. This matrix is multiplied first by LadderOnSurvery
// and then by LadderOnShell matrices to be placed on SVT Clamshell.    
// 
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_Survey")) return 0;
  Survey_st row = {0, 1,0,0, 0,1,0, 0,0,1, 0,0,0, 1e-5,1e-5,1e-4,4e-3,4e-3,4e-3,"Ideal"};
  Int_t n = 216;
  St_Survey *tableSet = new St_Survey("WaferOnLadder",n);
  Int_t NB = 3;
  Double_t dZ = 6.305;
  for (Int_t barrel = 1; barrel <= NB; barrel++) {
    Int_t NL = 8;
    Int_t NW = 4;
    Double_t zmin = -9.4575;
    if (barrel == 2) {NL = 12; NW = 6; zmin = -15.7625;}
    if (barrel == 3) {NL = 16; NW = 7; zmin = -18.915 ;}
    for (Int_t ladder = 1; ladder <= NL; ladder++) {
      for (Int_t wafer = 1; wafer <= NW; wafer++) {
	row.Id = 1000*barrel + 100*wafer + ladder;
	row.t2 = zmin + dZ*(wafer - 1) + 23.5250;
	tableSet->AddAt(&row.Id);
      }
    }
  }
  return (TDataSet *)tableSet;
}
