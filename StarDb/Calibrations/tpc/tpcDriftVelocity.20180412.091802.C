TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 102023
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55769; // +/- 0.000245987 cm/us All: East = 0.593481 +/- 0.937154
  row.laserDriftVelocityWest	 =   5.55769; // +/- 0.000245987 cm/us All: West = -999 +/- 999
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55769 +/- 0.000245987
  return (TDataSet *)tableSet;// West = -999 +/- 999 East = 5.55769 +/- 0.000245987
};
