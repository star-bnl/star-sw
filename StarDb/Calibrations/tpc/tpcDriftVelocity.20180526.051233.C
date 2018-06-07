TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 146003
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5517; // +/- 5.85614e-05 cm/us All: East = 0.248662 +/- 0.0558811
  row.laserDriftVelocityWest	 =   5.5517; // +/- 5.85614e-05 cm/us All: West = 0.113119 +/- 0.0235021
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5517 +/- 5.85614e-05
  return (TDataSet *)tableSet;// West = 5.55154 +/- 7.34131e-05 East = 5.55198 +/- 9.71074e-05
};
