TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 154023
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55266; // +/- 5.99016e-06 cm/us All: East = 0.412328 +/- 0.00304369
  row.laserDriftVelocityWest	 =   5.55266; // +/- 5.99016e-06 cm/us All: West = 0.160535 +/- 0.0011194
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55266 +/- 5.99016e-06
  return (TDataSet *)tableSet;// West = 5.55283 +/- 6.38163e-06 East = 5.55136 +/- 1.73703e-05
};
