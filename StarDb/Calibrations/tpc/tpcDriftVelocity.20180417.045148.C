TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 107002
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55593; // +/- 1.12311e-05 cm/us All: East = 0.0542687 +/- 0.00250168
  row.laserDriftVelocityWest	 =   5.55593; // +/- 1.12311e-05 cm/us All: West = 0.381006 +/- 0.00331915
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55593 +/- 1.12311e-05
  return (TDataSet *)tableSet;// West = 5.55481 +/- 1.86661e-05 East = 5.55657 +/- 1.4061e-05
};
