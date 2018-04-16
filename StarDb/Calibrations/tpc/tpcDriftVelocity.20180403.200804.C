TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 93042
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53052; // +/- 8.91897e-06 cm/us All: East = -1.18528 +/- 0.00635395
  row.laserDriftVelocityWest	 =   5.53052; // +/- 8.91897e-06 cm/us All: West = -0.21225 +/- 0.00162954
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53052 +/- 8.91897e-06
  return (TDataSet *)tableSet;// West = 5.53019 +/- 9.205e-06 East = 5.53557 +/- 3.60581e-05
};
