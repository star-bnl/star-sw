TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 152072
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55178; // +/- 5.13033e-05 cm/us All: East = -0.0851216 +/- 0.0293643
  row.laserDriftVelocityWest	 =   5.55178; // +/- 5.13033e-05 cm/us All: West = 0.0226913 +/- 0.00949292
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55178 +/- 5.13033e-05
  return (TDataSet *)tableSet;// West = 5.55121 +/- 7.53509e-05 East = 5.55227 +/- 7.00467e-05
};
