TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 85026
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5192; // +/- 8.28542e-06 cm/us All: East = -0.172562 +/- 0.00250747
  row.laserDriftVelocityWest	 =   5.5192; // +/- 8.28542e-06 cm/us All: West = 0.351826 +/- 0.00186384
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5192 +/- 8.28542e-06
  return (TDataSet *)tableSet;// West = 5.51823 +/- 1.0302e-05 East = 5.52099 +/- 1.39418e-05
};
