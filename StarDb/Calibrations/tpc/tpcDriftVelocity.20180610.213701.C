TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 161036
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54762; // +/- 6.31229e-06 cm/us All: East = 0.13172 +/- 0.0033932
  row.laserDriftVelocityWest	 =   5.54762; // +/- 6.31229e-06 cm/us All: West = 0.257544 +/- 0.00118926
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54762 +/- 6.31229e-06
  return (TDataSet *)tableSet;// West = 5.54755 +/- 6.69224e-06 East = 5.54815 +/- 1.90042e-05
};
