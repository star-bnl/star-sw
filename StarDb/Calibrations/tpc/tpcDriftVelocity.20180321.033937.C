TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 79057
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5639; // +/- 1.01731e-05 cm/us All: East = -3.86238 +/- 0.0113702
  row.laserDriftVelocityWest	 =   5.5639; // +/- 1.01731e-05 cm/us All: West = -3.94161 +/- 0.00185026
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5639 +/- 1.01731e-05
  return (TDataSet *)tableSet;// West = 5.56392 +/- 1.032e-05 East = 5.56336 +/- 6.05093e-05
};
