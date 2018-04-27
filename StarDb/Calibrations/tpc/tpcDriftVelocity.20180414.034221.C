TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 103055
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54397; // +/- 8.12888e-06 cm/us All: East = -0.0804776 +/- 0.00196316
  row.laserDriftVelocityWest	 =   5.54397; // +/- 8.12888e-06 cm/us All: West = 0.120312 +/- 0.00215761
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54397 +/- 8.12888e-06
  return (TDataSet *)tableSet;// West = 5.54338 +/- 1.1979e-05 East = 5.54447 +/- 1.10671e-05
};
