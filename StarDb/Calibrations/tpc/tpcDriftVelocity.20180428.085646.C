TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 118011
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53923; // +/- 1.1752e-05 cm/us All: East = -0.607612 +/- 0.0828998
  row.laserDriftVelocityWest	 =   5.53923; // +/- 1.1752e-05 cm/us All: West = 0.190553 +/- 0.00211778
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53923 +/- 1.1752e-05
  return (TDataSet *)tableSet;// West = 5.53915 +/- 1.18522e-05 East = 5.54392 +/- 9.05971e-05
};
