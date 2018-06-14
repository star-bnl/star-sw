TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 162004
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5475; // +/- 2.90997e-05 cm/us All: East = 0.0806257 +/- 0.0162723
  row.laserDriftVelocityWest	 =   5.5475; // +/- 2.90997e-05 cm/us All: West = 0.252626 +/- 0.00794386
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5475 +/- 2.90997e-05
  return (TDataSet *)tableSet;// West = 5.54726 +/- 3.33674e-05 East = 5.54824 +/- 5.94686e-05
};
