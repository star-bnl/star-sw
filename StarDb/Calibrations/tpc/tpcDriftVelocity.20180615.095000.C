TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 166011
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54757; // +/- 8.10245e-06 cm/us All: East = -0.507768 +/- 0.00310392
  row.laserDriftVelocityWest	 =   5.54757; // +/- 8.10245e-06 cm/us All: West = 0.330343 +/- 0.00162731
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54757 +/- 8.10245e-06
  return (TDataSet *)tableSet;// West = 5.54654 +/- 9.17849e-06 East = 5.5512 +/- 1.7246e-05
};
