TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 122005
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53145; // +/- 8.46084e-06 cm/us All: East = -0.331992 +/- 0.00460617
  row.laserDriftVelocityWest	 =   5.53145; // +/- 8.46084e-06 cm/us All: West = 0.213435 +/- 0.00160044
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53145 +/- 8.46084e-06
  return (TDataSet *)tableSet;// West = 5.53113 +/- 8.96911e-06 East = 5.53404 +/- 2.54958e-05
};
