TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 133026
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54775; // +/- 8.79675e-06 cm/us All: East = -0.277508 +/- 0.00291733
  row.laserDriftVelocityWest	 =   5.54775; // +/- 8.79675e-06 cm/us All: West = 0.230352 +/- 0.00185647
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54775 +/- 8.79675e-06
  return (TDataSet *)tableSet;// West = 5.54695 +/- 1.04235e-05 East = 5.54972 +/- 1.63983e-05
};
