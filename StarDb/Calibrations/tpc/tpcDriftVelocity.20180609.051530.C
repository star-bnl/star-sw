TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 160003
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55067; // +/- 6.15121e-06 cm/us All: East = -0.486263 +/- 0.00238824
  row.laserDriftVelocityWest	 =   5.55067; // +/- 6.15121e-06 cm/us All: West = 0.0989472 +/- 0.00121728
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55067 +/- 6.15121e-06
  return (TDataSet *)tableSet;// West = 5.55004 +/- 6.86474e-06 East = 5.55324 +/- 1.38561e-05
};
