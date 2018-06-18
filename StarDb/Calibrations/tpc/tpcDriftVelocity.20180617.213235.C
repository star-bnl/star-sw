TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 168039
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54601; // +/- 7.1698e-06 cm/us All: East = -0.316938 +/- 0.00719083
  row.laserDriftVelocityWest	 =   5.54601; // +/- 7.1698e-06 cm/us All: West = 0.211979 +/- 0.00130622
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54601 +/- 7.1698e-06
  return (TDataSet *)tableSet;// West = 5.5459 +/- 7.29224e-06 East = 5.549 +/- 3.92921e-05
};
