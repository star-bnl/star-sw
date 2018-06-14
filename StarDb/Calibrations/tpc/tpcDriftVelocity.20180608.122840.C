TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 159019
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55019; // +/- 5.01459e-06 cm/us All: East = 0.24616 +/- 0.00193227
  row.laserDriftVelocityWest	 =   5.55019; // +/- 5.01459e-06 cm/us All: West = 0.175045 +/- 0.000999834
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55019 +/- 5.01459e-06
  return (TDataSet *)tableSet;// West = 5.55027 +/- 5.64835e-06 East = 5.54988 +/- 1.08957e-05
};
