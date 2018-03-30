TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 85001
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53087; // +/- 1.17865e-05 cm/us All: East = 1.55708 +/- 0.0137889
  row.laserDriftVelocityWest	 =   5.53087; // +/- 1.17865e-05 cm/us All: West = 2.02155 +/- 0.00215368
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53087 +/- 1.17865e-05
  return (TDataSet *)tableSet;// West = 5.5308 +/- 1.19706e-05 East = 5.53326 +/- 6.74589e-05
};
