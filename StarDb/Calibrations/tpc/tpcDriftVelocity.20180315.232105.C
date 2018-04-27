TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 74100
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.532; // +/- 1.44819e-05 cm/us All: East = 0.218619 +/- 0.0160329
  row.laserDriftVelocityWest	 =   5.532; // +/- 1.44819e-05 cm/us All: West = 0.153539 +/- 0.00272301
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.532 +/- 1.44819e-05
  return (TDataSet *)tableSet;// West = 5.53201 +/- 1.47852e-05 East = 5.53162 +/- 7.18618e-05
};
