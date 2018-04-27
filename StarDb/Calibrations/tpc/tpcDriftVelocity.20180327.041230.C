TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 86001
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.51899; // +/- 7.53823e-06 cm/us All: East = 0.123485 +/- 0.00692264
  row.laserDriftVelocityWest	 =   5.51899; // +/- 7.53823e-06 cm/us All: West = 0.210649 +/- 0.00137566
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.51899 +/- 7.53823e-06
  return (TDataSet *)tableSet;// West = 5.51902 +/- 7.66673e-06 East = 5.51806 +/- 4.13473e-05
};
