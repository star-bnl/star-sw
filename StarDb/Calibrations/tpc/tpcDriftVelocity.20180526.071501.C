TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 146008
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55157; // +/- 6.71705e-05 cm/us All: East = 0.265786 +/- 0.145523
  row.laserDriftVelocityWest	 =   5.55157; // +/- 6.71705e-05 cm/us All: West = 0.177718 +/- 0.260564
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55157 +/- 6.71705e-05
  return (TDataSet *)tableSet;// West = 5.55153 +/- 7.13481e-05 East = 5.55192 +/- 0.000199226
};
