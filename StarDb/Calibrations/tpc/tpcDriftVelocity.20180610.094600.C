TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 161012
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54829; // +/- 7.97032e-06 cm/us All: East = -0.0307018 +/- 0.00352846
  row.laserDriftVelocityWest	 =   5.54829; // +/- 7.97032e-06 cm/us All: West = 0.25862 +/- 0.00154571
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54829 +/- 7.97032e-06
  return (TDataSet *)tableSet;// West = 5.54803 +/- 8.71678e-06 East = 5.54961 +/- 1.96852e-05
};
