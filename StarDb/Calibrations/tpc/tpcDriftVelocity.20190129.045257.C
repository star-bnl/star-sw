TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 28065
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52941; // +/- 1.57096e-05 cm/us All: East = -0.170504 +/- 0.00276455
  row.laserDriftVelocityWest	 =   5.52941; // +/- 1.57096e-05 cm/us All: West = -999 +/- 999
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52941 +/- 1.57096e-05
  return (TDataSet *)tableSet;// West = -999 +/- 999 East = 5.52941 +/- 1.57096e-05
};
