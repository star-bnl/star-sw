TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 83018
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53409; // +/- 9.77782e-06 cm/us All: East = 0.952446 +/- 0.0202045
  row.laserDriftVelocityWest	 =   5.53409; // +/- 9.77782e-06 cm/us All: West = 1.4286 +/- 0.00177798
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53409 +/- 9.77782e-06
  return (TDataSet *)tableSet;// West = 5.53407 +/- 9.82801e-06 East = 5.53651 +/- 9.68743e-05
};
