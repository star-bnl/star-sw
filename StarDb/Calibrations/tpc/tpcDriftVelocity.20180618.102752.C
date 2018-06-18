TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 169014
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54649; // +/- 6.36852e-06 cm/us All: East = -0.315397 +/- 0.00249235
  row.laserDriftVelocityWest	 =   5.54649; // +/- 6.36852e-06 cm/us All: West = 0.33432 +/- 0.00126763
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54649 +/- 6.36852e-06
  return (TDataSet *)tableSet;// West = 5.54577 +/- 7.13163e-06 East = 5.54935 +/- 1.41502e-05
};
