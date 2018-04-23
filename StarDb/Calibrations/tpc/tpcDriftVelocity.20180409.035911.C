TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 98060
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55061; // +/- 4.01015e-05 cm/us All: East = -0.207143 +/- 0.00988933
  row.laserDriftVelocityWest	 =   5.55061; // +/- 4.01015e-05 cm/us All: West = 0.32555 +/- 0.0150398
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55061 +/- 4.01015e-05
  return (TDataSet *)tableSet;// West = 5.54981 +/- 5.42528e-05 East = 5.55156 +/- 5.95394e-05
};
