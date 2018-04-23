TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 106037
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55576; // +/- 1.17863e-05 cm/us All: East = -0.00881779 +/- 0.00301291
  row.laserDriftVelocityWest	 =   5.55576; // +/- 1.17863e-05 cm/us All: West = 0.324432 +/- 0.0029411
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55576 +/- 1.17863e-05
  return (TDataSet *)tableSet;// West = 5.55482 +/- 1.67036e-05 East = 5.5567 +/- 1.66332e-05
};
