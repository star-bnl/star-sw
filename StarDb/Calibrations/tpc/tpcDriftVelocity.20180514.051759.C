TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 134003
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54743; // +/- 1.21052e-05 cm/us All: East = -0.0510024 +/- 0.00485423
  row.laserDriftVelocityWest	 =   5.54743; // +/- 1.21052e-05 cm/us All: West = 0.477531 +/- 0.00242709
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54743 +/- 1.21052e-05
  return (TDataSet *)tableSet;// West = 5.54688 +/- 1.3508e-05 East = 5.54965 +/- 2.72798e-05
};
