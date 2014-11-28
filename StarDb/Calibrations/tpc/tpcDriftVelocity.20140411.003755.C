TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 100099
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52192; // +/- 1.65526e-05 cm/us All: East = 0.785742 +/- 0.0104674
  row.laserDriftVelocityWest	 =   5.52192; // +/- 1.65526e-05 cm/us All: West = 0.526864 +/- 0.00318174
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52192 +/- 1.65526e-05
  return (TDataSet *)tableSet;// West = 5.52205 +/- 1.7415e-05 East = 5.52068 +/- 5.32592e-05
};
