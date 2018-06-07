TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 156001
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55632; // +/- 8.72558e-06 cm/us All: East = 0.0196902 +/- 0.00209649
  row.laserDriftVelocityWest	 =   5.55632; // +/- 8.72558e-06 cm/us All: West = 0.48776 +/- 0.00233066
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55632 +/- 8.72558e-06
  return (TDataSet *)tableSet;// West = 5.5549 +/- 1.31398e-05 East = 5.55745 +/- 1.16702e-05
};
