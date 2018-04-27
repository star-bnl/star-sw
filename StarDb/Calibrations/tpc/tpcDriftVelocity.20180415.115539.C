TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 105019
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54296; // +/- 7.95287e-06 cm/us All: East = 0.141062 +/- 0.0018798
  row.laserDriftVelocityWest	 =   5.54296; // +/- 7.95287e-06 cm/us All: West = 0.286195 +/- 0.00218672
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54296 +/- 7.95287e-06
  return (TDataSet *)tableSet;// West = 5.54249 +/- 1.21427e-05 East = 5.54331 +/- 1.05243e-05
};
