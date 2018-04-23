TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 95017
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52191; // +/- 2.47553e-05 cm/us All: East = -0.0219035 +/- 0.0186625
  row.laserDriftVelocityWest	 =   5.52191; // +/- 2.47553e-05 cm/us All: West = 0.21828 +/- 0.00567953
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52191 +/- 2.47553e-05
  return (TDataSet *)tableSet;// West = 5.52157 +/- 2.80839e-05 East = 5.5231 +/- 5.24223e-05
};
