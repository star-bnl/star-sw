TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 125020
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5482; // +/- 5.07421e-06 cm/us All: East = -0.441738 +/- 0.00135124
  row.laserDriftVelocityWest	 =   5.5482; // +/- 5.07421e-06 cm/us All: West = -0.407834 +/- 0.00121759
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5482 +/- 5.07421e-06
  return (TDataSet *)tableSet;// West = 5.54812 +/- 6.80124e-06 East = 5.54831 +/- 7.62045e-06
};
