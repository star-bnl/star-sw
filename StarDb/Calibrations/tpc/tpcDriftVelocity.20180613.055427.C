TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 164005
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54859; // +/- 5.72958e-06 cm/us All: East = -0.41698 +/- 0.00645485
  row.laserDriftVelocityWest	 =   5.54859; // +/- 5.72958e-06 cm/us All: West = 0.172852 +/- 0.00105258
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54859 +/- 5.72958e-06
  return (TDataSet *)tableSet;// West = 5.54838 +/- 5.91608e-06 East = 5.55172 +/- 2.3e-05
};
