TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 160028
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54903; // +/- 6.81552e-06 cm/us All: East = 0.251189 +/- 0.00260441
  row.laserDriftVelocityWest	 =   5.54903; // +/- 6.81552e-06 cm/us All: West = 0.170581 +/- 0.00136566
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54903 +/- 6.81552e-06
  return (TDataSet *)tableSet;// West = 5.54912 +/- 7.68799e-06 East = 5.54869 +/- 1.47299e-05
};
