TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 81005
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55003; // +/- 9.15905e-06 cm/us All: East = -2.34715 +/- 40.5085
  row.laserDriftVelocityWest	 =   5.55003; // +/- 9.15905e-06 cm/us All: West = -1.44856 +/- 0.00162693
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55003 +/- 9.15905e-06
  return (TDataSet *)tableSet;// West = 5.55003 +/- 9.15905e-06 East = 5.55961 +/- 0.610101
};
