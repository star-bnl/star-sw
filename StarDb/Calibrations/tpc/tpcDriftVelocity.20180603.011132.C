TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 153059
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55249; // +/- 5.3876e-06 cm/us All: East = 0.31298 +/- 0.00187821
  row.laserDriftVelocityWest	 =   5.55249; // +/- 5.3876e-06 cm/us All: West = 0.129001 +/- 0.00111725
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55249 +/- 5.3876e-06
  return (TDataSet *)tableSet;// West = 5.55276 +/- 6.3023e-06 East = 5.55174 +/- 1.03837e-05
};
