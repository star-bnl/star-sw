TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 83063
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53361; // +/- 1.02327e-05 cm/us All: East = -0.31741 +/- 0.00778828
  row.laserDriftVelocityWest	 =   5.53361; // +/- 1.02327e-05 cm/us All: West = 0.203668 +/- 0.00186848
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53361 +/- 1.02327e-05
  return (TDataSet *)tableSet;// West = 5.53346 +/- 1.05259e-05 East = 5.53629 +/- 4.3663e-05
};
