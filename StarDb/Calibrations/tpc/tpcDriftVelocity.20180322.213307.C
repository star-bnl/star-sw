TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 81050
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54413; // +/- 1.31317e-05 cm/us All: East = 0.146556 +/- 0.00899424
  row.laserDriftVelocityWest	 =   5.54413; // +/- 1.31317e-05 cm/us All: West = 0.209705 +/- 0.00243806
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54413 +/- 1.31317e-05
  return (TDataSet *)tableSet;// West = 5.5441 +/- 1.36828e-05 East = 5.54447 +/- 4.67429e-05
};
