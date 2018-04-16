TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 96055
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54469; // +/- 9.67947e-06 cm/us All: East = -4.08878 +/- 0.00232195
  row.laserDriftVelocityWest	 =   5.54469; // +/- 9.67947e-06 cm/us All: West = -3.84236 +/- 0.00257711
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54469 +/- 9.67947e-06
  return (TDataSet *)tableSet;// West = 5.54393 +/- 1.45859e-05 East = 5.5453 +/- 1.29392e-05
};
