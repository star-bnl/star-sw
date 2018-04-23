TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 102044
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55379; // +/- 1.21962e-05 cm/us All: East = -0.892266 +/- 0.00872341
  row.laserDriftVelocityWest	 =   5.55379; // +/- 1.21962e-05 cm/us All: West = 0.23746 +/- 0.00226362
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55379 +/- 1.21962e-05
  return (TDataSet *)tableSet;// West = 5.5534 +/- 1.25868e-05 East = 5.55975 +/- 4.93423e-05
};
