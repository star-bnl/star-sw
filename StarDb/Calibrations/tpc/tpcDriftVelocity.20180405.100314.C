TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 95018
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52179; // +/- 4.269e-05 cm/us All: East = -0.602532 +/- 0.0637138
  row.laserDriftVelocityWest	 =   5.52179; // +/- 4.269e-05 cm/us All: West = 0.0108692 +/- 0.0133996
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52179 +/- 4.269e-05
  return (TDataSet *)tableSet;// West = 5.52129 +/- 4.58851e-05 East = 5.52496 +/- 0.000116441
};
