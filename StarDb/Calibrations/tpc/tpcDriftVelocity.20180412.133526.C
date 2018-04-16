TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 102034
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54346; // +/- 1.72199e-05 cm/us All: East = -3.85477 +/- 0.00482313
  row.laserDriftVelocityWest	 =   5.54346; // +/- 1.72199e-05 cm/us All: West = -3.6866 +/- 0.00421607
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54346 +/- 1.72199e-05
  return (TDataSet *)tableSet;// West = 5.54306 +/- 2.32942e-05 East = 5.54393 +/- 2.55699e-05
};
