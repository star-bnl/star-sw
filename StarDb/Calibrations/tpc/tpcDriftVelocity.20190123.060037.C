TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 23002
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5634; // +/- 9.99855e-06 cm/us All: East = -0.850156 +/- 0.00710944
  row.laserDriftVelocityWest	 =   5.5634; // +/- 9.99855e-06 cm/us All: West = 0.122866 +/- 0.0018305
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5634 +/- 9.99855e-06
  return (TDataSet *)tableSet;// West = 5.56306 +/- 1.03379e-05 East = 5.5684 +/- 3.93484e-05
};
