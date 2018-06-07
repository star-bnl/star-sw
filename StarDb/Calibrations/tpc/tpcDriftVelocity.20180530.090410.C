TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 150014
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54929; // +/- 6.74148e-06 cm/us All: East = 0.218892 +/- 0.00463462
  row.laserDriftVelocityWest	 =   5.54929; // +/- 6.74148e-06 cm/us All: West = 0.335705 +/- 0.00124843
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54929 +/- 6.74148e-06
  return (TDataSet *)tableSet;// West = 5.54925 +/- 6.99496e-06 East = 5.54978 +/- 2.52718e-05
};
