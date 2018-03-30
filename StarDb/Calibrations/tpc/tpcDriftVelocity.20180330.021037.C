TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 88066
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52707; // +/- 1.30832e-05 cm/us All: East = 1.74626 +/- 0.00861476
  row.laserDriftVelocityWest	 =   5.52707; // +/- 1.30832e-05 cm/us All: West = 2.77856 +/- 0.00244742
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52707 +/- 1.30832e-05
  return (TDataSet *)tableSet;// West = 5.52662 +/- 1.36261e-05 East = 5.53237 +/- 4.68179e-05
};
