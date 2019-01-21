TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 21023
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55999; // +/- 1.06869e-05 cm/us All: East = -1.03817 +/- 0.00507136
  row.laserDriftVelocityWest	 =   5.55999; // +/- 1.06869e-05 cm/us All: West = -0.616967 +/- 0.00202908
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55999 +/- 1.06869e-05
  return (TDataSet *)tableSet;// West = 5.55968 +/- 1.15371e-05 East = 5.56185 +/- 2.83648e-05
};
