TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 107051
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55449; // +/- 2.32608e-05 cm/us All: East = 3.96124 +/- 14.9807
  row.laserDriftVelocityWest	 =   5.55449; // +/- 2.32608e-05 cm/us All: West = 0.188527 +/- 0.00420027
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55449 +/- 2.32608e-05
  return (TDataSet *)tableSet;// West = 5.55449 +/- 2.32608e-05 East = 5.42688 +/- 0.114151
};
