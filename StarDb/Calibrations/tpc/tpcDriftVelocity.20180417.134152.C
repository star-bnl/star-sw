TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 107025
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55546; // +/- 1.36344e-05 cm/us All: East = 0.0626047 +/- 0.00333065
  row.laserDriftVelocityWest	 =   5.55546; // +/- 1.36344e-05 cm/us All: West = 0.396037 +/- 0.00372096
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55546 +/- 1.36344e-05
  return (TDataSet *)tableSet;// West = 5.55442 +/- 2.0485e-05 East = 5.55629 +/- 1.82685e-05
};
