TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 85001
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5208; // +/- 1.14967e-05 cm/us All: East = -0.242328 +/- 0.00991851
  row.laserDriftVelocityWest	 =   5.5208; // +/- 1.14967e-05 cm/us All: West = 0.209922 +/- 0.00212102
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5208 +/- 1.14967e-05
  return (TDataSet *)tableSet;// West = 5.52069 +/- 1.17689e-05 East = 5.52313 +/- 5.37607e-05
};
