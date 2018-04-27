TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 89034
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.51566; // +/- 1.89592e-05 cm/us All: East = -0.657962 +/- 0.0162331
  row.laserDriftVelocityWest	 =   5.51566; // +/- 1.89592e-05 cm/us All: West = 0.298877 +/- 0.00358056
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.51566 +/- 1.89592e-05
  return (TDataSet *)tableSet;// West = 5.51531 +/- 1.96246e-05 East = 5.52057 +/- 7.34335e-05
};
