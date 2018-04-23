TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 99018
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55123; // +/- 1.50247e-05 cm/us All: East = 0.0194456 +/- 0.00341031
  row.laserDriftVelocityWest	 =   5.55123; // +/- 1.50247e-05 cm/us All: West = 0.407859 +/- 0.00450397
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55123 +/- 1.50247e-05
  return (TDataSet *)tableSet;// West = 5.54997 +/- 2.43251e-05 East = 5.55201 +/- 1.91046e-05
};
