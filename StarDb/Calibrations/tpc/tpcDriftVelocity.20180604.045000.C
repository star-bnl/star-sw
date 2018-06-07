TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 155004
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55423; // +/- 7.07075e-06 cm/us All: East = 0.0733025 +/- 0.00324854
  row.laserDriftVelocityWest	 =   5.55423; // +/- 7.07075e-06 cm/us All: West = 0.249316 +/- 0.00136457
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55423 +/- 7.07075e-06
  return (TDataSet *)tableSet;// West = 5.55409 +/- 7.6532e-06 East = 5.55502 +/- 1.84785e-05
};
