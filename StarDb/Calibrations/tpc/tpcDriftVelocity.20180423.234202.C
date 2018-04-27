TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 113029
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5413; // +/- 6.8677e-06 cm/us All: East = -0.669325 +/- 0.00418312
  row.laserDriftVelocityWest	 =   5.5413; // +/- 6.8677e-06 cm/us All: West = 0.196104 +/- 0.00127769
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5413 +/- 6.8677e-06
  return (TDataSet *)tableSet;// West = 5.54091 +/- 7.16826e-06 East = 5.54571 +/- 2.39681e-05
};
