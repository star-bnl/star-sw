TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 79057
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55387; // +/- 9.99936e-06 cm/us All: East = 0.72909 +/- 0.0141063
  row.laserDriftVelocityWest	 =   5.55387; // +/- 9.99936e-06 cm/us All: West = 0.256026 +/- 0.00185664
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55387 +/- 9.99936e-06
  return (TDataSet *)tableSet;// West = 5.55392 +/- 1.0106e-05 East = 5.55137 +/- 6.90199e-05
};
