TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 79012
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55565; // +/- 7.17135e-06 cm/us All: East = 1.89876 +/- 0.0052636
  row.laserDriftVelocityWest	 =   5.55565; // +/- 7.17135e-06 cm/us All: West = 1.92837 +/- 0.00131657
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55565 +/- 7.17135e-06
  return (TDataSet *)tableSet;// West = 5.55568 +/- 7.36543e-06 East = 5.55518 +/- 3.14465e-05
};
