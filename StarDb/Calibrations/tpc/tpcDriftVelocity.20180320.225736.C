TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 79046
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5665; // +/- 8.94944e-06 cm/us All: East = 0.432233 +/- 0.0137408
  row.laserDriftVelocityWest	 =   5.5665; // +/- 8.94944e-06 cm/us All: West = 0.170604 +/- 0.00159482
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5665 +/- 8.94944e-06
  return (TDataSet *)tableSet;// West = 5.56653 +/- 9.02898e-06 East = 5.56485 +/- 6.75719e-05
};
