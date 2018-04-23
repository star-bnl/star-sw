TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 104036
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55332; // +/- 7.0496e-06 cm/us All: East = 0.0198909 +/- 0.0017532
  row.laserDriftVelocityWest	 =   5.55332; // +/- 7.0496e-06 cm/us All: West = 0.336189 +/- 0.001812
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55332 +/- 7.0496e-06
  return (TDataSet *)tableSet;// West = 5.55242 +/- 1.0053e-05 East = 5.55418 +/- 9.88834e-06
};
