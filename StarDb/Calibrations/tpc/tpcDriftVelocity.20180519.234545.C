TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 139068
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55892; // +/- 5.59382e-06 cm/us All: East = -0.0423442 +/- 0.00321551
  row.laserDriftVelocityWest	 =   5.55892; // +/- 5.59382e-06 cm/us All: West = 0.209655 +/- 0.00103157
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55892 +/- 5.59382e-06
  return (TDataSet *)tableSet;// West = 5.5588 +/- 5.88379e-06 East = 5.5601 +/- 1.8041e-05
};
