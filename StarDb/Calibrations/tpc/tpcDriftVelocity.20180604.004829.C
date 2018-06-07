TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 154056
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55367; // +/- 1.59359e-05 cm/us All: East = -0.116281 +/- 0.00795436
  row.laserDriftVelocityWest	 =   5.55367; // +/- 1.59359e-05 cm/us All: West = 0.0596162 +/- 0.00302894
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55367 +/- 1.59359e-05
  return (TDataSet *)tableSet;// West = 5.55356 +/- 1.76002e-05 East = 5.55416 +/- 3.75423e-05
};
