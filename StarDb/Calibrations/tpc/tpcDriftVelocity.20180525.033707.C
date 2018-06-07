TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 144047
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55105; // +/- 1.39073e-05 cm/us All: East = -0.694313 +/- 0.00536791
  row.laserDriftVelocityWest	 =   5.55105; // +/- 1.39073e-05 cm/us All: West = 0.253668 +/- 0.00178541
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55105 +/- 1.39073e-05
  return (TDataSet *)tableSet;// West = 5.54973 +/- 1.5799e-05 East = 5.55559 +/- 2.93103e-05
};
