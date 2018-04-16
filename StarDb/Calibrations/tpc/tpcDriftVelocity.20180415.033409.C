TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 104047
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54281; // +/- 9.55359e-06 cm/us All: East = -4.17714 +/- 0.00431411
  row.laserDriftVelocityWest	 =   5.54281; // +/- 9.55359e-06 cm/us All: West = -3.54703 +/- 0.00185378
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54281 +/- 9.55359e-06
  return (TDataSet *)tableSet;// West = 5.54228 +/- 1.03822e-05 East = 5.54573 +/- 2.44038e-05
};
