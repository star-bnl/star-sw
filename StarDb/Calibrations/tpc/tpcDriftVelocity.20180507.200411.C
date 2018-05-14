TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 127034
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.56833; // +/- 7.55994e-06 cm/us All: East = -0.0471779 +/- 0.00174093
  row.laserDriftVelocityWest	 =   5.56833; // +/- 7.55994e-06 cm/us All: West = 0.205179 +/- 0.00212469
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.56833 +/- 7.55994e-06
  return (TDataSet *)tableSet;// West = 5.56751 +/- 1.18974e-05 East = 5.56888 +/- 9.79066e-06
};
