TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 77018
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54572; // +/- 1.32193e-05 cm/us All: East = -1.06038 +/- 17.174
  row.laserDriftVelocityWest	 =   5.54572; // +/- 1.32193e-05 cm/us All: West = 2.03282 +/- 0.00235241
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54572 +/- 1.32193e-05
  return (TDataSet *)tableSet;// West = 5.54572 +/- 1.32193e-05 East = 5.56156 +/- 0.0257266
};
