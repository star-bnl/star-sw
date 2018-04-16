TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 96026
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54184; // +/- 1.69713e-05 cm/us All: East = -3.98525 +/- 0.00528468
  row.laserDriftVelocityWest	 =   5.54184; // +/- 1.69713e-05 cm/us All: West = -3.15705 +/- 0.00390389
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54184 +/- 1.69713e-05
  return (TDataSet *)tableSet;// West = 5.54013 +/- 2.18083e-05 East = 5.54447 +/- 2.70239e-05
};
