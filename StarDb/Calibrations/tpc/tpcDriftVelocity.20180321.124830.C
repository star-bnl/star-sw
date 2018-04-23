TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 80038
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54951; // +/- 1.70314e-05 cm/us All: East = -1.05343 +/- 0.0146464
  row.laserDriftVelocityWest	 =   5.54951; // +/- 1.70314e-05 cm/us All: West = 0.225932 +/- 0.00321194
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54951 +/- 1.70314e-05
  return (TDataSet *)tableSet;// West = 5.54918 +/- 1.74538e-05 East = 5.55609 +/- 7.78825e-05
};
