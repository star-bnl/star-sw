TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 156047
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55333; // +/- 6.32157e-06 cm/us All: East = 0.216502 +/- 0.002367
  row.laserDriftVelocityWest	 =   5.55333; // +/- 6.32157e-06 cm/us All: West = 0.149646 +/- 0.00128534
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55333 +/- 6.32157e-06
  return (TDataSet *)tableSet;// West = 5.55341 +/- 7.19296e-06 East = 5.55305 +/- 1.32503e-05
};
