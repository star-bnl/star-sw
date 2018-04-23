TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 97034
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54564; // +/- 8.54275e-06 cm/us All: East = 0.0866994 +/- 0.00223323
  row.laserDriftVelocityWest	 =   5.54564; // +/- 8.54275e-06 cm/us All: West = 0.25528 +/- 0.00211806
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54564 +/- 8.54275e-06
  return (TDataSet *)tableSet;// West = 5.54522 +/- 1.173e-05 East = 5.54611 +/- 1.24661e-05
};
