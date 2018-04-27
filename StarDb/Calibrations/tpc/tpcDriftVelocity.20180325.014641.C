TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 83063
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52364; // +/- 9.78966e-06 cm/us All: East = 1.43617 +/- 0.00603371
  row.laserDriftVelocityWest	 =   5.52364; // +/- 9.78966e-06 cm/us All: West = 2.04361 +/- 0.00183299
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52364 +/- 9.78966e-06
  return (TDataSet *)tableSet;// West = 5.52336 +/- 1.02274e-05 East = 5.52665 +/- 3.38242e-05
};
