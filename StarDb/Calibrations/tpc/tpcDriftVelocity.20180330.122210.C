TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 89019
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52671; // +/- 1.87005e-05 cm/us All: East = 1.75916 +/- 0.00889604
  row.laserDriftVelocityWest	 =   5.52671; // +/- 1.87005e-05 cm/us All: West = 2.94935 +/- 0.00365282
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52671 +/- 1.87005e-05
  return (TDataSet *)tableSet;// West = 5.52568 +/- 2.03646e-05 East = 5.5322 +/- 4.72332e-05
};
