TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 103032
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55526; // +/- 1.95088e-05 cm/us All: East = 0.0751336 +/- 0.00432386
  row.laserDriftVelocityWest	 =   5.55526; // +/- 1.95088e-05 cm/us All: West = 0.455669 +/- 0.00713278
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55526 +/- 1.95088e-05
  return (TDataSet *)tableSet;// West = 5.55379 +/- 3.6435e-05 East = 5.55585 +/- 2.3099e-05
};
