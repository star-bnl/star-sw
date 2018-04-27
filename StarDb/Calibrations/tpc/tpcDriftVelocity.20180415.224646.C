TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 105045
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54396; // +/- 8.52531e-06 cm/us All: East = -0.0536868 +/- 0.00192671
  row.laserDriftVelocityWest	 =   5.54396; // +/- 8.52531e-06 cm/us All: West = 0.162246 +/- 0.00250734
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54396 +/- 8.52531e-06
  return (TDataSet *)tableSet;// West = 5.54322 +/- 1.39667e-05 East = 5.54439 +/- 1.0763e-05
};
