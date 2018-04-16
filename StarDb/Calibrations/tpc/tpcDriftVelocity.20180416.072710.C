TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 106007
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55483; // +/- 1.3592e-05 cm/us All: East = -0.886115 +/- 0.00857391
  row.laserDriftVelocityWest	 =   5.55483; // +/- 1.3592e-05 cm/us All: West = 0.127713 +/- 0.00253752
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55483 +/- 1.3592e-05
  return (TDataSet *)tableSet;// West = 5.55434 +/- 1.42287e-05 East = 5.56 +/- 4.59497e-05
};
