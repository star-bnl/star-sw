TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 106007
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55485; // +/- 1.35838e-05 cm/us All: East = -0.745024 +/- 0.00835804
  row.laserDriftVelocityWest	 =   5.55485; // +/- 1.35838e-05 cm/us All: West = 0.272145 +/- 0.00253923
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55485 +/- 1.35838e-05
  return (TDataSet *)tableSet;// West = 5.55436 +/- 1.42191e-05 East = 5.55999 +/- 4.59588e-05
};
