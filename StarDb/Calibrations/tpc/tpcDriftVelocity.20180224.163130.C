TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 55025
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.50527; // +/- 1.35779e-05 cm/us All: East = -0.0627201 +/- 0.00462773
  row.laserDriftVelocityWest	 =   5.50527; // +/- 1.35779e-05 cm/us All: West = 0.245978 +/- 0.00299106
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.50527 +/- 1.35779e-05
  return (TDataSet *)tableSet;// West = 5.50489 +/- 1.60403e-05 East = 5.50625 +/- 2.55028e-05
};
