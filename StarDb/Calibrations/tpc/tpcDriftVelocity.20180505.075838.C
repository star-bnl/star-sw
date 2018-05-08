TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 125008
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54552; // +/- 1.32229e-05 cm/us All: East = -0.1632 +/- 0.00469769
  row.laserDriftVelocityWest	 =   5.54552; // +/- 1.32229e-05 cm/us All: West = 0.167262 +/- 0.00289376
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54552 +/- 1.32229e-05
  return (TDataSet *)tableSet;// West = 5.54516 +/- 1.54415e-05 East = 5.54649 +/- 2.56039e-05
};
