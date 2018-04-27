TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 106007
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54487; // +/- 1.34837e-05 cm/us All: East = -0.882815 +/- 0.00845889
  row.laserDriftVelocityWest	 =   5.54487; // +/- 1.34837e-05 cm/us All: West = 0.115249 +/- 0.00254365
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54487 +/- 1.34837e-05
  return (TDataSet *)tableSet;// West = 5.54442 +/- 1.40628e-05 East = 5.54996 +/- 4.7475e-05
};
