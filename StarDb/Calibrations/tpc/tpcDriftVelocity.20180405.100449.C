TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 95019
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52156; // +/- 1.37896e-05 cm/us All: East = -0.810776 +/- 0.00735327
  row.laserDriftVelocityWest	 =   5.52156; // +/- 1.37896e-05 cm/us All: West = 0.290586 +/- 0.00263808
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52156 +/- 1.37896e-05
  return (TDataSet *)tableSet;// West = 5.52096 +/- 1.45413e-05 East = 5.52692 +/- 4.34518e-05
};
