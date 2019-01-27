TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 26009
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54976; // +/- 2.54483e-05 cm/us All: East = 1.45044 +/- 0.00653902
  row.laserDriftVelocityWest	 =   5.54976; // +/- 2.54483e-05 cm/us All: West = 2.13524 +/- 0.00704579
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54976 +/- 2.54483e-05
  return (TDataSet *)tableSet;// West = 5.54788 +/- 3.70813e-05 East = 5.55144 +/- 3.49884e-05
};
