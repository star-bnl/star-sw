TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 110009
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55369; // +/- 5.28039e-06 cm/us All: East = 0.0119514 +/- 0.00218834
  row.laserDriftVelocityWest	 =   5.55369; // +/- 5.28039e-06 cm/us All: West = 0.210786 +/- 0.00103417
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55369 +/- 5.28039e-06
  return (TDataSet *)tableSet;// West = 5.55349 +/- 5.8406e-06 East = 5.55457 +/- 1.23559e-05
};
