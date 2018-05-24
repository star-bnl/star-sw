TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 139004
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5529; // +/- 6.02687e-06 cm/us All: East = 0.419839 +/- 0.00532541
  row.laserDriftVelocityWest	 =   5.5529; // +/- 6.02687e-06 cm/us All: West = 0.147292 +/- 0.00108643
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5529 +/- 6.02687e-06
  return (TDataSet *)tableSet;// West = 5.55296 +/- 6.16448e-06 East = 5.55157 +/- 2.8684e-05
};
