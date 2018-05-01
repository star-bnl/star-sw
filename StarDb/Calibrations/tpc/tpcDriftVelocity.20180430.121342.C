TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 120029
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53273; // +/- 3.55721e-05 cm/us All: East = -0.636241 +/- 1.35319
  row.laserDriftVelocityWest	 =   5.53273; // +/- 3.55721e-05 cm/us All: West = 0.311995 +/- 0.00668708
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53273 +/- 3.55721e-05
  return (TDataSet *)tableSet;// West = 5.53273 +/- 3.55759e-05 East = 5.53921 +/- 0.00244518
};
