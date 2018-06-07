TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 154007
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55287; // +/- 5.87723e-06 cm/us All: East = 0.179036 +/- 0.0030804
  row.laserDriftVelocityWest	 =   5.55287; // +/- 5.87723e-06 cm/us All: West = 0.17457 +/- 0.00110735
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55287 +/- 5.87723e-06
  return (TDataSet *)tableSet;// West = 5.55288 +/- 6.25612e-06 East = 5.55286 +/- 1.71488e-05
};
