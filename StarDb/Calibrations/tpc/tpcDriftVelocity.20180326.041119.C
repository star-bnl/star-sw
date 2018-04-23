TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 85001
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53079; // +/- 1.16095e-05 cm/us All: East = -0.200968 +/- 0.0110138
  row.laserDriftVelocityWest	 =   5.53079; // +/- 1.16095e-05 cm/us All: West = 0.215095 +/- 0.00211018
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53079 +/- 1.16095e-05
  return (TDataSet *)tableSet;// West = 5.53069 +/- 1.18562e-05 East = 5.533 +/- 5.72023e-05
};
