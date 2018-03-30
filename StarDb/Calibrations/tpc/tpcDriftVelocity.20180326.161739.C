TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 85026
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52897; // +/- 8.7384e-06 cm/us All: East = 2.00746 +/- 0.00294864
  row.laserDriftVelocityWest	 =   5.52897; // +/- 8.7384e-06 cm/us All: West = 2.48936 +/- 0.00186017
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52897 +/- 8.7384e-06
  return (TDataSet *)tableSet;// West = 5.52821 +/- 1.04132e-05 East = 5.53078 +/- 1.6067e-05
};
