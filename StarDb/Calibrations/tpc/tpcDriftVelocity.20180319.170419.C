TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 78015
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.56311; // +/- 0.000174655 cm/us All: East = -1.44088 +/- 45.2416
  row.laserDriftVelocityWest	 =   5.56311; // +/- 0.000174655 cm/us All: West = -3.83569 +/- 0.185888
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.56311 +/- 0.000174655
  return (TDataSet *)tableSet;// West = 5.56311 +/- 0.000174655 East = -999 +/- 999
};
