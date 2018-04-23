TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 96055
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54475; // +/- 9.54805e-06 cm/us All: East = 0.0707112 +/- 0.00227043
  row.laserDriftVelocityWest	 =   5.54475; // +/- 9.54805e-06 cm/us All: West = 0.316971 +/- 0.00260426
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54475 +/- 9.54805e-06
  return (TDataSet *)tableSet;// West = 5.54396 +/- 1.46016e-05 East = 5.54534 +/- 1.26201e-05
};
