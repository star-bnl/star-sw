TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 75055
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53337; // +/- 9.9818e-06 cm/us All: East = 0.857745 +/- 3.5374
  row.laserDriftVelocityWest	 =   5.53337; // +/- 9.9818e-06 cm/us All: West = 2.05528 +/- 0.00178153
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53337 +/- 9.9818e-06
  return (TDataSet *)tableSet;// West = 5.53338 +/- 9.99157e-06 East = 5.53246 +/- 0.000225791
};
