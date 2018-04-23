TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 93042
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5305; // +/- 9.08796e-06 cm/us All: East = -0.712787 +/- 0.00633478
  row.laserDriftVelocityWest	 =   5.5305; // +/- 9.08796e-06 cm/us All: West = 0.240217 +/- 0.00168545
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5305 +/- 9.08796e-06
  return (TDataSet *)tableSet;// West = 5.53018 +/- 9.38536e-06 East = 5.53543 +/- 3.63894e-05
};
