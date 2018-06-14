TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 162018
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54802; // +/- 1.065e-05 cm/us All: East = -0.640648 +/- 0.00452685
  row.laserDriftVelocityWest	 =   5.54802; // +/- 1.065e-05 cm/us All: West = 0.241638 +/- 0.00211611
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54802 +/- 1.065e-05
  return (TDataSet *)tableSet;// West = 5.54712 +/- 1.17871e-05 East = 5.55203 +/- 2.48527e-05
};
