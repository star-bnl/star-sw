TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 90038
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5281; // +/- 9.37191e-06 cm/us All: East = -0.371543 +/- 0.00385189
  row.laserDriftVelocityWest	 =   5.5281; // +/- 9.37191e-06 cm/us All: West = 0.340107 +/- 0.00187228
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5281 +/- 9.37191e-06
  return (TDataSet *)tableSet;// West = 5.52737 +/- 1.03879e-05 East = 5.53132 +/- 2.17276e-05
};
