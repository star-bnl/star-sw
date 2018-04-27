TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 85043
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.51904; // +/- 7.54911e-06 cm/us All: East = -0.293927 +/- 0.00551217
  row.laserDriftVelocityWest	 =   5.51904; // +/- 7.54911e-06 cm/us All: West = 0.221092 +/- 0.00140224
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.51904 +/- 7.54911e-06
  return (TDataSet *)tableSet;// West = 5.51887 +/- 7.78138e-06 East = 5.52166 +/- 3.11301e-05
};
