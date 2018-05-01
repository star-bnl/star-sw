TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 66043
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53123; // +/- 5.56386e-06 cm/us All: East = -0.094277 +/- 0.00137057
  row.laserDriftVelocityWest	 =   5.53123; // +/- 5.56386e-06 cm/us All: West = 0.452141 +/- 0.00145799
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53123 +/- 5.56386e-06
  return (TDataSet *)tableSet;// West = 5.52964 +/- 8.12009e-06 East = 5.53265 +/- 7.63891e-06
};
