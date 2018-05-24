TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 135031
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53703; // +/- 1.26832e-05 cm/us All: East = 0.0769786 +/- 0.00414317
  row.laserDriftVelocityWest	 =   5.53703; // +/- 1.26832e-05 cm/us All: West = 0.0486481 +/- 0.00280352
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53703 +/- 1.26832e-05
  return (TDataSet *)tableSet;// West = 5.53705 +/- 1.52273e-05 East = 5.53699 +/- 2.2919e-05
};
