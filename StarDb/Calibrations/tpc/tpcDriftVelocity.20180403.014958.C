TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 92093
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52923; // +/- 9.75716e-06 cm/us All: East = -0.872938 +/- 0.0069294
  row.laserDriftVelocityWest	 =   5.52923; // +/- 9.75716e-06 cm/us All: West = 0.0199913 +/- 0.00180235
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52923 +/- 9.75716e-06
  return (TDataSet *)tableSet;// West = 5.52891 +/- 1.00843e-05 East = 5.53385 +/- 3.86217e-05
};
