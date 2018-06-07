TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 145020
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.57387; // +/- 0.000217644 cm/us All: East = -4.38769 +/- 0.199949
  row.laserDriftVelocityWest	 =   5.57387; // +/- 0.000217644 cm/us All: West = -3.92808 +/- 0.229681
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.57387 +/- 0.000217644
  return (TDataSet *)tableSet;// West = 5.5737 +/- 0.000232227 East = 5.57512 +/- 0.000624022
};
