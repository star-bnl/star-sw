TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 93030
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53022; // +/- 9.37822e-06 cm/us All: East = -1.14919 +/- 0.00616652
  row.laserDriftVelocityWest	 =   5.53022; // +/- 9.37822e-06 cm/us All: West = -0.148467 +/- 0.00173587
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53022 +/- 9.37822e-06
  return (TDataSet *)tableSet;// West = 5.52984 +/- 9.72667e-06 East = 5.53533 +/- 3.53543e-05
};
