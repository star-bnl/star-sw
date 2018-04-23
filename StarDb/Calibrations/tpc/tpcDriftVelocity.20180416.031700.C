TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 105054
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55457; // +/- 9.05557e-06 cm/us All: East = 0.0500201 +/- 0.00220837
  row.laserDriftVelocityWest	 =   5.55457; // +/- 9.05557e-06 cm/us All: West = 0.303767 +/- 0.00240658
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55457 +/- 9.05557e-06
  return (TDataSet *)tableSet;// West = 5.55383 +/- 1.33551e-05 East = 5.55519 +/- 1.23204e-05
};
