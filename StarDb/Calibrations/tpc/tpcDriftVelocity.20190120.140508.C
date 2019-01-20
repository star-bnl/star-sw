TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 20031
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55528; // +/- 8.51739e-06 cm/us All: East = 0.589456 +/- 0.00362114
  row.laserDriftVelocityWest	 =   5.55528; // +/- 8.51739e-06 cm/us All: West = 0.590554 +/- 0.00168989
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55528 +/- 8.51739e-06
  return (TDataSet *)tableSet;// West = 5.55625 +/- 9.53047e-06 East = 5.5514 +/- 1.8984e-05
};
