TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 153051
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55254; // +/- 7.19433e-06 cm/us All: East = 0.145115 +/- 0.00281142
  row.laserDriftVelocityWest	 =   5.55254; // +/- 7.19433e-06 cm/us All: West = 0.0380769 +/- 0.00142395
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55254 +/- 7.19433e-06
  return (TDataSet *)tableSet;// West = 5.55266 +/- 8.07603e-06 East = 5.55207 +/- 1.58345e-05
};
