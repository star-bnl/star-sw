TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 77026
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54644; // +/- 1.70641e-05 cm/us All: East = -999 +/- 999
  row.laserDriftVelocityWest	 =   5.54644; // +/- 1.70641e-05 cm/us All: West = 2.10971 +/- 0.00309459
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54644 +/- 1.70641e-05
  return (TDataSet *)tableSet;// West = 5.54644 +/- 1.70641e-05 East = -999 +/- 999
};
