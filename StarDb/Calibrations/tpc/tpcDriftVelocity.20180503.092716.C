TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 123011
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53129; // +/- 8.32461e-06 cm/us All: East = -0.286825 +/- 0.00233212
  row.laserDriftVelocityWest	 =   5.53129; // +/- 8.32461e-06 cm/us All: West = 0.263955 +/- 0.0019538
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53129 +/- 8.32461e-06
  return (TDataSet *)tableSet;// West = 5.53007 +/- 1.08264e-05 East = 5.53305 +/- 1.30205e-05
};
