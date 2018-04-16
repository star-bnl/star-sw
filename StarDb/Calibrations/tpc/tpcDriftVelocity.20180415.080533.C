TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 105009
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55246; // +/- 1.18361e-05 cm/us All: East = -6.09819 +/- 0.604223
  row.laserDriftVelocityWest	 =   5.55246; // +/- 1.18361e-05 cm/us All: West = -5.3743 +/- 0.00209544
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55246 +/- 1.18361e-05
  return (TDataSet *)tableSet;// West = 5.55246 +/- 1.18361e-05 East = 5.55759 +/- 1.0207
};
