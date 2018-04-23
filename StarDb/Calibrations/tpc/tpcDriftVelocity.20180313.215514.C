TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 72030
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53842; // +/- 3.28185e-05 cm/us All: East = -0.633755 +/- 0.0189641
  row.laserDriftVelocityWest	 =   5.53842; // +/- 3.28185e-05 cm/us All: West = 0.24384 +/- 0.00471999
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53842 +/- 3.28185e-05
  return (TDataSet *)tableSet;// West = 5.53766 +/- 3.57216e-05 East = 5.54253 +/- 8.31092e-05
};
