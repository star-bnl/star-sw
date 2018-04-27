TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 95028
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52291; // +/- 1.63195e-05 cm/us All: East = -0.56821 +/- 0.00641362
  row.laserDriftVelocityWest	 =   5.52291; // +/- 1.63195e-05 cm/us All: West = 0.292934 +/- 0.00332203
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52291 +/- 1.63195e-05
  return (TDataSet *)tableSet;// West = 5.52189 +/- 1.84019e-05 East = 5.52666 +/- 3.53172e-05
};
