TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 149051
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55006; // +/- 5.80465e-06 cm/us All: East = -0.279795 +/- 0.00544317
  row.laserDriftVelocityWest	 =   5.55006; // +/- 5.80465e-06 cm/us All: West = 0.203086 +/- 0.00104858
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55006 +/- 5.80465e-06
  return (TDataSet *)tableSet;// West = 5.54997 +/- 5.89967e-06 East = 5.5526 +/- 3.24732e-05
};
