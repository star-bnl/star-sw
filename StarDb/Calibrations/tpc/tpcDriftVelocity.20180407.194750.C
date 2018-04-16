TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 97051
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5465; // +/- 1.33693e-05 cm/us All: East = -4.46173 +/- 0.00358929
  row.laserDriftVelocityWest	 =   5.5465; // +/- 1.33693e-05 cm/us All: West = -4.18369 +/- 0.00323928
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5465 +/- 1.33693e-05
  return (TDataSet *)tableSet;// West = 5.54581 +/- 1.80128e-05 East = 5.54734 +/- 1.99494e-05
};
