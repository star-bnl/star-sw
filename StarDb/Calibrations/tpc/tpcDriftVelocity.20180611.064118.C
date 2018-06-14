TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 162009
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54761; // +/- 9.9638e-06 cm/us All: East = -0.275175 +/- 0.00456942
  row.laserDriftVelocityWest	 =   5.54761; // +/- 9.9638e-06 cm/us All: West = 0.265248 +/- 0.0019359
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54761 +/- 9.9638e-06
  return (TDataSet *)tableSet;// West = 5.54716 +/- 1.08239e-05 East = 5.55011 +/- 2.55054e-05
};
