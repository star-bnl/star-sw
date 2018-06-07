TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 154040
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55292; // +/- 0.000305358 cm/us All: East = 0.0291202 +/- 0.272311
  row.laserDriftVelocityWest	 =   5.55292; // +/- 0.000305358 cm/us All: West = 0.090652 +/- 0.188165
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55292 +/- 0.000305358
  return (TDataSet *)tableSet;// West = 5.55278 +/- 0.000354384 East = 5.55335 +/- 0.000601708
};
