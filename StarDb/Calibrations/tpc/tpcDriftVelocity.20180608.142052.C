TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 159024
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55011; // +/- 5.08652e-06 cm/us All: East = 0.233393 +/- 0.0020686
  row.laserDriftVelocityWest	 =   5.55011; // +/- 5.08652e-06 cm/us All: West = 0.177117 +/- 0.00100421
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55011 +/- 5.08652e-06
  return (TDataSet *)tableSet;// West = 5.55017 +/- 5.65521e-06 East = 5.54986 +/- 1.16385e-05
};
