TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 155023
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55561; // +/- 7.8515e-06 cm/us All: East = -0.549917 +/- 0.00511949
  row.laserDriftVelocityWest	 =   5.55561; // +/- 7.8515e-06 cm/us All: West = -0.134989 +/- 0.00145072
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55561 +/- 7.8515e-06
  return (TDataSet *)tableSet;// West = 5.55544 +/- 8.19046e-06 East = 5.55753 +/- 2.75778e-05
};
