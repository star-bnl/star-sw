TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 112040
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54256; // +/- 7.79862e-06 cm/us All: East = -0.107168 +/- 0.00305846
  row.laserDriftVelocityWest	 =   5.54256; // +/- 7.79862e-06 cm/us All: West = 0.242141 +/- 0.00157476
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54256 +/- 7.79862e-06
  return (TDataSet *)tableSet;// West = 5.54216 +/- 8.80867e-06 East = 5.54399 +/- 1.67729e-05
};
