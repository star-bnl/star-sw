TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 130077
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55172; // +/- 4.67336e-06 cm/us All: East = 0.131359 +/- 0.00199173
  row.laserDriftVelocityWest	 =   5.55172; // +/- 4.67336e-06 cm/us All: West = 0.124884 +/- 0.000901429
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55172 +/- 4.67336e-06
  return (TDataSet *)tableSet;// West = 5.55173 +/- 5.13302e-06 East = 5.55169 +/- 1.12987e-05
};
