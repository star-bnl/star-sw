TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 151067
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54975; // +/- 6.67237e-06 cm/us All: East = -0.0887947 +/- 0.00278102
  row.laserDriftVelocityWest	 =   5.54975; // +/- 6.67237e-06 cm/us All: West = 0.279573 +/- 0.00131878
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54975 +/- 6.67237e-06
  return (TDataSet *)tableSet;// West = 5.54937 +/- 7.39115e-06 East = 5.55146 +/- 1.55113e-05
};
