TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 167051
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5469; // +/- 5.69615e-06 cm/us All: East = -0.410956 +/- 0.00230945
  row.laserDriftVelocityWest	 =   5.5469; // +/- 5.69615e-06 cm/us All: West = 0.190904 +/- 0.00113053
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5469 +/- 5.69615e-06
  return (TDataSet *)tableSet;// West = 5.54627 +/- 6.32917e-06 East = 5.54959 +/- 1.30668e-05
};
