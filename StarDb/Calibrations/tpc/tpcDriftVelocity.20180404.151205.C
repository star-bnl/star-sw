TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 94043
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52036; // +/- 8.80982e-06 cm/us All: East = -0.0234683 +/- 0.00337033
  row.laserDriftVelocityWest	 =   5.52036; // +/- 8.80982e-06 cm/us All: West = 0.260521 +/- 0.00179716
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52036 +/- 8.80982e-06
  return (TDataSet *)tableSet;// West = 5.52001 +/- 9.96889e-06 East = 5.52157 +/- 1.88246e-05
};
