TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 90002
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52707; // +/- 1.13034e-05 cm/us All: East = -0.68061 +/- 0.00973161
  row.laserDriftVelocityWest	 =   5.52707; // +/- 1.13034e-05 cm/us All: West = 0.22053 +/- 0.00206479
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52707 +/- 1.13034e-05
  return (TDataSet *)tableSet;// West = 5.52686 +/- 1.15537e-05 East = 5.5318 +/- 5.46023e-05
};
