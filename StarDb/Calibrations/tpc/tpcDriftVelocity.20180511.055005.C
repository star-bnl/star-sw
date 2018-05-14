TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 131005
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54956; // +/- 8.97315e-06 cm/us All: East = 0.174137 +/- 0.00689106
  row.laserDriftVelocityWest	 =   5.54956; // +/- 8.97315e-06 cm/us All: West = 0.160678 +/- 0.00102173
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54956 +/- 8.97315e-06
  return (TDataSet *)tableSet;// West = 5.5495 +/- 9.28513e-06 East = 5.5503 +/- 3.49097e-05
};
