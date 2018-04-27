TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 110028
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55402; // +/- 8.66077e-05 cm/us All: East = 0.0113488 +/- 0.214061
  row.laserDriftVelocityWest	 =   5.55402; // +/- 8.66077e-05 cm/us All: West = 0.253826 +/- 0.0599318
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55402 +/- 8.66077e-05
  return (TDataSet *)tableSet;// West = 5.5537 +/- 0.00012146 East = 5.55435 +/- 0.000123529
};
