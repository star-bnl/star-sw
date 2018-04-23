TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 97024
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54538; // +/- 7.77799e-06 cm/us All: East = 0.0236705 +/- 0.0020978
  row.laserDriftVelocityWest	 =   5.54538; // +/- 7.77799e-06 cm/us All: West = 0.289574 +/- 0.00183146
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54538 +/- 7.77799e-06
  return (TDataSet *)tableSet;// West = 5.54475 +/- 1.0362e-05 East = 5.54618 +/- 1.17719e-05
};
