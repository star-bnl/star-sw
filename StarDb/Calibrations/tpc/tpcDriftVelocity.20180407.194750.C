TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 97051
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53652; // +/- 1.28888e-05 cm/us All: East = 0.0351687 +/- 0.00336754
  row.laserDriftVelocityWest	 =   5.53652; // +/- 1.28888e-05 cm/us All: West = 0.287657 +/- 0.00323868
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53652 +/- 1.28888e-05
  return (TDataSet *)tableSet;// West = 5.5359 +/- 1.79371e-05 East = 5.53719 +/- 1.85325e-05
};
