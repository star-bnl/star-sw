TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 164015
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54868; // +/- 6.46447e-06 cm/us All: East = -0.322795 +/- 0.00575376
  row.laserDriftVelocityWest	 =   5.54868; // +/- 6.46447e-06 cm/us All: West = 0.211935 +/- 0.00116294
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54868 +/- 6.46447e-06
  return (TDataSet *)tableSet;// West = 5.54861 +/- 6.55355e-06 East = 5.55129 +/- 3.93408e-05
};
