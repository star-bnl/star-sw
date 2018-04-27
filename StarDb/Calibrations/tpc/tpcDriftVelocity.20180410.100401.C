TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 100013
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54352; // +/- 2.10571e-05 cm/us All: East = 0.0295031 +/- 0.0039689
  row.laserDriftVelocityWest	 =   5.54352; // +/- 2.10571e-05 cm/us All: West = 1.15479 +/- 0.0905889
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54352 +/- 2.10571e-05
  return (TDataSet *)tableSet;// West = 5.53896 +/- 0.000113257 East = 5.54368 +/- 2.14307e-05
};
