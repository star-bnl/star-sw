TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 104024
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55409; // +/- 7.74494e-06 cm/us All: East = 0.0993929 +/- 0.00179852
  row.laserDriftVelocityWest	 =   5.55409; // +/- 7.74494e-06 cm/us All: West = 0.311065 +/- 0.00212644
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55409 +/- 7.74494e-06
  return (TDataSet *)tableSet;// West = 5.5534 +/- 1.19928e-05 East = 5.55459 +/- 1.01439e-05
};
