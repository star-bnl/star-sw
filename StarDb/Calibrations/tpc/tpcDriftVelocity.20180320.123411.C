TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 79023
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.56577; // +/- 7.69568e-06 cm/us All: East = 0.156115 +/- 0.00656509
  row.laserDriftVelocityWest	 =   5.56577; // +/- 7.69568e-06 cm/us All: West = 0.175254 +/- 0.0014069
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.56577 +/- 7.69568e-06
  return (TDataSet *)tableSet;// West = 5.56576 +/- 7.88311e-06 East = 5.56611 +/- 3.55019e-05
};
