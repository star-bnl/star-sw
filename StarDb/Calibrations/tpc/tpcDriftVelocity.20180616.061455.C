TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 167004
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54741; // +/- 6.11756e-06 cm/us All: East = -0.310621 +/- 0.00234252
  row.laserDriftVelocityWest	 =   5.54741; // +/- 6.11756e-06 cm/us All: West = 0.153749 +/- 0.00120421
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54741 +/- 6.11756e-06
  return (TDataSet *)tableSet;// West = 5.54691 +/- 6.82446e-06 East = 5.54944 +/- 1.38028e-05
};
