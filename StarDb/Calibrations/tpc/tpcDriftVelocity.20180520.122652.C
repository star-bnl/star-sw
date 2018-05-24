TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 140021
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55807; // +/- 8.17615e-06 cm/us All: East = 0.00734282 +/- 0.00246013
  row.laserDriftVelocityWest	 =   5.55807; // +/- 8.17615e-06 cm/us All: West = 0.33132 +/- 0.000946943
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55807 +/- 8.17615e-06
  return (TDataSet *)tableSet;// West = 5.55694 +/- 1.00969e-05 East = 5.5602 +/- 1.39347e-05
};
