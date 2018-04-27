TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 82021
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52746; // +/- 1.36414e-05 cm/us All: East = 1.02825 +/- 0.555534
  row.laserDriftVelocityWest	 =   5.52746; // +/- 1.36414e-05 cm/us All: West = 2.00569 +/- 0.00244964
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52746 +/- 1.36414e-05
  return (TDataSet *)tableSet;// West = 5.52746 +/- 1.36454e-05 East = 5.53166 +/- 0.000569475
};
