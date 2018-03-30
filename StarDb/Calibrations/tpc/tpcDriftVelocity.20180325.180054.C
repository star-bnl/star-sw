TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 84038
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53192; // +/- 1.06928e-05 cm/us All: East = 1.388 +/- 0.00755677
  row.laserDriftVelocityWest	 =   5.53192; // +/- 1.06928e-05 cm/us All: West = 1.85298 +/- 0.00198958
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53192 +/- 1.06928e-05
  return (TDataSet *)tableSet;// West = 5.53173 +/- 1.11063e-05 East = 5.53431 +/- 3.95541e-05
};
