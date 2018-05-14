TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 133031
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54741; // +/- 9.59095e-06 cm/us All: East = -0.435644 +/- 0.00379294
  row.laserDriftVelocityWest	 =   5.54741; // +/- 9.59095e-06 cm/us All: West = 0.187646 +/- 0.00192784
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54741 +/- 9.59095e-06
  return (TDataSet *)tableSet;// West = 5.54671 +/- 1.0745e-05 East = 5.55013 +/- 2.12727e-05
};
