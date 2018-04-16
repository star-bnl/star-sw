TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 96045
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5444; // +/- 1.48834e-05 cm/us All: East = -4.19692 +/- 0.00363102
  row.laserDriftVelocityWest	 =   5.5444; // +/- 1.48834e-05 cm/us All: West = -3.63067 +/- 0.00395259
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5444 +/- 1.48834e-05
  return (TDataSet *)tableSet;// West = 5.54271 +/- 2.17212e-05 East = 5.54589 +/- 2.04344e-05
};
