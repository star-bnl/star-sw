TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 97063
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54682; // +/- 8.00008e-06 cm/us All: East = 0.149912 +/- 0.00180715
  row.laserDriftVelocityWest	 =   5.54682; // +/- 8.00008e-06 cm/us All: West = 0.251085 +/- 0.00236874
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54682 +/- 8.00008e-06
  return (TDataSet *)tableSet;// West = 5.54645 +/- 1.32763e-05 East = 5.54704 +/- 1.00245e-05
};
