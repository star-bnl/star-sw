TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 132020
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54795; // +/- 6.70008e-06 cm/us All: East = -0.0131063 +/- 0.00203139
  row.laserDriftVelocityWest	 =   5.54795; // +/- 6.70008e-06 cm/us All: West = 0.106065 +/- 0.00147588
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54795 +/- 6.70008e-06
  return (TDataSet *)tableSet;// West = 5.54772 +/- 8.29606e-06 East = 5.5484 +/- 1.13618e-05
};
