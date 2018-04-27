TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 84038
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52193; // +/- 1.04712e-05 cm/us All: East = 1.48328 +/- 0.00631589
  row.laserDriftVelocityWest	 =   5.52193; // +/- 1.04712e-05 cm/us All: West = 2.04847 +/- 0.00195665
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52193 +/- 1.04712e-05
  return (TDataSet *)tableSet;// West = 5.52164 +/- 1.09967e-05 East = 5.52477 +/- 3.42835e-05
};
