TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 134016
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54746; // +/- 1.59066e-05 cm/us All: East = -0.415162 +/- 0.00726723
  row.laserDriftVelocityWest	 =   5.54746; // +/- 1.59066e-05 cm/us All: West = 0.500001 +/- 0.00230834
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54746 +/- 1.59066e-05
  return (TDataSet *)tableSet;// West = 5.54667 +/- 1.73044e-05 East = 5.55178 +/- 4.03985e-05
};
