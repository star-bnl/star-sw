TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 83010
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53436; // +/- 1.01598e-05 cm/us All: East = 1.17965 +/- 2.55418
  row.laserDriftVelocityWest	 =   5.53436; // +/- 1.01598e-05 cm/us All: West = 1.37761 +/- 0.00181797
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53436 +/- 1.01598e-05
  return (TDataSet *)tableSet;// West = 5.53436 +/- 1.01602e-05 East = 5.53658 +/- 0.00118209
};
