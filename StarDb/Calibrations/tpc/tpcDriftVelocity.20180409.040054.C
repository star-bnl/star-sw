TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 99001
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54091; // +/- 1.23706e-05 cm/us All: East = 0.0156819 +/- 0.00278818
  row.laserDriftVelocityWest	 =   5.54091; // +/- 1.23706e-05 cm/us All: West = 0.356383 +/- 0.00373557
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54091 +/- 1.23706e-05
  return (TDataSet *)tableSet;// West = 5.5397 +/- 2.06801e-05 East = 5.54159 +/- 1.54371e-05
};
