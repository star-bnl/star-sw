TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 133048
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54712; // +/- 1.08384e-05 cm/us All: East = -0.207844 +/- 0.00509176
  row.laserDriftVelocityWest	 =   5.54712; // +/- 1.08384e-05 cm/us All: West = 0.223527 +/- 0.00212023
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54712 +/- 1.08384e-05
  return (TDataSet *)tableSet;// West = 5.54678 +/- 1.17789e-05 East = 5.54899 +/- 2.76804e-05
};
