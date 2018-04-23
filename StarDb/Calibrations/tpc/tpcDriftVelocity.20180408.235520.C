TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 98047
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55052; // +/- 1.16076e-05 cm/us All: East = 0.0131626 +/- 0.0028067
  row.laserDriftVelocityWest	 =   5.55052; // +/- 1.16076e-05 cm/us All: West = 0.373177 +/- 0.00314262
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55052 +/- 1.16076e-05
  return (TDataSet *)tableSet;// West = 5.54944 +/- 1.742e-05 East = 5.55138 +/- 1.55671e-05
};
