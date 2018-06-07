TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 149031
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54986; // +/- 8.91453e-06 cm/us All: East = -0.689412 +/- 0.00390656
  row.laserDriftVelocityWest	 =   5.54986; // +/- 8.91453e-06 cm/us All: West = 0.0450013 +/- 0.00102191
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54986 +/- 8.91453e-06
  return (TDataSet *)tableSet;// West = 5.54899 +/- 9.78915e-06 East = 5.55406 +/- 2.1576e-05
};
