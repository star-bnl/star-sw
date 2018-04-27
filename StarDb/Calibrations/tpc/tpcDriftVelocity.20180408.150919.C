TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 98028
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53934; // +/- 1.24858e-05 cm/us All: East = 0.116359 +/- 0.00310009
  row.laserDriftVelocityWest	 =   5.53934; // +/- 1.24858e-05 cm/us All: West = 0.357086 +/- 0.0032607
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53934 +/- 1.24858e-05
  return (TDataSet *)tableSet;// West = 5.5387 +/- 1.79172e-05 East = 5.53995 +/- 1.74088e-05
};
