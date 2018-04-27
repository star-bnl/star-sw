TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 115011
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54256; // +/- 6.41529e-06 cm/us All: East = -0.717449 +/- 0.00700171
  row.laserDriftVelocityWest	 =   5.54256; // +/- 6.41529e-06 cm/us All: West = 0.170929 +/- 0.00114285
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54256 +/- 6.41529e-06
  return (TDataSet *)tableSet;// West = 5.54244 +/- 6.48936e-06 East = 5.54749 +/- 4.25832e-05
};
