TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 87050
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.51828; // +/- 5.30052e-06 cm/us All: East = -0.405377 +/- 0.00290954
  row.laserDriftVelocityWest	 =   5.51828; // +/- 5.30052e-06 cm/us All: West = 0.228078 +/- 0.00101041
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.51828 +/- 5.30052e-06
  return (TDataSet *)tableSet;// West = 5.51791 +/- 5.60727e-06 East = 5.52141 +/- 1.62483e-05
};
