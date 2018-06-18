TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 164023
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54922; // +/- 8.07185e-06 cm/us All: East = -0.331066 +/- 0.00626105
  row.laserDriftVelocityWest	 =   5.54922; // +/- 8.07185e-06 cm/us All: West = 0.254508 +/- 0.00118139
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54922 +/- 8.07185e-06
  return (TDataSet *)tableSet;// West = 5.54908 +/- 8.30874e-06 East = 5.55149 +/- 3.40467e-05
};
