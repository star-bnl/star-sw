TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 88006
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5272; // +/- 0.000264086 cm/us All: East = 0.229929 +/- 2.38768
  row.laserDriftVelocityWest	 =   5.5272; // +/- 0.000264086 cm/us All: West = 0.292016 +/- 0.244808
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5272 +/- 0.000264086
  return (TDataSet *)tableSet;// West = 5.52719 +/- 0.000264732 East = 5.52962 +/- 0.00378389
};
