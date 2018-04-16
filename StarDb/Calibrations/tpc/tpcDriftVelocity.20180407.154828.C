TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 97034
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54561; // +/- 8.54822e-06 cm/us All: East = -4.23619 +/- 0.00223581
  row.laserDriftVelocityWest	 =   5.54561; // +/- 8.54822e-06 cm/us All: West = -4.07169 +/- 0.00208939
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54561 +/- 8.54822e-06
  return (TDataSet *)tableSet;// West = 5.5452 +/- 1.16098e-05 East = 5.5461 +/- 1.26329e-05
};
