TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 132005
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54833; // +/- 6.27838e-06 cm/us All: East = 0.113799 +/- 0.00224205
  row.laserDriftVelocityWest	 =   5.54833; // +/- 6.27838e-06 cm/us All: West = 0.231184 +/- 0.00129626
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54833 +/- 6.27838e-06
  return (TDataSet *)tableSet;// West = 5.54816 +/- 7.29274e-06 East = 5.54879 +/- 1.23406e-05
};
