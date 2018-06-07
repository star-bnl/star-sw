TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 152073
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5514; // +/- 4.97193e-06 cm/us All: East = 0.181937 +/- 0.00189675
  row.laserDriftVelocityWest	 =   5.5514; // +/- 4.97193e-06 cm/us All: West = 0.153234 +/- 0.000981065
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5514 +/- 4.97193e-06
  return (TDataSet *)tableSet;// West = 5.55144 +/- 5.62022e-06 East = 5.55126 +/- 1.06636e-05
};
