TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 104047
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54284; // +/- 9.50579e-06 cm/us All: East = -0.399788 +/- 0.00420483
  row.laserDriftVelocityWest	 =   5.54284; // +/- 9.50579e-06 cm/us All: West = 0.198077 +/- 0.00186337
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54284 +/- 9.50579e-06
  return (TDataSet *)tableSet;// West = 5.54231 +/- 1.03804e-05 East = 5.5456 +/- 2.366e-05
};
