TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 137037
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53784; // +/- 5.24731e-06 cm/us All: East = -0.271311 +/- 0.00302311
  row.laserDriftVelocityWest	 =   5.53784; // +/- 5.24731e-06 cm/us All: West = 0.166219 +/- 0.000972842
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53784 +/- 5.24731e-06
  return (TDataSet *)tableSet;// West = 5.5376 +/- 5.52586e-06 East = 5.54007 +/- 1.67385e-05
};
