TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 167026
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54688; // +/- 2.25985e-05 cm/us All: East = 0.0420212 +/- 0.0422866
  row.laserDriftVelocityWest	 =   5.54688; // +/- 2.25985e-05 cm/us All: West = 0.160523 +/- 0.00428865
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54688 +/- 2.25985e-05
  return (TDataSet *)tableSet;// West = 5.54684 +/- 2.29683e-05 East = 5.54798 +/- 0.000126457
};
