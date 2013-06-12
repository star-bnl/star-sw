TDataSet *CreateTable() { 
  if (!TClass::GetClass("St_tpcDriftVelocity")) return 0;
  tpcDriftVelocity_st row;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  memset(&row,0,tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =      5.387; // cm/us : from laser beam analysis  ;
  row.laserDriftVelocityWest	 =      5.388; // cm/us : from laser beam analysis  ;
  row.cathodeDriftVelocityEast	 =      5.387; // cm/us : from cathode emission  ;
  row.cathodeDriftVelocityWest	 =      5.388; // cm/us : from cathode emission  ;
  tableSet->AddAt(&row);
 return (TDataSet *)tableSet;
}
