TDataSet *CreateTable() { 
  if (!TClass::GetClass("St_starClockOnl")) return 0;
  starClockOnl_st row;
  St_starClockOnl *tableSet = new St_starClockOnl("starClockOnl",1);
  memset(&row,0,tableSet->GetRowSize());
  row.runNumber	 =          0; // run number  ;
  row.time	 =          0; // unix time of entry  ;
  row.frequency	 =    9383160; // frequency in Hz, RHIC clock
  tableSet->AddAt(&row);
 return (TDataSet *)tableSet;
}
