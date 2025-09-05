#ifndef __CINT__
#include "tables/St_starClockOnl_Table.h"
#endif

TDataSet *CreateTable() { 
  if (!TClass::GetClass("St_starClockOnl")) return 0;
  starClockOnl_st row;
  St_starClockOnl *tableSet = new St_starClockOnl("starClockOnl",1);
  memset(&row,0,tableSet->GetRowSize());
  row.runNumber	 =    2214033; // run number  ;
  row.time	 =  996787667; // unix time of entry  ;
  row.frequency	 =    9399962; // local clock
  tableSet->AddAt(&row);
 return (TDataSet *)tableSet;
}
