TDataSet *CreateTable() { 
  // ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_itpcDeadFEE")) return 0;
  /* 20174045  -  20175028   { 5, 55, 30, 45, 6,  0},   Dead ALTRO RDO 6 
     20174045  -  20175028   { 5, 56, 30, 45, 6,  0},   Dead 

     20174046  -  20174046   { 5, 55, 30, 37, 6,  0},   Dead 
     20174046  -  20174046   { 5, 55, 39, 45, 6,  0},   Dead 
     20174046  -  20174046   { 5, 56, 30, 41, 6,  0},   Dead 

     20174048  -  20174048   { 5, 55, 30, 36, 6,  0},   Dead 
     20174048  -  20174048   { 5, 55, 38, 45, 6,  0},   Dead 
     20174048  -  20174048   { 5, 56, 34, 45, 6,  0},   Dead 
 
     20175001  -  20175001   { 5, 55, 33, 45, 6,  0},   Dead 
     20175001  -  20175001   { 5, 56, 35, 45, 6,  0},   Dead 

     20175003  -  20175003   { 5, 56, 30, 37, 6,  0},   Dead 
     20175003  -  20175003   { 5, 56, 39, 45, 6,  0},   Dead 

     20175006  -  20175006   { 5, 55, 31, 45, 6,  0},   Dead 

     20175007  -  20175007   { 5, 56, 30, 36, 6,  0},   Dead 
     20175007  -  20175007   { 5, 56, 38, 45, 6,  0},   Dead 

     20175024  -  20175024   { 5, 56, 30, 43, 6,  0},   Dead 

     20175025  -  20175025   { 5, 55, 30, 40, 6,  0},   Dead 
     20175025  -  20175025   { 5, 55, 42, 45, 6,  0},   Dead 
  */
  St_itpcDeadFEE *tableSet = new St_itpcDeadFEE("itpcDeadFEE",2);
  itpcDeadFEE_st row = {19, 29,  2, 12, 3, 17};
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); 
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
