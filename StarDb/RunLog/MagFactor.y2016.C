TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// StarDb/.data/StarDb/RunLog/MagFactor Allocated rows: 1  Used rows: 1  Row size: 4 bytes
//  Table: MagFactor_st[0]--> MagFactor_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_MagFactor")) return 0;
MagFactor_st row;
St_MagFactor *tableSet = new St_MagFactor("MagFactor",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.ScaleFactor	 =        -1.002; // ;
tableSet->AddAt(&row,0);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
