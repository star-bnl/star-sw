TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// StarDb/.make/StarDb/.data/StarDb/RunLog/TpcTimeGain Allocated rows: 1  Used rows: 1  Row size: 4 bytes
//  Table: TpcTimeGain_st[0]--> TpcTimeGain_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_TpcTimeGain")) return 0;
TpcTimeGain_st row;
St_TpcTimeGain *tableSet = new St_TpcTimeGain("TpcTimeGain",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.ScaleFactor	 =          1; // ;
    row.ErrorScaleFactor =          0; // ;
tableSet->AddAt(&row,0);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
