St_DataSet *CreateTable() { 
// -----------------------------------------------------------------
// Top/sprpars/sprpar Allocated rows: 1  Used rows: 1  Row size: 8 bytes
//  Table: spr_sprpar_st[0]--> spr_sprpar_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_spr_sprpar")) return 0;
spr_sprpar_st row;
St_spr_sprpar *tableSet = new St_spr_sprpar("sprpar",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.method	 =          0; // flag to define the output of spr    ;
    row.p	 =          0; // momentum cut off for dE/dx in GeV/c ;
tableSet->AddAt(&row,0);
// ----------------- end of code ---------------
 return (St_DataSet *)tableSet;
}
