St_DataSet *CreateTable() { 
// -----------------------------------------------------------------
// Top/ftepars/ftepar Allocated rows: 1  Used rows: 1  Row size: 68 bytes
//  Table: fte_ftepar_st[0]--> fte_ftepar_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_fte_ftepar")) return 0;
fte_ftepar_st row;
St_fte_ftepar *tableSet = new St_fte_ftepar("ftepar",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.debug[0]	 =          0; // Flags for debug print ;
    row.debug[1]	 =          0;
    row.debug[2]	 =          0;
    row.debug[3]	 =          0;
    row.debug[4]	 =          0;
    row.debug[5]	 =          0;
    row.debug[6]	 =          0;
    row.debug[7]	 =          0;
    row.debug[8]	 =          0;
    row.debug[9]	 =          0;
    row.sizer	 =         10; // # of padplanes ;
    row.void1	 =          1; // not used yet ;
    row.void2	 =          2; // not used yet ;
    row.void3	 =          0; // not used yet ;
    row.void4	 =          0; // not used yet ;
    row.void5	 =          0; // not used yet ;
    row.void6	 =          0; // not used yet ;
tableSet->AddAt(&row,0);
// ----------------- end of code ---------------
 return (St_DataSet *)tableSet;
}
