St_DataSet *CreateTable() { 
// -----------------------------------------------------------------
// Top/stkpars/stk_filler Allocated rows: 1  Used rows: 1  Row size: 240 bytes
//  Table: stk_filler_st[0]--> stk_filler_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_stk_filler")) return 0;
stk_filler_st row;
St_stk_filler *tableSet = new St_stk_filler("stk_filler",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.th12_min	 =          0; // min cone angle 12 ;
    row.th12_step	 =          0; // step size cone12 ;
    row.th21	 =          0; // cone 1 to 2 ;
    row.th23_min	 =          0; // min cone angle 23 (degrees) ;
    row.th23_step	 =          0; // step size cone23 ;
    row.th31	 = 1.30821e-38; // cone 1 to 3 ;
    row.th32	 = 1.4013e-45; // cone 2 to 3 ;
    row.th41	 = 3.36312e-44; // cone 1 to 4 ;
    row.th42	 = 3.36312e-44; // cone 2 to 4 ;
    row.th43	 =          0; // cone 3 to 4 ;
    row.th51	 =          0; // cone 1 to 5 ;
    row.th52	 =          0; // cone 2 to 5 ;
    row.th53	 =          0; // cone 3 to 5 ;
    row.th54	 = 7.35883e+31; // cone 4 to 5 ;
    row.th61	 =          0; // cone 1 to 6 ;
    row.th62	 =          0; // cone 2 to 6 ;
    row.th63	 =          0; // cone 3 to 6 ;
    row.th64	 =          0; // cone 4 to 6 ;
    row.th65	 =          0; // cone 5 to 6 ;
    row.th71	 =          0; // cone 1 to 7 ;
    row.th72	 =          0; // cone 2 to 7 ;
    row.th73	 = 1.30821e-38; // cone 3 to 7 ;
    row.th74	 = 1.4013e-45; // cone 4 to 7 ;
    row.th75	 = 3.92364e-44; // cone 5 to 7 ;
    row.th76	 = 3.92364e-44; // cone 6 to 7 ;
    row.th81	 =          0; // cone 1 to 8 ;
    row.th82	 =          0; // cone 2 to 8 ;
    row.th83	 =          0; // cone 3 to 8 ;
    row.th84	 =          0; // cone 4 to 8 ;
    row.th85	 = 7.35883e+31; // cone 5 to 8 ;
    row.th86	 =          0; // cone 6 to 8 ;
    row.th87	 =          0; // cone 7 to 8 ;
    row.wth21	 =        120; // wafer cone from 1 to 2 ;
    row.wth31	 =         90; // wafer cone from 1 to 3 ;
    row.wth32	 =         90; // wafer cone from 2 to 3 ;
    row.wth41	 =         90; // wafer cone from 1 to 4 ;
    row.wth42	 =         90; // wafer cone from 2 to 4 ;
    row.wth43	 =        120; // wafer cone from 3 to 4 ;
    row.wth51	 =         90; // wafer cone from 1 to 5 ;
    row.wth52	 =         90; // wafer cone from 2 to 5 ;
    row.wth53	 =         90; // wafer cone from 3 to 5 ;
    row.wth54	 =         90; // wafer cone from 4 to 5 ;
    row.wth61	 =         90; // wafer cone from 1 to 6 ;
    row.wth62	 =         90; // wafer cone from 2 to 6 ;
    row.wth63	 =         90; // wafer cone from 3 to 6 ;
    row.wth64	 =         90; // wafer cone from 4 to 6 ;
    row.wth65	 =        120; // wafer cone from 5 to 6 ;
    row.wth71	 =         60; // wafer cone 1 to 7 ;
    row.wth72	 =         60; // wafer cone 2 to 7 ;
    row.wth73	 =         60; // wafer cone 3 to 7 ;
    row.wth74	 =         60; // wafer cone 4 to 7 ;
    row.wth75	 =         60; // wafer cone 5 to 7 ;
    row.wth76	 =         60; // wafer cone 6 to 7 ;
    row.wth81	 =         60; // wafer cone 1 to 8 ;
    row.wth82	 =         60; // wafer cone 2 to 8 ;
    row.wth83	 =         60; // wafer cone 3 to 8 ;
    row.wth84	 =         60; // wafer cone 4 to 8 ;
    row.wth85	 =         60; // wafer cone 5 to 8 ;
    row.wth86	 =         60; // wafer cone 6 to 8 ;
    row.wth87	 =        120; // wafer cone 7 to 8 ;
tableSet->AddAt(&row,0);
// ----------------- end of code ---------------
 return (St_DataSet *)tableSet;
}
