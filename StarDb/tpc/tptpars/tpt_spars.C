St_DataSet *CreateTable() { 
// -----------------------------------------------------------------
// Top/tptpars/tpt_spars Allocated rows: 1  Used rows: 1  Row size: 216 bytes
//  Table: tpt_spars_st[0]--> tpt_spars_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpt_spars")) return 0;
tpt_spars_st row;
St_tpt_spars *tableSet = new St_tpt_spars("tpt_spars",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.first_row	 =          1; // first row for tracking ;
    row.last_row	 =         45; // last row for tracking ;
    row.nskip	 =          0; // number of points to skip from the fit ;
    row.skip[0]	 =          0; // list of rownumbers where to skip them ;
    row.skip[1]	 =          0;
    row.skip[2]	 =          0;
    row.skip[3]	 =          0;
    row.skip[4]	 =          0;
    row.skip[5]	 =          0;
    row.skip[6]	 =          0;
    row.skip[7]	 =          0;
    row.skip[8]	 =          0;
    row.skip[9]	 =          0;
    row.skip[10]	 =          0;
    row.skip[11]	 =          0;
    row.skip[12]	 =          0;
    row.skip[13]	 =          0;
    row.skip[14]	 =          0;
    row.skip[15]	 =          0;
    row.skip[16]	 =          0;
    row.skip[17]	 =          0;
    row.skip[18]	 =          0;
    row.skip[19]	 =          0;
    row.skip[20]	 =          0;
    row.skip[21]	 =          0;
    row.skip[22]	 =          0;
    row.skip[23]	 =          0;
    row.skip[24]	 =          0;
    row.skip[25]	 =          0;
    row.skip[26]	 =          0;
    row.skip[27]	 =          0;
    row.skip[28]	 =          0;
    row.skip[29]	 =          0;
    row.skip[30]	 =          0;
    row.skip[31]	 =          0;
    row.skip[32]	 =          0;
    row.skip[33]	 =          0;
    row.skip[34]	 =          0;
    row.skip[35]	 =          0;
    row.skip[36]	 =          0;
    row.skip[37]	 =          0;
    row.skip[38]	 =          0;
    row.skip[39]	 =          0;
    row.skip[40]	 =          0;
    row.skip[41]	 =          0;
    row.skip[42]	 =          0;
    row.skip[43]	 =          0;
    row.skip[44]	 =          0;
    row.hole	 =          4; // acceptable number of missing points ;
    row.nmin	 =          8; // minimum number of points on a track ;
    row.ilimit	 =          4; // ;
    row.oy	 =       1.01; // cut for outliers in the pad plane ;
    row.oz	 =       1.01; // cut for outliers in the drift direction;
    row.outlimit	 =          4; // ;
tableSet->AddAt(&row,0);
// ----------------- end of code ---------------
 return (St_DataSet *)tableSet;
}
