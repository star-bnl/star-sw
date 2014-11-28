St_DataSet *CreateTable() { 
// -----------------------------------------------------------------
// Top/tidpars/tdeparm Allocated rows: 1  Used rows: 1  Row size: 60 bytes
//  Table: tdeparm_st[0]--> tdeparm_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tdeparm")) return 0;
tdeparm_st row;
St_tdeparm *tableSet = new St_tdeparm("tdeparm",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.debug[0]	 =          0; // tde debugging parameters ;
    row.debug[1]	 =          1;
    row.debug[2]	 =          0;
    row.debug[3]	 =          0;
    row.debug[4]	 =          0;
    row.debug[5]	 =          0;
    row.debug[6]	 =          0;
    row.debug[7]	 =          0;
    row.debug[8]	 =          0;
    row.debug[9]	 =          0;
    row.minrow	 =          0; // Minimum row to include in dE/dx calc. ;
    row.averaging_method  =          1; // Select averaging option;
    row.truncopt	 =          4; // Select dedx trunc option ;
    row.use_glob	 =          0; // use global tracks parameter ;
    row.usemerge	 =          0; // if =1, then use merged hits in the calc. ;
    row.truncfact	 =        0.3; // fraction of dedx values to truncate ;
tableSet->AddAt(&row,0);
// ----------------- end of code ---------------
 return (St_DataSet *)tableSet;
}


