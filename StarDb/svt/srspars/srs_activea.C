St_DataSet *CreateTable() { 
// -----------------------------------------------------------------
// Top/srspars/srs_activea Allocated rows: 4  Used rows: 4  Row size: 44 bytes
//  Table: srs_activea_st[0]--> srs_activea_st[3]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_srs_activea")) return 0;
srs_activea_st row;
St_srs_activea *tableSet = new St_srs_activea("srs_activea",4);
//
memset(&row,0,tableSet->GetRowSize());
    row.id	 =          1; // active area type ;
    row.param[0]	 =          0; // active area shape parameters ;
    row.param[1]	 =      0.135;
    row.param[2]	 =          0;
    row.param[3]	 =          0;
    row.param[4]	 =          0;
    row.param[5]	 =          0;
    row.param[6]	 =          0;
    row.param[7]	 =          0;
    row.param[8]	 =          0;
    row.param[9]	 =          0;
tableSet->AddAt(&row,0);
memset(&row,0,tableSet->GetRowSize());
    row.id	 =          2; // active area type ;
    row.param[0]	 =     0.3475; // active area shape parameters ;
    row.param[1]	 =      0.125;
    row.param[2]	 =       0.34;
    row.param[3]	 =          0;
    row.param[4]	 =          0;
    row.param[5]	 =          0;
    row.param[6]	 =          0;
    row.param[7]	 =          0;
    row.param[8]	 =          0;
    row.param[9]	 =          0;
tableSet->AddAt(&row,1);
memset(&row,0,tableSet->GetRowSize());
    row.id	 =          3; // active area type ;
    row.param[0]	 =     0.3475; // active area shape parameters ;
    row.param[1]	 =      0.125;
    row.param[2]	 =       0.68;
    row.param[3]	 =       0.68;
    row.param[4]	 =          0;
    row.param[5]	 =          0;
    row.param[6]	 =          0;
    row.param[7]	 =          0;
    row.param[8]	 =          0;
    row.param[9]	 =          0;
tableSet->AddAt(&row,2);
memset(&row,0,tableSet->GetRowSize());
    row.id	 =          4; // active area type ;
    row.param[0]	 =        0.1; // active area shape parameters ;
    row.param[1]	 =        0.1;
    row.param[2]	 =          0;
    row.param[3]	 =          0;
    row.param[4]	 =          0;
    row.param[5]	 =          0;
    row.param[6]	 =          0;
    row.param[7]	 =          0;
    row.param[8]	 =          0;
    row.param[9]	 =          0;
tableSet->AddAt(&row,3);
// ----------------- end of code ---------------
 return (St_DataSet *)tableSet;
}
