St_DataSet *CreateTable() { 
// -----------------------------------------------------------------
// Top/ctg/ctb_slat_eta Allocated rows: 4  Used rows: 4  Row size: 36 bytes
//  Table: ctg_slat_eta_st[0]--> ctg_slat_eta_st[3]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_ctg_slat_eta")) return 0;
ctg_slat_eta_st row;
St_ctg_slat_eta *tableSet = new St_ctg_slat_eta("ctb_slat_eta",4);
//
memset(&row,0,tableSet->GetRowSize());
    row.ieta	 =          1; // ;
    row.cosang	 =          1; // ;
    row.eta	 =   -0.75634; // ;
    row.eta_max	 =  -0.503158; // ;
    row.eta_min	 =  -0.974627; // ;
    row.r	 =      212.5; // ;
    row.z	 =    -176.49; // ;
    row.z_max	 =    -111.49; // ;
    row.z_min	 =    -241.49; // ;
tableSet->AddAt(&row,0);
memset(&row,0,tableSet->GetRowSize());
    row.ieta	 =          2; // ;
    row.cosang	 =         -1; // ;
    row.eta	 =  -0.258834; // ;
    row.eta_max	 =  -0.501567; // ;
    row.eta_min	 = -0.000603481; // ;
    row.r	 =     215.41; // ;
    row.z	 =     -56.38; // ;
    row.z_max	 =    -112.63; // ;
    row.z_min	 =      -0.13; // ;
tableSet->AddAt(&row,1);
memset(&row,0,tableSet->GetRowSize());
    row.ieta	 =          3; // ;
    row.cosang	 =          1; // ;
    row.eta	 =   0.258834; // ;
    row.eta_max	 =   0.501567; // ;
    row.eta_min	 = 0.000603513; // ;
    row.r	 =     215.41; // ;
    row.z	 =      56.38; // ;
    row.z_max	 =     112.63; // ;
    row.z_min	 =       0.13; // ;
tableSet->AddAt(&row,2);
memset(&row,0,tableSet->GetRowSize());
    row.ieta	 =          4; // ;
    row.cosang	 =         -1; // ;
    row.eta	 =    0.75634; // ;
    row.eta_max	 =   0.503158; // ;
    row.eta_min	 =   0.974627; // ;
    row.r	 =      212.5; // ;
    row.z	 =     176.49; // ;
    row.z_max	 =     111.49; // ;
    row.z_min	 =     241.49; // ;
tableSet->AddAt(&row,3);
// ----------------- end of code ---------------
 return (St_DataSet *)tableSet;
}
