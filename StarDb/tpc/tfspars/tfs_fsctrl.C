St_DataSet *CreateTable() { 
// -----------------------------------------------------------------
// Top/tfspars/tfs_fsctrl Allocated rows: 1  Used rows: 1  Row size: 32 bytes
//  Table: tfs_fsctrl_st[0]--> tfs_fsctrl_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tfs_fsctrl")) return 0;
tfs_fsctrl_st row;
St_tfs_fsctrl *tableSet = new St_tfs_fsctrl("tfs_fsctrl",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.init	 =          0; // flag to initialize TFS (0 = initialize) ;
    row.print	 =          0; // controls diagnostic prints (0 = off) ;
    row.rndm	 =          0; // Seed initialisatio( 1=on, 0=off) ;
    row.sys_test	 =  0; // sys_test=1 for system tests o/w = 0 ;
    row.alpha_hi	 =  72; // max angle for scaling (deg) ;
    row.alpha_lo	 =  45; // min angle for scaling (deg) ;
    row.drmin	 =          1; // merging criterion radial (pads) ;
    row.dzmin	 =          2; // merging criterion drift (time bins) ;
    row.merge_r	 =         10; // max merged size radial (pads) ;
    row.merge_z	 =         20; // max merged size drift (bins) ;
    row.reslim	 =         20; // maximum cluster size (cm) ;
    row.sigma0[0]	 =     0.0135; // resolution intrinsic term ;
    row.sigma0[1]	 =     0.0303;
    row.sigma0[2]	 =          1;
    row.sigma0[3]	 =          1;
    row.ang_wireF[0]	 =          1; // resolution angular wire term ;
    row.ang_wireF[1]	 =          1;
    row.ang_wireF[2]	 =      3.648;
    row.ang_wireF[3]	 =      0.525;
    row.ang_padF[0]	 =          1; // resolution angular pad term ;
    row.ang_padF[1]	 =          1;
    row.ang_padF[2]	 =      4.324;
    row.ang_padF[3]	 =      5.034;
    row.sprf_0[0]	 =        0.2; // prf intrinsic term ;
    row.sprf_0[1]	 =      0.362;
    row.sprf_dr0[0]	 =      0.067; // zero B-field prf drift term ;
    row.sprf_dr0[1]	 =      0.067;
    row.sprf_dr0[2]	 =     0.0698;
    row.sprf_dr0[3]	 =     0.0746;
    row.sprf_ta[0]	 =      0.349; // prf tan-alpha term ;
    row.sprf_ta[1]	 =      0.637;
tableSet->AddAt(&row,0);
// ----------------- end of code ---------------
 return (St_DataSet *)tableSet;
}
