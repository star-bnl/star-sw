St_DataSet *CreateTable() { 
// -----------------------------------------------------------------
// Top/tptpars/tpt_pars Allocated rows: 2  Used rows: 2  Row size: 160 bytes
//  Table: tpt_pars_st[0]--> tpt_pars_st[1]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpt_pars")) return 0;
tpt_pars_st row;
St_tpt_pars *tableSet = new St_tpt_pars("tpt_pars",2);
//
memset(&row,0,tableSet->GetRowSize());
    row.debug[0]	 =          1; // Flags for debug prlong ;
    row.debug[1]	 =          0;
    row.debug[2]	 =          0;
    row.debug[3]	 =          0;
    row.debug[4]	 =          0;
    row.debug[5]	 =          0;
    row.debug[6]	 =          0;
    row.debug[7]	 =          0;
    row.debug[8]	 =          0;
    row.debug[9]	 =          0;
    row.display	 =          0; // Enables longeractive tracking display ;
    row.extend	 =          1; // Switch for extending segments. ;
    row.form_uv	 =          2; // Switch for segment formation & mapping. ;
    row.form_xy	 =          1; // Switch for xy segment formation ;
    row.g1	 =          0; // allowed gap between 1 and 2 point ;
    row.g2	 =          0; // allowable gap between 2 and 3 point ;
    row.irow_root_beg	 =         45; // row to start root formation ;
    row.irow_root_end	 =          5; // row to end root formation ;
    row.irow_seg_end	 =          1; // row to end segment formation (no begin) ;
    row.mctest	 =          0; // switch for the MC testing ;
    row.merge	 =          0; // Switch for segment merging ;
    row.minfit	 =         8; // minimum number of points on a track ;
    row.mxtry	 =         10; // max # of attempts to fit ;
    row.nhol	 =          4; // allowed gap size ;
    row.nzslic	 =         10; // # of slices( even) along the z axis. ;
    row.pass_on	 =          1; // Pass on or off ;
    row.delrlm	 =          8; // limit on the extrapolation in bend plane ;
    row.delzlm	 =          8; // limit on the extrapolation ;
    row.drlm1	 =          1; // tolerance on the 1 step in the bend plan ;
    row.drmax	 =          2; // max road width in x,y ;
    row.drplim	 =          2; // Limit on the linear extrap. in bend plan ;
    row.drstr	 =        3.0; // road width in st dev  in the bend plane ;
    row.dzlm1	 =        1.0; // tolerance on the first step ;
    row.dzmax	 =          4; // maximum road width in z ;
    row.dzplim	 =          4; // Limit for linear extrapolation in z ;
    row.dzstr	 =        3.0; // road width in st dev in z ;
    row.mxweig	 =         10; // max # of hits/channel for the starter ;
    row.prob[0]	 =         0.80; // probability cut ;
    row.prob[1]	 =         0.80;
    row.zslim	 =          2; // limit on the msc ;
    row.lfit     =          0; // straigth line fit;
tableSet->AddAt(&row,0);
memset(&row,0,tableSet->GetRowSize());
    row.debug[0]	 =          1; // Flags for debug prlong ;
    row.debug[1]	 =          0;
    row.debug[2]	 =          0;
    row.debug[3]	 =          0;
    row.debug[4]	 =          0;
    row.debug[5]	 =          0;
    row.debug[6]	 =          0;
    row.debug[7]	 =          0;
    row.debug[8]	 =          0;
    row.debug[9]	 =          0;
    row.display	 =          0; // Enables longeractive tracking display ;
    row.extend	 =          1; // Switch for extending segments. ;
    row.form_uv	 =          2; // Switch for segment formation & mapping. ;
    row.form_xy	 =          1; // Switch for xy segment formation ;
    row.g1	 =          0; // allowed gap between 1 and 2 point ;
    row.g2	 =          1; // allowable gap between 2 and 3 point ;
    row.irow_root_beg	 =         45; // row to start root formation ;
    row.irow_root_end	 =          4; // row to end root formation ;
    row.irow_seg_end	 =          1; // row to end segment formation (no begin) ;
    row.mctest	 =          0; // switch for the MC testing ;
    row.merge	 =          0; // Switch for segment merging ;
    row.minfit	 =          5; // minimum number of polongs on a track ;
    row.mxtry	 =          4; // max # of attempts to fit ;
    row.nhol	 =          5; // allowed gap size ;
    row.nzslic	 =          4; // # of slices( even) along the z axis. ;
    row.pass_on	 =          1; // Pass on or off ;
    row.delrlm	 =          8; // limit on the extrapolation in bend plane ;
    row.delzlm	 =         10; // limit on the extrapolation ;
    row.drlm1	 =          4; // tolerance on the 1 step in the bend plan ;
    row.drmax	 =          2; // max road width in x,y ;
    row.drplim	 =          2; // Limit on the linear extrap. in bend plan ;
    row.drstr	 =        0.8; // road width in st dev  in the bend plane ;
    row.dzlm1	 =          4; // tolerance on the first step ;
    row.dzmax	 =          2; // maximum road width in z ;
    row.dzplim	 =          4; // Limit for linear extrapolation in z ;
    row.dzstr	 =          2; // road width in st dev in z ;
    row.mxweig	 =         10; // max # of hits/channel for the starter ;
    row.prob[0]	 =        200; // probability cut ;
    row.prob[1]	 =        100;
    row.zslim	 =          4; // limit on the msc ;
    row.lfit     =          0; // straigth line fit;
tableSet->AddAt(&row,1);
// ----------------- end of code ---------------
 return (St_DataSet *)tableSet;
}
