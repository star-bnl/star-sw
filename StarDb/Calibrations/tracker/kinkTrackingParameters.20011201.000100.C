TDataSet *CreateTable(){
// -----------------------------------------------------------------
// tkfpar Allocated rows: 1  Used rows: 1 Row
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tkf_tkfpar")) return 0;
  St_tkf_tkfpar *tablePars =  new St_tkf_tkfpar("tkf_tkfpar",1); 
  tkf_tkfpar_st parRow;  
//
// **************************************************************************
// One raw, with TPC cuts for kinks
// **************************************************************************
//
//
// *** Primary kink cuts used until 2005 (before year 5 pp production) ******

  memset(&parRow,0,sizeof(tkf_tkfpar_st));
  parRow.dcaParentDaughterMax      =  0.5; //DCA parent-daughter kink
  parRow.parentPtMin               =  0.2; // min kink parent Pt
  parRow.vertexRMax2D              =  179.;// upper limit fiducial volume  
  parRow.vertexRMin2D              =  133.;//lower limit fiducial volume  
  parRow.thetaMin                  =  1.; //parent kink decay angle
  parRow.numOfPadRows              =  40;  	// not used (value for year1)
  parRow.parentDipAngleMax         =  0.79;	
  parRow.impactCut                 =  2.; //impact parameter
  parRow.parentLastDaughterStart2D =  14.; //last point parent-first point daughter track 2D distance
  parRow.parentLastDaughterStartZ  =  20.;
  parRow.projectPointZDiff         =  2.; //
  parRow.distanceKinkParent2D      =  14.;//radial distance parent-PV
  parRow.distanceKinkDaughter2D    =  14.;//radial distance parent-PV
  parRow.distanceKinkParentZ       =  20.; //parent-PV z distance
  parRow.distanceKinkDaughterZ     =  20.; // daughter-PV z distance
  tablePars->AddAt(&parRow,0);

// ----------------- end of code ---------------
  return (TDataSet *)tablePars; 

}
