TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// StvTpcOuterHitErrs Allocated rows: 1  Used rows: 1  Row size: 48 bytes
//  Table: StvKonst_st[0]--> StvKonst_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_StvKonst")) return 0;
StvKonst_st row;
St_StvKonst *tableSet = new St_StvKonst("StvKonst",1);
//
memset(&row,0,tableSet->GetRowSize());
  row.mXi2Hit = 22*3;		//Xi2 to accept new hit
  row.mXi2Vtx = 55*3;		//Xi2 to accept vertex
  row.mXi2Joi = 55*3;		//Xi2 in Refit join left & right subtrack
  row.mXi2Hlx = row.mXi2Hit*9;  //Xi2 in Helix, .
  row.mRxyMax = 300;		//Max radius for tracking
  row.mZMax   = 300;		//Max Z      for tracking
  row.mDca2dZeroXY 	= 6;	//max 2d dca to X=Y=0  for primary track
  row.mDca3dVertex 	= 3;	//max 3d dca to vertex for primary track
  row.mMaxCurv     	= 0.1;	//Maximal allowed curvature
  row.mMinP2       	= 0.003*0.003;	//Geant3 cut for too small momentum**2	
  row.mMaxWindow   	= 3.;	//Maximal window to search hits
  row.mMinHits 	= 5;		//Min number of hits allowed
  row.mGoodHits 	=15;	//Good number of hits allowed


tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
