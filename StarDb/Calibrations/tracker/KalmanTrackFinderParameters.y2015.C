TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// db/.const/StarDb/Calibrations/tracker/.KalmanTrackFinderParameters/KalmanTrackFinderParameters Allocated rows: 1  Used rows: 1  Row size: 68 bytes
//  Table: KalmanTrackFinderParameters_st[0]--> KalmanTrackFinderParameters_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_KalmanTrackFinderParameters")) return 0;
KalmanTrackFinderParameters_st row;
St_KalmanTrackFinderParameters *tableSet = new St_KalmanTrackFinderParameters("KalmanTrackFinderParameters",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.useMcAsRec	 =          0; // ;
    row.useTrackFilter	 =          1; // Whether a track filter should be used internally;
    row.elossCalculated	 =          1; // Whether the Energy loss should be calculated while propagating tracks;
    row.mcsCalculated	 =          1; // Whether multiple coulomb scattering should be include in erro matrices;
    row.maxNullCount	 =         13; // Maximum number of null hits on active detector layers;
    row.maxContigNullCount	 =          8; // Maximum number of contiguous null hits;
    row.minCountForReset	 =          2; // Number of adjacent layers with hits before nullContiguous is reset to zero ;
//PXL 1 -- 2 < R < 4 cm
//PXL 2 -- 7 < R < 9 cm
//IST 3 -- 12 < R < 17 cm
//SST 4 -- 21 < R < 28 cm 

    row.mHitRegions	 =   28170904; // pxl1<4>pxl2<9<pxl3<17<SST<28;
    row.mHitWeights	 =    9090309; // Coeffs of nhits. sum must be >=20;
    row.maxChi2Vertex	 =        900; // max vertex incremental chi2 value acceptable;
    row.massHypothesis	 =      0.139; // mass used in the tracking for mcs and eloss calculation purposes;
    row.maxDca2dZeroXY	 =          6; // max 2d dca to X=Y=0  for primary track;
    row.maxDca3dVertex	 =          3; // max 3d dca to vertex for primary track;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}

/**
   hit weights = 9939 --> all 3-layer combinations of HFT hits
 */
