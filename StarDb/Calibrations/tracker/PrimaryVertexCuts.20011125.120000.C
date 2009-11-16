TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_VertexCuts")) return 0;
  St_VertexCuts *tableSet = new St_VertexCuts("PrimaryVertexCuts",1);
  VertexCuts_st row;
  memset(&row,0,tableSet->GetRowSize());
  row.MinNumberOfFitPointsOnTrack =  20;
  row.MinTrack                    =   5;         // Min number of tracks
  row.DcaZMax                     =   6;
  row.RImpactMax                  =   6;       // Max distance between helix and nominal beamline (0,0,z)
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
