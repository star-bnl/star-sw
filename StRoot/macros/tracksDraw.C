void tracksDraw(){
  gSystem->Load("St_base");
  gSystem->Load("xdf2root");
  gSystem->Load("St_Tables");

Int_t GeantID(g2t_tpc_hit_st *pRows,Int_t i,Float_t *x){
  g2t_tpc_hit_st *p = pRows[i];
  x[0] = p->x[0];
  x[1] = p->x[1];
  x[2] = p->x[2];
  return  p->track_p;
}

Int_t HitsID(tcl_tphit_st *ppRows,Int_t i,Float_t *x){
  tcl_tphit_st *pp = ppRows[i];
  x[0] = pp->x;
  x[1] = pp->y;
  x[2] = pp->z;
  return  pp->track;
}

  TCanvas *m_TreeD = new TCanvas("STAF","Events",200,200);
//  Draw STAR detector first
//   TWebFile stargeom("http://root.cern.ch/files/star.root");
//   year2a_hadron.Draw();
  TView *view = m_TreeD->GetView();
 

  St_XDFFile fileHit("tphit.xdf");
  St_XDFFile fileGeant("geant.xdf");
  // skip first record
 
//  St_DataSet *skip = file.NextEventGet();
//  if (skip) delete skip;
  St_DataSet *hits   = fileHit.NextEventGet();
  St_DataSet *geants = fileGeant.NextEventGet();
  if (!(hits && geants) ) return;

  St_DataSetIter nextHits(hits);
  St_DataSetIter nextGeants(geants);
  const Char_t *tableGeants = "g2t_tpc_hit";
  const Char_t *tableHits   = "tphit";
  nextHits.Du();
  nextGeants.Du();
// struct g2t_tpc_hit_st {
//        long    id;             // primary key
//        long    next_tr_hit_p;  // Id of next hit on same track
//        long    track_p;        // Id of parent track
//        long    volume_id;      // STAR volume identification
//        float   de;             // energy deposition at hit
//        float   ds;             // path length within padrow
//        float   p[3];           // local momentum
//        float   tof;            // time of flight
//        float   x[3];           // coordinate (Cartesian)
// }
//

//  * tcl_tphit.h *

// typedef struct tcl_tphit_st {
//         long cluster;        // id of a cluster used to reconstruct pnt. 
//         long flag;           // evaluation of the hit quality */
//         long id;             // a unique point id */
//         long id_globtrk;     // id of reconstructed/sim global track */
//         long nseq;           // number of sequences contributing to hit */
//         long row;            // TPC row number */
//         long track;          // id of a track to which the pnt was assgn */
//         float alpha;         // reconstructed crossing angle in xy */
//         float dalpha;        // error on the crossing angle */
//         float dlambda;       // error on the dip angle (degree) */
//         float dq;            // error on the charge */
//         float dx;            // error on the x coordinate */
//         float dy;            // error on the y coordinate */
//         float dz;            // error on the z coordinate */
//         float lambda;        // dip angle (degree) */
//         float phi;           // orientation of the hit w.r.t padplane */
//         float prf;           // value of the pad response (cm) */
//         float q;             // total charge assigned to this point */
//         float x;             // reconstructed x coordinate */
//         float y;             // reconstructed y coordinate */
//         float z;             // reconstructed z coordinate */
//         float zrf;           // value of the drift response (cm) */
// } TCL_TPHIT_ST;


  St_g2t_tpc_hit *pointsGeants = nextGeants.FindObject(tableGeants);
  g2t_tpc_hit_st *pgFirstRow = 0;
  if (pointsGeants) {
     pointsGeants->Print(1,1);    
     pgFirstRow = pointsGeants->GetTable();
  }
  else {
     printf("Error. The table <%s> was not found\n",tableGeants);
     return;
  }

  St_tcl_tphit *pointsHits = nextHits.FindObject(tableHits);
  tcl_tphit_st *hFirstRow = 0;
  if (pointsHits) {
    pointsHits->Print(1,1);    
    hFirstRow = pointsHits->GetTable();
  }
  else {
    printf("Error. The table <%s> was not found\n",tableHits);
    return;
  }


    St_TableSorter *index[2];
    index[0] = new St_TableSorter(*pointsGeants,"track_p");
    index[1] = new St_TableSorter(*pointsHits,"track");

    Float_t  minrange[3] = {0,0,0};
    Float_t  maxrange[3] = {0,0,0};

    Int_t nMarkers = 0;
    TPolyLine3D *track = 0;
    TObjArray tracks;
    Int_t i = 0;
    Int_t ntracks = 0;
    //  const Int_t maxtracks = 30;
    const Int_t maxtracks = 5;
//---------------------------- Fill tracks -------------------
    gBenchmark->Start("Fill time");   
    Int_t j = 0;
    TPolyMarker3D *notTrack=0;
    for (j=0;j<2;j++) 
    {
       Long_t thisTrackId = -1;
       St_TableSorter *ix = index[j];
       for (i=0;i<ix->GetNRows() && i < 5000;i++) 
       {
         Float_t position[3];
         Int_t indx = ix->GetIndex(i);
         Long_t nextTrackId = 0;
         if (j==0)
            nextTrackId =  GeantID(pgFirstRow,indx,position);
         else
            nextTrackId =  HitsID(hFirstRow,indx,position);

//         printf(" Table: %d : %d rows, next track = %d, this track = %d :: %f %f %f \n",
//                j, ix->GetNRows(),nextTrackId, thisTrackId, position[0],position[1],position[2]);
         if (nextTrackId && thisTrackId != nextTrackId) {
           printf("new track ID %d\n",nextTrackId);
           track =  new TPolyLine3D;   
           track->SetLineColor(kYellow+j);
           track->SetLineWidth(5-j);
           tracks.Add(track);
           thisTrackId = nextTrackId;
           ntracks++;
         } else if (nextTrackId == 0)  track =0; 

         if (track)
           track->SetNextPoint(position[0],position[1],position[2]);
         else {
           if (!notTrack) { 
                notTrack =  new TPolyMarker3D(10);
                notTrack->SetMarkerColor(kRed);  
//                notTrack->SetMarkerSize(2);
                notTrack->SetMarkerStyle(20);
                tracks.Add(notTrack);
                nMarkers = 0;
           }
           notTrack->SetNextPoint(position[0],position[1],position[2]);
           nMarkers++;

         }
         // calculate ranges
         for (int jj=0;jj<3;jj++) {
            minrange[jj] = TMath::Min(minrange[jj],position[jj]);
            maxrange[jj] = TMath::Max(maxrange[jj],position[jj]);
         }
       }
     }
     gBenchmark->Stop("Fill time");

   printf(" Now we will try to write out %d tracks and %d markers\n",ntracks,nMarkers);
   TFile star_events_file("starevents.root","RECREATE");
   tracks.Write();
   star_events_file.Close();

   printf(" Now we will try to draw something \n");

   if (!view)   view = new TView(minrange,maxrange,1);
   else         view->SetRange(minrange,maxrange);
   Int_t irep;
  // view->SetView(0,0,90,&irep);
   view->SetView(0.0,90.0,90.0,irep);
//---------------------------- Draw tracks -------------------
  TIter nx(&tracks);

//  while (track = (TPolyLine3D *)nx()) track->Draw();
  tracks.Draw();
  gBenchmark->Stop("Draw time");
  gBenchmark->Summary();
}
