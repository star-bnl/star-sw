 {
   // Copyright(c) 2001 [BNL] Brookhaven National Laboratory, Valeri Fine  (fine@bnl.gov). All right reserved",
   // TGeneric table text macro
    gSystem->Load("libStar");
 
    struct track {
     float  curvature;
     Ptr_t hitList; 
    };
     track t;
    // Create the track table and fill it
    TGenericTable *allTracks = new TGenericTable("track","tracks",20);
    // allTracks->Print();
 }
 
