 {
   // Copyright(c) 2001 [BNL] Brookhaven National Laboratory, Valeri Fine  (fine@bnl.gov). All right reserved",
   //
   // TGenericTable and TTableMap classes test macro
   // ----------------------------------------------
   // This example creates a simple event containing one "track" table and one "hit" table
   // The "track" table is in possession of the "column" with variable length array  referencing
   // the appropriated "hit table rows"
   //
   // Macro:
   //  1. Create an event
   //  2. Write it out
   //  3. Close ROOT file
   //  4. Re-open ROOT file
   //  5. Read the event back
   //  6. Plot a histogram for all hits of the first track (for instance)
   //
   // --------------------------------------------------
   // One needs NO extra share library to be loaded !!!!
   // --------------------------------------------------

   //---------------------------------------------------
   //  1. Load "libStar" share library
   //---------------------------------------------------
    gSystem->Load("libStar");

   //---------------------------------------------------
   //  2. Define the C-structure to describe "track" and "hit"
   //---------------------------------------------------
    struct hit {
     float  energy;     // energy
     int    detectorId; // geometry id
    };
   
   // Pay attention each track element has a pointer to the container
   // of hit-references (watch "hitList" data-member)

    struct track {
     float  curvature;     // curvature
     Ptr_t  hitList;       // the list of this track hits from hit table
    };

   //---------------------------------------------------
   //  3. Create and fill the hit table object first
   //---------------------------------------------------

    TGenericTable *allHits = new TGenericTable("hit","hits",1000);
    hit  a;
    int i = 0;
    for (i=0; i<100; i++) {
	   a.energy = sin(i*0.1);
	   a.detectorId++;
	   allHits->AddAt(&a);
    }
    allHits->Print();
   //---------------------------------------------------
   //  4. Create and fill the track table object
   //---------------------------------------------------
    TGenericTable *allTracks = new TGenericTable("track","tracks",21);
    allTracks->Print();
    track  t;
    int i = 0;
    for (i=0; i<20; i++) {
        t.curvature = gRandom->Rndm();
  	    t.hitList = new TTableMap(allHits);
        // TGenericTable::iterator h     = allHits->begin();
        // TGenericTable::iterator hLast = allHits->end();
        //---------------------------------------------------
        // 5. Simulate track->hit association
        //---------------------------------------------------
        const char *h = allHits->GetArray();
        Int_t len = allHits->GetNRows();
        UInt_t indx;
        for (indx = 0;indx < len; indx++ ) {
          if (gRandom->Rndm() < 0.2 ) 
                   t.hitList.Push_back(indx); 
          //  In compiled code one can use the "regular" vector::push_back(indx)
          //  as follows:
          //         t.hitList.push_back(indx); 
        }
        allTracks->AddAt(&t);
    }
    allTracks->Print(0,5);
   //---------------------------------------------------
   //  6. Create the full event
   //---------------------------------------------------
    TDataSet *event = new TDataSet("event");
    event->Add(allHits);
    event->Add(allTracks);
    event->ls(9);
   //---------------------------------------------------
   //  6. Write the full event out
   //---------------------------------------------------
    TFile ff("generic.root","RECRETE");
    event->Write();
    ff.Write();
    ff.Close();
    printf(" One event has been written out\n");
    //-----------------------------------------------------
    //    Read the event back now
    //-----------------------------------------------------
    printf("\n  ----- \n  Now we will try to read it back\n");
    printf("  ----- \n");

    // delete event;
    TFile newFile("generic.root");
    event = (TDataSet *)newFile.Get("event");
    event->ls(4);
    TGenericTable *rdTracks = (TGenericTable *)event->FindByName("tracks");
    rdTracks->Print(0,5);
    //-----------------------------------------------------
    //    Getting the list of the hits for some 1-st track
    //-----------------------------------------------------
    TH1F *ehist = new TH1F("ehist","Energy deposit",30,-1,1);
    track &firstTrack = *(track *)rdTracks->GetTable();
    TTable::iterator fHit = firstTrack.hitList.Begin();
    TTable::iterator lHit = firstTrack.hitList.End();
    for (;fHit != lHit; fHit++) {
       hit &nextHit = *(hit *)fHit.rowPtr();
       // in compiled code one can apply (operator *())
       // hit &nextHit = *(hit *)*fHit;
       ehist->Fill(nextHit.energy);
    }
    ehist->Draw();
    printf("\n ------------  >> finish !!! << ----------------  \n");
 }
