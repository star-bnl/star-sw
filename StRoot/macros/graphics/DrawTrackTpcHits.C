//*-- Author :    Valery Fine   25/05/99  (E-mail: fine@bnl.gov)
// $Id: DrawTrackTpcHits.C,v 1.3 1999/06/27 22:45:34 fisyak Exp $
// $Log: DrawTrackTpcHits.C,v $
// Revision 1.3  1999/06/27 22:45:34  fisyak
// Merge StRootEvent and StEvent
//
// Revision 1.2  1999/06/24 16:49:00  fine
// StTrack.length method replaced the 120.0 cm constant
//
// Revision 1.1  1999/05/29 20:56:17  fine
//  An example of 3D representation of StEventStGlobalTrack and StHits objects
//
{
//_______________________________________
//
//  reading STAR GEANT geometry database
//_______________________________________
    TCanvas *m_TreeD = new TCanvas("STAF","Events",10,600,200,200);
    TFile f("/star/u2a/fine/WWW/star.root");
    //    TWebFile f("http://www.star.bnl.gov/~fine/star.root");
    // read STAR geometry database remotely
    TGeometry *star = f.Get("STAR");
    if (!star) {
      printf("Sorry, STAR was not found !\n");
      return;
    }
    TList *listOfNode = star->GetListOfNodes();
    St_Node *hall = listOfNode->First();
    // Remove hall from the kist of ROOT nodes to make it free of ROOT control
    listOfNode->Remove(hall);
    listOfNode->Remove(hall);
    St_DataSetIter volume(hall);

//_______________________________________
//
//   Creating tracks and hits shapes
//_______________________________________
    St_Node *thisTrack = 0;
//    StTpcHitCollection *t   = ((StEvent *)Event)->tpcHitCollection();
    StTrackCollection *tracks = ((StEvent *)Event)->trackCollection();
    if (tracks) {
      StGlobalTrackIterator next(tracks);
      StGlobalTrack *globTrack = 0;
      Int_t trackCounter = 0;
      while (globTrack = (StGlobalTrack *)next() && trackCounter < 50) {
         printf ("Track %d\n",trackCounter);
         StVecPtrTpcHit *hits = globTrack->tpcHits();
         StHits3DPoints *hitPoints     = new StHits3DPoints(hits);
         StHelix3DPoints *helixPoints  = new StHelix3DPoints(globTrack,globTrack->length(),30);
         if (hitPoints->Size()>1) {
           St_PolyLineShape *trackShape  = new St_PolyLineShape(hitPoints);
           trackShape->SetVisibility(1); trackShape->SetColorAttribute(kYellow);
           trackShape->SetLineStyle(1);  trackShape->SetSizeAttribute(1);
         } 
         Int_t colorIndx = trackCounter%7;
         St_PolyLineShape *helixShape  = new St_PolyLineShape(helixPoints,"L");
           helixShape->SetVisibility(1); helixShape->SetColorAttribute(colorIndx+kGreen);
           helixShape->SetLineStyle(1);  helixShape->SetSizeAttribute(2);

         thisTrack = new St_Node("hits","hits",helixShape);
         thisTrack->Mark(); thisTrack->SetVisibility();

         thisTrack->Add(trackShape);
         St_NodePosition *pp = hall->Add(thisTrack); 
         trackCounter++;
         if (!pp) 
            printf(" no position %d\n",trackCounter);
      }
      printf(" %d tracks has been found\n",trackCounter);
    }
//    hall->ls(2);
//_______________________________________

  gBenchmark->Stop("Draw time");
// Select sensors
   fullView = new St_NodeView(*hall); 
   // Create the "open" sub-structure from the full one
   sensible = new St_NodeView(fullView);
   printf(" drawing the STAR geometry sensible volumes and hits \n");
 //====  sensible->Draw();

//  Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/HitsDrawFullView.gif"> </P> End_Html // 

//===   gPad->Update();

//_______________________________________
//
//   Make simple view
//   Select node to be left
//_______________________________________
   printf(" Marking the current structure to create another simplified one\n");
   sensible->Mark();                                              // Select HALL
   shortView = (St_NodeView *)sensible->FindByName("TPGV");       // Select TPGV
   if (shortView) shortView->Mark();     

//_______________________________________
//
//    Select all hits
//_______________________________________
   St_DataSetIter nextHits(sensible);
   while (shortView = (St_NodeView *) nextHits()) {
      if (strcmp(shortView->GetName(),"ZCAL")==0) continue;       // skip ZCAL detector element
      shortView->Mark();
   }
   
//_______________________________________
//
//   Create new short view                                        // New "short" dataset
//_______________________________________
   printf(" Creating a new structure simplified structure\n");
   shortView = new St_NodeView(sensible);
   TCanvas *m_simpleD = new TCanvas("Detector","Simple view and hits",500,500,400,400);
   shortView->Draw("L");

 //  Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/HitsDrawSimpleView.gif"> </P> End_Html // 

//_______________________________________
//
//   Final comments
//_______________________________________
   printf(" Drawing the new structure simplified structure\n");
   gPad->Update();
   gBenchmark->Summary();
   printf(" Drawing 3d solid view of the last structure\n"); 

}
