{

    StTpcHitCollection *t = ((StEvent *)Event)->tpcHitCollection();
    if (!t) {
     printf(" No colection have been found!\n Did you forget to run <EventRead.C> macro first ?\n");
     return;
    }
    TCanvas *m_TreeD = new TCanvas("STAF","Events",10,600,200,200);
    TFile f("/star/u2a/fine/WWW/STAR");
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
    St_Node *thisTrack = 0;
  // Create a shape for this node
     StHits3DPoints hitPoints(t);
     St_PolyLineShape *trackShape  =  new St_PolyLineShape(&hitPoints);
     trackShape->SetVisibility(1);
     trackShape->SetLineColor(kYellow);
     trackShape->SetLineStyle(1);
     trackShape->SetLineWidth(2);
     // Create a node to hold it
     if (!thisTrack) {
         thisTrack = new St_Node("hits","hits",trackShape);
         thisTrack->Mark();
         thisTrack->SetVisibility();
         St_NodePosition *pp = hall->Add(thisTrack);
         if (!pp) printf(" no position %d\n",ntrack);
     }
//_______________________________________

  gBenchmark->Stop("Draw time");
// Select sensors
   fullView = new St_NodeView(*hall); 
   // Create the "open" sub-structure from the full one
   sensible = new St_NodeView(fullView);
   printf(" drawing the STAR geometry sensible volumes and hits \n");
   sensible->Draw();

 //  Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/HitsDrawFullView.gif"> </P> End_Html // 

   gPad->Update();
   // Draw a help for x3d picture
   printf(" drawing the HELP windows\n");
   // === x3dHelpDraw();
   // make simple view

   // Select node to be left
   printf(" Marking the current structure to create another simplified one\n");
   sensible->Mark();                                                      //  Select HALL
   shortView = (St_NodeView *)sensible->FindByName("TPGV");  // Select TPGV
   if (shortView) shortView->Mark();     

   // Select all hits                                              // Select ALL Hits
   St_DataSetIter nextHits(sensible);
   while (shortView = (St_NodeView *) nextHits()) {
      if (strcmp(shortView->GetName(),"ZCAL")==0) continue;       // skip ZCAL detector element
      shortView->Mark();
   }
   
   // Create new short view                                        // New "short" dataset
   printf(" Creating a new structure simplified structure\n");
   shortView = new St_NodeView(sensible);
   TCanvas *m_simpleD = new TCanvas("Detector","Simple view and hits",500,500,400,400);
   shortView->Draw("L");

 //  Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/HitsDrawSimpleView.gif"> </P> End_Html // 

   printf(" Drawing the new structure simplified structure\n");
   gPad->Update();
   gBenchmark->Summary();
   printf(" Drawing 3d solid view of the last structure\n"); 

}
