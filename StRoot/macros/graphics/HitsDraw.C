//*-- Author :    Valery Fine(fine@bnl.gov)   23/04/99  
// $Id: HitsDraw.C,v 1.3 1999/04/27 21:32:03 fine Exp $
// $Log: HitsDraw.C,v $
// Revision 1.3  1999/04/27 21:32:03  fine
// some new comments and clean up
//
// Revision 1.2  1999/04/27 15:29:58  fine
// Make up plus Polyline was replaced by PolyMarker3D
//
// Revision 1.1  1999/04/23 22:42:59  fine
// How to draw Geometry and hits all together
//
// Forward declarations
class St_Node;
class St_NodeView;

// Global variables to access from an interactive session
St_Node     *hall      = 0;
St_NodeView *fullView  = 0;
St_NodeView *shortView = 0;
St_NodeView *sensible  = 0;

// Subroutine to draw a separate "HELP" TCanvas object
void x3dHelpDraw(){
 TCanvas *paveCanvas =  new TCanvas("x3d","X3D Help",500,20,540,220);
 TPaveText *label  = new TPaveText(0,0,1,1);
 const Char_t *myMessage[] = {"Plot  3D view"
       ,"Note: Under Windows NT OpenGL will be used by default instead x3d for UNIX"
       ,"Select x3d windows and type \"m\" in there to get x3d HELP windows"
       ,"DON NOT FORGET to type \"q\" letter to terminate x3d widget"
       ,"to continue this session"};

 Int_t lenMessage = 0;
 lenMessage = sizeof(myMessage)/4;
 Int_t j;
 label->Draw();
 for (j=lenMessage-1;j>0;j--) {
   label->InsertText(myMessage[j]);
   gPad->Update();
 }
 paveCanvas->Modified();
 paveCanvas->Update();
}

// Loading the share libraries
void Load() {
  gSystem->Load("St_base");
  gSystem->Load("xdf2root");
  gSystem->Load("St_Tables");
}
// Drawing subroutine
void HitsDraw(){
  TCanvas *m_TreeD = new TCanvas("STAF","Events",10,600,200,200);
  Load();
//  Draw STAR detector first
  TWebFile f("http://www.star.bnl.gov/~fine/star.root");
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

//   TView *view = m_TreeD->GetView();

   St_XDFFile file("/disk00000/star/auau200/hijing135/default/b0_3/year2a/hadronic_on/tss_dst/set184_01_48evts.p1.xdf");
  // skip first record
 
  St_DataSet *skip = file.NextEventGet();
  if (skip) delete skip;
  St_DataSet *muons = file.NextEventGet();
  if (!muons) return;
  muons->ls(2);
  St_DataSetIter next(muons);
  const Char_t *table = "event/geant/Event/g2t_tpc_hit";
  next.ls(table);
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
  St_g2t_tpc_hit *points = (St_g2t_tpc_hit *)next(table);
  g2t_tpc_hit_st *p = 0;
  if (points) 
     g2t_tpc_hit_st *p = points->GetTable();
  else {
     printf("Error. The table <%s> was not found\n",table);
     return;
  }

//  Float_t  minrange[3] = {0,0,0};
//  Float_t  maxrange[3] = {0,0,0};

//  TPolyLine3D *track = 0;
  TPolyMarker3D *track = 0;
  TString tr;
  tr = "track_p";
  St_Table &ttt = *((St_Table *)points);
  ttt.ls();
  St_TableSorter Track2Line(ttt,tr);

  Int_t i = 0;
  Char_t buffer[10];
  Int_t ntracks = 0;
//  const Int_t maxtracks = 30;
  const Int_t maxtracks = 5;
//---------------------------- Fill tracks -------------------
  gBenchmark->Start("Fill time");
  // Get Cave
  long currentId = -1;
  long newId =  0;
  g2t_tpc_hit_st *hitPoint = 0;
  printf(" new track found:");
  for (i=0;i<Track2Line.GetNRows() && i < 5000;i++) 
  {
   hitPoint = p + Track2Line.GetIndex(i);
   newId =  hitPoint->track_p;

   if (newId != currentId)  {
     printf(".");
//     track =  new TPolyLine3D;   
     track =  new TPolyMarker3D;   
//     track->SetLineColor(kYellow);
     track->SetMarkerColor(kYellow);
//     track->SetLineWidth(5);
//     track->SetLineWidth(2);
     track->SetMarkerSize(1);
     track->SetMarkerStyle(7);
     // Create a shape for this node
     St_PolyLineShape *trackShape  =  new St_PolyLineShape(track);
     trackShape->SetVisibility(1);
     // Create a node to hold it
     St_Node *thisTrack = new St_Node("hits","hits",trackShape);
     thisTrack->Mark();
     thisTrack->SetVisibility();
     currentId = newId;
     St_NodePosition *pp = hall->Add(thisTrack);
     ntracks++;
     if (!pp) printf(" no position %d\n",ntrack);
   }

   if (track) {
      track->SetNextPoint(hitPoint->x[0],hitPoint->x[1],hitPoint->x[2]);
     // calculate ranges
     //   for (int jj=0;jj<3;jj++) {
     //     minrange[jj] = TMath::Min(minrange[jj],hitPoint->x[jj]);
     //     maxrange[jj] = TMath::Max(maxrange[jj],hitPoint->x[jj]);
     //   }
   }
   else if (ntracks < maxtracks)
     printf("\nerror. There is no place for the track\n");
 }
  printf("\n");
  gBenchmark->Stop("Fill time");

   printf(" Now we will try to draw something \n");
//---------------------------- Draw tracks -------------------
//   TCanvas *m_TreeD = new TCanvas("STAF","Events",200,200,600,600);
//   m_TreeD->SetTheta(90.0);
//   m_TreeD->SetPhi(90.0);

   // creating a view
//   minrange[1]  = -5.0;
//   if (!view)   view = new TView(minrange,maxrange,1);
////   else         view->SetRange(minrange,maxrange);
   Int_t irep;
// view->SetView(0,0,90,&irep);
// view->SetView(0.0,90.0,90.0,irep);

//       if (trackview) {
//         trackview->SetShape(0);
//         trackview->SetSmooth(kTRUE);
//         trackview->Draw();
//       }
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
   x3dHelpDraw();
   // make simple view

   // Select node to be left
   printf(" Marking the current strucutre to create another simplified one\n");
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
   shortView->Draw();

 //  Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/HitsDrawSimpleView.gif"> </P> End_Html // 

   printf(" Drawing the new structure simplified structure\n");
   gPad->Update();
   gBenchmark->Summary();
   printf(" Drawing 3d solid view of the last structure\n");
   m_simpleD->x3d();

 //  Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/HitsDrawX3D.gif"> </P> End_Html // 

  printf(" The following global variables are available:\n");
  printf("\tSt_Node     *hall      -  \"closed\" full GEANT geometry structure\n");
  printf("\tSt_NodeView *fullView  -  \"open\"   full GEANT geometry structure\n");
  printf("\tSt_NodeView *sensible  - all \"sensible\" detector elements of the full GEANT structure\n");
  printf("\tSt_NodeView *shortView - subset of the structure above\n");
}
