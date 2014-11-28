//*-- Author :    Valery Fine(fine@bnl.gov)   05/05/99  
//
// Test for the new St_Points3D class family
// Derived from HitsDraw.C macro
//
// begin_html
// Full text of this macro can be downloaded from the repository: <a href = "http://www.star.bnl.gov/cgi-bin/cvsweb.cgi/StRoot/macros/HitsDraw.C">HitsDraw.C</a> 
// end_html
// $Id: HitsDraw.C,v 1.7 1999/11/11 20:28:24 fisyak Exp $
// $Log: HitsDraw.C,v $
// Revision 1.7  1999/11/11 20:28:24  fisyak
// Remove BFC.C
//
// Revision 1.6  1999/07/09 17:54:12  fine
// A example of selecting subdetectors and range was introduced
//
// Revision 1.5  1999/06/03 00:48:59  fine
// New version of HitsDraw
//
// Revision 1.4  1999/04/27 21:42:07  fine
// Web ref. have been added
//
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
class St_g2t_tpc_hit;

// Global variables to access from an interactive session
St_Node     *hall         = 0;
St_NodeView *fullView     = 0;
St_NodeView *shortView    = 0;
St_NodeView *sensible     = 0;
St_NodeView *singleSector = 0;
St_g2t_tpc_hit *points    = 0;
//________________________________________________________________________________
// Subroutine to define the track color
Color_t trackColor(Long_t geantId)
{
  Color_t color = kYellow;
  switch (geantId) {
   case 1:
   case 2:
   case 3:
     color = kRed;
     break;
   case 4:
   case 5:
   case 6:
     color = kGreen;
     break;
   case 7:
   case 8:
   case 9:
     color = kYellow;
     break;
  default:
     color = kBlue;
  };
  return color;
}

//________________________________________________________________________________
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

//________________________________________________________________________________
// Loading the share libraries
void Load() {
  gSystem->Load("St_base");
  gSystem->Load("xdf2root");
  gSystem->Load("St_Tables");
}
//________________________________________________________________________________
// Drawing subroutine
void HitsDraw(){
  TCanvas *m_TreeD = new TCanvas("STAF","Events",10,600,200,200);
  Load();
//  Draw STAR detector first
//  TWebFile f("http://www.star.bnl.gov/~fine/star.root");
//_______________________________________
//
//  reading STAR GEANT geometry database
//_______________________________________

  TFile f("/star/u2a/fine/WWW/star.root");
  // read STAR geometry database remotely
  TGeometry *star = f.Get("STAR");
  if (!star) {
    printf("Sorry, STAR was not found !\n");
    return;
  }
//_______________________________________
//
// Remove hall from the list of ROOT nodes
// to make it free of ROOT control
//_______________________________________
  TList *listOfNode = gGeometry->GetListOfNodes();
  St_Node *hall = listOfNode->First();
  // Remove hall from the list of ROOT nodes to make it free of ROOT control
  listOfNode->Remove(hall);
  listOfNode->Remove(hall);
//_______________________________________
//
// Create an iterator to navigate STAR geometry
//_______________________________________
  St_DataSetIter volume(hall);


//_______________________________________
//
// Read XDF file with some table information
//_______________________________________
  //   St_XDFFile file("/disk00000/star/auau200/hijing135/default/b0_3/year2a/hadronic_on/tss_dst/set184_01_48evts.p1.xdf");
   St_XDFFile file("/disk00000/star/test/dev/trs_Linux/Fri/year_1b/set0352_01_35evts.dst.xdf"); 
  // skip first record 
  St_DataSet *skip = file.NextEventGet();
  if (skip) delete skip;
  // Read the second "real" record
  St_DataSet *muons = file.NextEventGet();
  if (!muons) return;
//_______________________________________
//
// Find in there two kind of the tables we whan to use now
//_______________________________________
  St_DataSetIter next(muons);
  const Char_t *table      = "event/geant/Event/g2t_tpc_hit";
  const Char_t *trackTable = "event/geant/Event/g2t_track";

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
  points = (St_g2t_tpc_hit *)next(table);
  if (!points) {
    next.Du();
    return;
  }
  points->Print(0,10);
  g2t_tpc_hit_st *p = 0;
  if (points) 
     g2t_tpc_hit_st *p = points->GetTable();
  else {
     printf("Error. The table <%s> was not found\n",table);
     return;
  }

//  struct g2t_track_st {
//        . . . .
//	long ge_pid; /* GEANT particle id */
//        . . . .
//} G2T_TRACK_ST;

  St_g2t_track *trackT = (St_g2t_track *)next(trackTable);
  trackT->Print(0,10);
  g2t_track_st *geTrack = trackT->GetTable(); 

//_______________________________________
//
// The table was found, let's create 3D viewer for that
//
//   We want to see all x[0],x[1],x[2] columns of each row
//   with one and the same value of "track_id" columns as one
//   3D objects.
//
//   One objects means all part (points) of this simgle object
//   will have one and same 3D attributes: color, size, style etc
//_______________________________________
  St_Table3Points *track = 0;
  TString tr;
  tr = "track_p";
  St_Table &ttt = *((St_Table *)points);
  // Track2Line MUST be on heap othwesie 3D view will crash just code leaves this
  // subroutine
  St_TableSorter *Track2Line = new St_TableSorter (ttt,tr);

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
  St_Node *thisTrack[7] = {0,0,0,0,0,0,0}; // seven noded for 7 different colors
  Int_t  MaxRowtoCount = 5000; // 5000;
  Int_t  MaxTracks = Track2Line->CountKeys();
  printf(" Total tracks here %d \n",MaxTracks);
  MaxTracks = 100;
// Create one "dummy" node to hold all tracks
  St_Node *allTracks = new St_Node(".hits","Global Hits", (TShape *)0);
  if (!allTracks) { printf("Bug !!!\n"); return; }
  allTracks->Mark();
  allTracks->SetVisibility();
  hall->Add(allTracks);
  for (i=0;i<Track2Line->GetNRows() && ntracks <  MaxTracks ;i++) 
  {
   hitPoint = p + Track2Line->GetIndex(i);
   newId =  hitPoint->track_p;

   if (newId != currentId)  {
     printf(".");
     const Char_t *xName = "x[0]";
     const Char_t *yName = "x[1]";
     const Char_t *zName = "x[2]";
     track =  new St_Table3Points(Track2Line,(const void *)&newId,xName,yName,zName);

     // Create a shape for this node
     St_PolyLineShape *trackShape  =  new St_PolyLineShape(track);
     trackShape->SetVisibility(1);
     Int_t colorIndx = ntracks%7;
//     trackShape->SetLineColor(trackColor(geTrack[newId]->ge_pid));
     trackShape->SetColorAttribute(colorIndx+kGreen);
     trackShape->SetLineStyle(1);   trackShape->SetSizeAttribute(2);
     // Create a node to hold it
     if (!thisTrack[colorIndx])  {
         thisTrack[colorIndx] = new St_Node("hits","hits",trackShape);
         thisTrack[colorIndx]->Mark();  thisTrack[colorIndx]->SetVisibility();
         St_NodePosition *pp = allTracks->Add(thisTrack[colorIndx]);
         if (!pp) printf(" no position %d\n",ntrack);
     }
     else 
       thisTrack[colorIndx]->Add(trackShape);
     currentId = newId;
     ntracks++;
   }  
 }
  printf("\n");
  printf("%d tracks have been read\n",ntracks);
  gBenchmark->Stop("Fill time");

   printf(" Now we will try to draw something \n");
   Int_t irep;
  gBenchmark->Stop("Draw time");
// Select sensors
   ((St_Node *)volume.FindByName("ZCAL"))->SetVisibility(0);
   ((St_Node *)volume.FindByName("QDIV"))->SetVisibility(0);
   fullView = new St_NodeView(*hall); 
   // Create the "open" sub-structure from the full one
   sensible = new St_NodeView(fullView);
   printf(" drawing the STAR geometry sensible volumes and hits \n");
   sensible->Draw();

 //  Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/HitsDrawFullView.gif"> </P> End_Html // 

   gPad->Update();
   // Draw a help for x3d picture
   printf(" drawing the HELP windows\n");
//   x3dHelpDraw();
   // make simple view

   // Select node to be left
   printf(" Marking the current structure to create another simplified one\n");
   sensible->Mark();                                                      //  Select HALL
   shortView = (St_NodeView *)sensible->FindByName("TPGV");  // Select TPGV
   if (shortView) shortView->Mark();     

   // Select all hits                                              // Select ALL Hits
   sensible->FindByName(".hits")->Mark();
   St_DataSetIter nextHits(sensible->FindByName(".hits"));
   while (shortView = (St_NodeView *) nextHits())  shortView->Mark();
   
   nextHits.Reset(sensible);   
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
//==   m_simpleD->x3d();

 //  Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/HitsDrawX3D.gif"> </P> End_Html // 

  printf(" The following global variables are available:\n");
  printf("\tSt_Node     *hall      -  \"closed\" full GEANT geometry structure\n");
  printf("\tSt_NodeView *fullView  -  \"open\"   full GEANT geometry structure\n");
  printf("\tSt_NodeView *sensible  - all \"sensible\" detector elements of the full GEANT structure\n");
  printf("\tSt_NodeView *shortView - subset of the structure above\n");

  // Draw hits and one sector only   
  St_NodeView *singleSector = new St_NodeView(fullView,".hits","TPSS");
  St_NodeView *sector = (St_NodeView *)singleSector->FindByName("TPSS");
  sector->SetVisibility(2);  // This node is invisibible but sons
  TCanvas *ccc = new TCanvas("STARTPC","Sectors",500,10,200,200);
  ccc.cd();
  singleSector->Draw();
  gPad->Update();
  //  Set a new range 
#if 1
  Float_t min[3];
  Float_t max[3];
  TVirtualPad *thisPad = gPad;
  TView *view = thisPad->GetView(); 
  sector->GetGlobalRange(singleSector,min,max);
  view->SetRange(min,max);
  // Update the last picture within the range provided
  thisPad->Modified();
  thisPad->Update();
#endif
 
}
