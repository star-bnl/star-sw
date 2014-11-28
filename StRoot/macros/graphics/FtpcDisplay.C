#include "TCanvas.h"
#include "TNode.h"
#include "TTUBE.h"
#include "TTUBS.h"
#include "TBRIK.h"
#include "TFile.h"
#include "TPolyMarker3D.h"
#include "TPolyLine3D.h"
#include "TControlBar.h"
#include <iostream.h> 

Int_t openFile(Char_t *name);
void drawControlPanel(Int_t tracks, Int_t found, Int_t unused);
void showNode();
void deleteNode();
void deleteAxis();
void showFoundCluster(Bool_t clusters);
void showUnusedCluster(Bool_t clusters);
void showTracks(Bool_t tracks);
void createDependencies();
void toggleFoundClusterVisibility();
void toggleUnusedClusterVisibility();
void toggleTrackVisibility();
void toggleAxisVisibility();
void toggleFrameVisibility();
void toggleDeadAreaVisibility();
void togglePadVisibility();
void detectorOn();
void detectorOff();
void measurementsOn();
void measurementsOff();
void setFrameVisibility();
void setPadVisibility();
void setDeadAreaVisibility();
void setDefaults();
void defineDefaults();
void toggleFrameDensity();
void setFrameDensity();
void clear();
void show(Int_t s, Bool_t erase = kTRUE);
void showAll();
void showEast();
void showWest();
void showBoth();
void showAxis();
void generateAxis(Float_t arrowSize);
void closeX3d();
void quit();

TBRIK *origin;
TTUBE *ftpc1_out, *ftpc2_out, *ftpc1_in, *ftpc2_in;
TTUBS *dead_seg1, *dead_seg2, *dead_seg3, *dead_seg4, *dead_seg5, *dead_seg6;
TTUBS *padrow1, *padrow2, *padrow3, *padrow4, *padrow5, *padrow6;
TNode *node0,  *node1, *node2, *node01_out, *node01_in, *node02_out, *node02_in, *node1_out, *node1_in, *node2_out, *node2_in;
TNode *node01_seg1, *node01_seg2, *node01_seg3, *node01_seg4, *node01_seg5, *node01_seg6;
TNode *node02_seg1, *node02_seg2, *node02_seg3, *node02_seg4, *node02_seg5, *node02_seg6;
TNode *node1_seg1, *node1_seg2, *node1_seg3, *node1_seg4, *node1_seg5, *node1_seg6;
TNode *node2_seg1, *node2_seg2, *node2_seg3, *node2_seg4, *node2_seg5, *node2_seg6;
TNode *node01_pad01, *node01_pad11, *node01_pad21, *node01_pad31, *node01_pad41, *node01_pad51, *node01_pad61, *node01_pad71, *node01_pad81, *node01_pad91; 
TNode *node01_pad02, *node01_pad12, *node01_pad22, *node01_pad32, *node01_pad42, *node01_pad52, *node01_pad62, *node01_pad72, *node01_pad82, *node01_pad92; 
TNode *node01_pad03, *node01_pad13, *node01_pad23, *node01_pad33, *node01_pad43, *node01_pad53, *node01_pad63, *node01_pad73, *node01_pad83, *node01_pad93; 
TNode *node01_pad04, *node01_pad14, *node01_pad24, *node01_pad34, *node01_pad44, *node01_pad54, *node01_pad64, *node01_pad74, *node01_pad84, *node01_pad94; 
TNode *node01_pad05, *node01_pad15, *node01_pad25, *node01_pad35, *node01_pad45, *node01_pad55, *node01_pad65, *node01_pad75, *node01_pad85, *node01_pad95; 
TNode *node01_pad06, *node01_pad16, *node01_pad26, *node01_pad36, *node01_pad46, *node01_pad56, *node01_pad66, *node01_pad76, *node01_pad86, *node01_pad96; 
TNode *node02_pad01, *node02_pad11, *node02_pad21, *node02_pad31, *node02_pad41, *node02_pad51, *node02_pad61, *node02_pad71, *node02_pad81, *node02_pad91; 
TNode *node02_pad02, *node02_pad12, *node02_pad22, *node02_pad32, *node02_pad42, *node02_pad52, *node02_pad62, *node02_pad72, *node02_pad82, *node02_pad92; 
TNode *node02_pad03, *node02_pad13, *node02_pad23, *node02_pad33, *node02_pad43, *node02_pad53, *node02_pad63, *node02_pad73, *node02_pad83, *node02_pad93; 
TNode *node02_pad04, *node02_pad14, *node02_pad24, *node02_pad34, *node02_pad44, *node02_pad54, *node02_pad64, *node02_pad74, *node02_pad84, *node02_pad94; 
TNode *node02_pad05, *node02_pad15, *node02_pad25, *node02_pad35, *node02_pad45, *node02_pad55, *node02_pad65, *node02_pad75, *node02_pad85, *node02_pad95; 
TNode *node02_pad06, *node02_pad16, *node02_pad26, *node02_pad36, *node02_pad46, *node02_pad56, *node02_pad66, *node02_pad76, *node02_pad86, *node02_pad96; 
TNode *node1_pad01, *node1_pad11, *node1_pad21, *node1_pad31, *node1_pad41, *node1_pad51, *node1_pad61, *node1_pad71, *node1_pad81, *node1_pad91; 
TNode *node1_pad02, *node1_pad12, *node1_pad22, *node1_pad32, *node1_pad42, *node1_pad52, *node1_pad62, *node1_pad72, *node1_pad82, *node1_pad92; 
TNode *node1_pad03, *node1_pad13, *node1_pad23, *node1_pad33, *node1_pad43, *node1_pad53, *node1_pad63, *node1_pad73, *node1_pad83, *node1_pad93; 
TNode *node1_pad04, *node1_pad14, *node1_pad24, *node1_pad34, *node1_pad44, *node1_pad54, *node1_pad64, *node1_pad74, *node1_pad84, *node1_pad94; 
TNode *node1_pad05, *node1_pad15, *node1_pad25, *node1_pad35, *node1_pad45, *node1_pad55, *node1_pad65, *node1_pad75, *node1_pad85, *node1_pad95; 
TNode *node1_pad06, *node1_pad16, *node1_pad26, *node1_pad36, *node1_pad46, *node1_pad56, *node1_pad66, *node1_pad76, *node1_pad86, *node1_pad96; 
TNode *node2_pad01, *node2_pad11, *node2_pad21, *node2_pad31, *node2_pad41, *node2_pad51, *node2_pad61, *node2_pad71, *node2_pad81, *node2_pad91; 
TNode *node2_pad02, *node2_pad12, *node2_pad22, *node2_pad32, *node2_pad42, *node2_pad52, *node2_pad62, *node2_pad72, *node2_pad82, *node2_pad92; 
TNode *node2_pad03, *node2_pad13, *node2_pad23, *node2_pad33, *node2_pad43, *node2_pad53, *node2_pad63, *node2_pad73, *node2_pad83, *node2_pad93; 
TNode *node2_pad04, *node2_pad14, *node2_pad24, *node2_pad34, *node2_pad44, *node2_pad54, *node2_pad64, *node2_pad74, *node2_pad84, *node2_pad94; 
TNode *node2_pad05, *node2_pad15, *node2_pad25, *node2_pad35, *node2_pad45, *node2_pad55, *node2_pad65, *node2_pad75, *node2_pad85, *node2_pad95; 
TNode *node2_pad06, *node2_pad16, *node2_pad26, *node2_pad36, *node2_pad46, *node2_pad56, *node2_pad66, *node2_pad76, *node2_pad86, *node2_pad96; 

Int_t side, def_side = 1;
Bool_t axis, def_axis = kTRUE;
Bool_t pads, def_pads = kFALSE;
Bool_t frame_on, def_frame_on = kTRUE;
Bool_t dead_area, def_dead_area = kFALSE;
Bool_t divisions, def_divisions = kFALSE;
Int_t divi = 50;
Int_t divipad = 16;
Bool_t foundclusters, def_foundclusters = kTRUE;
Bool_t unusedclusters, def_unusedclusters = kTRUE;
Bool_t tracks, def_tracks = kTRUE;

TControlBar *bar;
TCanvas *canvas;
TFile *file;
Int_t tr, fo, un;
TPolyLine3D *lx[3], *ly[3], *lz[3];


Int_t FtpcDisplay(Char_t *filename = 0)
{
  // Display for ftpc track reconstruction. It needs an input file generated by StFtpcDisplay.
  // This you get by uncommenting the 'Track Display' block in StFtpcTrackmaker.cxx.
  
  if (openFile(filename) == 0) {
    
    // create 3 canvases (for +, -, and both Ftpcs)
    canvas = new TCanvas("canvas", "3D views of FTPC", 300, 100);
    canvas->Divide(3,1);
    
    // create point of origin (our vertex has the shape of a cube, of course)
    origin = new TBRIK("origin", "origin", "void", 0.1, 0.1, 0.1);
    
    // create 3 nodes (+, -, both) to generate the dependencies of the different geometric shapes
    node0 = new TNode("node0", "node0", "origin");
    node1 = new TNode("node1", "node1", "origin");
    node2 = new TNode("node2", "node2", "origin");
    
    generateAxis(70.);
    
    setDefaults();
    showAll();
    drawControlPanel(tr, fo, un);
    
    return 0;
  }
  
  else {
    return -1; 
  }
  
  return 0;
}

void setDefaults() {
  // Set default settings.

  axis = def_axis;
  
  frame_on = def_frame_on;
  dead_area = def_dead_area;
  pads = def_pads;
  divisions = def_divisions;

  tracks = def_tracks;
  unusedclusters = def_unusedclusters;
  foundclusters = def_foundclusters;

  side = def_side;
}

void defineDefaults() {
  // Define default settings.

  def_axis = axis;
  
  def_frame_on = frame_on;
  def_dead_area = dead_area;
  def_pads = pads;
  def_divisions = divisions;

  def_tracks = tracks;
  def_unusedclusters = unusedclusters;
  def_foundclusters = foundclusters;

  def_side = side;
} 

Int_t openFile(Char_t *name) {
  // Opens the file with the given name or "ftpc_display.root" by default.
  
  if (!name) {
    file = new TFile("ftpc_display.root", "READ");
    
    if (!file->IsOpen()) {
      cout << "Sorry, file (ftpc_display.root) not found!" << endl;
      return -1;
    }
  }
  
  else {
    file = new TFile(name, "READ");
    
    if (!file->IsOpen()) {
      cout << "Sorry, file (" << name << ") not found!" << endl;
      return -1;
    }
  }
  
  return 0;
}

void showNode() {
  // Draws nodes in correct pads.
  
  canvas->cd(1);
  node0->cd();
  node0->Draw("same");
  canvas->cd(2);
  node2->cd();
  node2->Draw("same");
  canvas->cd(3);
  node1->cd();
  node1->Draw("same");
}

void showAxis() {
  // Draws axis.
  
  Int_t s;

  if (axis) {

    for (s = 0; s < 3; s++) {
      canvas->cd(s+1);
      lx[s]->Draw("same");
      ly[s]->Draw("same");
      lz[s]->Draw("same");
    }
  }
}

void showTracks(Bool_t tracks) {
  // Shows tracks if 'tracks' is true.
  
  if (tracks) {
    TPolyLine3D *l;
    Int_t l_count = 1;
    Char_t name[20];
    sprintf(name, "TPolyLine3D;%d", l_count);
    
    while ((l = (TPolyLine3D *)file->Get(name))) {
      Float_t *coord = l->GetP();
      
      if (coord[2] > 0.) {
	canvas->cd(3);
      }
      
      else {
	canvas->cd(2);
      }
      
      l->Draw("same");
      canvas->cd(1);
      l->Draw("same");
      
      sprintf(name, "TPolyLine3D;%d", ++l_count);
    }
    
    tr = l_count - 1;
  }
}

void showFoundCluster(Bool_t clusters) {
  // Shows found clusters if 'clusters' is true.
  
  if (clusters) {
    TPolyMarker3D *p;
    
    p = (TPolyMarker3D *)file->Get("TPolyMarker3D;2");
    
    if (p->Size() > 1) { // to avoid crashes due to ROOT problem
      canvas->cd(3);
      p->Draw("same");
    }
    
    p = (TPolyMarker3D *)file->Get("TPolyMarker3D;3");
    
    if (p->Size() > 1) { // to avoid crashes due to ROOT problem
      canvas->cd(2);
      p->Draw("same");
    }

    p = (TPolyMarker3D *)file->Get("TPolyMarker3D;1");
    
    if (p->Size() > 1) { // to avoid crashes due to ROOT problem
      canvas->cd(1);
      p->Draw("same");
    }
    
    fo = p->Size();
  }
}

void showUnusedCluster(Bool_t clusters) {
  // Shows unused clusters if 'clusters' is true.
  
  if (clusters) {
    TPolyMarker3D *p;
    
    p = (TPolyMarker3D *)file->Get("TPolyMarker3D;5");
    
    if (p->Size() > 1) { // to avoid crashes due to ROOT problem
      canvas->cd(3);
      p->Draw("same");
    }
    
    p = (TPolyMarker3D *)file->Get("TPolyMarker3D;6");

    if (p->Size() > 1) { // to avoid crashes due to ROOT problem
      canvas->cd(2);
      p->Draw("same");
    }
    
    p = (TPolyMarker3D *)file->Get("TPolyMarker3D;4");
    
    if (p->Size() > 1) { // to avoid crashes due to ROOT problem
      canvas->cd(1);
      p->Draw("same");
    }
    
    un = p->Size();
  }
}

void deleteAxis() {
  // Deletes axis.
  
  for (Int_t i = 0; i < 3; i++) {
    delete lx[i];
    delete ly[i];
    delete lz[i];
  }
}

void toggleAxisVisibility() {
  // Switchs axis visibility.
  
  axis = !axis;
  clear();
  showAll();
}

void toggleFoundClusterVisibility() {
  // Switchs found cluster visibility.
  
  foundclusters = !foundclusters;
  clear();
  showAll();
}

void toggleUnusedClusterVisibility() {
  // Switchs unused cluster visibility.
  
  unusedclusters = !unusedclusters;
  clear();
  showAll();
}

void toggleTrackVisibility() {
  // Switchs track visibility.
  
  tracks = !tracks;
  clear();
  showAll();
}

void detectorOn() {
  // Switchs complete detector to visibile.

  if (!frame_on) {
    frame_on = kTRUE;
    //setFrameVisibility();
  }
  
  if (!pads) {
    pads = kTRUE;
    //setPadVisibility();
  }
  
  if (!dead_area) {
    dead_area = kTRUE;
    //setDeadAreaVisibility();
  }
  
  clear();
  showAll();
}

void detectorOff() {
  // Switchs complete detector to visibile.

  if (frame_on) {
    frame_on = kFALSE;
    //setFrameVisibility();
  }
  
  if (pads) {
    pads = kFALSE;
    //setPadVisibility();
  }
  
  if (dead_area) {
    dead_area = kFALSE;
    //setDeadAreaVisibility();
  }
  
  clear();
  showAll();
}

void measurementsOn() {
  // Switchs all clusters and tracks on.

  if (!tracks) tracks = kTRUE;
  if (!unusedclusters) unusedclusters = kTRUE;
  if (!foundclusters) foundclusters = kTRUE;
  
  clear();
  showAll();
}

void measurementsOff() {
  // Switchs all clusters and tracks off.

  if (tracks) tracks = kFALSE;
  if (unusedclusters) unusedclusters = kFALSE;
  if (foundclusters) foundclusters = kFALSE;
  
  clear();
  showAll();
}

void showAll() {
  // Shows all stuff in correct pads.
  
  createDependencies();
  setFrameDensity();
  setFrameVisibility();
  setDeadAreaVisibility();
  setPadVisibility();
  showNode();
  showTracks(tracks);
  showFoundCluster(foundclusters);
  showUnusedCluster(unusedclusters);
  showAxis();
  canvas->Update();
  show(side, kFALSE);
}

void clear() {
  // Closes x3d view and clears canvas.
  
  closeX3d();
  canvas->Clear();
  deleteNode();
  canvas->Divide(3,1);
}

void toggleFrameVisibility() {
  // Switchs FTPC frame visibility.
  
  frame_on = !frame_on;
  setFrameVisibility();
  show(side);
}

void toggleDeadAreaVisibility() {
  // Switchs FTPC dead area visibility.
  
  dead_area = !dead_area;
  setDeadAreaVisibility();
  show(side);
}

void togglePadVisibility() {
  // Switchs FTPC pad visibility.
  
  pads = !pads;
  setPadVisibility();
  show(side);
}

void setFrameVisibility() {
  // Sets frame visibility.
  
  ftpc1_out->SetVisibility(frame_on);
  ftpc2_out->SetVisibility(frame_on);
  ftpc1_in->SetVisibility(frame_on);
  ftpc2_in->SetVisibility(frame_on);
}

void setDeadAreaVisibility() {
  // Sets dead area visibility.
  
  dead_seg1->SetVisibility(dead_area);
  dead_seg2->SetVisibility(dead_area);
  dead_seg3->SetVisibility(dead_area);
  dead_seg4->SetVisibility(dead_area);
  dead_seg5->SetVisibility(dead_area);
  dead_seg6->SetVisibility(dead_area);
}

void setPadVisibility() {
  // Sets pad visibility.
  
  padrow1->SetVisibility(pads);
  padrow2->SetVisibility(pads);
  padrow3->SetVisibility(pads);
  padrow4->SetVisibility(pads);
  padrow5->SetVisibility(pads);
  padrow6->SetVisibility(pads);
}

void toggleFrameDensity() {
  // Switchs number of divisions of FTPC frames.
  
  if (divisions) {
    divisions = !divisions;
    divi = 50;
    divipad = 16;
  }
  
  else {
    divisions = !divisions;
    divi = 250;
    divipad = 160;
  }
  
  setFrameDensity();
  show(side);  
}

void setFrameDensity() {
  // Sets number of divisions of FTPC frames.
  
  ftpc1_out->SetNumberOfDivisions(divi);
  ftpc2_out->SetNumberOfDivisions(divi);
  ftpc1_in->SetNumberOfDivisions(divi);
  ftpc2_in->SetNumberOfDivisions(divi);

  dead_seg1->SetNumberOfDivisions(1);
  dead_seg2->SetNumberOfDivisions(1);
  dead_seg3->SetNumberOfDivisions(1);
  dead_seg4->SetNumberOfDivisions(1);
  dead_seg5->SetNumberOfDivisions(1);
  dead_seg6->SetNumberOfDivisions(1);

  padrow1->SetNumberOfDivisions(divipad);
  padrow2->SetNumberOfDivisions(divipad);
  padrow3->SetNumberOfDivisions(divipad);
  padrow4->SetNumberOfDivisions(divipad);
  padrow5->SetNumberOfDivisions(divipad);
  padrow6->SetNumberOfDivisions(divipad);
}

void show(Int_t s, Bool_t erase) {
  // Actual display routine.
  
  if (erase) {
    closeX3d();
  }
  
  canvas->cd(s); 
  gPad->x3d();
  side = s;
}

void showEast() {
  // Shows east FTPC.
  
  if (side != 2) {
    show(2);
  }
}

void showWest() {
  // Shows west FTPC.
  
  if (side != 3) {
    show(3);
  }
}

void showBoth() {
  // Shows both FTPCs.
  
  if (side != 1) {
    show(1);
  }
}

void closeX3d() {
  // Closes x3d view.
  
  gROOT->ProcessLine("R__x3d->CloseWindow()");
}

void quit() {
  // Quits ROOT.
  
  gROOT->ProcessLine(".q");
}

void close() {
  // Closes everything.
  
  closeX3d();
  canvas->Clear();
  deleteNode();
  deleteAxis();
  
  file->Close();
  delete file;
  delete bar;
  delete canvas;
}

void drawControlPanel(Int_t tracks, Int_t found, Int_t unused) {
  // Control panel generation.
  
  Char_t n1[40];
  Char_t n2[40];
  Char_t n3[40];
  
  sprintf(n1, "tracks (%d) [green]", tr);
  sprintf(n2, "clusters on tracks (%d) [red]", fo);
  sprintf(n3, "unused clusters (%d) [yellow]", un);
  
  bar = new TControlBar("vertical", "3D view control bar");
  
  bar->AddButton("show both FTPCs", "showBoth();", "3D view of both FTPCs");
  bar->AddButton("show FTPC east", "showEast();", "3D view of FTPC east");
  bar->AddButton("show FTPC west", "showWest();", "3D view FTPC of west");
  bar->AddButton("ALL MEASUREMENTS ON", "measurementsOn();", "switchs all clusters and tracks on");
  bar->AddButton("ALL MEASUREMENTS OFF", "measurementsOff();", "switchs all clusters and tracks off");  
  bar->AddButton(n1, "toggleTrackVisibility();", "switchs tracks on/off");
  bar->AddButton(n2, "toggleFoundClusterVisibility();", "switchs found clusters on/off");
  bar->AddButton(n3, "toggleUnusedClusterVisibility();", "switchs unused clusters on/off");
  bar->AddButton("DETECTOR ON", "detectorOn();", "switchs complete detector on");
  bar->AddButton("DETECTOR OFF", "detectorOff();", "switchs complete detector off");
  bar->AddButton("frame [blue] visibility", "toggleFrameVisibility();", "switchs frames on/off");
  bar->AddButton("pad [yellow] visibility", "togglePadVisibility();", "switchs pads on/off");
  bar->AddButton("dead segments [red] visibility", "toggleDeadAreaVisibility();", "switchs dead segments on/off");
  bar->AddButton("line density","toggleFrameDensity();", "switchs frame grid density");
  bar->AddButton("axis (x,y,z) = (yellow, green, red)", "toggleAxisVisibility();", "switchs axis visibility");
  bar->AddButton("BACK TO DEFAULT SETTINGS", "setDefaults(); clear(); showAll();", "resets all changes");
  bar->AddButton("set default settings", "defineDefaults();", "defines current settings as default");  
  bar->AddButton("close x3d display", "close();", "closes x3d window and control panel");
  bar->AddButton("quit ROOT", "quit();", "quits ROOT");
  
  bar->Show();
}

void generateAxis(Float_t arrowSize) {
  ///////////////////////////////////////////////////////////////////////////////////////
  //                                                                                   //
  //  generateAxis() draws three axice  Ox,   Oy,    Oz   with three different colors: //
  //                                   red   gree   blue                               //
  //   TVirtualPad - the point to TPad objest these axice will be drawn into           //
  //               = 0 (= gPad, by default)                                            //
  //                                                                                   //
  ///////////////////////////////////////////////////////////////////////////////////////

  
  // 3 options for the origin positions
  //  -  at (0,0,0}
  //  -  left front angle of the view port
  //  -  the center of the view port  
  //       Axis / Cube
  
  //   const Float_t arrowLegthFactor = 0.25;
  const Float_t arrowWidthFactor = 0.25;
  
  Float_t origin[3] = {0,0,0};
  Float_t x = 0;
  Float_t y = 0;
  Float_t z = 0;
  Int_t indx = 0;

  // caclulate an arrow size
  const Float_t arrowHeadLegth = 0.2*arrowSize;
  const Float_t arrWidth       = arrowWidthFactor*arrowHeadLegth;
  //___________  X axis ____________

  for (Int_t ii = 0; ii < 3; ii++) {
    indx = 0;
    
    lx[ii] = new TPolyLine3D(5,"L");
    x = origin[0];
    y = origin[1];
    z = origin[2];
    lx[ii]->SetNextPoint(x,y,z);               // initial point
    x += arrowSize;
    lx[ii]->SetNextPoint(x,y,z);               //     --------
    //____________ x_head ____________
    x -= arrowHeadLegth;
    y += arrWidth;                        //            
    lx[ii]->SetNextPoint(x,y,z);                //     --------
    x = origin[indx] + arrowSize;
    y = origin[1];
    lx[ii]->SetNextPoint(x,y,z);                // go back
    x -= arrowHeadLegth;
    y -= arrWidth;                        //            
    lx[ii]->SetNextPoint(x,y,z);                //     --------> finish it
    //            
    lx[ii]->SetLineColor(kYellow);
    
    //___________  Y axis ____________
    indx++;
    ly[ii] = new TPolyLine3D(5,"L");
    x = origin[0];
    y = origin[1];
    z = origin[2];
    ly[ii]->SetNextPoint(x,y,z);               // initial point
    y = origin[indx] + arrowSize;
    ly[ii]->SetNextPoint(x,y,z);               //     --------
    //____________ y_head ____________
    y -= arrowHeadLegth;
    x += arrWidth;                       //            
    ly[ii]->SetNextPoint(x,y,z);               //     --------
    x = origin[0];
    y = origin[indx] + arrowSize;
    ly[ii]->SetNextPoint(x,y,z);               // go back
    y -= arrowHeadLegth;
    x -= arrWidth;                       //            
    ly[ii]->SetNextPoint(x,y,z);               //     --------> finish it
    //            
    ly[ii]->SetLineColor(kGreen);
    
    //___________  Z axis ____________
    indx++;
    lz[ii] = new TPolyLine3D(5,"L");
    x = origin[0];
    y = origin[1];
    z = origin[2];
    lz[ii]->SetNextPoint(x,y,z);               // initial point
    z += arrowSize;
    lz[ii]->SetNextPoint(x,y,z);               //     --------
    //____________ z_head ____________
    z -= arrowHeadLegth;
    x += arrWidth;                       //            
    lz[ii]->SetNextPoint(x,y,z);               //     --------
    x = origin[0];
    z = origin[indx] + arrowSize;
    lz[ii]->SetNextPoint(x,y,z);               // go back
    z -= arrowHeadLegth;
    x -= arrWidth;                       //            
    lz[ii]->SetNextPoint(x,y,z);               //     --------> finish it
    lz[ii]->SetLineColor(kRed);
  }
}

void createDependencies() {
  // Creates deoencies of different shapes for the specific nodes.
  
  Float_t inner_radius =  7.73;
  Float_t outer_radius = 30.05;
  Float_t padlength = 2.;
  Float_t z_first = 162.75 - 7.5;
  Float_t z_last  = 256.45 + 7.5;
  Float_t z[10] = {162.75, 171.25, 184.05, 192.55, 205.35, 213.85, 226.65, 235.15, 247.95, 256.45};

  // dead segments and padrows
  
  Float_t dead_angle = outer_radius/8./TMath::Pi()*1.6;
  Float_t used_angle = 30.-dead_angle;
  Float_t angle = 30.;
  
  dead_seg1 = new TTUBS("dead_seg1", "dead_seg1", "void", inner_radius, outer_radius, (z[9]-z[0]+padlength)/2., -dead_angle+angle, dead_angle+angle);
  padrow1 = new TTUBS("padrow1", "padrow1", "void", outer_radius, outer_radius, padlength, -used_angle+angle - 30., used_angle+angle - 30.);
  angle += 60.;
  dead_seg2 = new TTUBS("dead_seg2", "dead_seg2", "void", inner_radius, outer_radius, (z[9]-z[0]+padlength)/2., -dead_angle+angle, dead_angle+angle);
  padrow2 = new TTUBS("padrow2", "padrow2", "void", outer_radius, outer_radius, padlength, -used_angle+angle - 30., used_angle+angle - 30.);
  angle += 60.;
  dead_seg3 = new TTUBS("dead_seg3", "dead_seg3", "void", inner_radius, outer_radius, (z[9]-z[0]+padlength)/2., -dead_angle+angle, dead_angle+angle);
  padrow3 = new TTUBS("padrow3", "padrow3", "void", outer_radius, outer_radius, padlength, -used_angle+angle - 30., used_angle+angle - 30.);
  angle += 60.;
  dead_seg4 = new TTUBS("dead_seg4", "dead_seg4", "void", inner_radius, outer_radius, (z[9]-z[0]+padlength)/2., -dead_angle+angle, dead_angle+angle);
  padrow4 = new TTUBS("padrow4", "padrow4", "void", outer_radius, outer_radius, padlength, -used_angle+angle - 30., used_angle+angle - 30.);
  angle += 60.;
  dead_seg5 = new TTUBS("dead_seg5", "dead_seg5", "void", inner_radius, outer_radius, (z[9]-z[0]+padlength)/2., -dead_angle+angle, dead_angle+angle);
  padrow5 = new TTUBS("padrow5", "padrow5", "void", outer_radius, outer_radius, padlength, -used_angle+angle - 30., used_angle+angle - 30.);
  angle += 60.;
  dead_seg6 = new TTUBS("dead_seg6", "dead_seg6", "void", inner_radius, outer_radius, (z[9]-z[0]+padlength)/2., -dead_angle+angle, dead_angle+angle);
  padrow6 = new TTUBS("padrow6", "padrow6", "void", outer_radius, outer_radius, padlength, -used_angle+angle - 30., used_angle+angle - 30.);
  angle += 60.;

    // create 4 tubes (cylinders) - two big ones (out) and two small ones (in) - to draw the Ftpcs
  ftpc1_out = new TTUBE("ftpc1_out", "Ftpc + (out)", "void", outer_radius, outer_radius, (z_last-z_first)/2., 1);
  ftpc1_in =  new TTUBE("ftpc1_in",  "Ftpc + (in)",  "void",  inner_radius,  inner_radius, (z_last-z_first)/2., 1);
  ftpc2_out = new TTUBE("ftpc2_out", "Ftpc - (out)", "void", outer_radius, outer_radius, (z_last-z_first)/2., 1);
  ftpc2_in =  new TTUBE("ftpc2_in",  "Ftpc - (in)",  "void",  inner_radius,  inner_radius, (z_last-z_first)/2., 1);
  
  // set colors
  origin->SetLineColor(1);
  ftpc1_out->SetLineColor(4);
  ftpc1_in->SetLineColor(4);
  ftpc2_out->SetLineColor(4);
  ftpc2_in->SetLineColor(4);
  
  dead_seg1->SetLineColor(kRed);
  dead_seg2->SetLineColor(kRed);
  dead_seg3->SetLineColor(kRed);
  dead_seg4->SetLineColor(kRed);
  dead_seg5->SetLineColor(kRed);
  dead_seg6->SetLineColor(kRed);

  padrow1->SetLineColor(kYellow);
  padrow2->SetLineColor(kYellow);
  padrow3->SetLineColor(kYellow);
  padrow4->SetLineColor(kYellow);
  padrow5->SetLineColor(kYellow);
  padrow6->SetLineColor(kYellow);
  
  // create dependencies for 'both' Ftpcs
  canvas->cd(1);
  node0->cd();  
  node01_out = new TNode("node01_out", "node01_out", "ftpc1_out", 0, 0,  z_first+(z_last-z_first)/2.);
  node01_in  = new TNode("node01_in",  "node01_in",  "ftpc1_in",  0, 0,  z_first+(z_last-z_first)/2.);
  node02_out = new TNode("node02_out", "node02_out", "ftpc2_out", 0, 0, -z_first-(z_last-z_first)/2.);
  node02_in  = new TNode("node02_in",  "node02_in",  "ftpc2_in",  0, 0, -z_first-(z_last-z_first)/2.);  

  node01_seg1 = new TNode("node01_seg1", "node01_seg1", "dead_seg1", 0., 0., z_first+(z_last-z_first)/2.);
  node01_seg2 = new TNode("node01_seg2", "node01_seg2", "dead_seg2", 0., 0., z_first+(z_last-z_first)/2.);
  node01_seg3 = new TNode("node01_seg3", "node01_seg3", "dead_seg3", 0., 0., z_first+(z_last-z_first)/2.);
  node01_seg4 = new TNode("node01_seg4", "node01_seg4", "dead_seg4", 0., 0., z_first+(z_last-z_first)/2.);
  node01_seg5 = new TNode("node01_seg5", "node01_seg5", "dead_seg5", 0., 0., z_first+(z_last-z_first)/2.);
  node01_seg6 = new TNode("node01_seg6", "node01_seg6", "dead_seg6", 0., 0., z_first+(z_last-z_first)/2.);
  node02_seg1 = new TNode("node02_seg1", "node02_seg1", "dead_seg1", 0., 0., -z_first-(z_last-z_first)/2.);
  node02_seg2 = new TNode("node02_seg2", "node02_seg2", "dead_seg2", 0., 0., -z_first-(z_last-z_first)/2.);
  node02_seg3 = new TNode("node02_seg3", "node02_seg3", "dead_seg3", 0., 0., -z_first-(z_last-z_first)/2.);
  node02_seg4 = new TNode("node02_seg4", "node02_seg4", "dead_seg4", 0., 0., -z_first-(z_last-z_first)/2.);
  node02_seg5 = new TNode("node02_seg5", "node02_seg5", "dead_seg5", 0., 0., -z_first-(z_last-z_first)/2.);
  node02_seg6 = new TNode("node02_seg6", "node02_seg6", "dead_seg6", 0., 0., -z_first-(z_last-z_first)/2.);

  node01_pad01 = new TNode("node01_pad01", "node01_pad01", "padrow1", 0., 0., z[0]);
  node01_pad02 = new TNode("node01_pad02", "node01_pad02", "padrow2", 0., 0., z[0]);
  node01_pad03 = new TNode("node01_pad03", "node01_pad03", "padrow3", 0., 0., z[0]);
  node01_pad04 = new TNode("node01_pad04", "node01_pad04", "padrow4", 0., 0., z[0]);
  node01_pad05 = new TNode("node01_pad05", "node01_pad05", "padrow5", 0., 0., z[0]);
  node01_pad06 = new TNode("node01_pad06", "node01_pad06", "padrow6", 0., 0., z[0]);
  node02_pad01 = new TNode("node02_pad01", "node02_pad01", "padrow1", 0., 0., -z[0]);
  node02_pad02 = new TNode("node02_pad02", "node02_pad02", "padrow2", 0., 0., -z[0]);
  node02_pad03 = new TNode("node02_pad03", "node02_pad03", "padrow3", 0., 0., -z[0]);
  node02_pad04 = new TNode("node02_pad04", "node02_pad04", "padrow4", 0., 0., -z[0]);
  node02_pad05 = new TNode("node02_pad05", "node02_pad05", "padrow5", 0., 0., -z[0]);
  node02_pad06 = new TNode("node02_pad06", "node02_pad06", "padrow6", 0., 0., -z[0]);

  node01_pad11 = new TNode("node01_pad11", "node01_pad11", "padrow1", 0., 0., z[1]);
  node01_pad12 = new TNode("node01_pad12", "node01_pad12", "padrow2", 0., 0., z[1]);
  node01_pad13 = new TNode("node01_pad13", "node01_pad13", "padrow3", 0., 0., z[1]);
  node01_pad14 = new TNode("node01_pad14", "node01_pad14", "padrow4", 0., 0., z[1]);
  node01_pad15 = new TNode("node01_pad15", "node01_pad15", "padrow5", 0., 0., z[1]);
  node01_pad16 = new TNode("node01_pad16", "node01_pad16", "padrow6", 0., 0., z[1]);
  node02_pad11 = new TNode("node02_pad11", "node02_pad11", "padrow1", 0., 0., -z[1]);
  node02_pad12 = new TNode("node02_pad12", "node02_pad12", "padrow2", 0., 0., -z[1]);
  node02_pad13 = new TNode("node02_pad13", "node02_pad13", "padrow3", 0., 0., -z[1]);
  node02_pad14 = new TNode("node02_pad14", "node02_pad14", "padrow4", 0., 0., -z[1]);
  node02_pad15 = new TNode("node02_pad15", "node02_pad15", "padrow5", 0., 0., -z[1]);
  node02_pad16 = new TNode("node02_pad16", "node02_pad16", "padrow6", 0., 0., -z[1]);

  node01_pad21 = new TNode("node01_pad21", "node01_pad21", "padrow1", 0., 0., z[2]);
  node01_pad22 = new TNode("node01_pad22", "node01_pad22", "padrow2", 0., 0., z[2]);
  node01_pad23 = new TNode("node01_pad23", "node01_pad23", "padrow3", 0., 0., z[2]);
  node01_pad24 = new TNode("node01_pad24", "node01_pad24", "padrow4", 0., 0., z[2]);
  node01_pad25 = new TNode("node01_pad25", "node01_pad25", "padrow5", 0., 0., z[2]);
  node01_pad26 = new TNode("node01_pad26", "node01_pad26", "padrow6", 0., 0., z[2]);
  node02_pad21 = new TNode("node02_pad21", "node02_pad21", "padrow1", 0., 0., -z[2]);
  node02_pad22 = new TNode("node02_pad22", "node02_pad22", "padrow2", 0., 0., -z[2]);
  node02_pad23 = new TNode("node02_pad23", "node02_pad23", "padrow3", 0., 0., -z[2]);
  node02_pad24 = new TNode("node02_pad24", "node02_pad24", "padrow4", 0., 0., -z[2]);
  node02_pad25 = new TNode("node02_pad25", "node02_pad25", "padrow5", 0., 0., -z[2]);
  node02_pad26 = new TNode("node02_pad26", "node02_pad26", "padrow6", 0., 0., -z[2]);

  node01_pad31 = new TNode("node01_pad31", "node01_pad31", "padrow1", 0., 0., z[3]);
  node01_pad32 = new TNode("node01_pad32", "node01_pad32", "padrow2", 0., 0., z[3]);
  node01_pad33 = new TNode("node01_pad33", "node01_pad33", "padrow3", 0., 0., z[3]);
  node01_pad34 = new TNode("node01_pad34", "node01_pad34", "padrow4", 0., 0., z[3]);
  node01_pad35 = new TNode("node01_pad35", "node01_pad35", "padrow5", 0., 0., z[3]);
  node01_pad36 = new TNode("node01_pad36", "node01_pad36", "padrow6", 0., 0., z[3]);
  node02_pad31 = new TNode("node02_pad31", "node02_pad31", "padrow1", 0., 0., -z[3]);
  node02_pad32 = new TNode("node02_pad32", "node02_pad32", "padrow2", 0., 0., -z[3]);
  node02_pad33 = new TNode("node02_pad33", "node02_pad33", "padrow3", 0., 0., -z[3]);
  node02_pad34 = new TNode("node02_pad34", "node02_pad34", "padrow4", 0., 0., -z[3]);
  node02_pad35 = new TNode("node02_pad35", "node02_pad35", "padrow5", 0., 0., -z[3]);
  node02_pad36 = new TNode("node02_pad36", "node02_pad36", "padrow6", 0., 0., -z[3]);

  node01_pad41 = new TNode("node01_pad41", "node01_pad41", "padrow1", 0., 0., z[4]);
  node01_pad42 = new TNode("node01_pad42", "node01_pad42", "padrow2", 0., 0., z[4]);
  node01_pad43 = new TNode("node01_pad43", "node01_pad43", "padrow3", 0., 0., z[4]);
  node01_pad44 = new TNode("node01_pad44", "node01_pad44", "padrow4", 0., 0., z[4]);
  node01_pad45 = new TNode("node01_pad45", "node01_pad45", "padrow5", 0., 0., z[4]);
  node01_pad46 = new TNode("node01_pad46", "node01_pad46", "padrow6", 0., 0., z[4]);
  node02_pad41 = new TNode("node02_pad41", "node02_pad41", "padrow1", 0., 0., -z[4]);
  node02_pad42 = new TNode("node02_pad42", "node02_pad42", "padrow2", 0., 0., -z[4]);
  node02_pad43 = new TNode("node02_pad43", "node02_pad43", "padrow3", 0., 0., -z[4]);
  node02_pad44 = new TNode("node02_pad44", "node02_pad44", "padrow4", 0., 0., -z[4]);
  node02_pad45 = new TNode("node02_pad45", "node02_pad45", "padrow5", 0., 0., -z[4]);
  node02_pad46 = new TNode("node02_pad46", "node02_pad46", "padrow6", 0., 0., -z[4]);

  node01_pad51 = new TNode("node01_pad51", "node01_pad51", "padrow1", 0., 0., z[5]);
  node01_pad52 = new TNode("node01_pad52", "node01_pad52", "padrow2", 0., 0., z[5]);
  node01_pad53 = new TNode("node01_pad53", "node01_pad53", "padrow3", 0., 0., z[5]);
  node01_pad54 = new TNode("node01_pad54", "node01_pad54", "padrow4", 0., 0., z[5]);
  node01_pad55 = new TNode("node01_pad55", "node01_pad55", "padrow5", 0., 0., z[5]);
  node01_pad56 = new TNode("node01_pad56", "node01_pad56", "padrow6", 0., 0., z[5]);
  node02_pad51 = new TNode("node02_pad51", "node02_pad51", "padrow1", 0., 0., -z[5]);
  node02_pad52 = new TNode("node02_pad52", "node02_pad52", "padrow2", 0., 0., -z[5]);
  node02_pad53 = new TNode("node02_pad53", "node02_pad53", "padrow3", 0., 0., -z[5]);
  node02_pad54 = new TNode("node02_pad54", "node02_pad54", "padrow4", 0., 0., -z[5]);
  node02_pad55 = new TNode("node02_pad55", "node02_pad55", "padrow5", 0., 0., -z[5]);
  node02_pad56 = new TNode("node02_pad56", "node02_pad56", "padrow6", 0., 0., -z[5]);

  node01_pad61 = new TNode("node01_pad61", "node01_pad61", "padrow1", 0., 0., z[6]);
  node01_pad62 = new TNode("node01_pad62", "node01_pad62", "padrow2", 0., 0., z[6]);
  node01_pad63 = new TNode("node01_pad63", "node01_pad63", "padrow3", 0., 0., z[6]);
  node01_pad64 = new TNode("node01_pad64", "node01_pad64", "padrow4", 0., 0., z[6]);
  node01_pad65 = new TNode("node01_pad65", "node01_pad65", "padrow5", 0., 0., z[6]);
  node01_pad66 = new TNode("node01_pad66", "node01_pad66", "padrow6", 0., 0., z[6]);
  node02_pad61 = new TNode("node02_pad61", "node02_pad61", "padrow1", 0., 0., -z[6]);
  node02_pad62 = new TNode("node02_pad62", "node02_pad62", "padrow2", 0., 0., -z[6]);
  node02_pad63 = new TNode("node02_pad63", "node02_pad63", "padrow3", 0., 0., -z[6]);
  node02_pad64 = new TNode("node02_pad64", "node02_pad64", "padrow4", 0., 0., -z[6]);
  node02_pad65 = new TNode("node02_pad65", "node02_pad65", "padrow5", 0., 0., -z[6]);
  node02_pad66 = new TNode("node02_pad66", "node02_pad66", "padrow6", 0., 0., -z[6]);

  node01_pad71 = new TNode("node01_pad71", "node01_pad71", "padrow1", 0., 0., z[7]);
  node01_pad72 = new TNode("node01_pad72", "node01_pad72", "padrow2", 0., 0., z[7]);
  node01_pad73 = new TNode("node01_pad73", "node01_pad73", "padrow3", 0., 0., z[7]);
  node01_pad74 = new TNode("node01_pad74", "node01_pad74", "padrow4", 0., 0., z[7]);
  node01_pad75 = new TNode("node01_pad75", "node01_pad75", "padrow5", 0., 0., z[7]);
  node01_pad76 = new TNode("node01_pad76", "node01_pad76", "padrow6", 0., 0., z[7]);
  node02_pad71 = new TNode("node02_pad71", "node02_pad71", "padrow1", 0., 0., -z[7]);
  node02_pad72 = new TNode("node02_pad72", "node02_pad72", "padrow2", 0., 0., -z[7]);
  node02_pad73 = new TNode("node02_pad73", "node02_pad73", "padrow3", 0., 0., -z[7]);
  node02_pad74 = new TNode("node02_pad74", "node02_pad74", "padrow4", 0., 0., -z[7]);
  node02_pad75 = new TNode("node02_pad75", "node02_pad75", "padrow5", 0., 0., -z[7]);
  node02_pad76 = new TNode("node02_pad76", "node02_pad76", "padrow6", 0., 0., -z[7]);

  node01_pad81 = new TNode("node01_pad81", "node01_pad81", "padrow1", 0., 0., z[8]);
  node01_pad82 = new TNode("node01_pad82", "node01_pad82", "padrow2", 0., 0., z[8]);
  node01_pad83 = new TNode("node01_pad83", "node01_pad83", "padrow3", 0., 0., z[8]);
  node01_pad84 = new TNode("node01_pad84", "node01_pad84", "padrow4", 0., 0., z[8]);
  node01_pad85 = new TNode("node01_pad85", "node01_pad85", "padrow5", 0., 0., z[8]);
  node01_pad86 = new TNode("node01_pad86", "node01_pad86", "padrow6", 0., 0., z[8]);
  node02_pad81 = new TNode("node02_pad81", "node02_pad81", "padrow1", 0., 0., -z[8]);
  node02_pad82 = new TNode("node02_pad82", "node02_pad82", "padrow2", 0., 0., -z[8]);
  node02_pad83 = new TNode("node02_pad83", "node02_pad83", "padrow3", 0., 0., -z[8]);
  node02_pad84 = new TNode("node02_pad84", "node02_pad84", "padrow4", 0., 0., -z[8]);
  node02_pad85 = new TNode("node02_pad85", "node02_pad85", "padrow5", 0., 0., -z[8]);
  node02_pad86 = new TNode("node02_pad86", "node02_pad86", "padrow6", 0., 0., -z[8]);

  node01_pad91 = new TNode("node01_pad91", "node01_pad91", "padrow1", 0., 0., z[9]);
  node01_pad92 = new TNode("node01_pad92", "node01_pad92", "padrow2", 0., 0., z[9]);
  node01_pad93 = new TNode("node01_pad93", "node01_pad93", "padrow3", 0., 0., z[9]);
  node01_pad94 = new TNode("node01_pad94", "node01_pad94", "padrow4", 0., 0., z[9]);
  node01_pad95 = new TNode("node01_pad95", "node01_pad95", "padrow5", 0., 0., z[9]);
  node01_pad96 = new TNode("node01_pad96", "node01_pad96", "padrow6", 0., 0., z[9]);
  node02_pad91 = new TNode("node02_pad91", "node02_pad91", "padrow1", 0., 0., -z[9]);
  node02_pad92 = new TNode("node02_pad92", "node02_pad92", "padrow2", 0., 0., -z[9]);
  node02_pad93 = new TNode("node02_pad93", "node02_pad93", "padrow3", 0., 0., -z[9]);
  node02_pad94 = new TNode("node02_pad94", "node02_pad94", "padrow4", 0., 0., -z[9]);
  node02_pad95 = new TNode("node02_pad95", "node02_pad95", "padrow5", 0., 0., -z[9]);
  node02_pad96 = new TNode("node02_pad96", "node02_pad96", "padrow6", 0., 0., -z[9]);


  // create dependencies for '-' Ftpc
  canvas->cd(2);
  node2->cd();
  node2_out = new TNode("node2_out", "node2_out", "ftpc2_out", 0, 0, -z_first-(z_last-z_first)/2.);
  node2_in = new TNode("node2_in", "node2_in", "ftpc2_in", 0, 0, -z_first-(z_last-z_first)/2.);

  node2_seg1 = new TNode("node2_seg1", "node2_seg1", "dead_seg1", 0., 0., -z_first-(z_last-z_first)/2.);
  node2_seg2 = new TNode("node2_seg2", "node2_seg2", "dead_seg2", 0., 0., -z_first-(z_last-z_first)/2.);
  node2_seg3 = new TNode("node2_seg3", "node2_seg3", "dead_seg3", 0., 0., -z_first-(z_last-z_first)/2.);
  node2_seg4 = new TNode("node2_seg4", "node2_seg4", "dead_seg4", 0., 0., -z_first-(z_last-z_first)/2.);
  node2_seg5 = new TNode("node2_seg5", "node2_seg5", "dead_seg5", 0., 0., -z_first-(z_last-z_first)/2.);
  node2_seg6 = new TNode("node2_seg6", "node2_seg6", "dead_seg6", 0., 0., -z_first-(z_last-z_first)/2.);

  node2_pad01 = new TNode("node2_pad01", "node2_pad01", "padrow1", 0., 0., -z[0]);
  node2_pad02 = new TNode("node2_pad02", "node2_pad02", "padrow2", 0., 0., -z[0]);
  node2_pad03 = new TNode("node2_pad03", "node2_pad03", "padrow3", 0., 0., -z[0]);
  node2_pad04 = new TNode("node2_pad04", "node2_pad04", "padrow4", 0., 0., -z[0]);
  node2_pad05 = new TNode("node2_pad05", "node2_pad05", "padrow5", 0., 0., -z[0]);
  node2_pad06 = new TNode("node2_pad06", "node2_pad06", "padrow6", 0., 0., -z[0]);

  node2_pad11 = new TNode("node2_pad11", "node2_pad11", "padrow1", 0., 0., -z[1]);
  node2_pad12 = new TNode("node2_pad12", "node2_pad12", "padrow2", 0., 0., -z[1]);
  node2_pad13 = new TNode("node2_pad13", "node2_pad13", "padrow3", 0., 0., -z[1]);
  node2_pad14 = new TNode("node2_pad14", "node2_pad14", "padrow4", 0., 0., -z[1]);
  node2_pad15 = new TNode("node2_pad15", "node2_pad15", "padrow5", 0., 0., -z[1]);
  node2_pad16 = new TNode("node2_pad16", "node2_pad16", "padrow6", 0., 0., -z[1]);

  node2_pad21 = new TNode("node2_pad21", "node2_pad21", "padrow1", 0., 0., -z[2]);
  node2_pad22 = new TNode("node2_pad22", "node2_pad22", "padrow2", 0., 0., -z[2]);
  node2_pad23 = new TNode("node2_pad23", "node2_pad23", "padrow3", 0., 0., -z[2]);
  node2_pad24 = new TNode("node2_pad24", "node2_pad24", "padrow4", 0., 0., -z[2]);
  node2_pad25 = new TNode("node2_pad25", "node2_pad25", "padrow5", 0., 0., -z[2]);
  node2_pad26 = new TNode("node2_pad26", "node2_pad26", "padrow6", 0., 0., -z[2]);

  node2_pad31 = new TNode("node2_pad31", "node2_pad31", "padrow1", 0., 0., -z[3]);
  node2_pad32 = new TNode("node2_pad32", "node2_pad32", "padrow2", 0., 0., -z[3]);
  node2_pad33 = new TNode("node2_pad33", "node2_pad33", "padrow3", 0., 0., -z[3]);
  node2_pad34 = new TNode("node2_pad34", "node2_pad34", "padrow4", 0., 0., -z[3]);
  node2_pad35 = new TNode("node2_pad35", "node2_pad35", "padrow5", 0., 0., -z[3]);
  node2_pad36 = new TNode("node2_pad36", "node2_pad36", "padrow6", 0., 0., -z[3]);

  node2_pad41 = new TNode("node2_pad41", "node2_pad41", "padrow1", 0., 0., -z[4]);
  node2_pad42 = new TNode("node2_pad42", "node2_pad42", "padrow2", 0., 0., -z[4]);
  node2_pad43 = new TNode("node2_pad43", "node2_pad43", "padrow3", 0., 0., -z[4]);
  node2_pad44 = new TNode("node2_pad44", "node2_pad44", "padrow4", 0., 0., -z[4]);
  node2_pad45 = new TNode("node2_pad45", "node2_pad45", "padrow5", 0., 0., -z[4]);
  node2_pad46 = new TNode("node2_pad46", "node2_pad46", "padrow6", 0., 0., -z[4]);

  node2_pad51 = new TNode("node2_pad51", "node2_pad51", "padrow1", 0., 0., -z[5]);
  node2_pad52 = new TNode("node2_pad52", "node2_pad52", "padrow2", 0., 0., -z[5]);
  node2_pad53 = new TNode("node2_pad53", "node2_pad53", "padrow3", 0., 0., -z[5]);
  node2_pad54 = new TNode("node2_pad54", "node2_pad54", "padrow4", 0., 0., -z[5]);
  node2_pad55 = new TNode("node2_pad55", "node2_pad55", "padrow5", 0., 0., -z[5]);
  node2_pad56 = new TNode("node2_pad56", "node2_pad56", "padrow6", 0., 0., -z[5]);

  node2_pad61 = new TNode("node2_pad61", "node2_pad61", "padrow1", 0., 0., -z[6]);
  node2_pad62 = new TNode("node2_pad62", "node2_pad62", "padrow2", 0., 0., -z[6]);
  node2_pad63 = new TNode("node2_pad63", "node2_pad63", "padrow3", 0., 0., -z[6]);
  node2_pad64 = new TNode("node2_pad64", "node2_pad64", "padrow4", 0., 0., -z[6]);
  node2_pad65 = new TNode("node2_pad65", "node2_pad65", "padrow5", 0., 0., -z[6]);
  node2_pad66 = new TNode("node2_pad66", "node2_pad66", "padrow6", 0., 0., -z[6]);

  node2_pad71 = new TNode("node2_pad71", "node2_pad71", "padrow1", 0., 0., -z[7]);
  node2_pad72 = new TNode("node2_pad72", "node2_pad72", "padrow2", 0., 0., -z[7]);
  node2_pad73 = new TNode("node2_pad73", "node2_pad73", "padrow3", 0., 0., -z[7]);
  node2_pad74 = new TNode("node2_pad74", "node2_pad74", "padrow4", 0., 0., -z[7]);
  node2_pad75 = new TNode("node2_pad75", "node2_pad75", "padrow5", 0., 0., -z[7]);
  node2_pad76 = new TNode("node2_pad76", "node2_pad76", "padrow6", 0., 0., -z[7]);

  node2_pad81 = new TNode("node2_pad81", "node2_pad81", "padrow1", 0., 0., -z[8]);
  node2_pad82 = new TNode("node2_pad82", "node2_pad82", "padrow2", 0., 0., -z[8]);
  node2_pad83 = new TNode("node2_pad83", "node2_pad83", "padrow3", 0., 0., -z[8]);
  node2_pad84 = new TNode("node2_pad84", "node2_pad84", "padrow4", 0., 0., -z[8]);
  node2_pad85 = new TNode("node2_pad85", "node2_pad85", "padrow5", 0., 0., -z[8]);
  node2_pad86 = new TNode("node2_pad86", "node2_pad86", "padrow6", 0., 0., -z[8]);

  node2_pad91 = new TNode("node2_pad91", "node2_pad91", "padrow1", 0., 0., -z[9]);
  node2_pad92 = new TNode("node2_pad92", "node2_pad92", "padrow2", 0., 0., -z[9]);
  node2_pad93 = new TNode("node2_pad93", "node2_pad93", "padrow3", 0., 0., -z[9]);
  node2_pad94 = new TNode("node2_pad94", "node2_pad94", "padrow4", 0., 0., -z[9]);
  node2_pad95 = new TNode("node2_pad95", "node2_pad95", "padrow5", 0., 0., -z[9]);
  node2_pad96 = new TNode("node2_pad96", "node2_pad96", "padrow6", 0., 0., -z[9]);
 
  
  // create dependencies for '+' Ftpc
  canvas->cd(3);
  node1->cd();
  node1_out = new TNode("node1_out", "node1_out", "ftpc1_out", 0, 0, z_first+(z_last-z_first)/2.);
  node1_in  = new TNode("node1_in",  "node1_in",  "ftpc1_in",  0, 0, z_first+(z_last-z_first)/2.);

  node1_seg1 = new TNode("node1_seg1", "node1_seg1", "dead_seg1", 0., 0., z_first+(z_last-z_first)/2.);
  node1_seg2 = new TNode("node1_seg2", "node1_seg2", "dead_seg2", 0., 0., z_first+(z_last-z_first)/2.);
  node1_seg3 = new TNode("node1_seg3", "node1_seg3", "dead_seg3", 0., 0., z_first+(z_last-z_first)/2.);
  node1_seg4 = new TNode("node1_seg4", "node1_seg4", "dead_seg4", 0., 0., z_first+(z_last-z_first)/2.);
  node1_seg5 = new TNode("node1_seg5", "node1_seg5", "dead_seg5", 0., 0., z_first+(z_last-z_first)/2.);
  node1_seg6 = new TNode("node1_seg6", "node1_seg6", "dead_seg6", 0., 0., z_first+(z_last-z_first)/2.);

  node1_pad01 = new TNode("node1_pad01", "node1_pad01", "padrow1", 0., 0., z[0]);
  node1_pad02 = new TNode("node1_pad02", "node1_pad02", "padrow2", 0., 0., z[0]);
  node1_pad03 = new TNode("node1_pad03", "node1_pad03", "padrow3", 0., 0., z[0]);
  node1_pad04 = new TNode("node1_pad04", "node1_pad04", "padrow4", 0., 0., z[0]);
  node1_pad05 = new TNode("node1_pad05", "node1_pad05", "padrow5", 0., 0., z[0]);
  node1_pad06 = new TNode("node1_pad06", "node1_pad06", "padrow6", 0., 0., z[0]);

  node1_pad11 = new TNode("node1_pad11", "node1_pad11", "padrow1", 0., 0., z[1]);
  node1_pad12 = new TNode("node1_pad12", "node1_pad12", "padrow2", 0., 0., z[1]);
  node1_pad13 = new TNode("node1_pad13", "node1_pad13", "padrow3", 0., 0., z[1]);
  node1_pad14 = new TNode("node1_pad14", "node1_pad14", "padrow4", 0., 0., z[1]);
  node1_pad15 = new TNode("node1_pad15", "node1_pad15", "padrow5", 0., 0., z[1]);
  node1_pad16 = new TNode("node1_pad16", "node1_pad16", "padrow6", 0., 0., z[1]);

  node1_pad21 = new TNode("node1_pad21", "node1_pad21", "padrow1", 0., 0., z[2]);
  node1_pad22 = new TNode("node1_pad22", "node1_pad22", "padrow2", 0., 0., z[2]);
  node1_pad23 = new TNode("node1_pad23", "node1_pad23", "padrow3", 0., 0., z[2]);
  node1_pad24 = new TNode("node1_pad24", "node1_pad24", "padrow4", 0., 0., z[2]);
  node1_pad25 = new TNode("node1_pad25", "node1_pad25", "padrow5", 0., 0., z[2]);
  node1_pad26 = new TNode("node1_pad26", "node1_pad26", "padrow6", 0., 0., z[2]);

  node1_pad31 = new TNode("node1_pad31", "node1_pad31", "padrow1", 0., 0., z[3]);
  node1_pad32 = new TNode("node1_pad32", "node1_pad32", "padrow2", 0., 0., z[3]);
  node1_pad33 = new TNode("node1_pad33", "node1_pad33", "padrow3", 0., 0., z[3]);
  node1_pad34 = new TNode("node1_pad34", "node1_pad34", "padrow4", 0., 0., z[3]);
  node1_pad35 = new TNode("node1_pad35", "node1_pad35", "padrow5", 0., 0., z[3]);
  node1_pad36 = new TNode("node1_pad36", "node1_pad36", "padrow6", 0., 0., z[3]);

  node1_pad41 = new TNode("node1_pad41", "node1_pad41", "padrow1", 0., 0., z[4]);
  node1_pad42 = new TNode("node1_pad42", "node1_pad42", "padrow2", 0., 0., z[4]);
  node1_pad43 = new TNode("node1_pad43", "node1_pad43", "padrow3", 0., 0., z[4]);
  node1_pad44 = new TNode("node1_pad44", "node1_pad44", "padrow4", 0., 0., z[4]);
  node1_pad45 = new TNode("node1_pad45", "node1_pad45", "padrow5", 0., 0., z[4]);
  node1_pad46 = new TNode("node1_pad46", "node1_pad46", "padrow6", 0., 0., z[4]);

  node1_pad51 = new TNode("node1_pad51", "node1_pad51", "padrow1", 0., 0., z[5]);
  node1_pad52 = new TNode("node1_pad52", "node1_pad52", "padrow2", 0., 0., z[5]);
  node1_pad53 = new TNode("node1_pad53", "node1_pad53", "padrow3", 0., 0., z[5]);
  node1_pad54 = new TNode("node1_pad54", "node1_pad54", "padrow4", 0., 0., z[5]);
  node1_pad55 = new TNode("node1_pad55", "node1_pad55", "padrow5", 0., 0., z[5]);
  node1_pad56 = new TNode("node1_pad56", "node1_pad56", "padrow6", 0., 0., z[5]);

  node1_pad61 = new TNode("node1_pad61", "node1_pad61", "padrow1", 0., 0., z[6]);
  node1_pad62 = new TNode("node1_pad62", "node1_pad62", "padrow2", 0., 0., z[6]);
  node1_pad63 = new TNode("node1_pad63", "node1_pad63", "padrow3", 0., 0., z[6]);
  node1_pad64 = new TNode("node1_pad64", "node1_pad64", "padrow4", 0., 0., z[6]);
  node1_pad65 = new TNode("node1_pad65", "node1_pad65", "padrow5", 0., 0., z[6]);
  node1_pad66 = new TNode("node1_pad66", "node1_pad66", "padrow6", 0., 0., z[6]);

  node1_pad71 = new TNode("node1_pad71", "node1_pad71", "padrow1", 0., 0., z[7]);
  node1_pad72 = new TNode("node1_pad72", "node1_pad72", "padrow2", 0., 0., z[7]);
  node1_pad73 = new TNode("node1_pad73", "node1_pad73", "padrow3", 0., 0., z[7]);
  node1_pad74 = new TNode("node1_pad74", "node1_pad74", "padrow4", 0., 0., z[7]);
  node1_pad75 = new TNode("node1_pad75", "node1_pad75", "padrow5", 0., 0., z[7]);
  node1_pad76 = new TNode("node1_pad76", "node1_pad76", "padrow6", 0., 0., z[7]);

  node1_pad81 = new TNode("node1_pad81", "node1_pad81", "padrow1", 0., 0., z[8]);
  node1_pad82 = new TNode("node1_pad82", "node1_pad82", "padrow2", 0., 0., z[8]);
  node1_pad83 = new TNode("node1_pad83", "node1_pad83", "padrow3", 0., 0., z[8]);
  node1_pad84 = new TNode("node1_pad84", "node1_pad84", "padrow4", 0., 0., z[8]);
  node1_pad85 = new TNode("node1_pad85", "node1_pad85", "padrow5", 0., 0., z[8]);
  node1_pad86 = new TNode("node1_pad86", "node1_pad86", "padrow6", 0., 0., z[8]);

  node1_pad91 = new TNode("node1_pad91", "node1_pad91", "padrow1", 0., 0., z[9]);
  node1_pad92 = new TNode("node1_pad92", "node1_pad92", "padrow2", 0., 0., z[9]);
  node1_pad93 = new TNode("node1_pad93", "node1_pad93", "padrow3", 0., 0., z[9]);
  node1_pad94 = new TNode("node1_pad94", "node1_pad94", "padrow4", 0., 0., z[9]);
  node1_pad95 = new TNode("node1_pad95", "node1_pad95", "padrow5", 0., 0., z[9]);
  node1_pad96 = new TNode("node1_pad96", "node1_pad96", "padrow6", 0., 0., z[9]);
 
  // The following lines are unnecessary. They avoid compiler warnings. They can be simply deleted.
  /*
  node0->cd();
  node1->cd();
  node2->cd();
  node1_in->cd();
  node2_in->cd();
  node1_out->cd();
  node2_out->cd();
  node01_in->cd();
  node02_in->cd();
  node01_out->cd();
  node02_out->cd();
  node01_seg1->cd();
  node01_seg2->cd();
  node01_seg3->cd();
  node01_seg4->cd();
  node01_seg5->cd();
  node01_seg6->cd();
  node02_seg1->cd();
  node02_seg2->cd();
  node02_seg3->cd();
  node02_seg4->cd();
  node02_seg5->cd();
  node02_seg6->cd();
  node1_seg1->cd();
  node1_seg2->cd();
  node1_seg3->cd();
  node1_seg4->cd();
  node1_seg5->cd();
  node1_seg6->cd();
  node2_seg1->cd();
  node2_seg2->cd();
  node2_seg3->cd();
  node2_seg4->cd();
  node2_seg5->cd();
  node2_seg6->cd();

  node01_pad01->cd();
  node01_pad02->cd();
  node01_pad03->cd();
  node01_pad04->cd();
  node01_pad05->cd();
  node01_pad06->cd();
  node02_pad01->cd();
  node02_pad02->cd();
  node02_pad03->cd();
  node02_pad04->cd();
  node02_pad05->cd();
  node02_pad06->cd();
*/
}

void deleteNode() {
  // Deletes nodes.
  
  delete ftpc1_in;
  delete ftpc2_in;
  delete ftpc1_out;
  delete ftpc2_out;
  delete padrow1;
  delete padrow2;
  delete padrow3;
  delete padrow4;
  delete padrow5;
  delete padrow6;
  delete dead_seg1;
  delete dead_seg2;
  delete dead_seg3;
  delete dead_seg4;
  delete dead_seg5;
  delete dead_seg6;
  delete node01_in;
  delete node01_out;
  delete node02_in;
  delete node02_out;
  delete node1_in;
  delete node1_out;
  delete node2_in;
  delete node2_out;
  delete node01_seg1;
  delete node01_seg2;
  delete node01_seg3;
  delete node01_seg4;
  delete node01_seg5;
  delete node01_seg6;
  delete node02_seg1;
  delete node02_seg2;
  delete node02_seg3;
  delete node02_seg4;
  delete node02_seg5;
  delete node02_seg6;
  delete node1_seg1;
  delete node1_seg2;
  delete node1_seg3;
  delete node1_seg4;
  delete node1_seg5;
  delete node1_seg6;
  delete node2_seg1;
  delete node2_seg2;
  delete node2_seg3;
  delete node2_seg4;
  delete node2_seg5;
  delete node2_seg6;
  delete node01_pad01;
  delete node01_pad02;
  delete node01_pad03;
  delete node01_pad04;
  delete node01_pad05;
  delete node01_pad06;
  delete node02_pad01;
  delete node02_pad02;
  delete node02_pad03;
  delete node02_pad04;
  delete node02_pad05;
  delete node02_pad06;
  delete node1_pad01;
  delete node1_pad02;
  delete node1_pad03;
  delete node1_pad04;
  delete node1_pad05;
  delete node1_pad06;
  delete node2_pad01;
  delete node2_pad02;
  delete node2_pad03;
  delete node2_pad04;
  delete node2_pad05;
  delete node2_pad06;
  delete node1_pad11;
  delete node1_pad12;
  delete node1_pad13;
  delete node1_pad14;
  delete node1_pad15;
  delete node1_pad16;
  delete node2_pad11;
  delete node2_pad12;
  delete node2_pad13;
  delete node2_pad14;
  delete node2_pad15;
  delete node2_pad16;
  delete node01_pad11;
  delete node01_pad12;
  delete node01_pad13;
  delete node01_pad14;
  delete node01_pad15;
  delete node01_pad16;
  delete node02_pad11;
  delete node02_pad12;
  delete node02_pad13;
  delete node02_pad14;
  delete node02_pad15;
  delete node02_pad16;
  delete node1_pad21;
  delete node1_pad22;
  delete node1_pad23;
  delete node1_pad24;
  delete node1_pad25;
  delete node1_pad26;
  delete node2_pad21;
  delete node2_pad22;
  delete node2_pad23;
  delete node2_pad24;
  delete node2_pad25;
  delete node2_pad26;
  delete node01_pad21;
  delete node01_pad22;
  delete node01_pad23;
  delete node01_pad24;
  delete node01_pad25;
  delete node01_pad26;
  delete node02_pad21;
  delete node02_pad22;
  delete node02_pad23;
  delete node02_pad24;
  delete node02_pad25;
  delete node02_pad26;
  delete node1_pad31;
  delete node1_pad32;
  delete node1_pad33;
  delete node1_pad34;
  delete node1_pad35;
  delete node1_pad36;
  delete node2_pad31;
  delete node2_pad32;
  delete node2_pad33;
  delete node2_pad34;
  delete node2_pad35;
  delete node2_pad36;
  delete node01_pad31;
  delete node01_pad32;
  delete node01_pad33;
  delete node01_pad34;
  delete node01_pad35;
  delete node01_pad36;
  delete node02_pad31;
  delete node02_pad32;
  delete node02_pad33;
  delete node02_pad34;
  delete node02_pad35;
  delete node02_pad36;
  delete node1_pad41;
  delete node1_pad42;
  delete node1_pad43;
  delete node1_pad44;
  delete node1_pad45;
  delete node1_pad46;
  delete node2_pad41;
  delete node2_pad42;
  delete node2_pad43;
  delete node2_pad44;
  delete node2_pad45;
  delete node2_pad46;
  delete node01_pad41;
  delete node01_pad42;
  delete node01_pad43;
  delete node01_pad44;
  delete node01_pad45;
  delete node01_pad46;
  delete node02_pad41;
  delete node02_pad42;
  delete node02_pad43;
  delete node02_pad44;
  delete node02_pad45;
  delete node02_pad46;
  delete node1_pad51;
  delete node1_pad52;
  delete node1_pad53;
  delete node1_pad54;
  delete node1_pad55;
  delete node1_pad56;
  delete node2_pad51;
  delete node2_pad52;
  delete node2_pad53;
  delete node2_pad54;
  delete node2_pad55;
  delete node2_pad56;
  delete node01_pad51;
  delete node01_pad52;
  delete node01_pad53;
  delete node01_pad54;
  delete node01_pad55;
  delete node01_pad56;
  delete node02_pad51;
  delete node02_pad52;
  delete node02_pad53;
  delete node02_pad54;
  delete node02_pad55;
  delete node02_pad56;
  delete node1_pad61;
  delete node1_pad62;
  delete node1_pad63;
  delete node1_pad64;
  delete node1_pad65;
  delete node1_pad66;
  delete node2_pad61;
  delete node2_pad62;
  delete node2_pad63;
  delete node2_pad64;
  delete node2_pad65;
  delete node2_pad66;
  delete node01_pad61;
  delete node01_pad62;
  delete node01_pad63;
  delete node01_pad64;
  delete node01_pad65;
  delete node01_pad66;
  delete node02_pad61;
  delete node02_pad62;
  delete node02_pad63;
  delete node02_pad64;
  delete node02_pad65;
  delete node02_pad66;
  delete node1_pad71;
  delete node1_pad72;
  delete node1_pad73;
  delete node1_pad74;
  delete node1_pad75;
  delete node1_pad76;
  delete node2_pad71;
  delete node2_pad72;
  delete node2_pad73;
  delete node2_pad74;
  delete node2_pad75;
  delete node2_pad76;
  delete node01_pad71;
  delete node01_pad72;
  delete node01_pad73;
  delete node01_pad74;
  delete node01_pad75;
  delete node01_pad76;
  delete node02_pad71;
  delete node02_pad72;
  delete node02_pad73;
  delete node02_pad74;
  delete node02_pad75;
  delete node02_pad76;
  delete node1_pad81;
  delete node1_pad82;
  delete node1_pad83;
  delete node1_pad84;
  delete node1_pad85;
  delete node1_pad86;
  delete node2_pad81;
  delete node2_pad82;
  delete node2_pad83;
  delete node2_pad84;
  delete node2_pad85;
  delete node2_pad86;
  delete node01_pad81;
  delete node01_pad82;
  delete node01_pad83;
  delete node01_pad84;
  delete node01_pad85;
  delete node01_pad86;
  delete node02_pad81;
  delete node02_pad82;
  delete node02_pad83;
  delete node02_pad84;
  delete node02_pad85;
  delete node02_pad86;
  delete node1_pad91;
  delete node1_pad92;
  delete node1_pad93;
  delete node1_pad94;
  delete node1_pad95;
  delete node1_pad96;
  delete node2_pad91;
  delete node2_pad92;
  delete node2_pad93;
  delete node2_pad94;
  delete node2_pad95;
  delete node2_pad96;
  delete node01_pad91;
  delete node01_pad92;
  delete node01_pad93;
  delete node01_pad94;
  delete node01_pad95;
  delete node01_pad96;
  delete node02_pad91;
  delete node02_pad92;
  delete node02_pad93;
  delete node02_pad94;
  delete node02_pad95;
  delete node02_pad96;
}

