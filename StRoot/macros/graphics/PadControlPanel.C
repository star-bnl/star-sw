//*-- Author :    Valery Fine   25/05/99  (E-mail: fine@bnl.gov)
//
// Copyright (C)  Valery Fine, Brookhaven National Laboratory, 1999. All right reserved
//
// $Id: PadControlPanel.C,v 1.3 1999/06/02 22:25:12 fine Exp $
// $Log: PadControlPanel.C,v $
// Revision 1.3  1999/06/02 22:25:12  fine
// 4 view command has been introduced
//
// Revision 1.2  1999/06/02 16:30:12  fine
// Clean up
//
// Revision 1.1  1999/05/29 20:55:11  fine
// macro to control any 3D view
//
//
//  PadControl panel is the set of the static methods to control 
//  TView of any "current" TPad with some "primitive"
//  operation:
//
//
////////////////////////////////////////////////////////////////////////
//
// This macro generates a Controlbar panel: 
// begin_html  <P ALIGN=CENTER> <IMG SRC="gif/PadControlPanel.gif" ></P> end_html
//
// To execute an item, click with the left mouse button.
//  
// Just start this macro wheneven you want:
//
//  From Root/Cint macro:
//  --------------------
//   .x PadControlPanel.C
//   .L PadControlPanel.C
// or
//   gROOT->LoadMacro("PadControlPanel.C");
//
//  From the compiled C++ code:
//  --------------------
//   gROOT->LoadMacro("PadControlPanel.C");
//
//  Note:  If you don't like what it does make your private copy 
//         change it with your favorite text editor and load it right
//         away.
//         NO EXTRA STEP like : compilation, linking, loading required 
//
///////////////////////////////////////////////////////////////////////

class StPadControlPanel {
  private:
   TControlBar *fBar;  
  public:
StPadControlPanel() { fBar=PadControlPanel();}
//_______________________________________________________________________________________
static TControlBar *PadControlPanel(TControlBar *bar=0){
//
// This macro generates a Controlbar panel: 
// begin_html  <P ALIGN=CENTER> <IMG SRC="gif/PadControlPanel.gif" ></P> end_html
//
// To execute an item, click with the left mouse button.
//  
// Just start this macro wheneven you want:
//
//  From Root/Cint macro:
//  --------------------
//   .x PadControlPanel.C
//   .L PadControlPanel.C
// or
//   gROOT->LoadMacro("PadControlPanel.C");
//
//  From the compiled C++ code:
//  --------------------
//   gROOT->LoadMacro("PadControlPanel.C");
//
   if (bar) delete bar;
   bar = new TControlBar("vertical", "Pad Control Panel");
   bar->AddButton("Black background", "StPadControlPanel::SetBackround(kBlack);", "Change the backgroung color to black");
   bar->AddButton("White background", "StPadControlPanel::SetBackround(kWhite);", "Change the backgroung color to white");
//   bar->AddButton("Set background", "StPadControlPanel::SetBackroundStyle();", "Change the backgroung color to white");
   bar->AddButton("Centered","StPadControlPanel::Centered3DImages();","Place (0,0,0) into the center of the view port");
   bar->AddButton("Scale +","StPadControlPanel::Inscrease3DScale();","Change the scale of the image");
   bar->AddButton("Scale -","StPadControlPanel::Decrease3DScale();","Change the scale of the image");
   bar->AddButton("Top View","StPadControlPanel::TopView();","Show the top view");
   bar->AddButton("Side View","StPadControlPanel::SideView();","Show the side view");
   bar->AddButton("Front View","StPadControlPanel::FrontView();","Show the front view");
   bar->AddButton("4 views","StPadControlPanel::MakeFourView();","4 view");

   bar->Show();
   return bar;
}
//_______________________________________________________________________________________
~StPadControlPanel(){ if(fBar) delete fBar; fBar = 0;}
//_______________________________________________________________________________________
static void SetBackround(Color_t color, TVirtualPad *pad=0)
{
  TVirtualPad *thisPad = pad;
  if (!thisPad) thisPad = gPad;
  if (thisPad)  {
    thisPad->SetFillColor(color);
    thisPad->Modified();
    thisPad->Update();
  }
}

//_______________________________________________________________________________________
static void SetBackroundStyle(TVirtualPad *pad=0)
{
  TVirtualPad *thisPad = pad;
  if (!thisPad) thisPad = gPad;
  if (thisPad) thisPad->SetFillAttributes();  
}

//_______________________________________________________________________________________
static void RotateView(Float_t phi, Float_t theta, TVirtualPad *pad=0)
{
  TVirtualPad *thisPad = pad;
  if (!thisPad) thisPad = gPad;
  if (thisPad) {
    TView *view = thisPad->GetView(); 
    if (view) {
      Int_t iret;
      view->SetView(phi, theta, 0, iret);
      thisPad->SetPhi(-90-phi);
      thisPad->SetTheta(90-theta);
      thisPad->Modified();
      thisPad->Update();
    }
  }
}

//_______________________________________________________________________________________
static void FrontView(TVirtualPad *pad=0){
  RotateView(0,-90.0,pad);
}
//_______________________________________________________________________________________
static void TopView(TVirtualPad *pad=0){
  RotateView(90.0,-90.0,pad);
}
//_______________________________________________________________________________________
static void SideView(TVirtualPad *pad=0){
  RotateView(90.0,0.0,pad);
}
//_______________________________________________________________________________________
static void Centered3DImages()
{
  // This macro prints out the sizes of the sekected 3d pad
  TVirtualPad *thisPad = gPad;
  if (thisPad) {
    TView *view = thisPad->GetView(); 
    Float_t min[3],max[3];
    view->GetRange(min,max);
    int i;
    for (i=0;i<3; i++) min[i]=-max[i];
    view->SetRange(min,max);
    thisPad->Modified();
    thisPad->Update();
 }
}

//_______________________________________________________________________________________
static void Decrease3DScale()
{
  TVirtualPad *thisPad = gPad;
  if (thisPad) {
    TView *view = thisPad->GetView(); 
    Float_t min[3],max[3];
    view->GetRange(min,max);
    int i;
    for (i=0;i<2; i++) {max[i] /= 0.8; min[i]=max[i]*0.1;}
    view->SetRange(min,max);
    thisPad->Modified();
    thisPad->Update();
 }
}

//_______________________________________________________________________________________
static void Inscrease3DScale()
{
  TVirtualPad *thisPad = gPad;
  if (thisPad) {
    TView *view = thisPad->GetView(); 
    Float_t min[3],max[3];
    view->GetRange(min,max);
    int i;
    for (i=0;i<2; i++) {max[i] *= 0.8; min[i]=max[i]*0.1;}
    view->SetRange(min,max);
    thisPad->Modified();
    thisPad->Update();
  }
}
//_______________________________________________________________________________________
void MakeFourView(TVirtualPad *pad=0)
{
//  Creates 4 pads view of the pad (or gPad)
//   ------------------------------
//   |              |             |
//   |              |             |
//   |              |             |
//   |    Front     |   Top       |
//   |    view      |   view      |
//   |              |             |
//   |              |             |
//   |              |             |
//   ---------------+-------------
//   |              |             |
//   |              |             |
//   |              |             |
//   |    Side      |  Spacial    |
//   |    view      |   view      |
//   |              |             |
//   |              |             |
//   |              |             |
//   ------------------------------
  TVirtualPad *thisPad = pad;
  if (!thisPad) thisPad = gPad;
  TView *view = 0; 
  TList *thisPrimitives = 0; 
  if (thisPad && (thisPrimitives = thisPad->GetListOfPrimitives()) && (view =  thisPad->GetView()) ) 
  {
    Float_t min[3],max[3];
    view->GetRange(min,max);
    Int_t system = view->GetSystem();
    TCanvas *c = new TCanvas(" 4 views", thisPad->GetTitle(),600,600);
    c->Divide(2,2);
    TIter next(thisPrimitives);
    for (int i =1; i <= 4; i++) {
      c->cd(i);
      TList *newPrimitives = gPad->GetListOfPrimitives();
      TObject *obj = 0;
      while (obj = next()) newPrimitives->Add(obj);
      TView *newView = new TView(system);
      newView->SetRange(min,max);
      next.Reset();
   }
   // set separate view;
   // Fron view
    Int_t j = 1;
    c->cd(j++); FrontView();
    c->cd(j++); TopView();
    c->cd(j++); SideView();
    c->cd(j++); RotateView(60.0,60.0,0);
    c->Modified();
    c->Update();
  }
}
//_______________________________________________________________________________________
};

StPadControlPanel __aa__;
void PadControlPanel(){}
