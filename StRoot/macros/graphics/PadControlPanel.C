//*-- Author :    Valery Fine   25/05/99  (E-mail: fine@bnl.gov)
// $Id: PadControlPanel.C,v 1.2 1999/06/02 16:30:12 fine Exp $
// $Log: PadControlPanel.C,v $
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
   bar->AddButton("Front View","StPadControlPanel::FrontView();","Show the fornt view");

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
      thisPad->Modified();
      thisPad->Update();
    }
  }
}
//_______________________________________________________________________________________
static void FrontView(){
  RotateView(0,-90.0);
}
//_______________________________________________________________________________________
static void TopView(){
  RotateView(90.0,-90.0);
}
//_______________________________________________________________________________________
static void SideView(){
  RotateView(90.0,0.0);
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
};

StPadControlPanel __aa__;
void PadControlPanel(){}
