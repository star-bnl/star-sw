//*-- Author :    Valery Fine   25/05/99  (E-mail: fine@bnl.gov)
//
// Copyright (C)  Valery Fine, Brookhaven National Laboratory, 1999. All right reserved
//
// $Id: PadControlPanel.C,v 1.20 2003/10/09 18:11:54 perev Exp $
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
//  After that one may "click" <4 views> button to get from the single "view"
//  the expanded view as follows:
//  begin_html  <P ALIGN=CENTER> <IMG SRC="gif/FourStarView.gif" ></P> end_html
//  To cutomize the default bar the dirived class with the custom void UserBar()
//  method can be done.
//         TGButtonGroup *Bar(){ return fBar;}  
//  method can be used.
//
//  Example:
//    TGButtonGroup *myBar =   __aa__.Bar();
//     myBar->AddButton("My custom","printf(\"here is my custom action\n\");","To add your own action replace the second parameter"); 
//
//  Note:  If you don't like what it does make your private copy 
//  ====   change it with your favorite text editor and load it right
//         away.
//         NO EXTRA STEP like : compilation, linking, loading required 
//
///////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////
//
//  PadControl panel is the set of the static methods to control 
//  TView of any "current" TPad with some "primitive"
//  operation:
//
///////////////////////////////////////////////////////////////////////
TGButtonGroup *mainBar=0;

TVirtualPad *qPad(){ return  TVirtualPad::Pad() ;}

class StPadControlPanel {
  private:
   TGButtonGroup *fBar;  
   TGLayoutHints *fL1;

protected:


//_______________________________________________________________________________________
   void AddButt(const Char_t *buttonName, const Char_t *command)
   {   
       TGTextButton *tb = new TGTextButton(fBar,buttonName,command);
       fBar->AddFrame(tb,fL1);   
   }

public:


StPadControlPanel() { Build();}
//_______________________________________________________________________________________
void  Build()
{
   const char *fills[] = {
    "Black background",	"StPadControlPanel::SetBackround(kBlack);"
   ,"White background",	"StPadControlPanel::SetBackround    (19);"
   ,"Adjust scales",	"StPadControlPanel::AdjustScales    ();"
   ,"Centered",		"StPadControlPanel::Centered3DImages();"
   ,"Scale +",		"StPadControlPanel::Inscrease3DScale();"
   ,"Scale -",		"StPadControlPanel::Decrease3DScale ();"
   ,"Top View (X-Y)",	"StPadControlPanel::TopView     ();"
   ,"Side View (Y-Z)",	"StPadControlPanel::SideView    ();"
   ,"Front View (X-Z)",	"StPadControlPanel::FrontView   ();"
   ,"4 views",		"StPadControlPanel::MakeFourView();"
   ,"Add Axes",		"StPadControlPanel::AddAxes     ();"
   ,"Rulers",		"StPadControlPanel::ToggleRulers();"
   ,"Zoom",		"StPadControlPanel::ToggleZoom  ();"
   ,"ReDraw canvas",	"StEventDisplayMaker::MakeLoop(1);"
   ,"Next event",	"StEventDisplayMaker::MakeLoop(2);"
   ,"End of Chain",	"StEventDisplayMaker::MakeLoop(3);"
   ,0};


   fBar = new TGButtonGroup(gClient->GetRoot(), "Pad Control Panel");
   mainBar=fBar;
   gVirtualX->SetWindowName(fBar->GetId(),"Pad");
   fL1 = new TGLayoutHints(kLHintsCenterY | kLHintsExpandX, 1, 1, 1, 1);
   for (int i=0;fills[i];i+=2) {AddButt(fills[i],fills[i+1]);}

   fBar->Show();
}
//_______________________________________________________________________________________
~StPadControlPanel(){ delete fBar; delete fL1;}
//_______________________________________________________________________________________
 TGButtonGroup *Bar() const { return fBar;}  

//_______________________________________________________________________________________
static void SetBackround(Color_t color, TVirtualPad *pad=0)
{
  TVirtualPad *thisPad = pad;
  if (!thisPad) thisPad = qPad();
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
  if (!thisPad) thisPad = qPad();
  if (thisPad) thisPad->SetFillAttributes();  
}

//_______________________________________________________________________________________
static void RotateView(Float_t phi, Float_t theta, TVirtualPad *pad=0)
{
  TVirtualPad *thisPad = pad;
  if (!thisPad) thisPad = qPad();
  if (thisPad) {
    TView *view = thisPad->GetView(); 
    if (view) {
      Int_t iret;
      Float_t p = phi;
      Float_t t = theta;
      view->SetView(p, t, 0, iret);
      thisPad->SetPhi(-90-p);
      thisPad->SetTheta(90-t);
      thisPad->Modified();
      thisPad->Update();
    }
  }
}

//_______________________________________________________________________________________
static void SideView(TVirtualPad *pad=0){
  RotateView(0,90.0,pad);
}
//_______________________________________________________________________________________
static void FrontView(TVirtualPad *pad=0){
  RotateView(270.0,90.0,pad);
}
//_______________________________________________________________________________________
static void TopView(TVirtualPad *pad=0){
  RotateView(270.0,0.0,pad);
}
//_______________________________________________________________________________________
static void ToggleRulers(TVirtualPad *pad=0)
{
  TAxis3D::ToggleRulers(pad);
}

//_______________________________________________________________________________________
static void ToggleZoom(TVirtualPad *pad=0)
{
  TAxis3D::ToggleZoom(pad);
}

//_______________________________________________________________________________________
static void AddGrid()
{ 
  TVirtualPad *thisPad = qPad();

  if (thisPad) {
 
    TView *view = thisPad->GetView(); 
    if (!view) return;
    Double_t min[3],max[3];
    view->GetRange(min,max);

    TList *list      = thisPad->GetListOfPrimitives();
    TString histName = thisPad->GetName();
    TH2F *m_DummyHist = 0; 
    const Char_t *dummyName = "Axis3D";
    histName += dummyName;
    m_DummyHist = list->FindObject(histName.Data());
    if (!m_DummyHist) { 
      m_DummyHist = new TH2F(histName.Data(),"",1,min[0],max[0],1,min[1],max[1]);
      m_DummyHist->SetDirectory(0);
      m_DummyHist->Draw("surf,same");
    }
    m_DummyHist->GetXaxis()->SetLimits(min[0],max[0]);
    m_DummyHist->GetYaxis()->SetLimits(min[1],max[1]);
    m_DummyHist->GetZaxis()->SetLimits(min[2],max[2]);
 
    thisPad->Modified();
    thisPad->Update();
  }
}
//_______________________________________________________________________________________
static void AdjustScales()
{
  TVirtualPad *thisPad = qPad();
  if (thisPad) {
    TView *view = thisPad->GetView(); 
    if (!view) return;
    Double_t min[3],max[3];
    view->GetRange(min,max);
    int i;
    Double_t maxSide = 0;
    // Find the largest side
    for (i=0;i<3; i++) maxSide = TMath::Max(maxSide,max[i]-min[i]);
    //Adjust scales:
    for (i=0;i<3; i++) max[i] += maxSide - (max[i]-min[i]);
    view->SetRange(min,max);
    thisPad->Modified();
    thisPad->Update();
 }
}
//_______________________________________________________________________________________
static void Centered3DImages()
{
  // This macro prints out the sizes of the sekected 3d pad
  TVirtualPad *thisPad = qPad();
  if (thisPad) {
    TView *view = thisPad->GetView(); 
    if (!view) return;
    Double_t min[3],max[3];
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
  TVirtualPad *thisPad = qPad();
  if (thisPad) {
    TView *view = thisPad->GetView(); 
    if (!view) return;
    Double_t min[3],max[3];
    view->GetRange(min,max);
    int i;
    for (i=0;i<3; i++) {max[i] /= 0.8; min[i]=max[i]*0.1;}
    view->SetRange(min,max);
    thisPad->Modified();
    thisPad->Update();
 }
}

//_______________________________________________________________________________________
static void Inscrease3DScale()
{
  TVirtualPad *thisPad = qPad();
  if (thisPad) {
    TView *view = thisPad->GetView(); 
    if (!view) return;
    Double_t min[3],max[3];
    view->GetRange(min,max);
    int i;
    for (i=0;i<3; i++) {max[i] *= 0.8; min[i]=max[i]*0.1;}
    view->SetRange(min,max);
    thisPad->Modified();
    thisPad->Update();
  }
}
//_______________________________________________________________________________________
void MakeFourView(TVirtualPad *pad=0)
{
//  Creates 4 pads view of the pad (or qPad)
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
// begin_html  <P ALIGN=CENTER> <IMG SRC="gif/FourStarView.gif" ></P> end_html
//
  TVirtualPad *thisPad = pad;
  if (!thisPad) thisPad = qPad();
  TView *view = 0; 
  TList *thisPrimitives = 0; 
  if (thisPad && (thisPrimitives = thisPad->GetListOfPrimitives()) && (view =  thisPad->GetView()) ) 
  {
    Double_t min[3],max[3];
    view->GetRange(min,max);
    Int_t system = view->GetSystem();
    TCanvas *c = new TCanvas(" 4 views", thisPad->GetTitle(),600,600);
    c->Divide(2,2);
    TIter *next=  new TIter(thisPrimitives);
    for (int i =1; i <= 4; i++) {
      c->cd(i);
      TList *newPrimitives = qPad()->GetListOfPrimitives();
      TObject *obj = 0;
      while (obj = next->Next()) newPrimitives->Add(obj);
      TView *newView = new TView(system);
      newView->SetRange(min,max);
      next->Reset();
   }
   delete next;
   // set separate view;
   // Fron view
    Int_t j = 1;
    c->cd(j++); FrontView();
    c->cd(j++); TopView();
    c->cd(j++); SideView();
    c->cd(j++); RotateView(-30.0,60.0,0);
    c->Modified();
    c->Update();
  }
}
//_______________________________________________________________________________________
void AddAxes(TVirtualPad *pad=0)
{
  // Add red, green, blue - X, Y, Z axice to the "pad"
  TVirtualPad *thisPad = pad;
  if (!thisPad) thisPad = qPad();
  if (thisPad) {
    if (!gROOT->GetClass("St_PolyLine3D"))  gSystem->Load("St_base");
    if ( gROOT->GetClass("St_PolyLine3D"))  gROOT->ProcessLineFast("St_PolyLine3D::Axis();");
  }
}
};

StPadControlPanel __StPadControlPanel__;


// $Log: PadControlPanel.C,v $
// Revision 1.20  2003/10/09 18:11:54  perev
// calculate gPad pointer
//
// Revision 1.19  2001/09/01 23:36:57  perev
// WindowName added
//
// Revision 1.18  2001/09/01 19:58:14  perev
// scripts for StEvent draw inside of StEventDisplayMaker
//
// Revision 1.17  2000/08/21 22:25:46  fine
// XYZ labels were added to the panel buttons
//
// Revision 1.16  2000/07/17 17:35:30  fine
// Adjusted to new ROOT requirements: float / double
//
// Revision 1.15  1999/12/09 20:42:47  fine
// Zoom
//
// Revision 1.14  1999/11/30 20:09:52  fine
// new static method to present rulers
//
// Revision 1.13  1999/11/30 03:00:00  fine
// Ruler button has been introduced
//
// Revision 1.12  1999/11/13 23:31:34  fine
// Constant kWhite has been replaced with number 19 due some bug wity 2.23
//
// Revision 1.11  1999/07/13 00:34:16  fine
//  new button has been added to draw 3D axes
//
// Revision 1.10  1999/06/11 19:14:01  fine
// Some extra protections agaist of view == 0
//
// Revision 1.9  1999/06/11 18:17:55  fine
// View have been standardtized
//
// Revision 1.8  1999/06/11 01:27:37  fine
// TIter fixed
//
// Revision 1.7  1999/06/10 19:44:16  fine
// AdjustScale button jas been introduced
//
// Revision 1.6  1999/06/10 03:40:53  fine
// New button to draw 3D axice added
//
// Revision 1.5  1999/06/03 00:35:34  fine
// Comments clean up
//
// Revision 1.4  1999/06/03 00:27:53  fine
//  4 view control has been activated
//
// Revision 1.3  1999/06/02 22:25:12  fine
// 4 view command has been introduced
//
// Revision 1.2  1999/06/02 16:30:12  fine
// Clean up
//
// Revision 1.1  1999/05/29 20:55:11  fine
// macro to control any 3D view
//

