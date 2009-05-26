// $Id: StDraw3D.cxx,v 1.36 2009/05/26 19:07:06 fine Exp $
//*-- Author :    Valery Fine(fine@bnl.gov)   27/04/2008
#include "StDraw3D.h"
#include "TCanvas.h"
#include "TPolyMarker3D.h"
#include "TPolyLine3D.h"
#include "TSystem.h"
#include "TROOT.h"
#include "TEnv.h"
#include "StCheckQtEnv.h"
#include "TStyle.h"
#include "TVirtualViewer3D.h"

static Color_t colorDefault = Color_t(-1);
static Style_t styDefault   = Style_t(-1);
static Size_t  sizDefault   = Size_t (-1);

Color_t StDraw3D::fgColorDefault = Color_t(-1);
Style_t StDraw3D::fgStyDefault   = Style_t(-1);
Size_t  StDraw3D::fgSizDefault   = Size_t (-1);
Color_t StDraw3D::fgBkColor      = kBlack;
// Canvas counter to create the Unique Canvas names
Int_t   StDraw3D::fDrawCanvasCounter = -1; 

ClassImp(StDraw3D)
           
  ////////////////////////////////////////////////////////////////////////
  //
  //  Class StDraw3D - to draw the 3D primitives like 3D points and 3D lines
  //  decoratated with the STAR detector geometry
  //
  //  It provides the simple way to visualize the event 
  //  primitives in 3D against of the STAR detector 
  //  geometry quickly.
  //  <begin_html> <img src="http://www.star.bnl.gov/public/comp/vis/StDraw3D/examples/Draw3DClass.png">end_html
  //
  ////////////////////////////////////////////////////////////////////////

//___________________________________________________
static inline TVirtualViewer3D *InitCoin(TVirtualPad *pad,const char *detectorName) 
{
   TVirtualViewer3D *viewer = 0;
   // check Coin env and load if present
   TString ivrootDir = gSystem->Getenv("IVROOT");
   if (ivrootDir.IsNull() )  ivrootDir = "$ROOT/5.99.99/Coin2/.$STAR_HOST_SYS";
   ivrootDir +=   "/lib/";
   gSystem->ExpandPathName(ivrootDir);
   static bool CheckCoin = false;
   if (!CheckCoin && !gSystem->AccessPathName(ivrootDir.Data())) {
      if (     !gSystem->Load(ivrootDir+"libCoin") 
            && !gSystem->Load(ivrootDir+"libSoQt")
            && !gSystem->Load(ivrootDir+"libSmallChange"));
      if (!StCheckQtEnv::SetQtEnv(false)) {   CheckCoin = true; }
   }

//   if (CheckCoin && pad ) {
   if (CheckCoin) {
      // define the background image
      TString backShape = detectorName;
      backShape.ReplaceAll(",",".iv:");
      backShape+= ".iv";
      printf(" Setting the background shape to be 	%s\n", backShape.Data());
      gEnv->SetValue("Gui.InventorShapeDir",":.:StRoot/macros/graphics:$STAR/StRoot/macros/graphics:/afs/rhic.bnl.gov/star/doc/www/comp/2/vis/iv");
      if  (viewer = TVirtualViewer3D::Viewer3D(pad,"oiv")) {
          viewer->SetDrawOption(backShape.Data());
         // Create Open GL viewer
//        TGQt::SetCoinFlag(1);
         viewer->BeginScene();
         viewer->EndScene();
      }
    }
    return viewer;
}

//___________________________________________________
//
//   view_3D
//___________________________________________________
class view_3D  {
   private:
      TObject *fModel;
      TString  fComment;
      TString  fObjectInfo;
      void makeInfo()
      {
         fObjectInfo="";
         if (fModel) {
            fObjectInfo = Form("( %s *)%p ",fModel->ClassName(),fModel);
         }
         if (!fComment.IsNull()) fObjectInfo += fComment;
      }
   public:
     view_3D(TObject *model = 0, const char *comment="") : fModel(model),fComment(comment)
     { makeInfo(); }
     ~view_3D(){;}
     TObject *model() const               { return fModel;                   }
     void setModel(TObject *model)        { fModel     = model ; makeInfo(); }
     void setComment(const char *comment) { fComment  = comment; makeInfo(); }
     void addComment(const char *comment) { fComment += comment; makeInfo(); }
     const TString &info() const { return fObjectInfo;              }
};

//___________________________________________________
//
//   poly_line_3D
//___________________________________________________
class poly_line_3D : public TPolyLine3D, public view_3D {
   public:
     poly_line_3D(Int_t n, Float_t *p, Option_t *option="") : TPolyLine3D(n,p),view_3D(){;}
     virtual ~poly_line_3D(){;}
     virtual char  *GetObjectInfo(Int_t x, Int_t y) const
     {
        const TString &customInfo = info();
        const char *info = 0;
        if (customInfo.IsNull()) 
           info = TPolyLine3D::GetObjectInfo(x,y);
        else 
           info = customInfo.Data();
        return (char *)info;
     }
     void Inspect() const {
        if ( model() ) model()->Inspect();
        else TPolyLine3D::Inspect();
     }
};

//___________________________________________________
//
//   poly_marker_3D
//___________________________________________________
class poly_marker_3D : public TPolyMarker3D, public view_3D {
   public:
     poly_marker_3D(Int_t n, Float_t *p, Option_t *option="") : TPolyMarker3D(n,p,1,option),view_3D(){;}
     virtual ~poly_marker_3D(){;}
     virtual char  *GetObjectInfo(Int_t x, Int_t y) const
     {
        const TString &customInfo = info();
        const char *info = 0;
        if (customInfo.IsNull()) 
           info = TPolyMarker3D::GetObjectInfo(x,y);
        else 
           info = customInfo.Data();
        return (char *)info;
     }
     void Inspect() const {
        if ( model() ) model()->Inspect();
        else TPolyMarker3D::Inspect();
     }
};
//___________________________________________________
StDraw3D::StDraw3D(const char *detectorName,TVirtualPad *pad): fPad(pad),fBkColor(fgBkColor),fViewer(0),fView(0)
      , fDetectorName(detectorName),fMaster(0)
{

   // The detectorName is a comma separated list of the OpenInventor files with no extension
   // For all names on the list one should provide the iv file with the "iv extension:
   //                         <name>.iv

   static const Style_t UHitSty = 4; static const Size_t UHitSiz = 0.35; static const Color_t UHitCol=kBlue;
   static const Style_t NHitSty = 1; static const Size_t NHitSiz = 1.00; static const Color_t NHitCol=kGreen;
   static const Style_t TrakSty = 1; static const Size_t TrakSiz = 2.00; static const Color_t TrakCol=kRed;
   static const Style_t VertSty = 5; static const Size_t VertSiz = 3.50; static const Color_t VertCol=kYellow;
   AddStyle(kVtx,         VertCol,VertSty,VertSiz);
   AddStyle(kPrimaryTrack,TrakCol,TrakSty,TrakSiz);
   AddStyle(kGlobalTrack, TrakCol,TrakSty,TrakSiz);
   AddStyle(kTrackBegin,  VertCol,VertSty,VertSiz);
   AddStyle(kTrackEnd,    VertCol,VertSty,VertSiz);
   AddStyle(kUsedHit,     UHitCol,UHitSty,UHitSiz);
   AddStyle(kUnusedHit,   NHitCol,NHitSty,NHitSiz);
}

//__________________________________________________________________________________
TVirtualPad *StDraw3D::InitPad() 
{
   if (fMaster) fMaster->InitPad();
   else if (!fPad ) {
      fDrawCanvasCounter++;
      TString canvasName = "STAR";
      TString canvasTitle;
      if (fDrawCanvasCounter) {
           canvasName+="_";
           canvasName  += fDrawCanvasCounter;
           canvasTitle += fDrawCanvasCounter;
           canvasTitle += " : ";
     }
      canvasTitle += "STAR Event Viewer";
      fPad = new TCanvas(canvasName.Data(),canvasTitle.Data(), 400,400);
      fPad->SetFillColor(fBkColor);
      fPad->Modified();
      fPad->Update();
   }
   return Pad();
}
//__________________________________________________________________________________
void StDraw3D::InitViewer()
{
   //Create 3D viewer if  no master provided

   if (fMaster) fMaster->InitViewer();
   else if ( !fViewer ) fViewer = InitCoin(fPad,fDetectorName);
   assert(Viewer());
}
//___________________________________________________
StDraw3D::~StDraw3D()
{
    if (fPad) {
       if (!fMaster) fPad->Clear();
       delete fPad;
       fPad    = 0;
       fMaster = 0;
    }
}

//__________________________________________________________________________________
const TString &StDraw3D::DetectorNames() const 
{
   // return the list of the names 
   return fDetectorName; 
} 
//__________________________________________________________________________________
TVirtualPad *StDraw3D::Pad() const 
{ 
   return fMaster ? fMaster->Pad() : fPad;
}

//__________________________________________________________________________________
TVirtualViewer3D *StDraw3D::Viewer() const
{
   return fMaster ? fMaster->Viewer() : fViewer;
}

//__________________________________________________________________________________
void StDraw3D::SetDetectors(const char*nameDetectors)
{
   // The detectorName is a comma separated list of the OpenInventor files with no extension
   // For all names on the list one should provide the iv file with the "iv extension:
   //                         <name>.iv
   if (fViewer) {
      Warning("StDraw3D::SetDetectors","Can not change the detector names. It is too late. The viewer had been created");
   } else {
      fDetectorName = nameDetectors;
   }
}
//__________________________________________________________________________________
void StDraw3D::AddDetectors(const char*nameDetectors)
{ 
   // The detectorName is a comma separated list of the OpenInventor files with no extension
   // For all names on the list one should provide the iv file with the "iv extension:
   //                         <name>.iv

   if (fViewer) {
      Warning("StDraw3D::AddDetectors","Can not the change detector names. It is too late. The viewer had been created");
   } else if (nameDetectors && nameDetectors[0]){
      fDetectorName += ",";
      fDetectorName += nameDetectors;
   }
}

//___________________________________________________
void  StDraw3D::Clear(Option_t *opt)
{
   // Clear the view
   TVirtualPad *pad = Pad();
   if (pad) {
      pad->Clear(opt);
      Update();
   }
}

//___________________________________________________
TObject *StDraw3D::Draw(TObject *o)
{
   // Draw the 3d object 
   // and set the new background color if needed
   if (o) {
      TVirtualPad *sav = gPad;
      if (!Pad())        InitPad();
      if (Pad() != sav)  Pad()->cd();
      assert (fPad==gPad);
      o->Draw();
      if (sav && (Pad() != sav)) sav->cd();
      if (!Viewer()) InitViewer();
   }
   return o;
}
//___________________________________________________
void StDraw3D::SetBkColor(Color_t newBkColor)
{
   // Set the canvas background color;
   fBkColor = newBkColor;
   TVirtualPad *pad = Pad();
   if (pad && pad->GetFillColor() != fBkColor)
       pad->SetFillColor(fBkColor);
}
//___________________________________________________
const StDraw3DStyle &StDraw3D::AddStyle(EDraw3DStyle type,Color_t col,Style_t sty,Size_t siz)
{
   fStyles.insert(std::pair<EDraw3DStyle,StDraw3DStyle>(type,StDraw3DStyle(type,col,sty,siz)));
   return Style(type);
}

//___________________________________________________
const StDraw3DStyle &StDraw3D::Style(EDraw3DStyle type)
{
    return fStyles[type];
}

//___________________________________________________
TObject *StDraw3D::Points(int n, const float *xyz, Color_t col,Style_t sty,Size_t siz)
{ 
   //
   // Draw "n" points of the "xyz" array of the float coordinates 
   // with ROOT TPolyMarker3D class
   // with the ROOT color, style, size attributes
   //
   
   poly_marker_3D *plMk  = new poly_marker_3D(n,(Float_t*)xyz);
   if (col != colorDefault) plMk->SetMarkerColor(col);
   if (sty != styDefault)   plMk->SetMarkerStyle(sty);
   if (siz != sizDefault)   plMk->SetMarkerSize(siz);
   fView = plMk;
   return Draw(plMk);
}
//___________________________________________________
TObject *StDraw3D::Points(int n, const float *xyz, EDraw3DStyle sty)
{
   //
   // Draw "n" points of the "xyz" array of the float coordinates 
   // with ROOT TPolyMarker3D class and the predefined attrbutes
   //
   // This is an overloaded member function, provided for convenience.
   // It behaves essentially like the above function.
   //
   
  const StDraw3DStyle &style =  Style(sty);
  return Points(n, xyz, style.Col(),style.Sty(),style.Siz());
}

//___________________________________________________
TObject *StDraw3D::Draw3D(int n,  const float *xyz)
{
   //
   // Draw "n" points of the "xyz" array of the float coordinates 
   // and the kVtx attrbute
   //
   // This is an overloaded member function, provided for convenience.
   // It behaves essentially like the above function.
   //
   return Points(n,xyz,kVtx);
}
//___________________________________________________
TObject *StDraw3D::Point(float x, float y, float z, Color_t col,Style_t sty,Size_t siz)
{
   //
   // Draw ONE 3D marker with ROOT TPolyMarker3D class at x,y,z position
   // with the ROOT color, style, size attributes
   //
   
   float xyz[]={x,y,z};
   return Points(1,xyz,col,sty,siz);
}

//___________________________________________________
TObject *StDraw3D::Point(float x, float y, float z, EDraw3DStyle sty)
{
   //
   // Draw ONE 3D marker with ROOT TPolyMarker3D class at x,y,z position
   // and the predefined attrbutes
   //
   // This is an overloaded member function, provided for convenience.
   // It behaves essentially like the above function.
   //
   const StDraw3DStyle &style =  Style(sty);
   return Point(x,y, z, style.Col(),style.Sty(),style.Siz());
}

//___________________________________________________
TObject *StDraw3D::Line(int n,  const float *xyz, Color_t col,Style_t sty,Size_t siz)
{
   //
   // Draw "n" points of the "xyz" array of the float coordinates 
   // with ROOT TPolyline3D class
   // with the ROOT color, style, size attributes
   //
   poly_line_3D *plLine  = new poly_line_3D(n,(Float_t*)xyz);
   if (col != colorDefault) plLine->SetLineColor(col);
   if (sty != styDefault)   plLine->SetLineStyle(sty);
   if (siz != sizDefault)   plLine->SetLineWidth(Width_t(siz));
   fView = plLine;
   return Draw(plLine);
}

//___________________________________________________
TObject *StDraw3D::Line(int n,  const float *xyz,EDraw3DStyle sty)
{
   //
   // Draw "n" points of the "xyz" array of the float coordinates 
   // with ROOT TPolyLine3D class and the predefined attrbutes
   //
   // This is an overloaded member function, provided for convenience.
   // It behaves essentially like the above function.
   //
   const StDraw3DStyle &style =  Style(sty);
   return Line(n,xyz,  style.Col(),style.Sty(),style.Siz() );
}
//___________________________________________________
void StDraw3D::Joint(StDraw3D *dsp)
{
   // The method to force two different instancses
   // of the StDraw3D class  paint onto one and the same
   // TPad.

   // Force "dsp" to share the fPad of this object
   // As result of the "Joint method both objects 
   // "this" as well as "dsp" will share the PAd of "this" object.
   // The original TPad of "dsp" is to be abandoned if exists

   if (dsp) dsp->SetMaster(this);
}
//___________________________________________________
void StDraw3D::Redraw()
{
   // Move all existent view to the master window if any
   if (fMaster && fPad) {
      TList *p = fPad->GetListOfPrimitives();
      if (p) {
         TObject *o = 0;
         TIter next(p);
         while (o=next())  Draw(o);
         p->Clear();
      }
   }
}
//___________________________________________________
void StDraw3D::SetMaster(StDraw3D *master) 
{
   //Make this object slave of the "master" object
   if (fMaster != master) {
      if (fMaster) 
         Error("StDraw3D::SetMaster"
               ,"The object (StDraw3D*)%p already has another master %p", this, fMaster);
     fMaster = master;
     Redraw();
   }
}

//___________________________________________________
void StDraw3D::SetModel(TObject *model)
{
   // add the "model" reference to the current view
   if (fView) fView->setModel(model);
}

//___________________________________________________
void StDraw3D::SetComment(const char *cmnt)
{
   // set the "model" comment for the current view
   if (fView) fView->setComment(cmnt);
}

//___________________________________________________
void StDraw3D::AddComment(const char *cmnt)
{
   // add the "model" comment for the current view
   if (fView) fView->addComment(cmnt);
}

//___________________________________________________
void StDraw3D::Update()
{
   TVirtualPad *pad = Pad();
   if (pad) {
      TVirtualPad *sav = gPad;
      if (pad != sav)  pad->cd();
      assert (pad==gPad);
      pad->Update();
      if (sav && (pad != sav)) sav->cd();
   }
}

//___________________________________________________
void StDraw3D::Modified()
{
   // One doesn't need to call this method
   // because one can not change any object yet
   TVirtualPad *pad = Pad();
   if (pad) {
      TVirtualPad *sav = gPad;
      if (pad != sav)  pad->cd();
      assert (pad==gPad);
      pad->Modified();
      if (sav && (pad != sav)) sav->cd();
   }
}

//___________________________________________________
void StDraw3D::Draw3DTest(){
   //  ------------------------------------------------
   //  The  method to test the class
   //   It should produce the #D Coin widget:
   //  <begin_html> <img src="http://www.star.bnl.gov/public/comp/vis/StDraw3D/examples/Draw3DClass.png">end_html
   //  ------------------------------------------------
   //                 x             y              z  
   //  ------------------------------------------------
   float xyz[] = { 189.195,       27.951,       123.966
                  ,187.195,       28.6187,      122.89
                  ,181.195       ,30.6788      ,119.556
                  ,179.195       ,31.3387      ,118.454
                  ,177.195       ,32.0065      ,117.328
                  ,175.195       ,32.6132      ,116.26
                  ,173.195       ,33.2385      ,115.146
                  ,171.195       ,33.8552      ,114.016
                  ,169.195       ,34.3924      ,112.964
         };

   int sizeXYZ = sizeof(xyz)/sizeof(float)/3;
   
   Draw3D(sizeXYZ,xyz);
   SetComment("The hits from the TPC sector");
   
   Line(sizeXYZ,xyz,kGlobalTrack);
   SetComment("The recontstructed track");
}

//______________________________________________________________________________
void StDraw3D::ShowDetectorTest(const char *detectorName)
{
   // Test to show the detector geometry only
   StDraw3D *viewer = new StDraw3D(detectorName); 
   if (!viewer->Viewer()) viewer->InitViewer();
}
//______________________________________________________________________________
void StDraw3D::ShowTest()
{
    // More complex test.
    //
    // It creates TWO different widgets
    // One is decorated with the detector geometry, 
    // another one "plain"
    //
    // Method does not recreate the widgets when it is called 
    // for several times
    //
    // It creates the widget at once and reuses it with each call.

   static StDraw3D *fine[2]={0};
   if (!fine[0]) {
      fine[0] = new StDraw3D;
      fine[1] = new StDraw3D(0);// View with no detector geometry decoration
   } else {
      fine[0]->Clear();
      fine[1]->Clear();
   }
//        P  G         P  G
  float NodX[100][3]= {{189.195,       27.951,       123.966}
                      ,{187.195,       28.6187,      122.89 }
                      ,{181.195       ,30.6788      ,119.556}
                      ,{179.195       ,31.3387      ,118.454}
                      ,{177.195       ,32.0065      ,117.328}
                      ,{175.195       ,32.6132      ,116.26 }
                      ,{173.195       ,33.2385      ,115.146}
                      ,{171.195       ,33.8552      ,114.016}
                      ,{169.195       ,34.3924      ,112.964}};

  float HitX[100][3]= {{189.195,       27.951,       123.966}
                      ,{187.195,       28.6187,      122.89 }
                      ,{181.195       ,30.6788      ,119.556}
                      ,{179.195       ,31.3387      ,118.454}
                      ,{177.195       ,32.0065      ,117.328}
                      ,{175.195       ,32.6132      ,116.26 }
                      ,{173.195       ,33.2385      ,115.146}
                      ,{171.195       ,33.8552      ,114.016}
                      ,{169.195       ,34.3924      ,112.964}};

  float NodL[100][3]= {{189.195+5,       27.951+10,       123.966-50}
                      ,{187.195+5,       28.6187+10,      122.89-50 }
                      ,{181.195+5       ,30.6788+10      ,119.556-50}
                      ,{179.195+5       ,31.3387+10      ,118.454-50}
                      ,{177.195+5       ,32.0065+10      ,117.328-50}
                      ,{175.195+5       ,32.6132+10      ,116.26-50 }
                      ,{173.195+5       ,33.2385+10      ,115.146-50}
                      ,{171.195+5       ,33.8552+10      ,114.016-50}};
  float HitL[100][3]= {{189.195+5,       27.951+10,       123.966-50}
                      ,{187.195+5,       28.6187+10,      122.89-50 }
                      ,{181.195+5       ,30.6788+10      ,119.556-50}
                      ,{179.195+5       ,31.3387+10      ,118.454-50}
                      ,{177.195+5       ,32.0065+10      ,117.328-50}
                      ,{175.195+5       ,32.6132+10      ,116.26-50 }
                      ,{173.195+5       ,33.2385+10      ,115.146-50}
                      ,{171.195+5       ,33.8552+10      ,114.016-50}};
  int nN=9,nH=9;
  // Draw the test points
  fine[0]->Points(nH, HitX[0], kVtx);
  fine[0]->SetComment("Hits and Geometry");
  
  fine[0]->Line  (nN, NodX[0], kGlobalTrack);  
  fine[0]->SetComment("Track and Geometry");
  
  fine[1]->Points(nH, HitL[0], kVtx);
  fine[1]->SetComment("Hits no Geometry");
  
  fine[1]->Line  (nN, NodL[0], kGlobalTrack);
  fine[1]->SetComment("Track no Geometry");
  for (int i=0;i<2;i++) { fine[i]->Modified(); fine[i]->Update();}
//  while(!gSystem->ProcessEvents()){}; 
}
