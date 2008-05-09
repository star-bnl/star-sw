// $Id: StDraw3D.cxx,v 1.24 2008/05/09 23:08:51 fine Exp $
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

int StDraw3D::fgDraw_3d_init = 0;

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
  //  primitives in 3D quickly against of the STAR detector 
  //  geometry.
  //  <begin_html> <img src="http://www.star.bnl.gov/public/comp/vis/StDraw3D/Draw3DClass.png">end_html
  //
  ////////////////////////////////////////////////////////////////////////

//___________________________________________________
static inline TVirtualViewer3D *InitCoin(TVirtualPad *pad,const char *detectorName) 
{
   TVirtualViewer3D *viewer = 0;
   // check Coin env and load if present
   TString ivrootDir = gSystem->Getenv("IVROOT");
   if (ivrootDir.IsNull() ) ivrootDir = "$ROOT/5.99.99/Coin2/.$STAR_HOST_SYS";
   ivrootDir +=   "/lib/";
   gSystem->ExpandPathName(ivrootDir);
   static bool CheckCoin = false;
   if (!gSystem->AccessPathName(ivrootDir.Data())) {
      if (     !gSystem->Load(ivrootDir+"libSoQt") 
            && !gSystem->Load(ivrootDir+"libCoin")
            && !gSystem->Load(ivrootDir+"libSmallChange"));
      if (!StCheckQtEnv::SetQtEnv(false)) {   CheckCoin = true; }
   }

   if (CheckCoin && pad ) {
      // define the background image
      TString backShape = detectorName;
      backShape.ReplaceAll(",",".iv;");
      backShape+= ".iv";
      printf(" Setting the background shape to be 	%s\n", backShape.Data());
      gEnv->SetValue("Gui.InventorShapeDir",":.:StRoot/macros/graphics:$STAR/StRoot/macros/graphics");
      gEnv->SetValue("Gui.InventorBackgroundShape",backShape.Data());
      if  (viewer = TVirtualViewer3D::Viewer3D(pad,"oiv")) {
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
StDraw3D::StDraw3D(TVirtualPad *pad, const char *detectorName): fPad(pad),fBkColor(fgBkColor),fViewer(0),fView(0)
      , fDetectorName(detectorName)
{

   // The detectorName is a comma separated list of the OpenInventor files with no extension
   // For all names on the list one should provide the iv file with the "iv extension:
   //                         <name>.iv

   static const Style_t UHitSty = 4; static const Size_t UHitSiz = 0.35; static const Color_t UHitCol=kBlue;
   static const Style_t NHitSty = 1; static const Size_t NHitSiz = 1.00; static const Color_t NHitCol=kGreen;
   static const Style_t TrakSty = 1; static const Size_t TrakSiz = 1.00; static const Color_t TrakCol=kRed;
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
   if (!fPad) {
      fDrawCanvasCounter++;
      TString canvasName = "STAR";
      if (fDrawCanvasCounter) {
           canvasName+="_";
           canvasName += fDrawCanvasCounter;
      }
      fPad = new TCanvas(canvasName.Data(),"Event Viewer", 400,400);
      fPad->SetFillColor(fBkColor);
      fPad->Modified();
      fPad->Update();
   }
   return fPad;
}

//___________________________________________________
StDraw3D::~StDraw3D()
{
    if (fPad) {
       fPad->Clear();
       delete fPad;
       fPad = 0;
    }
}

//__________________________________________________________________________________
const TString &StDraw3D::DetectorNames() const 
{
   // return the list of the names 
   return fDetectorName; 
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
   if (fPad) {
      fPad->Clear(opt);
      Modified();
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
      if (!fPad)        InitPad();
      if (fPad != sav)  fPad->cd();
      o->Draw();
      if (sav && (fPad != sav)) sav->cd();
      if (!fViewer) fViewer = InitCoin(fPad,fDetectorName);
   }
   return o;
}
//___________________________________________________
void StDraw3D::SetBkColor(Color_t newBkColor)
{
   // Set the canvas background color;
   fBkColor = newBkColor;
   if (fPad && fPad->GetFillColor() != fBkColor)
       fPad->SetFillColor(fBkColor);
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
   if (fPad) fPad->Update();
}

//___________________________________________________
void StDraw3D::Modified()
{
   if (fPad) fPad->Modified();
}

//___________________________________________________
void StDraw3D::Draw3DTest(){
   //  ------------------------------------------------
   //  The  method to test the class
   //   It should produce the #D Coin widget:
   //  <begin_html> <img src="http://www.star.bnl.gov/public/comp/vis/StDraw3D/Draw3DClass.png">end_html
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
   
   Line(sizeXYZ,xyz,kPrimaryTrack);
   SetComment("The recontstructed track");
}
