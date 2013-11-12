// $Id: StDraw3D.cxx,v 1.105 2013/11/12 17:50:22 perev Exp $
//*-- Author :    Valery Fine(fine@bnl.gov)   27/04/2008
#include "StDraw3D.h"
#include "TCanvas.h"
#include "TTRAP.h"
#include "TGeometry.h"
#include "TVolume.h"
#include "TVolumePosition.h"
#include "TRotMatrix.h"
#include "TMath.h"
#include "TPolyMarker3D.h"
#include "TPolyLine3D.h"
#include "TSystem.h"
// #include "TROOT.h"
#include "TColor.h"
#include "TEnv.h"
#include "StCheckQtEnv.h"
#include "TStyle.h"
#include "TVirtualViewer3D.h"
#include <cassert>
#include <cmath>

static Color_t colorDefault = Color_t(-1);
static Style_t styDefault   = Style_t(-1);
static Size_t  sizDefault   = Size_t (-1);

Color_t StDraw3D::fgColorDefault = Color_t(-1);
Style_t StDraw3D::fgStyDefault   = Style_t(-1);
Size_t  StDraw3D::fgSizDefault   = Size_t (-1);
Color_t StDraw3D::fgBkColor      = kBlack;

// Canvas counter to create the Unique Canvas names
Int_t   StDraw3D::fDrawCanvasCounter = -1; 

namespace {
     const double p2 = TMath::PiOver2();
     //__________________________________________________________________________________________
     static inline void ForceAnimate(unsigned int times=0, int msecDelay=0)
     {
         unsigned int  counter = times;
         while( (!times || counter) && !gSystem->ProcessEvents()) { --counter; if (msecDelay) gSystem->Sleep(msecDelay);} 
     }
}

//___________________________________________________
static inline TVirtualViewer3D *InitCoin(TVirtualPad *pad,const char *detectorName) 
{
   TVirtualViewer3D *viewer = 0;
   // check Coin env and load if present
   bool CheckCoin = true;
   if (!StCheckQtEnv::SetQtEnv(false)) {   CheckCoin = true; }

//   if (CheckCoin && pad ) {
   if (CheckCoin) {
      // define the background image
      TString backShape = detectorName;
      backShape.ReplaceAll(",",".iv,");
      backShape+= ".iv";
      printf(" Setting the background shape to be 	%s\n", backShape.Data());
      gEnv->SetValue("Gui.InventorShapeDir",":.:StRoot/macros/graphics:$STAR/StRoot/macros/graphics:/afs/rhic.bnl.gov/star/doc/www/comp/2/vis/iv");
      if  ( (viewer = TVirtualViewer3D::Viewer3D(pad,"oiv") )) {
          viewer->SetDrawOption(backShape.Data());
         // Create Open GL viewer
//        TGQt::SetCoinFlag(1);
         viewer->BeginScene();
         viewer->EndScene();
      }
    }
    return viewer;
}
//! Maps the track \a pt to the STAR StTrack track color code
/*!
  \param pt - pt value from some StEvent /StMuDst \c track object
  \return the ROOT color index
 */
//___________________________________________________
Color_t StDraw3DStyle::Pt2Color(double pt)
{ 
   const Int_t lightness    = 50;
   const Int_t saturation   = 100;
   Int_t hue  = (pt > 1.5 ) ? 0 : Int_t(256.*(1.-pt/1.5)); //color code from StuPostscript
   Int_t r,g,b;
   TColor::HLS2RGB(hue, lightness, saturation, r, g, b);
   // Normalize
   float factor = 1./sqrt(1.*r*r+1.*g*g+1.*b*b);
   return  TColor::GetColor(r*factor,g*factor,b*factor);
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
namespace {
//___________________________________________________
//
//   poly_line_3D
//___________________________________________________
class poly_line_3D : public TPolyLine3D, public view_3D {
   public:
     poly_line_3D(Int_t n, Float_t *p, Option_t *option="") : TPolyLine3D(n,p),view_3D()
     {  SetBit(kCanDelete);}
     poly_line_3D(Int_t n, Double_t *p, Option_t *option="") : TPolyLine3D(n,p),view_3D()
     {  SetBit(kCanDelete);}
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
     poly_marker_3D(Int_t n, Float_t *p, Option_t *option="") : TPolyMarker3D(n,p,1,option),view_3D()
     {  SetBit(kCanDelete);}
     poly_marker_3D(Int_t n, Double_t *p, Option_t *option="") : TPolyMarker3D(n,p,1,option),view_3D()
     {  SetBit(kCanDelete);}
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
//
//   TVolume_3D
//___________________________________________________
class volume_view_3D : public TVolume, public view_3D {
   public:
     volume_view_3D(const Text_t *name, const Text_t *title, TShape *shape, Option_t *option="")
   : TVolume(name,title, shape,option),view_3D()
     {         SetBit(kCanDelete);     }
     volume_view_3D() : TVolume(),view_3D()
     {        SetBit(kCanDelete);        }
     virtual ~volume_view_3D(){
        if (fListOfShapes) fListOfShapes->Delete();
 ;   }
     virtual char  *GetObjectInfo(Int_t x, Int_t y) const
     {
        const TString &customInfo = info();
        const char *info = 0;
        if (customInfo.IsNull()) 
           info = TVolume::GetObjectInfo(x,y);
        else 
           info = customInfo.Data();
        return (char *)info;
     }
     void Inspect() const {
        if ( model() ) model()->Inspect();
        else TVolume::Inspect();
     }
};
}

//! StDraw3D( const char *detectorName,TVirtualPad *pad) ctor
/*!  
     \param detectorName (default = "TPC") - the names of the STAR detectors 
                                                 to be used as the "event primitives" background.
                               The detectorName is a comma separated list of the OpenInventor files 
                               with no extension\n
                               For all names on the list one should provide the iv file with 
                               the "iv" extension:\n
                                     \code   <name>.iv \endcode   
         \param detectorName = 0  - no detector geometry is to be rendered
         \param pad (default = 0) - The ROOT TPad to be used to render the event wireframe view
 \htmlonly
 <table>
 <tr>
 <th>Event over detector geometry
 <th>Event with no detector geometry
 </tr>
 <tr>
 <td><img src="http://www.star.bnl.gov/public/comp/vis/StDraw3D/examples/EventDisplayWGeom.png">
 <td><img src="http://www.star.bnl.gov/public/comp/vis/StDraw3D/examples/EventDisplayWOGeom.png">
 </tr></table>
 \endhtmlonly
             
 \note This is the base class for the advanced EventDisplay subclasses. \n
        Normaly you do not need to instantiate StDraw3D directly.

 \note StDraw class ctor defines the set of the pre-defines styles:
 
\htmlonly 
  <table>
  <thead><th> Style       </th><th> Color   </th><th> Style </th><th> Size </th><th> comment </th></thead>
  <tr> <td> kVtx          </td><td> kYellow </td><td>  5    </td><td> 3.5 </td><td>  <a href="http://root.cern.ch/root/html/TAttMarker.html">3D marker</a> - vertex  </tr>
  <tr> <td> kPrimaryTrack </td><td> kRed    </td><td>  1    </td><td> 2.00 </td><td> <a href="http://root.cern.ch/root/html/TAttLine.html">3D polyline</a> - track </tr>
  <tr> <td> kGlobalTrack  </td><td> kRed    </td><td>  1    </td><td> 2.00 </td><td> <a href="http://root.cern.ch/root/html/TAttLine.html">3D polyline</a> - track </tr>
  <tr> <td> kTrackBegin   </td><td> kYellow </td><td>  5    </td><td> 3.5 </td><td>  <a href="http://root.cern.ch/root/html/TAttMarker.html">3D marker</a> - vertex  </tr>
  <tr> <td> kTrackEnd     </td><td> kYellow </td><td>  5    </td><td> 3.5 </td><td>  <a href="http://root.cern.ch/root/html/TAttMarker.html">3D marker</a> - vertex  </tr>
  <tr> <td> kUsedHit      </td><td> kBlue   </td><td>  4    </td><td> 0.35 </td><td> <a href="http://root.cern.ch/root/html/TAttMarker.html">3D marker</a> - track start point  </td></tr>
  <tr> <td> kUnusedHit    </td><td> kGreen  </td><td>  1    </td><td> 1.00 </td><td> <a href="http://root.cern.ch/root/html/TAttMarker.html">3D marker</a> - track end point   </td></tr>
  </table>
 \endhtmlonly
 \sa http://root.cern.ch/root/html/TColor.html
 \sa http://root.cern.ch/root/html/TAttLine.html
 \sa http://root.cern.ch/root/html/TAttMarker.html

*/
//___________________________________________________
StDraw3D::StDraw3D(const char *detectorName,TVirtualPad *pad): fPad(pad),fBkColor(fgBkColor),fViewer(0),fView(0)
      , fDetectorName(detectorName),fMaster(0),fTopVolume(0),fWantPad(0),fOwnViewer(kTRUE),fOwnPad(pad?kFALSE:kTRUE)
{

   // The detectorName is a comma separated list of the OpenInventor files with no extension
   // For all names on the list one should provide the iv file with the "iv extension:
   //                         <name>.iv
   Init();
}
//__________________________________________________________________________________________
void StDraw3D::Init()
{
   static const Style_t UHitSty = 4; static const Size_t UHitSiz = 0.0;  static const Color_t UHitCol=kBlue;
   static const Style_t NHitSty = 2; static const Size_t NHitSiz = 0.0;  static const Color_t NHitCol=kGreen;
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
//__________________________________________________________________________________________
//! This is an overloaded member function, provided for convenience.
/*! Create a EventDisplay using the external TVirtualViewer3D \a viewer and TVirtualPad \a pad
*/
StDraw3D::StDraw3D(TVirtualViewer3D *viewer,TVirtualPad *pad): fPad(pad),fBkColor(fgBkColor),fViewer(viewer),fView(0)
      , fDetectorName(),fMaster(),fTopVolume(),fWantPad(0),fOwnViewer(kFALSE),fOwnPad(kFALSE)
{
   Init();
}

//__________________________________________________________________________________
TVirtualPad *StDraw3D::InitPad() 
{
   if (fMaster) fMaster->InitPad();
   else if (!fPad && !fWantPad ) {
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
      fPad->GetCanvas()->GetCanvasImp()->Iconify();
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
       if (fOwnPad)  delete fPad; // TPad will destroy the viewer
       fPad       = 0;
       fMaster    = 0;
       fTopVolume = 0;
       fViewer = 0;
    }
}


//__________________________________________________________________________________________
//! \return The TString of the comma separated names 
//__________________________________________________________________________________
const TString &StDraw3D::DetectorNames() const 
{
   // return the list of the names 
   return fDetectorName; 
}

//__________________________________________________________________________________________ 
/*! \return The TPad pointer used to paint onto.
 */
//__________________________________________________________________________________
TVirtualPad *StDraw3D::Pad() const 
{ 
   return fMaster ? fMaster->Pad() : fPad;
}

//__________________________________________________________________________________________
//! \return The TVirtualViewer3D viewer pointer used to render into.
//__________________________________________________________________________________
TVirtualViewer3D *StDraw3D::Viewer() const
{
   return fMaster ? fMaster->Viewer() : fViewer;
}

//__________________________________________________________________________________________
//! Set the list of the detector names to be used as the event "background"
/*! \param  nameDetectors - a comma separated list of the OpenInventor files with no extension\n
                               For all names on the list one should provide the iv file with 
                               the "iv" extension:\n
                              \code   <name>.iv \endcode   
 */   
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

//__________________________________________________________________________________________
//! Append the detector names to the list of  the event "background" shapes.
/*! \param  nameDetectors - a comma separated list of the OpenInventor files with no extension\n
                               For all names on the list one should provide the iv file with 
                               the "iv" extension:\n
                              \code   <name>.iv \endcode   
*/
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

//! Remove all objects from the list and update the screen if \a opt is "update"
//___________________________________________________
void  StDraw3D::Clear(Option_t *opt)
{
   // Clear the view
   TVirtualPad *pad = Pad();
   if (pad) {
      pad->Clear();
      fTopVolume = 0;
      if ( !strcmp(opt,"update") ) Update();
   } else if ( TVirtualViewer3D *viewer = Viewer() ) {
      viewer->Clear();
   }
   if (gGeometry) {
       gGeometry->GetListOfMatrices()->Clear();
       gGeometry->GetListOfShapes()->Delete();
   }
   TCollection::EmptyGarbageCollection();
}

//___________________________________________________
TObject *StDraw3D::Draw(TObject *o,const char *option)
{
   // Draw the 3d object 
   // and set the new background color if needed
   if (o) {
      TVirtualPad *sav = gPad;
      if (!Pad())        InitPad();
      TVirtualPad *thisPad = Pad(); 
      if (thisPad) {
        if (thisPad != sav)  thisPad->cd();
        assert (fPad==gPad);
        o->Draw(option);
      }
      if (thisPad && sav && (thisPad != sav))  sav->cd();
      if (!Viewer()) InitViewer();
      if (!thisPad) {
         // no TPad was provided by the user 
         // Use TVirtualViewer3D directly
         Viewer()->ObjectPaint(o,option);
      }
   }
   return o;
}

//__________________________________________________________________________________________
//! Set the ROOT color as the widget background
/*! 
    \param newBkColor - ROOT index of the color to paint the widget backgorund ( \sa http://root.cern.ch/root/html/TColor.html )
*/
//___________________________________________________
void StDraw3D::SetBkColor(Color_t newBkColor)
{
   // Set the canvas background color;
   fBkColor = newBkColor;
   TVirtualPad *pad = Pad();
   if (pad && pad->GetFillColor() != fBkColor)
       pad->SetFillColor(fBkColor);
}


//__________________________________________________________________________________________
//! Map the predefined style \a type to the ROOT graphical attributes \a col color \a sty style \a siz size
/*! 
    Normally one does not need to call this  method. All pre-defined styles are to be filled by  StDraw3D class ctor
    \param   type - The pre-define type we want to define
    \param col - ROOT color attribute  (See:  http://root.cern.ch/root/html/TColor.html) 
    \param sty - ROOT style attribute. It can be either line http://root.cern.ch/root/html/TAttLine.html or marker 
                                       http://root.cern.ch/root/html/TAttMarker.html
    \param siz - ROOT graphical attribute size . It can be either line http://root.cern.ch/root/html/TAttLine.html or marker 
                                       http://root.cern.ch/root/html/TAttMarker.html                                   
 */
//___________________________________________________
const StDraw3DStyle &StDraw3D::AddStyle(EDraw3DStyle type,Color_t col,Style_t sty,Size_t siz)
{
   fStyles.insert(std::pair<EDraw3DStyle,StDraw3DStyle>(type,StDraw3DStyle(type,col,sty,siz)));
   return Style(type);
}


//__________________________________________________________________________________________
//! Return the reference to the predefined StDraw3DStyle object
/*! 
    \param type - The pre-defined \a type we want to get the reference to
 */
//__________________________________________________________________________________________
const StDraw3DStyle &StDraw3D::Style(EDraw3DStyle type) const
{
    return fStyles.find(type)->second;
}

//__________________________________________________________________________________________
StDraw3DStyle &StDraw3D::Style(EDraw3DStyle type)
{
    return fStyles[type];
}

//__________________________________________________________________________________________
//! Add \a n 3D coordinates from the \a xyz array to the display list with the \a col color, \a sty style, and \a siz size if provided
/*! 
   \param     n - the number of the 3D coordinates 
   \param   xyz - the pointer to the array of the floating ount values ( the array should be 3*n long at least )
   \param   col - ROOT line color ( see: http://root.cern.ch/root/html/TAttLine.html ) 
   \param   sty - ROOT line style ( see: http://root.cern.ch/root/html/TAttLine.html ) 
   \param   siz - ROOT line width ( see: http://root.cern.ch/root/html/TAttLine.html ) 
   \return - a pointer to the ROOT "view" TPolyMarker3D created to render the input \a xyz array 
*/
//__________________________________________________________________________________________
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

//__________________________________________________________________________________________
//! This is an overloaded member function, provided for convenience.
/*! Add \a n 3D coordinates from the \a xyz array of \c double values to the display list with the \a col color, \a sty style, and \a siz size if provided
   \param     n - the number of the 3D coordinates 
   \param   xyz - the pointer to the array of the floating ount values ( the array should be 3*n long at least )
   \param   col - ROOT line color ( see: http://root.cern.ch/root/html/TAttLine.html ) 
   \param   sty - ROOT line style ( see: http://root.cern.ch/root/html/TAttLine.html ) 
   \param   siz - ROOT line width ( see: http://root.cern.ch/root/html/TAttLine.html ) 
   \return - a pointer to the ROOT "view" TPolyMarker3D created to render the input \a xyz array 
*/
//__________________________________________________________________________________________
TObject *StDraw3D::Points(int n, const double *xyz, Color_t col,Style_t sty,Size_t siz)
{ 
   //
   // Draw "n" points of the "xyz" array of the float coordinates 
   // with ROOT TPolyMarker3D class
   // with the ROOT color, style, size attributes
   //
   
   poly_marker_3D *plMk  = new poly_marker_3D(n,(Double_t*)xyz);
   if (col != colorDefault) plMk->SetMarkerColor(col);
   if (sty != styDefault)   plMk->SetMarkerStyle(sty);
   if (siz != sizDefault)   plMk->SetMarkerSize(siz);
   fView = plMk;
   return Draw(plMk);
}

//__________________________________________________________________________________________
//! This is an overloaded member function, provided for convenience.
/*! Add \a n 3D coordinates from the \a xyz array to the display list with the \a col color, \a sty style, and \a siz size if provided
   \param   xyz - the vector of the floating ount values ( the container should be 3*n long at least )
   \param   col - ROOT line color ( see: http://root.cern.ch/root/html/TAttLine.html ) 
   \param   sty - ROOT line style ( see: http://root.cern.ch/root/html/TAttLine.html ) 
   \param   siz - ROOT line width ( see: http://root.cern.ch/root/html/TAttLine.html ) 
   \return - a pointer to the ROOT "view" TPolyMarker3D created to render the input \a xyz array 
*/
//__________________________________________________________________________________________
TObject *StDraw3D::Points(const std::vector<float> &xyz, Color_t col,Style_t sty,Size_t siz)
{ 
   //
   // Draw the "xyz" vector of the float coordinates 
   // with ROOT TPolyMarker3D class
   // with the ROOT color, style, size attributes
   //
   return Points(xyz.size()/3,&xyz[0],col,sty,siz);
}

//__________________________________________________________________________________________
//! This is an overloaded member function, provided for convenience.
/*! Add \a n 3D coordinates from the \a xyz array to the display list with the \a col color, \a sty style, and \a siz size if provided
   \param   xyz - the vector of the floating ount values ( the container should be 3*n long at least )
   \param   col - ROOT line color ( see: http://root.cern.ch/root/html/TAttLine.html ) 
   \param   sty - ROOT line style ( see: http://root.cern.ch/root/html/TAttLine.html ) 
   \param   siz - ROOT line width ( see: http://root.cern.ch/root/html/TAttLine.html ) 
   \return - a pointer to the ROOT "view" TPolyMarker3D created to render the input \a xyz array 
*/
//__________________________________________________________________________________________
TObject *StDraw3D::Points(const std::vector<double> &xyz, Color_t col,Style_t sty,Size_t siz)
{ 
   //
   // Draw the "xyz" vector of the float coordinates 
   // with ROOT TPolyMarker3D class
   // with the ROOT color, style, size attributes
   //
   return Points(xyz.size()/3,&xyz[0],col,sty,siz);
}

//__________________________________________________________________________________________
//! This is an overloaded member function, provided for convenience.
/*! Add \a n 3D coordinates from the \a xyz array to the display list with the \a sty pre-defined style if provided 
   \param     n - the number of the 3D coordinates 
   \param   xyz - the pointer to the array of the floating point values ( the array should be 3*n long at least )
   \param   sty  - EDraw3DStyle value selecting some predefined style 
   \return - a pointer to the ROOT "view" TPolyMarker3D created to render the input \a xyz array 
*/
//__________________________________________________________________________________________
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

//__________________________________________________________________________________________
//! This is an overloaded member function, provided for convenience.
/*! Add \a n 3D coordinates from the \a xyz array to the display list with the \a sty pre-defined style if provided 
   \param     n - the number of the 3D coordinates 
   \param   xyz - the pointer to the array of the floating point values ( the array should be 3*n long at least )
   \param   sty  - EDraw3DStyle value selecting some predefined style 
   \return - a pointer to the ROOT "view" TPolyMarker3D created to render the input \a xyz array 
*/
//__________________________________________________________________________________________
TObject *StDraw3D::Points(int n, const double *xyz, EDraw3DStyle sty)
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

//__________________________________________________________________________________________
//! This is an overloaded member function, provided for convenience.
/*! Add 3D coordinates from the \a xyz  vector to the display list with the \a sty pre-defined style if provided 
   \param   xyz - the vector of the floating point values ( the array should be 3*n long at least )
   \param   sty  - EDraw3DStyle value selecting some predefined style 
   \return - a pointer to the ROOT "view" TPolyMarker3D created to render the input \a xyz array 
*/
//__________________________________________________________________________________________
TObject *StDraw3D::Points(const std::vector<float> &xyz, EDraw3DStyle sty)
{
   //
   // Draw "xyz" vector of the float coordinates 
   // with ROOT TPolyMarker3D class and the predefined attrbutes
   //
   // This is an overloaded member function, provided for convenience.
   // It behaves essentially like the above function.
   //
   
  const StDraw3DStyle &style =  Style(sty);
  return Points(xyz, style.Col(),style.Sty(),style.Siz());
}

//__________________________________________________________________________________________
//! This is an overloaded member function, provided for convenience.
/*! Add 3D coordinates from the \a xyz  vector to the display list with the \a sty pre-defined style if provided 
   \param   xyz - the vector of the floating point values ( the array should be 3*n long at least )
   \param   sty  - EDraw3DStyle value selecting some predefined style 
   \return - a pointer to the ROOT "view" TPolyMarker3D created to render the input \a xyz array 
*/
//__________________________________________________________________________________________
TObject *StDraw3D::Points(const std::vector<double> &xyz, EDraw3DStyle sty)
{
   //
   // Draw "xyz" vector of the float coordinates 
   // with ROOT TPolyMarker3D class and the predefined attrbutes
   //
   // This is an overloaded member function, provided for convenience.
   // It behaves essentially like the above function.
   //
   
  const StDraw3DStyle &style =  Style(sty);
  return Points(xyz, style.Col(),style.Sty(),style.Siz());
}

//__________________________________________________________________________________________
//! This is an overloaded member function, provided for convenience.
/*! Add \a n 3D coordinates from the \a xyz array to the display list with the pre-defined \c kVtx style
   It is designed to be used from the interactive \c "gdb" session because it needs 2 parameters only it 
   \param     n - the number of the 3D coordinates 
   \param   xyz - the pointer to the array of the floating point values ( the array should be 3*n long at least )
   \return - a pointer to the ROOT "view" TPolyMarker3D created to render the input \a xyz array 
*/
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

//__________________________________________________________________________________________
//! This is an overloaded member function, provided for convenience.
/*! Add \a n 3D coordinates from the \a xyz array to the display list with the pre-defined \c kVtx style
   It is designed to be used from the interactive \c "gdb" session because it needs 2 parameters only it 
   \param     n - the number of the 3D coordinates 
   \param   xyz - the pointer to the array of the floating point values ( the array should be 3*n long at least )
   \return - a pointer to the ROOT "view" TPolyMarker3D created to render the input \a xyz array 
*/
//___________________________________________________
TObject *StDraw3D::Draw3D(int n,  const double *xyz)
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

//__________________________________________________________________________________________
//! Add \a n connected points defined by the \a "xyz" array of the 3D coordinates to the display list with the \a col color, \a sty style, and \a siz size if provided
/*! 
   \param     n - the number of the 3D coordinates 
   \param   xyz - the pointer to the array of the floating ount values ( the array should be 3*n long at least )
   \param   col - ROOT line color ( see: http://root.cern.ch/root/html/TAttLine.html ) 
   \param   sty - ROOT line style ( see: http://root.cern.ch/root/html/TAttLine.html ) 
   \param   siz - ROOT line width ( see: http://root.cern.ch/root/html/TAttLine.html ) 
   \return - a pointer to the ROOT "view" TPolyLine3D created to render the input \a xyz array 
*/
//__________________________________________________________________________________________
TObject *StDraw3D::Line(int n,  const float *xyz, Color_t col,Style_t sty,Size_t siz)
{
   //
   // Draw "n" connected points of the "xyz" array of the float coordinates 
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

//__________________________________________________________________________________________
//! This is an overloaded member function, provided for convenience.
/*!  Add a line sigment connecting 2 points defined by the \a(\a"x0",\a"y0" \a"z0"\a) and \a(\a"x1",\a"y1" \a"z1"\a) to the display list with the \a col color, \a sty style, and \a siz size if provided
   \param  x0,y0,z0 - the 3D coordinates of the first point
   \param  x1,y1,z1 - the 3D coordinates of the second point
   \param   col - ROOT line color ( see: http://root.cern.ch/root/html/TAttLine.html ) 
   \param   sty - ROOT line style ( see: http://root.cern.ch/root/html/TAttLine.html ) 
   \param   siz - ROOT line width ( see: http://root.cern.ch/root/html/TAttLine.html ) 
   \return - a pointer to the ROOT "view" TPolyLine3D created to render the line segment defined by 2 input points
*/
//__________________________________________________________________________________________
TObject *StDraw3D::Line(float x0, float y0, float z0,  float x1, float y1, float z1, Color_t col,Style_t sty,Size_t siz)
{
   //
   // Draw "n" connected points of the "xyz" array of the float coordinates 
   // with ROOT TPolyline3D class
   // with the ROOT color, style, size attributes
   //
   std::vector<float> line(6);
   int i = 0;
   line[i++]=x0;line[i++]=y0;line[i++]=z0;
   line[i++]=x1;line[i++]=y1;line[i++]=z1;
   return Line(line,col,sty,siz);   
}


//__________________________________________________________________________________________
//! This is an overloaded member function, provided for convenience.
/*! Add \a n connected points defined by the \a "xyz" array of the 3D coordinates to the display list with the \a col color, \a sty style, and \a siz size if provided
   \param     n - the number of the 3D coordinates 
   \param   xyz - the pointer to the array of the floating ount values ( the array should be 3*n long at least )
   \param   col - ROOT line color ( see: http://root.cern.ch/root/html/TAttLine.html ) 
   \param   sty - ROOT line style ( see: http://root.cern.ch/root/html/TAttLine.html ) 
   \param   siz - ROOT line width ( see: http://root.cern.ch/root/html/TAttLine.html ) 
   \return - a pointer to the ROOT "view" TPolyLine3D created to render the input \a xyz array 
*/
//__________________________________________________________________________________________
TObject *StDraw3D::Line(int n,  const double *xyz, Color_t col,Style_t sty,Size_t siz)
{
   //
   // Draw "n" connected points of the "xyz" array of the float coordinates 
   // with ROOT TPolyline3D class
   // with the ROOT color, style, size attributes
   //
   poly_line_3D *plLine  = new poly_line_3D(n,(Double_t*)xyz);
   if (col != colorDefault) plLine->SetLineColor(col);
   if (sty != styDefault)   plLine->SetLineStyle(sty);
   if (siz != sizDefault)   plLine->SetLineWidth(Width_t(siz));
   fView = plLine;
   return Draw(plLine);
}
//__________________________________________________________________________________________
//! This is an overloaded member function, provided for convenience.
/*! Add \a n connected points defined by the \a "xyz" array of the 3D coordinates to the display list with the \a col color, \a sty style, and \a siz size if provided
   \param   xyz - the vector of the floating ount values ( the array should be 3*n long at least )
   \param   col - ROOT line color ( see: http://root.cern.ch/root/html/TAttLine.html ) 
   \param   sty - ROOT line style ( see: http://root.cern.ch/root/html/TAttLine.html ) 
   \param   siz - ROOT line width ( see: http://root.cern.ch/root/html/TAttLine.html ) 
   \return - a pointer to the ROOT "view" TPolyLine3D created to render the input \a xyz array 
*/
//__________________________________________________________________________________________
TObject *StDraw3D::Line(const std::vector<float> &xyz, Color_t col,Style_t sty,Size_t siz)
{
   //
   // Draw the "xyz" vector of the float coordinates 
   // with ROOT TPolyline3D class
   // with the ROOT color, style, size attributes
   //
   return Line(xyz.size()/3, &xyz[0], col,sty,siz);
}

//__________________________________________________________________________________________
//! This is an overloaded member function, provided for convenience.
/*! Add \a n connected points defined by the \a "xyz" array of the 3D coordinates to the display list with the \a col color, \a sty style, and \a siz size if provided
   \param   xyz - the vector of the floating ount values ( the array should be 3*n long at least )
   \param   col - ROOT line color ( see: http://root.cern.ch/root/html/TAttLine.html ) 
   \param   sty - ROOT line style ( see: http://root.cern.ch/root/html/TAttLine.html ) 
   \param   siz - ROOT line width ( see: http://root.cern.ch/root/html/TAttLine.html ) 
   \return - a pointer to the ROOT "view" TPolyLine3D created to render the input \a xyz array 
*/
//__________________________________________________________________________________________
TObject *StDraw3D::Line(const std::vector<double> &xyz, Color_t col,Style_t sty,Size_t siz)
{
   //
   // Draw the "xyz" vector of the float coordinates 
   // with ROOT TPolyline3D class
   // with the ROOT color, style, size attributes
   //
   return Line(xyz.size()/3, &xyz[0], col,sty,siz);
}
   
//__________________________________________________________________________________________
//! This is an overloaded member function, provided for convenience.
/*! Add the connected points defined by the \xyz vector  of 3D coordinates to the display list with the \a sty pre-defined style if provided 
   \param   xyz - the vector of the floating ount values ( the array should be 3*n long at least )
   \param   sty  - EDraw3DStyle value selecting some predefined style 
   \return - a pointer to the ROOT "view" TPolyLine3D created to render the input \a xyz array 
*/
//___________________________________________________
TObject *StDraw3D::Line(const std::vector<float> &xyz, EDraw3DStyle sty)
{
   //
   // Draw "n" connected points of the "xyz" array of the float coordinates 
   // with ROOT TPolyLine3D class and the predefined attrbutes
   //
   // This is an overloaded member function, provided for convenience.
   // It behaves essentially like the above function.
   //
   const StDraw3DStyle &style =  Style(sty);
   return Line(xyz, style.Col(),style.Sty(),style.Siz() );
}

//__________________________________________________________________________________________
//! This is an overloaded member function, provided for convenience.
/*! Add the connected points defined by the \xyz vector  of 3D coordinates to the display list with the \a sty pre-defined style if provided 
   \param   xyz - the vector of the floating ount values ( the array should be 3*n long at least )
   \param   sty  - EDraw3DStyle value selecting some predefined style 
   \return - a pointer to the ROOT "view" TPolyLine3D created to render the input \a xyz array 
*/
//___________________________________________________
TObject *StDraw3D::Line(const std::vector<double> &xyz, EDraw3DStyle sty)
{
   //
   // Draw "n" connected points of the "xyz" array of the float coordinates 
   // with ROOT TPolyLine3D class and the predefined attrbutes
   //
   // This is an overloaded member function, provided for convenience.
   // It behaves essentially like the above function.
   //
   const StDraw3DStyle &style =  Style(sty);
   return Line(xyz, style.Col(),style.Sty(),style.Siz() );
}

//__________________________________________________________________________________________
//! This is an overloaded member function, provided for convenience.
/*! Add \a n  connected points defined by the \xyz array of 3D coordinates to the display list with the \a sty pre-defined style if provided 
   \param     n - the number of the 3D coordinates 
   \param   xyz - the pointer to the array of the floating ount values ( the array should be 3*n long at least )
   \param   sty  - EDraw3DStyle value selecting some predefined style 
   \return - a pointer to the ROOT "view" TPolyLine3D created to render the input \a xyz array 
*/
//___________________________________________________
TObject *StDraw3D::Line(int n,  const float *xyz,EDraw3DStyle sty)
{
   //
   // Draw "n" connected points of the "xyz" array of the float coordinates 
   // with ROOT TPolyLine3D class and the predefined attrbutes
   //
   // This is an overloaded member function, provided for convenience.
   // It behaves essentially like the above function.
   //
   const StDraw3DStyle &style =  Style(sty);
   return Line(n,xyz,  style.Col(),style.Sty(),style.Siz() );
}

//__________________________________________________________________________________________
//! This is an overloaded member function, provided for convenience.
/*! Add \a n  connected points defined by the \xyz array of 3D coordinates to the display list with the \a sty pre-defined style if provided 
   \param     n - the number of the 3D coordinates 
   \param   xyz - the pointer to the array of the floating ount values ( the array should be 3*n long at least )
   \param   sty  - EDraw3DStyle value selecting some predefined style 
   \return - a pointer to the ROOT "view" TPolyLine3D created to render the input \a xyz array 
*/
//___________________________________________________
TObject *StDraw3D::Line(int n, const double *xyz,EDraw3DStyle sty)
{
   //
   // Draw "n" connected points of the "xyz" array of the float coordinates 
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
   // The method to force two different instances
   // of the StDraw3D class  paint onto one and the same
   // TPad.

   // Force "dsp" to share the fPad of this object
   // As result of the "Joint" method both objects 
   // "this" as well as "dsp" will share the TPad of "this" object.
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
         while ( (o=next()) )  Draw(o);
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
//! Save the current 3D scene using "wrl" file format
/*! \param  filename - the file name to save the 3d scene
   \note : The "wrl" format can be converted to the standatd 3D PDF format 
   \htmlonly 
   It can be done <a href="http://blogs.adobe.com/mfg/2009/04/more_3d_reviewer_features">
   "Adobe 3D reviewer"</a>  
   <P>See examples:  
  <blockquote>
  You need to install the <a href="http://get.adobe.com/reader/?promoid=BUIGO">Adobe Reader version 9 or higher
  <img src="http://www.adobe.com/images/shared/download_buttons/get_adobe_reader.png"></a>
  to be able to "click and see" the interactive ( zoom, pan, select / highlight the pieces, etc )  3D image also
  </blockquote>
  <center> 
   <a href="http://www.star.bnl.gov/public/comp/vis/StDraw3D/examples/production_dAu2008.pdf">
  <img src="http://www.star.bnl.gov/public/comp/vis/StDraw3D/examples/production_dAu2008.png"></a>
  <br>
  <a href="http://www.star.bnl.gov/public/comp/vis/GeomBrowser/y2009.pdf">
  <img src="http://www.star.bnl.gov/public/comp/vis/GeomBrowser/StarGeometryY2007.png"></a>
  </center><p>
  \endhtmlonly
*/   
//___________________________________________________
void StDraw3D::Print(const char *filename) const
{
   Save(filename,"wrl");
}

//! This is an overloaded member function, provided for convenience.
/*! Save the current 3D scene using the \a type  file format 
   \param  filename - the file name to save the 3d scene
   \param  type - the pre-defined file format.
   It can be 3D scene format like "wrl"/"iv" 
   or the well-known pixmap formats like "pnd", "jpg" etc 
*/
//___________________________________________________
void StDraw3D::Print(const char *filename, const char*type) const
{
   Save(filename,type);
}

//___________________________________________________
//! This is an overloaded member function, provided for convenience.
/*! Save the current 3D scene using the 3D "wrl" format if possible otherwise use the \a type file format 
   \param  filename - the file name to save the 3d scene
   \param  type - the pre-defined file format.
   It can be 3D scene format like "wrl"/"iv" 
   or the well-known pixmap formats like "pnd", "jpg" etc 
*/
//___________________________________________________
void StDraw3D::Save(const char *filename, const char*type) const
{
    if ( TVirtualViewer3D *viewer = Viewer() ) {
       viewer->Print(filename);
//       viewer->PrintObjects();  
    }
    else if (Pad()) Pad()->Print(filename,type);
}


//___________________________________________________
//! Set the varous drawing option. The method passes the input \a options to TQtCoinWidget::SetDrawOption method 
/*! \param  options - [ <shape1> [, shape2 [, . . .  shape-n] - a comma separated list of the OpenInventor files with no extension\n
                    | <em> { parameter : value } </em> - enclosed into the curly brackets a pair "parameter : value"\n
                     \param - <em> { file : file.iv } </em> - the iv file defining the top level OpenInventor node. 
                          For example, it can be useful to customize the entire screen rotation / animation )\n
                     \param - <em>{ footer: text }</em>  - define the image footer (caption)\n
                     \param - <em>{ record : true | false }</em> - toogle the <em>"record scene"</em> option\n
                     \param - <em>{ save : filename }</em> - save the current image into file\n
                     \param - <em>{ screen : full }</em> - turn the <em>"fullscreen view"</em> option\n
                     \param - <em>{ view : all  }</em> - zoom the image  in/out to make sure it fits the entire screen\n
   <P>For example, the ROOT macro:
   \code
   void Draw3D()
   {
      gROOT->Macro("Load.C");  //< Load STAR framework shared libraries
      gEventDisplay->Draw3DTest(); //< Invoke the built-in rendering test
      gEventDisplay->SetDrawOption("{file:rotation.iv}");//< Add rotation to the scene
      gEventDisplay->SetFooter("STAR Event Display Example");
      gEventDisplay->Print("Draw3DTest.wrl"); //< Save the 3D scene into the file
      gEventDisplay->SetDrawOption("{view:all}"); // zoom the scene in/out to fit the entire screen
   }   
   \endcode
   is to produce the  animated image:
   \image html http://www.star.bnl.gov/public/comp/vis/StDraw3D/examples/Draw3D.C.gif "Animated version of the test image"
   \sa Draw3D.C 
*/   
//___________________________________________________
void StDraw3D::SetDrawOption(Option_t *options)
{ 
   if ( TVirtualViewer3D *viewer = Viewer() ) 
       viewer->SetDrawOption(options);
}

//___________________________________________________
//! Render  all items from the current display list onto the screen  and refesh the screen immiately if \a asap is defined
/*! \param  asap = (defaul:false) force the sysysten to refresh the screen woith the fresh image. 
                   This option can significantly slow dowbn the rednereing if abused.  
*/ 
//___________________________________________________
void StDraw3D::Update(bool asap)
{
   TVirtualPad *pad = Pad();
   if (pad) {
      TVirtualPad *sav = gPad;
      if (pad != sav)  pad->cd();
      assert (pad==gPad);
      pad->Update();
      if (sav && (pad != sav)) sav->cd();
   } else {
       UpdateViewer(0);
   }
   if (asap) ForceAnimate(1);
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

//_______________________________________________________________
void StDraw3D::UpdateModified()
{
   // One doesn't need to call this method
   // because one can not change any object yet
   TVirtualPad *pad = Pad();
   if (pad) {
      TVirtualPad *sav = gPad;
      if (pad != sav)  pad->cd();
      assert (pad==gPad);
      pad->Modified();
      pad->Update();
      if (sav && (pad != sav)) sav->cd();
   }
}

//_______________________________________________________________
void StDraw3D::UpdateViewer(TVirtualPad *pad) 
{
   TVirtualViewer3D *viewer = Viewer();
   if (viewer) {
      if (fTopVolume) Draw(fTopVolume,"same");
      viewer->PadPaint(pad);
   }
}

//___________________________________________________
//!  The built-in quick test to check the application environment and test the basic methods
/*! 
  \n Try:
  \code
   > ln -s  $STAR/StRoot/macros/.rootrc
   > root.exe Draw3D.C
  \endcode
   to get the picture:
   \image html http://www.star.bnl.gov/public/comp/vis/StDraw3D/examples/Draw3D.C.png "Test image is to show several tpc points , tpc track, barrel and endcap towers"
   \image html http://www.star.bnl.gov/public/comp/vis/StDraw3D/examples/Draw3D.C.gif "Animated version of the test image"
*/
//___________________________________________________
void StDraw3D::Draw3DTest(){
   //  ------------------------------------------------
   //  The  method to test the class
   //  It should produce the 3D Coin widget:
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

   Tower(192,0.365*p2,TMath::Pi()/22
            ,TMath::Pi()/65,TMath::Pi()/80
            ,kYellow, kBarrelStyle+4050, 250);
   SetComment(Form("The EMC tower lambda=%f phi=%f energy=%f",0.365*p2,TMath::Pi()/22,250.0));
   Tower(192,-0.365*p2,-TMath::Pi()/22-TMath::Pi()/4
            ,TMath::Pi()/65,TMath::Pi()/80
            ,kBlue, kBarrelStyle+4050, 50);
   Tower(230,-0.365*p2,-TMath::Pi()/22-3*TMath::Pi()/4
            ,TMath::Pi()/65,TMath::Pi()/80
            ,kCyan, 4050, 50);
   SetComment(Form("The EndCup tower lambda=%f phi=%f energy=%f",-0.365*p2,-TMath::Pi()/22,50.0));
   Tower(230,0.365*p2,-TMath::Pi()/22+p2
            ,TMath::Pi()/65,TMath::Pi()/80
            ,kGreen, 0, 150);
   // Use the pseudorapidity units:
   int sector=20;
   double stepEta = (1.0-0.0)/sector;
   StarRoot::StEta eta(1-stepEta,stepEta);
   StarRoot::StEta eta2(1,stepEta);
   StarRoot::StEta eta3(-1,stepEta);
   StarRoot::StEta eta4(-1+stepEta,stepEta);
   float phi = 0;
   int n  = sector;
   for (int i=0;i<n;i++) {
      Tower(193,eta,phi,TMath::Pi()/120,(i+1)%8,kBarrelStyle,5*i+25);
      SetComment(Form("The East EMC tower pseudorapidity=%f phi=%f energy=%f",eta.Eta(),phi,5*i+25.));

      Tower(193,eta4,phi+0.2,TMath::Pi()/120,(i+1)%8,kBarrelStyle,5*i+25);
      SetComment(Form("The West EMC tower pseudorapidity=%f phi=%f energy=%f",eta4.Eta(),phi+0.2,5*i+25.));

      Tower(230,eta2,phi+0.1,TMath::Pi()/60,(i+2)%8,4060,10*i+40);
      SetComment(Form("The East Endcap tower pseudorapidity=%f phi=%f energy=%f",eta2.Eta(),phi,10*i+40.));

      Tower(230,eta3,phi+0.1,TMath::Pi()/60,(i+2)%8,4060,10*i+40);
      SetComment(Form("The West Endcap tower pseudorapidity=%f phi=%f energy=%f",eta2.Eta(),phi,10*i+40.));

      eta  -=stepEta;
      eta2 +=stepEta;
      eta3 -=stepEta;
      eta4 +=stepEta;
      phi  += 4*TMath::Pi()/n;
   }
   if (!Pad()) Update();
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
  for (int i=0;i<2;i++) { fine[i]->UpdateModified(); }
//  while(!gSystem->ProcessEvents()){}; 
}

//! Draw the TTRAP object suitable to represent the calorimeter data
/*! The method creates one "tower" object (see:  http://root.cern.ch/root/html/TTRAP.html#TTRAP:description )
   \param  radius - the distance between the  base of the tower and the "origin".
   It can be either the distance from the Z-axis for the kBarrelStyle 
   or the distance from the xy plane for EndCap tower (default) style, for example.
   \param  lambda  - the tower direction (in rads). It is the angle in respect of the Y-axis for kBarrelStyle tower or Z-axis.
   \param  phi     - the angle (in rads) in XY plane against of the Ox
   \param  dlambda - the non-negative angle "width" of the tower.
   \param  dphi    - the non-negative angle "length" of the tower
   \param  col     - ROOT fill color ( see: http://root.cern.ch/root/html/TAttFill.html ) 
   \param  sty     - ROOT fill style ( see: http://root.cern.ch/root/html/TAttFIll.html ) 
                The default style is "Endcap".  One can add \c kBarrelStyle constant to ROOT style to get the "barrel" style tower\n 
		= 0 - solid color,  4001 - wireframe view, 4001 ... 4100 - solid translucent color from 99% transparent to 100% opaque
   \param   siz - the height of the tower. It can be used to visualize the energy deposit or for any other reason.
   \return - a pointer to the ROOT "view" TVolume created to render the input parameters.
 \htmlonly
 <table>
 <tr>
 <th colspan=2>Explanation of the StDraw::Tower(...) method parameters ("barrel" style )
</tr>
 <tr>
 <th>XZ plane view
 <th>XY plane view
 </tr>
 <tr>
 <td><center><img src="http://www.star.bnl.gov/public/comp/vis/StDraw3D/examples/Draw3DTowerZYPlane.png" width=340px></center>
 <td><center><img src="http://www.star.bnl.gov/public/comp/vis/StDraw3D/examples/Draw3DTowerXYPlane.png" width=340px></center>
 </tr>
 <tr>
 <th colspan=2>Explanation of the StDraw::Tower(...) method parameters (default "endcap" style )
 </tr>
 <tr>
 <th>XZ plane view
 <th>XY plane view
 </tr>
 <tr>
 <td><center><img src="http://www.star.bnl.gov/public/comp/vis/StDraw3D/examples/Draw3DTowerZYPlaneEC.png" width=340px></center>
 <td><center><img src="http://www.star.bnl.gov/public/comp/vis/StDraw3D/examples/Draw3DTowerXYPlaneEC.png" width=340px></center>
 </tr></table>
 \endhtmlonly */
//__________________________________________________________________________________________
TObject *StDraw3D::Tower(float radius
        , float lambda, float phi
        , float dlambda, float dphi
        , Color_t col,Style_t sty, Size_t siz)
{
   float d2 =  dlambda/2;
   return Tower(radius, lambda,lambda-d2, lambda+d2,phi, dphi, col, sty, siz);
}
//! This is an overloaded member function, provided for convenience.
/*! The method creates one "tower" object (see:  http://root.cern.ch/root/html/TTRAP.html#TTRAP:description )
   \param  radius - the distance between the  base of the tower and the "origin".
   It can be either the distance from the Z-axis for the kBarrelStyle 
   or the distance from the xy plane for EndCap tower (default) style, for example.
   \param  lambda  - the tower direction (in rads). It is the angle in respect of the Y-axis for kBarrelStyle tower or Z-axis.
   \param  lambda1  - the tower near edge  (in rads)
   \param  lambda2  - the tower far  edge  (in rads)
   \param  phi     - the angle (in rads) in XY plane against of the Ox
   \param  dphi    - the non-negative angle "length" of the tower
   \param  col     - ROOT fill color ( see: http://root.cern.ch/root/html/TAttFill.html ) 
   \param  sty     - ROOT fill style ( see: http://root.cern.ch/root/html/TAttFIll.html ) 
                The default style is "Endcap".  One can add \c kBarrelStyle constant to ROOT style to get the "barrel" style tower \n
		= 0 - solid color,  4001 - wireframe view, 4001 ... 4100 - solid translucent color from 99% transparent to 100% opaque
   \param   siz - the height of the tower. It can be used to visualize the energy deposit or for any other reason.
   \return - a pointer to the ROOT "view" TVolume created to render the input parameters.
 \htmlonly
 <table>
 <tr>
 <th colspan=2>Explanation of the StDraw::Tower(...) method parameters ("barrel" style )
</tr>
 <tr>
 <th>XZ plane view
 <th>XY plane view
 </tr>
 <tr>
 <td><center><img src="http://www.star.bnl.gov/public/comp/vis/StDraw3D/examples/Draw3DTowerZYL1Plane.png" width=340px></center>
 <td><center><img src="http://www.star.bnl.gov/public/comp/vis/StDraw3D/examples/Draw3DTowerXYPlane.png" width=340px></center>
 </tr>
 <tr>
 <th colspan=2>Explanation of the StDraw::Tower(...) method parameters (default "endcap" style )
 </tr>
 <tr>
 <th>XZ plane view
 <th>XY plane view
 </tr>
 <tr>
 <td><center><img src="http://www.star.bnl.gov/public/comp/vis/StDraw3D/examples/Draw3DTowerZYL1PlaneEC.png" width=340px></center>
 <td><center><img src="http://www.star.bnl.gov/public/comp/vis/StDraw3D/examples/Draw3DTowerXYPlaneEC.png" width=340px></center>
 </tr></table>
 \endhtmlonly 
*/
//__________________________________________________________________________________________
TObject *StDraw3D::Tower(float radius, float lambda, float lambda1, float lambda2, float phi,float dphi, Color_t col,Style_t sty, Size_t siz)
{   
   if (gGeometry) {
      gGeometry->GetListOfMatrices()->Clear();
   }
   if (lambda2-lambda1 < 0 ) {
       Warning("StDraw3D::Tower", "The illegal negative value for dlambda = %f", lambda2-lambda1);
       float swp = lambda1;
       lambda1 = lambda2;
       lambda2 = swp;
   }
   if (dphi < 0 ) {
       Warning("StDraw3D::Tower", "The illegal negative value for dphi = %f", dphi);
       dphi = -dphi;
   }
   float zNear, xNear, x1Near,x2Near, yNear, y1Near,y2Near, zFar, xFar, yFar, x1Far,x2Far, y1Far, y2Far;
   // redefine size 
   siz = siz *TMath::Cos(lambda);
   
   bool barrel = (sty >= kBarrelStyle);
   if (barrel) {
      lambda   = -lambda;
    //  dlambda1 = -dlambda1;
    //  dlambda2 = -dlambda2;
   }

   zNear  = radius;
   
   yNear  = zNear*TMath::Tan(lambda);
   y1Near = zNear*TMath::Tan(lambda1);
   y2Near = zNear*TMath::Tan(lambda2);
   
   xNear  = 0;
   x1Near = TMath::Sqrt(y1Near*y1Near + zNear*zNear) * TMath::Tan(dphi/2);
   x2Near = TMath::Sqrt(y2Near*y2Near + zNear*zNear) * TMath::Tan(dphi/2); 

   zFar  = radius+siz;
   
   yFar  = zFar*TMath::Tan(lambda);
   y1Far = zFar*TMath::Tan(lambda1);
   y2Far = zFar*TMath::Tan(lambda2);
   
   xFar  = 0;
   x1Far = TMath::Sqrt(y1Far*y1Far + zFar*zFar) * TMath::Tan(dphi/2);
   x2Far = TMath::Sqrt(y2Far*y2Far + zFar*zFar) * TMath::Tan(dphi/2); 

   float dy = TMath::Tan(lambda )*siz/2;
   
   // to fight Rene Brun one has to assign an unique name fro each tower. Weird !
   
   // const char *towerName= gGeometry ? Form("CALO%d", gGeometry->GetListOfShapes()->GetSize()): "CALO";
     
   TTRAP *trap = new TTRAP( "CALO", Form("Angle%d",int(lambda/M_PI*180))
         , "Barrel"                // Material
         , siz/2                   // dz
         , lambda*TMath::RadToDeg()// Float_t theta (ROOT needs degree)
         , 90                      // Float_t phi   (ROOT needs degree)
         , (y2Near-y1Near )/2      // Float_t h1
         , x1Near                  // Float_t bl1
         , x2Near                  // Float_t tl1
         , 0                       // Float_t alpha1 (ROOT needs degree)
         , (y2Far-y1Far )/2        // Float_t h2
         , x1Far                   // Float_t bl2
         , x2Far                   // Float_t tl2
         , 0                       // Float_t alpha2 (ROOT needs degree)
         );
   if (gGeometry) gGeometry->GetListOfShapes()->Remove(trap);
   bool draw = false;
   if (!fTopVolume) {
       draw = true;
       fTopVolume = new volume_view_3D();
   }
   volume_view_3D *thisShape = new volume_view_3D(Form("Lamda=%f : Phi=%f; ",lambda,phi),"tower",trap);

   static double rotmatrixZ[] =  {   1,        0,        0
                                   , 0,        0,       -1
                                   , 0,        1,        0
                                 };
   static double rotmatrixZNeg[] = {   1,        0,        0
                                     , 0,       -1,        0
                                     , 0,        0,       -1
                                   }; // for the lambda < 0 and End_Cup style
   float a = -phi+p2;
   double rotmatrixX[] = {  cos(a), -sin(a),    0
                          , sin(a),  cos(a),    0
                          ,   0,        0,      1
                         };
   TRotMatrix *rootMatrixX   = new TRotMatrix("rotx","rotx",rotmatrixX);
   if (gGeometry) gGeometry->GetListOfMatrices()->Remove(rootMatrixX);
   TVolumePosition *position = fTopVolume->Add(thisShape,0,0,0,rootMatrixX);
   position->SetMatrixOwner();
   if (barrel) {
      TRotMatrix *rootMatrixZ   = new TRotMatrix("rotz","rotZ",rotmatrixZ);
      if (gGeometry) gGeometry->GetListOfMatrices()->Remove(rootMatrixZ);
      TVolumePosition zpos(thisShape,0,0,0,rootMatrixZ);
      zpos.SetMatrixOwner();
      position->Mult(zpos);   
   } else if (lambda < 0) {
      TRotMatrix *rootMatrixZ   = new TRotMatrix("rotz","rotZ",rotmatrixZNeg);
      if (gGeometry) gGeometry->GetListOfMatrices()->Remove(rootMatrixZ);
      TVolumePosition zpos(thisShape,0,0,0,rootMatrixZ);
      zpos.SetMatrixOwner();
      position->Mult(zpos);   
   }
   TVolumePosition rpos(thisShape,0,yNear+dy,zNear + siz/2);
   position->Mult(rpos);   
   thisShape->SetFillColor(col);
   thisShape->SetLineColor(col);
   thisShape->SetFillStyle(barrel ? sty-kBarrelStyle : sty );
   if ( draw /* && Pad() */ ) {
      Draw(fTopVolume,"same");
   }
   fView = thisShape;
   return thisShape;
}
//! This is an overloaded member function, provided for convenience.
/*! The method creates one "tower" object (see:  http://root.cern.ch/root/html/TTRAP.html#TTRAP:description )
   \param  radius - the distance between the  base of the tower and the "origin". 
   It can be either the distance from the Z-axis for the kBarrelStyle
   or the distance from the xy plane for Endcap towers (default), for example.
   \param  eta \a (eta,dEta)  - the tower direction \a eta and width \a dEta (in eta units). (See: http://en.wikipedia.org/wiki/Pseudorapidity ) 
   \param  phi     - the angle (in rads) in XY plane against of the Ox
   \param  dphi    - the non-negative angle "length" of the tower
   \param  col     - ROOT fill color ( see: http://root.cern.ch/root/html/TAttFill.html ) 
   \param  sty     - ROOT fill style ( see: http://root.cern.ch/root/html/TAttFill.html ) 
                one can add kBarrelStyle constant to ROOT style to get the "barrel" style tower \n
			= 0 - solid color,  4001 - wireframe view, 4001 ... 4100 - solid translucent color from 99% transparent to 100% opaque
   \param   siz - the height of the tower. It can be used to visualize the energy deposit or for any other reason.
   \return - a pointer to the ROOT "view" TVolume created to render the input parameters.
 \htmlonly
 <table>
 <tr>
 <th colspan=2>Explanation of the StDraw::Tower(...) method parameters ("barrel" style )
</tr>
 <tr>
 <th>XZ plane view
 <th>XY plane view
 </tr>
 <tr>
 <td><center><img src="http://www.star.bnl.gov/public/comp/vis/StDraw3D/examples/Draw3DTowerZYEtaPlane.png" width=340px></center>
 <td><center><img src="http://www.star.bnl.gov/public/comp/vis/StDraw3D/examples/Draw3DTowerXYPlane.png" width=340px></center>
 </tr>
 <tr>
 <th colspan=2>Explanation of the StDraw::Tower(...) method parameters (default "endcap" style )
 </tr>
 <tr>
 <th>XZ plane view
 <th>XY plane view
 </tr>
 <tr>
 <td><center><img src="http://www.star.bnl.gov/public/comp/vis/StDraw3D/examples/Draw3DTowerZYEtaPlaneEC.png" width=340px></center>
 <td><center><img src="http://www.star.bnl.gov/public/comp/vis/StDraw3D/examples/Draw3DTowerXYPlaneEC.png" width=340px></center>
 </tr></table>
 \endhtmlonly */
//__________________________________________________________________________________________
TObject *StDraw3D::Tower( float radius, const StarRoot::StEta &eta
                         , float phi, float dphi
                         , Color_t col,Style_t sty, Size_t siz)
{
   bool barrel = (sty >= kBarrelStyle);
   double  lambda,lambda1,lambda2;
   lambda  = eta;
   if ( lambda > p2 ) lambda -= 2*p2;
   if (barrel) {
      lambda  = p2-eta;
      lambda2 = p2-eta.dLambda2();
      lambda1 = p2-eta.dLambda1();
   } else {
      lambda2 = eta.dLambda1();
      lambda1 = eta.dLambda2();
   }
   return Tower(radius,lambda,lambda1,lambda2, phi, dphi, col, sty, siz);
}
//! Set the footer (caption) defined by the input \a footer text string 
/*!
\param footer - the text string to be drawn onto the bottom of the 3D scene image. The string may contain sevral lines separated by \\n end-of-line symbol
 */
//__________________________________________________________________________________________
void StDraw3D::SetFooter(const char *footer)
{
   TString viewerFooter = "{footer:";
   viewerFooter += footer; viewerFooter += "}";
   SetDrawOption(viewerFooter.Data());
}

//__________________________________________________________________________________________
//! Animate the viewer from the gdb session 
/*! For example, use
  \code
      gdb> p gEventDisplay->Point(0,0,0,1,1,1)
  \endcode
   to add one 3D point to the plot followed by 
  \code
      gdb> p gEventDisplay->Animate();
  \endcode 
  to be able to interract with the display.
  To continue the debugger session select "option"->"Interrupt" menu
*/
//__________________________________________________________________________________________
void StDraw3D::Animate()
{
   TVirtualPad *pad = Pad();
   if (pad && pad->IsModified()) {
      Update();
   } 
   ForceAnimate(0,200);
}


ClassImp(StDraw3D)
