// @(#)root/base:$Name:  $:$Id: TEmbeddedPad.cxx,v 1.10 2013/08/30 16:00:23 perev Exp $
// Author: Valeri Fine   02/18/2006

/****************************************************************************
**
** Copyright (C) 2006 by Valeri Fine.  All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
**
*****************************************************************************/


/////////////////////////////////////////////////////////////////////////////
//                                                                         //
// TEmbeddedPad                                                            //
//                                                                         //
// Reimplementation of the TPad class to create the stand-alone offscreen   //
// TPad  (without any mother TCanvas                                       //
//                                                                         //
// begin_html <P ALIGN=CENTER> <IMG SRC="png/TEmbeddedPadLife.png"> </P>  end_html  //
//                                                                         //
/////////////////////////////////////////////////////////////////////////////


#include "TEmbeddedPad.h"
#include "TStyle.h"
#include "TView.h"
#include "TMath.h"
#include "TList.h"
#include "TImage.h"
#ifdef R__QT
#  include "TGQt.h"
#  include "TQtWidget.h"
#  include "qpaintdevice.h"
#  include "qpixmap.h"
#endif

#if ROOT_VERSION_CODE >= ROOT_VERSION(5,24,0)
#  include "TVirtualPadPainter.h"
#  include "TPadPainter.h"
#endif

ClassImp(TEmbeddedPad)

//______________________________________________________________________________
TEmbeddedPad::TEmbeddedPad(TPad *mother) :  TPad(), fPainter(0)
{   fMother = mother;                                                               }
//______________________________________________________________________________
TEmbeddedPad::TEmbeddedPad(const char *name, const char *title, UInt_t width,
        UInt_t height, Color_t color, Short_t bordersize, Short_t bordermode)
      : /* TVirtualPad(name,title,0,0,1,1,color,bordersize,bordermode), */
        fPw(width),fPh(height)
      , fFullPixmapID(-1), fPainter(0)
{
   // TEmbeddedPad constructor.
   //
   //  A pad is a linked list of primitives.
   //  A TEmbeddedPad pad is to create the off-screen images (a'la on screen TCanvas) 
   //  It is NOT contained in a canvas. It may contain other regular pads (TPad objects).
   //  A pad has attributes. When a pad is created, the attributes
   //  defined in the current style are copied to the pad attributes.
   //
   //  width, height - the pad size in pixel.
   //
   //  bordersize - is the border size in pixels
   //  bordermode = -1  box looks as it is behind the screen
   //  bordermode    0  no special effects
   //  bordermode    1  box looks as it is in front of the screen
   // --
   // The class objects are convinient tool for the Web-server ROOT-based applications.
   // On Unix platform one should regard using it with Xvfb server instead of the regular X11-server
   // begin_html (see:  <a href="http://en.wikipedia.org/wiki/Xvfb">X window virtual framebuffer </a>)  end_html
#ifdef R__QT
   TQtWidget::InitRint();
#endif   
   fCanvas     = 0;               // Compare with the basic  TPad::TPad 
   fMother     = 0;               // Compare with the basic  TPad::TPad 
   fPrimitives = new TList;
   fExecs      = new TList;
   fPadPointer = 0;
   fTheta      = 30;
   fPhi        = 30;
   fGridx      = gStyle->GetPadGridX();
   fGridy      = gStyle->GetPadGridY();
   fTickx      = gStyle->GetPadTickX();
   fTicky      = gStyle->GetPadTickY();
   fFrame      = 0;
   fView       = 0;
   fPadPaint   = 0;
   fPadView3D  = 0;
   fPixmapID   = -1;      // -1 means pixmap will be created by ResizePad()
#if ROOT_VERSION_CODE >= ROOT_VERSION(5,00,0)
   fGLDevice   = -1;
   fCopyGLDevice = kFALSE;
   fEmbeddedGL = kFALSE;
#endif
   fNumber     = 0;
   fAbsCoord   = kFALSE;
   fEditable   = kTRUE;
   fCrosshair  = 0;
   fCrosshairPos = 0;

   fFixedAspectRatio = kFALSE;
   fAspectRatio      = 0.;

   fViewer3D = 0;

   // Set default world coordinates to NDC [0,1]
   fX1 = 0;
   fX2 = 1;
   fY1 = 0;
   fY2 = 1;

   TPad *padsav = (TPad*)gPad;

   fLogx = gStyle->GetOptLogx();
   fLogy = gStyle->GetOptLogy();
   fLogz = gStyle->GetOptLogz();

   fUxmin = fUymin = fUxmax = fUymax = 0;

   // Set pad parameters and Compute conversion coefficients
   SetPad(name, title, 0, 0, 1, 1, color, bordersize, bordermode);
   Range(0, 0, 1, 1);
   SetBit(kCanDelete);

   if (padsav) padsav->cd();
   return;
   
}
//______________________________________________________________________________
TEmbeddedPad::~TEmbeddedPad()
{
   // Destroy yhte pixmap buffer
    if (fFullPixmapID != -1) {
       gVirtualX->SelectWindow(fFullPixmapID);
       gVirtualX->ClosePixmap();
    }
} 
//______________________________________________________________________________
void  TEmbeddedPad::Close(Option_t *option)
{
   // ROOT version has no propection against fCanvas == 0
#if ROOT_VERSION_CODE >= ROOT_VERSION(5,24,0)
   if (fPixmapID != -1) {
     if (gPad) {
        if (!gPad->IsBatch()) {
           GetPainter()->SelectDrawable(fPixmapID);
           GetPainter()->DestroyDrawable();
         }
      }
      fPixmapID = -1;
   }
#endif
   TPad
::Close(option);
}

//______________________________________________________________________________
TVirtualPadPainter *TEmbeddedPad::GetPainter()
{
#if ROOT_VERSION_CODE >= ROOT_VERSION(5,24,0)
   //Get pad painter from TCanvas.
   // The method is not virtual. The bug report has was filed
   // <http://savannah.cern.ch/bugs/?54044>
   if (!fPainter) {
      if (fCanvas) {
         // use Painter via base class
         fPainter =  TPad::GetPainter();
      } else {
         // Alas we have to copy code with the mouse from
         // TCanvas::GetCanvasPainter()
         // ROOT provides us no suitable interface
         // ----  TCanvas::GetCanvasPainter()

         // Access and (probably) creation of pad painter.
         // CreatePainter();
         fPainter = new TPadPainter;//Do not need plugin manager for this!
        //  if ( UseGL() )   fUseGL = kFALSE;
      }
   }
   return  fPainter;
#else
   return 0;
#endif

}

//______________________________________________________________________________
Int_t TEmbeddedPad::GetCanvasID() const
{
   return fCanvas ? TPad::GetCanvasID() : -1;
}
//______________________________________________________________________________
Int_t TEmbeddedPad::GetEvent() const
{
   // Get Event.

	return fCanvas ?  TPad::GetEvent() : 0;
}

//______________________________________________________________________________
Int_t TEmbeddedPad::GetEventX() const
{
   // Get X event.

	return fCanvas ?  TPad::GetEventX() : -1;
}

//______________________________________________________________________________
Int_t TEmbeddedPad::GetEventY() const
{
   // Get Y event.
	return fCanvas ?  TPad::GetEventY()  : -1;
}

//______________________________________________________________________________
Color_t TEmbeddedPad::GetHighLightColor() const
{
   // Get highlight color.

	return fCanvas ? TPad::GetHighLightColor() : 0;
}

//______________________________________________________________________________
TObject *TEmbeddedPad::GetSelected() const
{
   // Get selected.

	return fCanvas ? TPad::GetSelected(): 0;
}

//______________________________________________________________________________
TVirtualPad *TEmbeddedPad::GetSelectedPad() const
{
   // Get selected pad.

	return  fCanvas ? TPad::GetSelectedPad() : 0;
}

//______________________________________________________________________________
TVirtualPad *TEmbeddedPad::GetPadSave() const
{
   // Get save pad.

	return  fCanvas ? TPad::GetPadSave() : 0;
}

//______________________________________________________________________________
UInt_t TEmbeddedPad::GetWh() const
{
   // Get Wh.

	return fCanvas ? TPad::GetWh() : fPh;
}

//______________________________________________________________________________
UInt_t TEmbeddedPad::GetWw() const
{
   // Get Ww.

	return fCanvas ? TPad::GetWw() : fPw;
}

//______________________________________________________________________________
Bool_t TEmbeddedPad::IsBatch() const
{
   // Is pad in batch mode ?

	return fCanvas ? TPad::IsBatch() : kFALSE;
}

//______________________________________________________________________________
Bool_t TEmbeddedPad::IsRetained() const
{
   // Is pad retained ?

	return fCanvas ? TPad::IsRetained(): kFALSE;
}

//______________________________________________________________________________
Bool_t TEmbeddedPad::OpaqueMoving() const
{
   // Is pad moving in opaque mode ?

	return fCanvas ? TPad::OpaqueMoving() : kFALSE;
}

//______________________________________________________________________________
Bool_t TEmbeddedPad::OpaqueResizing() const
{
   // Is pad resizing in opaque mode ?

	return fCanvas ? TPad::OpaqueResizing() : kFALSE;
}

//______________________________________________________________________________
void TEmbeddedPad::RecursiveRemove(TObject *obj)
{
	if (fCanvas) TPad::RecursiveRemove(obj);
#if ROOT_VERSION_CODE >= ROOT_VERSION(5,24,0)
   else {
      // No protection against of 
      if (obj == fView) fView = 0;
      if (!fPrimitives) return;
      Int_t nold = fPrimitives->GetSize();
      fPrimitives->RecursiveRemove(obj);
      if (nold != fPrimitives->GetSize()) fModified = kTRUE;
   }
#endif
}

//______________________________________________________________________________
void TEmbeddedPad::SetBatch(Bool_t batch)
{
   // Set pad in batch mode.
   if (fCanvas) TPad::SetBatch(batch);
}
//______________________________________________________________________________
void TEmbeddedPad::SetCrosshair(Int_t crhair)
{
   if (fCanvas) TPad::SetCrosshair(crhair);
}

//______________________________________________________________________________
void TEmbeddedPad::SetCanvasSize(UInt_t ww, UInt_t wh)
{
   // Set canvas size.

     if (fCanvas) TPad::SetCanvasSize(ww,wh);
}

//______________________________________________________________________________
void TEmbeddedPad::SetCursor(ECursor cursor)
{
   // Set cursor type.

     if (fCanvas) TPad::SetCursor(cursor);
}

//______________________________________________________________________________
void TEmbeddedPad::SetDoubleBuffer(Int_t mode)
{
   // Set double buffer mode ON or OFF.

     if (fCanvas) TPad::SetDoubleBuffer(mode);
}

//______________________________________________________________________________
ULong_t TEmbeddedPad::GetHandle() const
{
    // Return the platform depended handle of the pixmap
    // implementation, 
    // QPixmap* in case of the Qt implementation

   UInt_t handle = 0;
   Int_t wid = HasChildren() ? fFullPixmapID : GetPixmapID();
   if (!wid || (wid == -1) ) return handle;
#ifdef R__QT
   QPaintDevice &dev = *TGQt::iwid(wid);
#if QT_VERSION < 0x40000
   QPixmap *pix=0;
   if  ( dev.devType() == QInternal::Pixmap )
   {
      pix = (QPixmap *)&dev;
   }
#else
   QImage *pix=0;
   if  ( dev.devType() == QInternal::Image )
   {
      pix = (QImage*)&dev;
   }
#endif
   handle = (ULong_t )pix;
#endif
   return handle;
}
//______________________________________________________________________________
Int_t TEmbeddedPad::GetHandle( Int_t id, double m11, double m12, double m21, double m22, double dx, double dy) const
{
   // Copy the pad pixmap onto the pixmap defined by "id"
   // To create the id one has to apply TVirtualX::OpenPixmap   method
   //    Int_t qpixid = gVirtualX->OpenPixmap(...);
   // to free the id one should apply 
   //        TVirtualX::SelectPixmap(Int_t qpixid)
   //        TVirtualX::ClosePixmap()
   // methods.
   // The original pixmap is not changed. 
   // The transformation matrix (via QWMatrix) is internally adjusted 
   // to compensate for unwanted translation, i.e. xForm() returns 
   // the smallest image that contains all the transformed points of the original image. 
#ifdef R__QT
   // Convert the "id" into the QPixmap pointer
   QPixmap *dst = (QPixmap *)TGQt::iwid(id);
   if (dst)
     GetHandle((ULong_t) dst, m11, m12, m21, m22, dx,dy);
   return id;
#endif
}

//______________________________________________________________________________
ULong_t TEmbeddedPad::GetHandle( ULong_t handle, double m11, double m12, double m21, double m22, double dx, double dy) const
{
   // Copy the pad pixmap onto the pixmap defined by "handle"
   // This is an overloaded member function, provided for convenience. 
   // It behaves essentially like the above function.
#ifdef R__QT
   QPixmap &padPixmap = *(QPixmap*)GetHandle();
   QPixmap &dstPixmap = *(QPixmap*)handle;
   dstPixmap = padPixmap.xForm(QWMatrix(m11, m12, m21, m22, dx,dy));
   return handle;
#endif 
}
//______________________________________________________________________________
Int_t TEmbeddedPad::GetHandleRotate( Int_t id, double angle) const
{
   // Copy the pad pixmap onto the pixmap defined by "id"
   // To create the id one has to apply TVirtualX::OpenPixmap   method
   // to free the id one should apply 
   //        TVirtualX::SelectPixmap(Int_t qpixid)
   //        TVirtualX::ClosePixmap()
   // methods.
   // The original pixmap is not changed. 
   // The transformation matrix (via QWMatrix) is internally adjusted 
   // to compensate for unwanted translation, i.e. xForm() returns 
   // the smallest image that contains all the transformed points of the original image. 
#ifdef R__QT
   // Convert the "id" into the QPixmap pointer
   QPixmap *dst = (QPixmap *)TGQt::iwid(id);
   if (dst)
     GetHandleRotate((ULong_t) dst, angle);
#endif
   return id;
}

//______________________________________________________________________________
ULong_t TEmbeddedPad::GetHandleRotate( ULong_t handle, double angle) const
{
   // Copy the pad pixmap onto the pixmap defined by "handle"
   // This is an overloaded member function, provided for convenience. 
   // It behaves essentially like the above function.
#ifdef R__QT
   QPixmap &padPixmap = *(QPixmap*)GetHandle();
   QPixmap &dstPixmap = *(QPixmap*)handle;
   dstPixmap = padPixmap.xForm(QWMatrix().rotate(angle));
#endif 
   return handle;
}
//______________________________________________________________________________
TImage *TEmbeddedPad::CreateImage( Int_t x, Int_t y, UInt_t w, UInt_t h)
{
	// Create a TImage object from this TPad
	// The user is responsible to delete it
   TImage *img = TImage::Create(); 
	// TASImage::FromPad implementation does ise TPad::Paint method
	// that can change "this" object. 
	// by this reason we can not make this method to be "const"
   img->FromPad(this,x,y,w,h); 
	return img;
}

//______________________________________________________________________________
void TEmbeddedPad::SetSelected(TObject *obj)
{
   // Set selected.

     if (fCanvas) TPad::SetSelected(obj);
}

//______________________________________________________________________________
void TEmbeddedPad::Update()
{
    // Update pad.

    if (fCanvas) TPad::Update();
    else {
       PaintModified();
       Flush();
    }
}
//______________________________________________________________________________
void TEmbeddedPad::Flush()
{
   // merge all subpad into one image buffers.
   if (HasChildren() ) {
      if (!IsBatch()) {
    //  gVirtualX->SelectWindow(fCanvasID);
        if (fFullPixmapID == -1)      // this case is handled via the ctor
            fFullPixmapID = gVirtualX->OpenPixmap(fPw, fPh);
        else 
            gVirtualX->ResizePixmap(fFullPixmapID, fPw, fPh);
        gVirtualX->SelectWindow(fFullPixmapID);
        CopyPixmaps();
      }
   }
   gVirtualX->SelectWindow(-1);
}

//______________________________________________________________________________
TObject *TEmbeddedPad::Pick(Int_t px, Int_t py)
{
	// Return the pointer to the ROOT object selected at the pixel px,py
	TObjLink *pickobj  = 0;
	TObject  *selected = 0;
	if (TPad::Pick( px,py, pickobj) && pickobj) 
	    selected =  pickobj->GetObject();
	return selected;
}

//______________________________________________________________________________
void TEmbeddedPad::Print(const char *filenam, Option_t *option)
{
    if (fCanvas) TPad::Print(filenam,option);
}

//______________________________________________________________________________
void TEmbeddedPad::ResizePad(UInt_t width, UInt_t height)
{
   // Change the pixel size of the pad
   fPw = width;
   fPh = height;
   ResizePad();
}
//______________________________________________________________________________
void TEmbeddedPad::ResizePad(Option_t *option)
{
   // Compute pad conversion coefficients. (see: TPad::Resize() 
   //
   //   Conversion from x to px & y to py
   //   =================================
   //
   //       x - xmin     px - pxlow              xrange  = xmax-xmin
   //       --------  =  ----------      with
   //        xrange        pxrange               pxrange = pxmax-pxmin
   //
   //               pxrange(x-xmin)
   //   ==>  px =   ---------------  + pxlow   = fXtoPixelk + fXtoPixel * x
   //                    xrange
   //
   //   ==>  fXtoPixelk = pxlow - pxrange*xmin/xrange
   //        fXtoPixel  = pxrange/xrange
   //           where  pxlow   = fAbsXlowNDC*fCw
   //                  pxrange = fAbsWNDC*fCw
   //
   //
   //       y - ymin     py - pylow              yrange  = ymax-ymin
   //       --------  =  ----------      with
   //        yrange        pyrange               pyrange = pymax-pymin
   //
   //               pyrange(y-ymin)
   //   ==>  py =   ---------------  + pylow   = fYtoPixelk + fYtoPixel * y
   //                    yrange
   //
   //   ==>  fYtoPixelk = pylow - pyrange*ymin/yrange
   //        fYtoPixel  = pyrange/yrange
   //           where  pylow   = (1-fAbsYlowNDC)*fCh
   //                  pyrange = -fAbsHNDC*fCh
   //
   //-  Conversion from px to x & py to y
   //   =================================
   //
   //             xrange(px-pxlow)
   //   ==>  x =  ----------------  + xmin  = fPixeltoXk + fPixeltoX * px
   //                 pxrange
   //-
   //   ==>  fPixeltoXk = xmin - pxlow*xrange/pxrange
   //        fPixeltoX  = xrange/pxrange
   //
   //             yrange(py-pylow)
   //   ==>  y =  ----------------  + ymin  = fPixeltoYk + fPixeltoY * py
   //                 pyrange
   //-
   //   ==>  fPixeltoYk = ymin - pylow*yrange/pyrange
   //        fPixeltoY  = yrange/pyrange
   //
   //-----------------------------------------------------------------------
   //
   //  Computation of the coefficients in case of LOG scales
   //- =====================================================
   //
   //   A, Conversion from pixel coordinates to world coordinates
   //
   //       Log(x) - Log(xmin)      Log(x/xmin)       px - pxlow
   //  u = --------------------- =  -------------  =  -----------
   //      Log(xmax) - Log(xmin)    Log(xmax/xmin)     pxrange
   //
   //  ==> Log(x/xmin) = u*Log(xmax/xmin)
   //      x = xmin*exp(u*Log(xmax/xmin)
   //   Let alfa = Log(xmax/xmin)/fAbsWNDC
   //
   //      x = xmin*exp(-alfa*pxlow) + exp(alfa*px)
   //      x = fPixeltoXk*exp(fPixeltoX*px)
   //  ==> fPixeltoXk = xmin*exp(-alfa*pxlow)
   //      fPixeltoX  = alfa
   //
   //       Log(y) - Log(ymin)      Log(y/ymin)       pylow - py
   //  v = --------------------- =  -------------  =  -----------
   //      Log(ymax) - Log(ymin)    Log(ymax/ymin)     pyrange
   //
   //   Let beta = Log(ymax/ymin)/pyrange
   //      Log(y/ymin) = beta*pylow - beta*py
   //      y/ymin = exp(beta*pylow - beta*py)
   //      y = ymin*exp(beta*pylow)*exp(-beta*py)
   //  ==> y = fPixeltoYk*exp(fPixeltoY*py)
   //      fPixeltoYk = ymin*exp(beta*pylow)
   //      fPixeltoY  = -beta
   //
   //-  B, Conversion from World coordinates to pixel coordinates
   //
   //  px = pxlow + u*pxrange
   //     = pxlow + Log(x/xmin)/alfa
   //     = pxlow -Log(xmin)/alfa  + Log(x)/alfa
   //     = fXtoPixelk + fXtoPixel*Log(x)
   //  ==> fXtoPixelk = pxlow -Log(xmin)/alfa
   //  ==> fXtoPixel  = 1/alfa
   //
   //  py = pylow - Log(y/ymin)/beta
   //     = fYtoPixelk + fYtoPixel*Log(y)
   //  ==> fYtoPixelk = pylow - Log(ymin)/beta
   //      fYtoPixel  = 1/beta

	if ( fCanvas ) { TPad::ResizePad(); return; }
   // Recompute subpad positions in case pad has been moved/resized
   TPad *parent = fMother;
	if (!(parent || fCanvas) ||  (this == (TPad *)gPad->GetCanvas()) ) {
      fAbsXlowNDC  = fXlowNDC;
      fAbsYlowNDC  = fYlowNDC;
      fAbsWNDC     = fWNDC;
      fAbsHNDC     = fHNDC;
   }
   else {
      fAbsXlowNDC  = fXlowNDC*parent->GetAbsWNDC() + parent->GetAbsXlowNDC();
      fAbsYlowNDC  = fYlowNDC*parent->GetAbsHNDC() + parent->GetAbsYlowNDC();
      fAbsWNDC     = fWNDC*parent->GetAbsWNDC();
      fAbsHNDC     = fHNDC*parent->GetAbsHNDC();
   }

   Double_t ww = (Double_t)GetWw();
   Double_t wh = (Double_t)GetWh();
   Double_t pxlow   = fAbsXlowNDC*ww;
   Double_t pylow   = (1-fAbsYlowNDC)*wh;
   Double_t pxrange = fAbsWNDC*ww;
   Double_t pyrange = -fAbsHNDC*wh;

   // Linear X axis
   Double_t rounding = 0.00005;
   Double_t xrange  = fX2 - fX1;
   fXtoAbsPixelk = rounding + pxlow - pxrange*fX1/xrange;      //origin at left
   fXtoPixelk = rounding +  -pxrange*fX1/xrange;
   fXtoPixel  = pxrange/xrange;
   fAbsPixeltoXk = fX1 - pxlow*xrange/pxrange;
   fPixeltoXk = fX1;
   fPixeltoX  = xrange/pxrange;
   // Linear Y axis
   Double_t yrange  = fY2 - fY1;
   fYtoAbsPixelk = rounding + pylow - pyrange*fY1/yrange;      //origin at top
   fYtoPixelk = rounding +  -pyrange - pyrange*fY1/yrange;
   fYtoPixel  = pyrange/yrange;
   fAbsPixeltoYk = fY1 - pylow*yrange/pyrange;
   fPixeltoYk = fY1;
   fPixeltoY  = yrange/pyrange;

   // Coefficients to convert from pad NDC coordinates to pixel coordinates

   fUtoAbsPixelk = rounding + pxlow;
   fUtoPixelk = rounding;
   fUtoPixel  = pxrange;
   fVtoAbsPixelk = rounding + pylow;
   fVtoPixelk = -pyrange;
   fVtoPixel  = pyrange;

   // Coefficients to convert from canvas pixels to pad world coordinates

   // Resize all subpads
   TObject *obj;
   TIter    next(GetListOfPrimitives());
   while ((obj = next())) {
      if (obj->InheritsFrom(TPad::Class()))
         ((TPad*)obj)->ResizePad(option);
   }

   // Reset all current sizes
   if (IsBatch()) {
       fPixmapID = 0;
   } else {
      gVirtualX->SetLineWidth(-1);
      gVirtualX->SetTextSize(-1);

      // create or re-create off-screen pixmap
      if (fPixmapID) {
         int w = TMath::Abs(XtoPixel(fX2) - XtoPixel(fX1));
         int h = TMath::Abs(YtoPixel(fY2) - YtoPixel(fY1));

         //protection in case of wrong pad parameters.
         //without this protection, the OpenPixmap or ResizePixmap crashes with
         //the message "Error in <RootX11ErrorHandler>: BadValue (integer parameter out of range for operation)"
         //resulting in a frozen xterm
         if (!(TMath::Finite(fX1)) || !(TMath::Finite(fX2))
             || !(TMath::Finite(fY1)) || !(TMath::Finite(fY2)))
            Warning("ResizePad", "Inf/NaN propagated to the pad. Check drawn objects.");
         if (w <= 0 || w > 10000) {
            Warning("ResizePad", "%s width changed from %d to %d\n",GetName(),w,10);
            w = 10;
         }
         if (h <= 0 || h > 10000) {
            Warning("ResizePad", "%s height changed from %d to %d\n",GetName(),h,10);
            h = 10;
         }
         if (fPixmapID == -1) {      // this case is handled via the ctor
            fPixmapID = gVirtualX->OpenPixmap(w, h);
         } else {
#if ROOT_VERSION_CODE >= ROOT_VERSION(5,00,0)
            if (fGLDevice != -1) {
               Int_t borderSize = fBorderSize > 0 ? fBorderSize : 2;
               Int_t ww = w - 2 * borderSize;
               Int_t hh = h - 2 * borderSize;
               Int_t px = 0, py = 0;
               XYtoAbsPixel(fX1, fY2, px, py);
               if (ww < 0) ww = 1;//not to get HUGE pixmap :)
               if (hh < 0) hh = 1;//not to get HUGE pixmap :)
               // gGLManager->ResizeGLPixmap(fGLDevice, px + borderSize, py + borderSize, ww, hh);
               //after gl-pixmap was resized, we need to repaint not to get something interesting
#ifdef GLMANAGER
               if (fEmbeddedGL) gGLManager->DrawViewer(fViewer3D);
               else 
#endif
						Modified(kTRUE);
            }
#endif

            if (gVirtualX->ResizePixmap(fPixmapID, w, h)) {
               Modified(kTRUE);
            }
         }
      }
   }
   if (fView) {
      TPad *padsav  = (TPad*)gPad;
      if (padsav == this) {
         fView->ResizePad();
      } else {
         cd();
         fView->ResizePad();
         padsav->cd();
      }
   }
}
//______________________________________________________________________________
Int_t TEmbeddedPad::HasChildren() const
{
   // Check whether there is any subpad
   // Count the children if present
   Int_t counter = 0;
   TIter    next(GetListOfPrimitives());
   TObject *obj = 0;
   while ((obj = next())) 
      if (obj->InheritsFrom(TPad::Class())) counter++;
   return counter;
}

//______________________________________________________________________________
TVirtualViewer3D *TEmbeddedPad::GetViewer3D(Option_t *type )
{
	return fCanvas ? TPad::GetViewer3D(type) : 0;
}

//______________________________________________________________________________
Int_t TEmbeddedPad::GetGLDevice()
{
#if ROOT_VERSION_CODE >= ROOT_VERSION(5,00,0)
	return fCanvas ? TPad::GetGLDevice() : -1;
#else
   return -1;
#endif
}

//______________________________________________________________________________
TPad    *TEmbeddedPad::Pick(Int_t px, Int_t py, TObjLink *&pickobj) 
{ 
   return TPad::Pick(px,py,pickobj); 
}
//______________________________________________________________________________
void  TEmbeddedPad::Print(Option_t *option) const 
{ 
   TPad::Print(option); 
}
