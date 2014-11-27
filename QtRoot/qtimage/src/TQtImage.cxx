// $Id: TQtImage.cxx,v 1.5 2013/08/30 16:00:27 perev Exp $
// Author: Valeri Fine   2/02/2004

/*************************************************************************
 * Derived from TASImage, author: Fons Rademakers, Reiner Rohlfs 28/11/01*
 *************************************************************************
 * Copyright (C) 1995-2001, Rene Brun, Fons Rademakers and Reiner Rohlfs *
 * All rights reserved.                                                  *
 *                                                                       *
 * For the licensing terms see $ROOTSYS/LICENSE.                         *
 * For the list of contributors see $ROOTSYS/README/CREDITS.             *
 *************************************************************************/

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// TQtImage                                                             //
//                                                                      //
// Interface to image processing library using QImage/QPixmap.          //
// It allows for the reading and writing of images in different         //
// formats, several image manipulations (scaling, tiling, merging,      //
// etc.) and displaying in pads.                                        //
// The size of the image on the screen does not depend on the original  //
// size of the image but on the size of the pad. Therefore it is very   //
// easy to resize the image on the screen by resizing the pad.          //
//                                                                      //
// Besides reading an image from a file an image can be defined by a    //
// two dimensional array of values. A palette defines the color of      //
// each value.                                                          //
//                                                                      //
// The image can be zoomed by defining a rectangle with the mouse.      //
// The color palette can be modified with a GUI, just select            //
// StartPaletteEditor() from the context menu.                          //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "TQtImage.h"
#include "TPluginManager.h"

#include "TGQt.h"
#include <QImage>
#include <QColor>
#include <QFileInfo>
#include <QDebug>
#include <QPainter>
#include <QPixmap>

#include "TROOT.h"
#include "TSystem.h"
#include "TVirtualX.h"
#include "TCanvas.h"
#include "TArrayD.h"
#include "TVectorD.h"
#include "TVirtualPS.h"
#include "TGaxis.h"
#include "TColor.h"
#include "TObjArray.h"
#include "TQtPaletteEditor.h"
//#include "TMath.h"
const Int_t kFRS = 5;  // size of frame of image on pad in pixels

ClassImp(TQtImage)

//______________________________________________________________________________
TQtImage::TQtImage() :fImage(0),fScaledImage(0),fPixmapBuffer(0)
{
   // Default image ctor.
}

//______________________________________________________________________________
TQtImage::TQtImage(const char *file, EImageFileTypes): TImage(file),
      fImage(0),fScaledImage(0),fPixmapBuffer(0)
{
   // Create an image object and read from specified file.
   // For more information see description of function ReadImage()
   // which is called by this constructor.

   ReadImage(file);
}

//______________________________________________________________________________
TQtImage::TQtImage(const char *name, const Double_t *imageData, UInt_t width,
                   UInt_t height, TImagePalette *palette) : 
        TImage(name)
       ,fImage(0),fScaledImage(0),fPixmapBuffer(0)
{
   // Creates an image depending on the values of imageData.
   // For more information see function SetImage() which is called
   // by this constructor.

   SetImage(imageData, width, height, palette);
}

//______________________________________________________________________________
TQtImage::TQtImage(const char *name, const TArrayD &imageData, UInt_t width,
                   TImagePalette *palette) : 
              TImage(name)
             ,fImage(0),fScaledImage(0),fPixmapBuffer(0)
{
   // Creates an image depending on the values of imageData. The size
   // of the image is width X (imageData.fN / width).
   // For more information see function SetImage() which is called by
   // this constructor.

   SetImage(imageData, width, palette);
}

//______________________________________________________________________________
TQtImage::TQtImage(const char *name, const TVectorD &imageData, UInt_t width,
                   TImagePalette *palette) : 
             TImage(name)
            ,fImage(0),fScaledImage(0),fPixmapBuffer(0)
{
   // Creates an image depending on the values of imageData. The size
   // of the image is width X (imageData.fN / width).
   // For more information see function SetImage() which is called by
   // this constructor.

   SetImage(imageData, width, palette);
}

//______________________________________________________________________________
TQtImage::TQtImage(const TQtImage &img) : 
        TImage(img)
       ,fImage(0),fScaledImage(0),fPixmapBuffer(0)
{
   // Image copy ctor.

   if (img.IsValid()) {
      if (img.fImage)        fImage       = new QImage(*img.fImage);
      if (img.fScaledImage)  fScaledImage = new TQtImage(*img.fScaledImage);
      if (img.fPixmapBuffer) fPixmapBuffer= new QPixmap(*img.fPixmapBuffer);
      fZoomUpdate = kTRUE;
      fZoomOffX   = img.fZoomOffX;
      fZoomOffY   = img.fZoomOffY;
      fZoomWidth  = img.fZoomWidth;
      fZoomHeight = img.fZoomHeight;
   }
}

//______________________________________________________________________________
TQtImage &TQtImage::operator=(const TQtImage &img)
{
   // Image assignment operator.

   if (this != &img && img.IsValid()) {
      TImage::operator=(img);
      if (fImage) delete fImage;
      fImage = new QImage(*img.fImage);
      delete fScaledImage; fScaledImage = 0;
      if  ( img.fScaledImage ) fScaledImage = new TQtImage(*img.fScaledImage);

      fZoomUpdate = kTRUE;
      fZoomOffX   = img.fZoomOffX;
      fZoomOffY   = img.fZoomOffY;
      fZoomWidth  = img.fZoomWidth;
      fZoomHeight = img.fZoomHeight;
   }
   return *this;
}
//______________________________________________________________________________
void TQtImage::DestroyImage()
{
   // Image dtor, clean up image and visual.
    QImage *tmp = fImage; fImage = 0;
    delete tmp;

    TQtImage *tmpImg = fScaledImage; fScaledImage = 0;
    delete tmpImg;
    
    QPixmap *tmpPix = fPixmapBuffer; fPixmapBuffer = 0;
    delete tmpPix;
    
    ResetZoom();
}

//______________________________________________________________________________
TQtImage::~TQtImage()
{
   // Image dtor, clean up image and visual.
    DestroyImage();
}
//______________________________________________________________________________
TObject *TQtImage::Clone(const char *newname) const
{ 
   // Clone image.
   if ( !fImage ) {
      Warning("Clone", "Image not initiated");
      return 0;
   }

   TQtImage *im = (TQtImage*)TImage::Create();

   if (!im) {
      Warning("Clone", "Failed to create image");
      return 0;
   }
   *im = *this;
   im->SetName(newname);
   return im;
}
//______________________________________________________________________________
void TQtImage::ReadImage(const char *file, EImageFileTypes /*type*/)
{
   // Read specified image file. The file type is determined by
   // the file extension (the type argument is ignored). It will
   // attempt to append .gz and then .Z to the filename and find such
   // a file. If the filename ends with extension consisting of digits
   // only, it will attempt to find the file with this extension stripped
   // off. On success this extension will be used to load subimage from
   // the file with that number. Subimage is supported only for GIF files.

   if (fImage)  { delete fImage;  fImage = 0; }
   fImage = new QImage(file);
   ResetZoom();

   SetName(file);
}

//______________________________________________________________________________
void TQtImage::WriteImage(const char *file, EImageFileTypes type)
{
   // Write image to specified file. If there is no file extension or
   // if the file extension is unknown, the type argument will be used
   // to determine the file type. The quality and compression is derived from
   // the TAttImage values.
   // The size of the image in the file is independent of the actually
   // displayed size and zooming factor on the screen. This function
   // writes always the original image with its size in the file

   if (!IsValid()) {
      Error("WriteImage", "no image loaded");
      return;
   }

   if (!file || !*file) {
      Error("WriteImage", "no file name specified");
      return;
   }

   QString outFormat =  QFileInfo(file).suffix().toUpper();   
   if (type == kUnknown) 
      outFormat = TGQt::QtFileFormat(outFormat);
   

   //  ASImageFileTypes atype;
   //  MapFileTypes(type, (UInt_t&)atype);

   UInt_t aquality;
   EImageQuality quality = GetImageQuality();
   MapQuality(quality, aquality);
   if (!fImage->save(file,outFormat.toLatin1().constData(),quality))
       Error("WriteImage", "error writing file %s", file);
}
//______________________________________________________________________________
TImage::EImageFileTypes TQtImage::GetFileType(const char *ext)
{
   // Return file type depending on specified extension.
   // Protected method.

   TString s(ext);

   if (s == "xpm")
      return kXpm;
   if (s == "png")
      return kPng;
   if (s == "jpg" || s == "jpeg")
      return kJpeg;
   if (s == "xcf")
      return kXcf;
   if (s == "ppm")
      return kPpm;
   if (s == "pnm")
      return kPnm;
   if (s == "bmp")
      return kBmp;
   if (s == "ico")
      return kIco;
   if (s == "cur")
      return kCur;
   if (s == "gif")
      return kGif;
   if (s == "tiff")
      return kTiff;
   if (s == "xbm")
      return kXbm;

   return kUnknown;
}
//______________________________________________________________________________
Bool_t TQtImage::IsValid() const { 
   return fImage && !fImage->isNull(); 
}
//______________________________________________________________________________
void TQtImage::MapQuality(EImageQuality &quality, UInt_t &asquality, Bool_t toas)
{
   // Map quality to/from AfterImage quality.
   // Protected method.

   if (toas) {
      switch (quality) {
         case kImgPoor:
            asquality = 25; break;
         case kImgFast:
            asquality = 75; break;
         case kImgGood:
            asquality = 50; break;
         case kImgBest:
            asquality = 100; break;
         default:
            asquality = 0;
      }
   } else {
      quality = kImgDefault;
      if (asquality > 0  && asquality <= 25)
         quality = kImgPoor;
      if (asquality > 26 && asquality <= 50)
         quality = kImgFast;
      if (asquality > 51 && asquality <= 75)
         quality = kImgGood;
      if (asquality > 76 && asquality <= 100)
         quality = kImgBest;
   }
}
//______________________________________________________________________________
void  TQtImage::ResetZoom() 
{
  // Reset the zoom members
   if (fScaledImage) {  delete fScaledImage; fScaledImage = 0;}
   
   fZoomUpdate = kFALSE;
   fZoomOffX   = 0;
   fZoomOffY   = 0;
   fZoomWidth  = GetWidth();
   fZoomHeight = GetHeight();
}

//______________________________________________________________________________
void TQtImage::SetImage(const Double_t *imageData, UInt_t width, UInt_t height,
                        TImagePalette *palette)
{
   // Deletes the old image and creates a new image depending on the values
   // of imageData. The size of the image is width X height.
   // The color of each pixel depends on the imageData of the corresponding
   // pixel. The palette is used to convert an image value into its color.
   // If palette is not defined (palette = 0) a default palette is used.
   // Any previously defined zooming is reset.

   TAttImage::SetPalette(palette);

   DestroyImage() ;
  
   // get min and max value of image
   fMinValue = fMaxValue = *imageData;
   for (Int_t pixel = 1; pixel < Int_t(width * height); pixel++) {
      if (fMinValue > *(imageData + pixel)) fMinValue = *(imageData + pixel);
      if (fMaxValue < *(imageData + pixel)) fMaxValue = *(imageData + pixel);
   }

   // copy ROOT palette to asImage palette
   const TImagePalette &pal = GetPalette();

   // QtVectorPalette asPalette;
   Int_t nColors = pal.fNumPoints;
   Int_t                                 depth =  1; // bpp
   QImage::Format format                        = QImage::Format_Mono;     // bpp
   if ( 1 < nColors  &&  nColors <=256 ) format = QImage::Format_Indexed8; // bpp
   else                                  format = QImage::Format_RGB32 ;   // bpp
   fImage =  new QImage(width,height,format);
   QImage &image = *fImage;

   // Fill the color table;
   depth = image.depth ();
   QVector<QRgb>  qPalette =  image.colorTable ();   
   
   if (qPalette.empty()) 
      return;
   Int_t col;
   for (col = 0; col < nColors; col++) {
         qPalette[col] = qRgba(pal.fColorRed  [col]
                              ,pal.fColorGreen[col]
                              ,pal.fColorBlue [col]
                              ,pal.fColorAlpha[col]);
   }
   unsigned int x,y;
   if (depth == 1) {
      for (x =0; x< width;x++) {
         for (y =0; y< height; y++) {
            if ( image.format() == QImage::Format_MonoLSB )
               *(image.scanLine(y) + (x >> 3)) |= 
                   int(((*(imageData++))-fMinValue) / (fMaxValue - fMinValue)) << (x & 7);
            else
               *(image.scanLine(y) + (x >> 3)) |= 
                   int(((*(imageData++))-fMinValue) / (fMaxValue - fMinValue)) << (7 - (x & 7));
      }   }
   } else if (depth == 8) {
      for (x =0; x< width;x++) {
         for (y =0; y< height; y++) {
            *(image.scanLine(y) + x) = (uchar)int(pal.fNumPoints * ((*(imageData++))-fMinValue) / (fMaxValue - fMinValue));
      }   }
   } else if (depth == 16) {
      for (x =0; x< width; x++) {
         for (y =0; y< height;y++) {
            uint *p = (uint *)image.scanLine(y) + x;
            *p = qPalette[int(pal.fNumPoints * ((*(imageData++))-fMinValue) / (fMaxValue - fMinValue))];
      }   }
   } else {
      for (x =0; x< width; x++) {
         for (y =0; y< height;y++) {
            ushort *p = (ushort *)image.scanLine(y) + x;
            *p = qPalette[int(pal.fNumPoints * ((*(imageData++))-fMinValue) / (fMaxValue - fMinValue))];
      }   }
   }
   ResetZoom();
}

//______________________________________________________________________________
void TQtImage::SetImage(const TArrayD &imageData, UInt_t width, TImagePalette *palette)
{
   // Deletes the old image and creates a new image depending on the values
   // of imageData. The size of the image is width X (imageData.fN / width).
   // The color of each pixel depends on the imageData of the corresponding
   // pixel. The palette is used to convert an image value into its color.
   // If palette is not defined (palette = 0) a default palette is used.
   // Any previously defined zooming is reset.

   SetImage(imageData.GetArray(), width, imageData.GetSize() / width, palette);
}

//______________________________________________________________________________
void TQtImage::SetImage(const TVectorD &imageData, UInt_t width, TImagePalette *palette)
{
   // Deletes the old image and creates a new image depending on the values
   // of imageData. The size of the image is width X (imageData.fN / width).
   // The color of each pixel depends on the imageData of the corresponding
   // pixel. The palette is used to convert an image value into its color.
   // If palette is not defined (palette = 0) a default palette is used.
   // Any previously defined zooming is reset.

#if ROOT_VERSION_CODE > ROOT_VERSION(3,10,02)
   SetImage(imageData.GetMatrixArray(), width,
            imageData.GetNoElements() / width, palette);
#else
   SetImage(imageData.GetElements(), width,
            imageData.GetNoElements() / width, palette);
#endif 
}
//______________________________________________________________________________
void  TQtImage::SetImage(Pixmap_t pxm, Pixmap_t /*mask*/)
{
   DestroyImage();
   SetName("unknown");
   if (pxm) {
       QPixmap &thisPix = *(QPixmap *)TGQt::iwid(pxm);
       fImage = new QImage(thisPix.toImage ());
   }
   ResetZoom();
}
//______________________________________________________________________________
void  TQtImage::SetImage(const QImage &img)
{
   DestroyImage();
   SetName("unknown");
   fImage = new QImage(img);
   ResetZoom();
}
//______________________________________________________________________________
void  TQtImage::SetImage(QImage *img)
{
   DestroyImage();
   SetName("unknown");
   fImage = img;
   ResetZoom();
}
//______________________________________________________________________________
void TQtImage::FromPad(TVirtualPad *pad, Int_t x, Int_t y, UInt_t w, UInt_t h)
{
   // Create an image from the given pad, afterwards this image can be
   // saved in any of the supported image formats.

   if (!pad) {
      Error("FromPad", "pad cannot be 0");
      return;
   }
   DestroyImage();

   SetName(pad->GetName());

   if (w == 0)
      w = pad->UtoPixel(1.);
   if (h == 0)
      h = pad->VtoPixel(0.);

   QPixmap *pix = (QPixmap *)TGQt::iwid(pad->GetPixmapID());
   if (pix) {
      QPixmap tmpPx = pix->copy(x,y,int(w),int(h));
      fImage = new QImage(tmpPx.toImage());
   }
}

//______________________________________________________________________________
void TQtImage::Draw(Option_t *option)
{
   // Draw image. Support the following drawing options:
   // "T[x,y[,tint]]" - tile image (use specified offset and tint),
   //                   e.g. "T100,100,#556655"
   //                   with this option the zooming is not possible
   //                   and disabled
   // "N"             - display in new canvas (of original image size)
   // The default is to display the image in the current gPad.

   static Bool_t calcBorder = kTRUE;
   static UInt_t bw = 0;
   static UInt_t bh = 0;

   TString opt = option;
   opt.ToLower();
   if (opt.Contains("n") || !gPad || !gPad->IsEditable()) {
      TCanvas *c = new TCanvas(GetName(), Form("%s (%d x %d)", GetName(),
                               fImage->width(), fImage->height()),
                               fImage->width()+bw, fImage->height()+bh);
      if (calcBorder) {
         bw = c->GetWindowWidth() - c->GetWw();
         bh = c->GetWindowHeight() - c->GetWh();
         c->SetWindowSize(fImage->width()+bw, fImage->height()+bh);
         c->Resize();
         calcBorder = kFALSE;
      }
   }
   TObject::Draw(option);
}

//______________________________________________________________________________
void TQtImage::Paint(Option_t *option)
{
   // Paint image in current pad. See Draw() function for drawing options.

   if ( !IsValid() ) {
      Error("Paint", "no image set");
      return;
   }

   Int_t   tile_x = 0, tile_y = 0;
   QColor tile_tint;
   Bool_t  tile = kFALSE;

   TString opt = option;
   opt.ToLower();
   if (opt.Contains("t")) {
      char stint[64];
      if (sscanf(opt.Data()+opt.Index("t"), "t%d,%d,%s", &tile_x, &tile_y,
                 stint) <= 3) {
         tile = kTRUE;
         tile_tint.setNamedColor(stint);
      } else
         Error("Paint", "tile option error");
   }

   QImage *image = fImage;

   // Get geometry of pad
   Int_t to_w = gPad->UtoPixel(1.);
   Int_t to_h = gPad->VtoPixel(0.);
   QPixmap  *padPixmap = (QPixmap  *)TGQt::iwid(gPad->GetPixmapID());
   if (padPixmap) {
      to_w = padPixmap->width();
      to_h = padPixmap->height();
   }
   Double_t pad_w = to_w;
   Double_t pad_h = to_h;

   // keep a frame of 5 pixels
   to_w -= 2 * kFRS;
   to_h -= 2 * kFRS;
   Int_t pal_w = 0;

#if 0
   if (fImage->alt.vector) {
      pal_w = Int_t(to_w * 0.2);
      to_w -= pal_w;
   }
#endif 

   if (to_w < 2 * kFRS + 1 || to_h < 2 * kFRS + 1) {
      Error("Paint", "pad too small to display an image");
      return;
   }

   if (GetConstRatio()) {
      if ((Double_t)to_w / (Double_t)fZoomWidth <
          (Double_t)to_h / (Double_t)fZoomHeight)
         to_h = Int_t(Double_t(fZoomHeight) * to_w / fZoomWidth);
      else
         to_w = Int_t(Double_t(fZoomWidth) * to_h / fZoomHeight);
   }

   QImage *grad_im = 0;
#if 0
   if (fImage->alt.vector) {
      // draw the palette
      ASGradient grad;
      const TImagePalette &pal = GetPalette();

      grad_im = new QImage(UInt_t(0.3 * pal_w), to_h - 20,32,pal.fNumPoints);
      grad.npoints = pal.fNumPoints;
      grad.type    = GRADIENT_Top2Bottom;
      grad.color   = new ARGB32[grad.npoints];
      grad.offset  = new double[grad.npoints];

      for (Int_t pt = 0; pt < grad.npoints; pt++) {
         Int_t oldPt = grad.npoints - pt -1;
         grad.offset[pt] = 1 - pal.fPoints[oldPt];
         grad.color[pt] = (((ARGB32)(pal.fColorBlue[oldPt]  & 0xff00)) >>  8) |
                          (((ARGB32)(pal.fColorGreen[oldPt] & 0xff00))      ) |
                          (((ARGB32)(pal.fColorRed[oldPt]   & 0xff00)) <<  8) |
                          (((ARGB32)(pal.fColorAlpha[oldPt] & 0xff00)) << 16);
      }

      grad_im = make_gradient(fgVisual, &grad , UInt_t(0.3 * pal_w),
                              to_h - 20, SCL_DO_COLOR,
                              ASA_ASImage, 0, GetImageQuality());
      delete [] grad.color;
      delete [] grad.offset;
   }
#endif 

   if (!tile) {
      // Scale and zoom image if needed
      if (Int_t(fImage->width()) != to_w || Int_t(fImage->height()) != to_h ||
          fImage->width() != ((int)fZoomWidth) || fImage->height() != ((int) fZoomHeight) ) {
         if (fScaledImage) {
            if (Int_t(fScaledImage->GetWidth()) != to_w ||
                Int_t(fScaledImage->GetHeight()) != to_h ||
                fZoomUpdate) {
                delete fScaledImage; fScaledImage = 0;
            }
         }
         if (!fScaledImage) {
            if (fImage->width() != ((int)fZoomWidth) || fImage->height() != ((int)fZoomHeight)) {
               // zoom and scale image
               QImage *scaledImg = 
                             new QImage( 
                                          (fImage->copy(fZoomOffX,
                                                       fImage->height() - fZoomHeight - fZoomOffY,
                                                       fZoomWidth,fZoomHeight)).scaled(to_w, to_h)
                                        );
               delete fScaledImage; 
               fScaledImage = new TQtImage(); fScaledImage->SetImage(scaledImg);
            } else {
               // scale image, no zooming
                QImage *scaledImg = new QImage(fImage->scaled(to_w, to_h));
                delete fScaledImage; 
                fScaledImage = new TQtImage(); fScaledImage->SetImage(scaledImg);
           }
         }
         image = fScaledImage->Image();
      }
   }
   fZoomUpdate = kFALSE;
   if (!image) {
      Error("Paint", "image could not be rendered to display");
      return;
   }
   
   QPixmap pxmap(to_w,to_h);
   Int_t pxmapId = TGQt::RegisterWid(&pxmap);
   
   if (tile) {
      delete fScaledImage; fScaledImage = 0;
      QPainter tile(&pxmap);
      QPixmap pxImage = QPixmap::fromImage(*fImage);
      tile.drawTiledPixmap(0,0,to_w,to_h,pxImage);
   } else {
      QPainter normal(&pxmap);
      normal.drawImage(0,0, *image);
   }
   gPad->cd();
   gVirtualX->CopyPixmap(pxmapId, kFRS, kFRS);
   if (grad_im && !gVirtualPS) {
#if 0
      // draw color bar
      pxmap = asimage2pixmap(fgVisual, gVirtualX->GetDefaultRootWindow(),
                             grad_im, 0, kTRUE);
      wid = gVirtualX->AddWindow(pxmap, UInt_t(0.3 * pal_w), to_h - 20);

      gPad->cd();
      gVirtualX->CopyPixmap(wid, Int_t(to_w + 0.2 * pal_w), kFRS + 20);
      gVirtualX->RemoveWindow(wid);
      gVirtualX->DeletePixmap(pxmap);
#endif

      gPad->cd();

      // values of palette
      TGaxis axis;
      Int_t ndiv = 510;
      double min = fMinValue;
      double max = fMaxValue;
      axis.SetLineColor(0);       // draw white ticks
      axis.PaintAxis((to_w + 0.5 * pal_w) / pad_w, (pad_h - to_h - kFRS - 1) / pad_h,
                     (to_w + 0.5 * pal_w) / pad_w, (pad_h - kFRS - 21) / pad_h,
                     min, max, ndiv, "+LU");
      min = fMinValue;
      max = fMaxValue;
      axis.SetLineColor(1);       // draw black ticks
      axis.PaintAxis((to_w + 0.5 * pal_w) / pad_w, (pad_h - to_h - kFRS) / pad_h,
                     (to_w + 0.5 * pal_w) / pad_w, (pad_h - kFRS - 20) / pad_h,
                     min, max, ndiv, "+L");

   }

   // loop over pxmap and draw image to PostScript
   if (gVirtualPS) {

      // get special color cell to be reused during image printing
      TObjArray *colors = (TObjArray*) gROOT->GetListOfColors();
      TColor *color = 0;
      // Look for color by name
      if ((color = (TColor*)colors->FindObject("Image_PS")) == 0)
         color = new TColor(colors->GetEntries(), 1., 1., 1., "Image_PS");

      gVirtualPS->SetFillColor(color->GetNumber());
      gVirtualPS->SetFillStyle(1001);

      Double_t xconv = gPad->PixeltoX(to_w) / image->width();
      Double_t yconv = TMath::Abs(gPad->PixeltoY(to_h)) / image->height();
      Double_t x1 = kFRS * xconv;
      Double_t x2 = (kFRS + 1) * xconv;
      Double_t y2 = 1 - kFRS * yconv;
      Double_t y1 = 1 - (kFRS + 1) * yconv;
      gVirtualPS->CellArrayBegin(image->width(), image->height(), x1, x2, y1, y2);
#if 0
      ASImageDecoder *imdec = start_image_decoding(fgVisual, image, SCL_DO_ALL,
                                 0, 0, image->width(), image->height(), 0);
      for (Int_t yt = 0; yt < (Int_t)image->height(); yt++) {
         imdec->decode_image_scanline(imdec);
         for (Int_t xt = 0; xt < (Int_t)image->width(); xt++)
            gVirtualPS->CellArrayFill(imdec->buffer.red[xt],
                                      imdec->buffer.green[xt],
                                      imdec->buffer.blue[xt]);
      }
      stop_image_decoding(&imdec);
#endif 
      gVirtualPS->CellArrayEnd();

      // print the color bar
      if (grad_im) {
         xconv = gPad->PixeltoX(Int_t(0.3 * pal_w)) / grad_im->width();
         yconv = TMath::Abs(gPad->PixeltoY(to_h - 20)) / grad_im->height();
         x1 = (to_w + 0.2 * pal_w) * xconv;
         x2 = ((to_w + 0.2 * pal_w) + 1) * xconv;
         y2 = 1 - (kFRS + 20) * yconv;
         y1 = 1 - (kFRS + 21) * yconv;
         gVirtualPS->CellArrayBegin(grad_im->width(), grad_im->height(),
                                    x1, x2, y1, y2);
#if 0
         imdec = start_image_decoding(fgVisual, grad_im, SCL_DO_ALL,
                                      0, 0, grad_im->width(), grad_im->height(), 0);
         for (Int_t yt = 0; yt < (Int_t)grad_im->height(); yt++) {
            imdec->decode_image_scanline(imdec);
            for (Int_t xt = 0; xt < (Int_t)grad_im->width(); xt++)
               gVirtualPS->CellArrayFill(imdec->buffer.red[xt],
                                         imdec->buffer.green[xt],
                                         imdec->buffer.blue[xt]);
         }
         stop_image_decoding(&imdec);
#endif 
         gVirtualPS->CellArrayEnd();

         // values of palette
         TGaxis axis;
         Int_t ndiv = 510;
         double min = fMinValue;
         double max = fMaxValue;
         axis.SetLineColor(1);       // draw black ticks
         axis.PaintAxis((to_w + 0.5 * pal_w) / pad_w, (pad_h - to_h - kFRS) / pad_h,
                        (to_w + 0.5 * pal_w) / pad_w, (pad_h - kFRS - 20) / pad_h,
                        min, max, ndiv, "+L");
      }
   }
   TGQt::UnRegisterWid(&pxmap);
   delete grad_im;
}

//______________________________________________________________________________
Int_t TQtImage::DistancetoPrimitive(Int_t px, Int_t py)
{
   // Is the mouse in the image?

   Int_t pxl, pyl, pxt, pyt;
   Int_t px1 = gPad->XtoAbsPixel(gPad->GetX1());
   Int_t py1 = gPad->YtoAbsPixel(gPad->GetY1());
   Int_t px2 = gPad->XtoAbsPixel(gPad->GetX2());
   Int_t py2 = gPad->YtoAbsPixel(gPad->GetY2());
   if (px1 < px2) {pxl = px1; pxt = px2;}
   else           {pxl = px2; pxt = px1;}
   if (py1 < py2) {pyl = py1; pyt = py2;}
   else           {pyl = py2; pyt = py1;}

   // Are we inside the image leave 5 (kFRS) pixels on all sides to
   // be able to grab the pad
   if ((px > pxl+kFRS && px < pxt-kFRS) && (py > pyl+kFRS && py < pyt-kFRS))
      return 0;

   return 999999;
}

//______________________________________________________________________________
void TQtImage::ExecuteEvent(Int_t event, Int_t px, Int_t py)
{
   // Execute mouse events.

   gPad->SetCursor(kCross);

   static Int_t stx, sty;
   static Int_t oldx, oldy;

   if (!IsValid()) return;

   if (event == kButton1Motion || event == kButton1Down  ||
       event == kButton1Up) {

      // convert to image pixel on screen
      Int_t imgX = (Int_t)(gPad->AbsPixeltoX(px) * gPad->XtoPixel(1) + 0.5) - kFRS;
      Int_t imgY = (Int_t)((1 - gPad->AbsPixeltoY(py)) * gPad->YtoPixel(0) + 0.5) - kFRS;

      if (imgX < 0)  px = px - imgX;
      if (imgY < 0)  py = py - imgY;

      QImage *image = fImage;
      if (fScaledImage) image = fScaledImage->Image();

      if (imgX >= (int)image->width())  px = px - imgX + image->width() - 1;
      if (imgY >= (int)image->height()) py = py - imgY + image->height() - 1;

      switch (event) {

         case kButton1Down:
            gVirtualX->SetLineColor(-1);

            stx = oldx = px;
            sty = oldy = py;
            break;

         case kButton1Motion:
            gVirtualX->DrawBox(oldx, oldy, stx, sty, TVirtualX::kHollow);
            oldx = px;
            oldy = py;
            gVirtualX->DrawBox(oldx, oldy, stx, sty, TVirtualX::kHollow);
            break;

         case kButton1Up:
            // do nothing if zoom area is too small
            if ( TMath::Abs(stx - px) < 5 || TMath::Abs(sty - py) < 5)
               return;

            Double_t xfact = (fScaledImage) ? (Double_t)fScaledImage->GetWidth()  / fZoomWidth  : 1;
            Double_t yfact = (fScaledImage) ? (Double_t)fScaledImage->GetHeight() / fZoomHeight : 1;

            Int_t imgX1 = (Int_t)(gPad->AbsPixeltoX(stx) * gPad->XtoPixel(1) + 0.5) - kFRS;
            Int_t imgY1 = (Int_t)((1 - gPad->AbsPixeltoY(sty)) * gPad->YtoPixel(0) + 0.5) - kFRS;
            Int_t imgX2 = (Int_t)(gPad->AbsPixeltoX(px)  * gPad->XtoPixel(1) + 0.5) - kFRS;
            Int_t imgY2 = (Int_t)((1 - gPad->AbsPixeltoY(py))  * gPad->YtoPixel(0) + 0.5) - kFRS;
            imgY1 = image->height() - 1 - imgY1;
            imgY2 = image->height() - 1 - imgY2;
            imgX1 = (Int_t)(imgX1 / xfact) + fZoomOffX;
            imgY1 = (Int_t)(imgY1 / yfact) + fZoomOffY;
            imgX2 = (Int_t)(imgX2 / xfact) + fZoomOffX;
            imgY2 = (Int_t)(imgY2 / yfact) + fZoomOffY;

            Zoom((imgX1 < imgX2) ? imgX1 : imgX2, (imgY1 < imgY2) ? imgY1 : imgY2,
                 abs(imgX1 - imgX2) + 1, abs(imgY1 - imgY2) + 1);

            gVirtualX->SetLineColor(-1);
            gPad->Modified(kTRUE);
            gPad->Update();
            break;
      }
   }
}

//______________________________________________________________________________
char *TQtImage::GetObjectInfo(Int_t px, Int_t py) const
{
   // Get image pixel coordinates and the pixel value at the mouse pointer.

   static char info[64];
   info[0] = 0;

   if (!IsValid()) return info;

   // convert to image pixel on screen
   px = (Int_t)(gPad->AbsPixeltoX(px) * gPad->XtoPixel(1) + 0.5) - kFRS;
   py = (Int_t)((1 - gPad->AbsPixeltoY(py)) * gPad->YtoPixel(0) + 0.5) - kFRS;

   // no info if mouse is outside of image
   if (px < 0 || py < 0)  return info;

   QImage *image = fImage;
   if (fScaledImage) image = fScaledImage->Image();
   if (px >= (int)image->width() || py >= (int)image->height())
      return info;

   py = image->height() - 1 - py;
   // convert to original image size and take zooming into account
   if (fScaledImage) {
      px = (Int_t)(px / (Double_t)fScaledImage->GetWidth()  * fZoomWidth ) + fZoomOffX;
      py = (Int_t)(py / (Double_t)fScaledImage->GetHeight() * fZoomHeight) + fZoomOffY;
   }
#if 0
   if (fImage->alt.vector)
      sprintf(info, "x: %d  y: %d   %.5g",
              px, py, fImage->alt.vector[px + py * fImage->width()]);
   else
#endif 
      sprintf(info, "x: %d  y: %d", px, py);

   return info;
}

//______________________________________________________________________________
void TQtImage::SetPalette(const TImagePalette *palette)
{
   // Set a new palette to an image. Only images that were created with the
   // SetImage() functions can be modified with this function.
   // The previously used palette is destroyed.

   TAttImage::SetPalette(palette);

   if (!IsValid())
      return;

#if 0
   if (fImage->alt.vector == 0)
      return;

   // copy ROOT palette to asImage palette
   const TImagePalette &pal = GetPalette();

   ASVectorPalette asPalette;
   asPalette.npoints = pal.fNumPoints;
   asPalette.channels[0] = new CARD16 [asPalette.npoints];
   asPalette.channels[1] = new CARD16 [asPalette.npoints];
   asPalette.channels[2] = new CARD16 [asPalette.npoints];
   asPalette.channels[3] = new CARD16 [asPalette.npoints];
   memcpy(asPalette.channels[0], pal.fColorBlue,  pal.fNumPoints * sizeof(UShort_t));
   memcpy(asPalette.channels[1], pal.fColorGreen, pal.fNumPoints * sizeof(UShort_t));
   memcpy(asPalette.channels[2], pal.fColorRed,   pal.fNumPoints * sizeof(UShort_t));
   memcpy(asPalette.channels[3], pal.fColorAlpha, pal.fNumPoints * sizeof(UShort_t));

   asPalette.points = new double[asPalette.npoints];
   for (Int_t point = 0; point < Int_t(asPalette.npoints); point++)
      asPalette.points[point] = fMinValue + (fMaxValue - fMinValue) * pal.fPoints[point];

   // use the new palette in this image
   colorize_asimage_vector(fgVisual, fImage, &asPalette, ASA_ASImage, GetImageQuality());

   delete [] asPalette.points;
   for (Int_t col = 0; col < 4; col++)
      delete [] asPalette.channels[col];

   delete fScaledImage;  fScaledImage = 0;
   }
#endif 
}

//______________________________________________________________________________
void TQtImage::Scale(UInt_t toWidth, UInt_t toHeight)
{
   // Scales the original image. The size of the image on the screen does not
   // change because it is defined by the size of the pad.
   // This function can be used to change the size of an image before writing
   // it into a file. The colors of the new pixels are interpolated.
   // An image created with the SetImage() functions cannot be modified with
   // the function SetPalette() any more after a call of this function!

   if (IsValid()) {
      if (toWidth  <  1   ) toWidth  = 1;
      if (toHeight <  1   ) toHeight = 1;
      if (toWidth  > 30000) toWidth  = 30000;
      if (toHeight > 30000) toHeight = 30000;
      QImage *img = new QImage(fImage->scaled( QSize(toWidth, toHeight )));
      delete fImage;
      delete fScaledImage; fScaledImage = 0;

      fImage = img;
   }
}

//______________________________________________________________________________
void TQtImage::Zoom(UInt_t offX, UInt_t offY, UInt_t width, UInt_t height)
{
   // The area of an image displayed in a pad is defined by this function.
   // Note: the size on the screen is defined by the size of the pad.
   // The original image is not modified by this function.
   // If width or height is larger than the original image they are reduced to
   // the width and height of the image.
   // If the off values are too large (off + width > image width) than the off
   // values are decreased. For example: offX = image width - width
   // Note: the parameters are always relative to the original image not to the
   // size of an already zoomed image.

   if (IsValid()) {
      fZoomUpdate = kTRUE;

      fZoomWidth  = (width == 0) ? 1 : ((width > (UInt_t)fImage->width()) ? (UInt_t)fImage->width() : width);
      fZoomHeight = (height == 0) ? 1 : ((height > (UInt_t)fImage->height()) ? (UInt_t)fImage->height() : height);
      fZoomOffX   = offX;
      if (fZoomOffX + fZoomWidth > (UInt_t)fImage->width())
         fZoomOffX = (UInt_t)fImage->width() - fZoomWidth;
      fZoomOffY   = offY;
      if (fZoomOffY + fZoomHeight > (UInt_t)fImage->height())
         fZoomOffY = (UInt_t)fImage->height() - fZoomHeight;
      gPad->Modified(kTRUE);
      gPad->Update();
   }
}

//______________________________________________________________________________
void TQtImage::UnZoom()
{
   // Un-zooms the image to original size.

   if (IsValid()) {
      ResetZoom();
      fZoomUpdate = kTRUE;
      gPad->Modified(kTRUE);
      gPad->Update();
   }
}

//______________________________________________________________________________
void TQtImage::Flip(Int_t flip)
{
   // Flip image in place. Flip is either 90, 180, 270, 180 is default.
   // This function manipulates the original image and destroys the
   // scaled and zoomed image which will be recreated at the next call of
   // the Draw function. If the image is zoomed the zoom - coordinates are
   // now relative to the new image.
   // This function cannot be used for images which were created with the
   // SetImage() functions, because the original pixel values would be
   // destroyed.

  if (IsValid() ) {
      QMatrix flipMatrix;  flipMatrix.rotate(flip);
      QImage *img = new QImage(fImage->transformed(flipMatrix));

      delete fImage;          fImage        = 0;
      delete fScaledImage;    fScaledImage  = 0;
      delete fPixmapBuffer;   fPixmapBuffer = 0;

      fImage = img;
      UnZoom();
   }
}

//______________________________________________________________________________
void TQtImage::Mirror(Bool_t vert)
{
   // Mirror image in place. If vert is true mirror in vertical axis,
   // horizontal otherwise. Vertical is default.
   // This function manipulates the original image and destroys the
   // scaled and zoomed image which will be recreated at the next call of
   // the Draw function. If the image is zoomed the zoom - coordinates are
   // now relative to the new image.
   // This function cannot be used for images which were created with the
   // SetImage() functions, because the original pixel values would be
   // destroyed.

   if (IsValid() ) {
      delete fScaledImage; fScaledImage = 0;
      QImage *mirror = new QImage
         (fImage->mirrored(!vert,vert));
      delete fImage;
      fImage = mirror;
      gPad->Modified(kTRUE);
      gPad->Update();
   }
}

//______________________________________________________________________________
UInt_t TQtImage::GetWidth() const
{
   // Return width of original image not of the displayed image.
   return fImage? fImage->width():0;
}

//______________________________________________________________________________
UInt_t TQtImage::GetHeight() const
{
   // Return height of original image not of the displayed image.

   return fImage? fImage->height():0;
}

//______________________________________________________________________________
void TQtImage::StartPaletteEditor()
{
   // Start palette editor.
   // Opens a GUI to edit the color palette
   if (IsValid())  {
      // TAttImage::StartPaletteEditor();
      if (fPaletteEditor == 0) {
         TPluginHandler *h;
         if ((h = gROOT->GetPluginManager()->FindHandler("TPaletteEditor"))) {
            if (h->LoadPlugin() == -1)
               return;
            // Due a multiply inheritance we have to cast this pointer ourselve  (TAttImage *)this
            fPaletteEditor = 
               (TQtPaletteEditor *) h->ExecPlugin(3, (TAttImage *)this, 80, 25);
         }
      }
   }
}

//______________________________________________________________________________
void  TQtImage::Tile(UInt_t toWidth, UInt_t toHeight){
 // Tile the original image.

   if (!IsValid()) {
      Warning("Tile", "Image not initiated");
      return;
   }

   if (toWidth < 1)
       toWidth = 1;
   if (toHeight < 1 )
      toHeight = 1;
   if (toWidth > 30000)
      toWidth = 30000;
   if (toHeight > 30000)
      toHeight = 30000;
   QPixmap px = QPixmap::fromImage(*fImage);
   fImage->fill(0);
   QPixmap img(toWidth, toHeight);
   QPainter p(&img);
   p.drawTiledPixmap (0,0, toWidth, toHeight,px); 
   p.end();   
   DestroyImage();
   fImage = new QImage(img.toImage());
   ResetZoom();
  // UnZoom();
   //fZoomUpdate = kZoomOps;
}

//______________________________________________________________________________
void  TQtImage::Crop(Int_t x , Int_t y, UInt_t width , UInt_t height) {
   if (fImage  && (x || y || width != GetWidth() || height != GetHeight() ) ) {
      QImage  *q = new QImage(fImage->copy (x, y, width, height ));
      DestroyImage();
      fImage = q;
   }
   ResetZoom();
}

//  To do 25 June 2005
//______________________________________________________________________________
void  TQtImage::Gray(Bool_t on )
{ if (on) {}  }
//______________________________________________________________________________
Bool_t TQtImage::IsGray() const
{ return fImage ? fImage->isGrayscale() : kFALSE; }

//______________________________________________________________________________
Pixmap_t  TQtImage::GetPixmap() 
{
   Pixmap_t pixmap = 0;
   QImage *img = fScaledImage ? fScaledImage->Image() : fImage;
   if (img) {
      gVirtualX->CreatePixmap(0,GetWidth(),GetHeight());
      QPixmap &thisPix = *(QPixmap *)TGQt::iwid(pixmap);
      thisPix = QPixmap::fromImage(*img);
   }
   return pixmap; 
}

//______________________________________________________________________________
Pixmap_t  TQtImage::GetMask() 
{
   Pixmap_t pixmap = 0;
   QImage *img = fScaledImage ? fScaledImage->Image() : fImage;
   if (img) {
      gVirtualX->CreatePixmap(0,GetWidth(),GetHeight());
      QPixmap &thisPix = *(QPixmap *)TGQt::iwid(pixmap);
      // QImage mask = img->
      thisPix = QPixmap::fromImage(*img);
   }
   return pixmap;
}

//______________________________________________________________________________
TArrayD *TQtImage::GetArray(UInt_t w, UInt_t h, TImagePalette *palette)
{
   // In case of vectorized image return an associated array of doubles
   // otherwise this method creates and returns a 2D array of doubles corresponding to palette.
   // If palette is ZERO a color converted to double value [0, 1] according to formula
   //   Double_t((r << 16) + (g << 8) + b)/0xFFFFFF
   // The returned array must be deleted after usage.

   if (!fImage) {
      Warning("GetArray", "Bad Image");
      return 0;
   }

   TArrayD *ret;
#if 0
   if (fImage->alt.vector) {
      ret = new TArrayD(fImage->width*fImage->height, fImage->alt.vector);
      return ret;
   }
   ASImageDecoder *imdec;
#endif

   w = w ? w : GetWidth();
   h = h ? h : GetHeight();

   if ((GetWidth() != w) || (GetHeight() != h)) {
      Scale(w, h);
   }

   QImage *img = fScaledImage ? fScaledImage->fImage : fImage;
#if 0
   if ((imdec = start_image_decoding(0, img, SCL_DO_ALL, 0, 0,
                                     img->width, 0, 0)) == 0) {
      Warning("GetArray", "Failed to create image decoder");
      return 0;
   }
#endif

   ret = new TArrayD(w * h);
   int r = 0;
   int g = 0;
   int b = 0;
   Int_t p = 0;
   Double_t v = 0;

   for (UInt_t k = 0; k < h; k++) {
      for (UInt_t i = 0; i < w; ++i)  {
         QRgb pixel =  img->pixel(k,w);
         r = qRed(pixel);
         g = qGreen(pixel);
         b = qBlue(pixel);
         if (palette) p = palette->FindColor(r, g, b);
         v = palette ? palette->fPoints[p] : Double_t((r << 16) + (g << 8) + b)/0xFFFFFF;
         ret->AddAt(v, (h-k-1)*w + i);
      }
   }
   return ret;
}

#if 0
   TArrayL  *GetPixels(Int_t /*x*/= 0, Int_t /*y*/= 0, UInt_t /*w*/ = 0, UInt_t /*h*/ = 0) { return 0; }
   //______________________________________________________________________________
TArrayL *TASImage::GetPixels(Int_t x, Int_t y, UInt_t width, UInt_t height)
{
   // Return 2D array of machine dependent pixel values.

   if (!fImage) {
      Warning("GetPixels", "Wrong Image");
      return 0;
   }

   ASImage *img =  fScaledImage ? fScaledImage->fImage : fImage;
   ASImageDecoder *imdec;

   width = !width  ? img->width : width;
   height = !height ? img->height : height;

   if (x < 0) {
      width -= x;
      x = 0 ;
   }
   if (y < 0) {
      height -= y;
      y = 0;
   }

   if ((x >= (int)img->width) || (y >= (int)img->height)) {
      return 0;
   }

   if ((int)(x + width) > (int)img->width) {
      width = img->width - x;
   }

   if ((int)(y + height) > (int)img->height) {
      height = img->height - y;
   }

   if ((imdec = start_image_decoding(0, fImage, SCL_DO_ALL, 0, y,
                                     img->width, height, 0)) == 0) {
      Warning("GetPixels", "Failed to create image decoder");
      return 0;
   }

   TArrayL *ret = new TArrayL(width * height);
   Int_t r = 0;
   Int_t g = 0;
   Int_t b = 0;
   Long_t p = 0;

   for (UInt_t k = 0; k < height; k++) {
      imdec->decode_image_scanline(imdec);

      for (UInt_t i = 0; i < width; ++i)  {
         if ((r == (Int_t)imdec->buffer.red[i]) &&
             (g == (Int_t)imdec->buffer.green[i]) &&
             (b == (Int_t)imdec->buffer.blue[i])) {
         } else {
            r = (Int_t)imdec->buffer.red[i];
            g = (Int_t)imdec->buffer.green[i];
            b = (Int_t)imdec->buffer.blue[i];
            p = (Long_t)TColor::RGB2Pixel(r, g, b);
         }
         ret->AddAt(p, k*width + i);
      }
   }

   stop_image_decoding(&imdec);
   return ret;
}
#endif
#if 0

   UInt_t   *GetArgbArray() { return 0; }
   UInt_t   *GetScanline(UInt_t /*y*/) { return 0; }

   virtual void      GetImageBuffer(char ** /*buffer*/, int* /*size*/, EImageFileTypes /*type*/ = TImage::kPng) {}
   virtual Bool_t    SetImageBuffer(char ** /*buffer*/, EImageFileTypes /*type*/ = TImage::kPng) { return kFALSE; }
   virtual void      PaintImage(Drawable_t /*wid*/, Int_t /*x*/, Int_t /*y*/) { }
   virtual void      FromWindow(Drawable_t /*wid*/, Int_t /*x*/ = 0, Int_t /*y*/ = 0, UInt_t /*w*/ = 0, UInt_t /*h*/ = 0) {}
#endif
   
