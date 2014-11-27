// @(#)root/asimage:$Name:  $:$Id: TQtImage.h,v 1.5 2013/08/30 16:00:27 perev Exp $
// Author: Valeri Fine 7/02/2004

/*************************************************************************
 * Derived from TASImage, author: Fons Rademakers, Reiner Rohlfs 28/11/01*
 *************************************************************************/
/*************************************************************************
 * Copyright (C) 1995-2001, Rene Brun, Fons Rademakers and Reiner Rohlfs *
 * All rights reserved.                                                  *
 *                                                                       *
 * For the licensing terms see $ROOTSYS/LICENSE.                         *
 * For the list of contributors see $ROOTSYS/README/CREDITS.             *
 *************************************************************************/

#ifndef ROOT_TQtImage
#define ROOT_TQtImage


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

#include "RVersion.h"

#ifndef ROOT_TImage
#include "TImage.h"
#endif
#include "TList.h"
class QImage;
class QPixmap;

class TQtImage : public TImage {

protected:
   QImage   *fImage;        //! pointer to image structure of original image
   TQtImage *fScaledImage;  //! pointer to scaled and zoomed image structure
   QPixmap  *fPixmapBuffer; //! pointer to pixmap shadow buffer of the fImage
   Double_t  fMaxValue;     // max value in image
   Double_t  fMinValue;     // min value in image
   UInt_t    fZoomOffX;     // X - offset for zooming
   UInt_t    fZoomOffY;     // Y - offset for zooming
   UInt_t    fZoomWidth;    // width of zoomed image
   UInt_t    fZoomHeight;   // hight of zoomed image
   Bool_t    fZoomUpdate;   // kTRUE: new zooming required

   EImageFileTypes GetFileType(const char *ext);
   void MapQuality(EImageQuality &quality, UInt_t &asquality,
                   Bool_t toas = kTRUE);
   void DestroyImage();
   void  SetImage(const QImage &img);
   void  SetImage(QImage *img);
   void  ResetZoom();
   QImage  *Image() { return  fImage; }

public:
   TQtImage();
   TQtImage(const char *file, EImageFileTypes type = kUnknown);
   TQtImage(const char *name, const Double_t *imageData, UInt_t width, UInt_t height, TImagePalette *palette = 0);
   TQtImage(const char *name, const TArrayD &imageData, UInt_t width, TImagePalette *palette = 0);
   TQtImage(const char *name, const TVectorD &imageData, UInt_t width, TImagePalette *palette = 0);
   TQtImage(const TQtImage &img);
   TQtImage &operator=(const TQtImage &img);
   virtual ~TQtImage();

   // Cloning
   virtual TObject *Clone(const char *) const; //  { return 0; }

   // Input / output
   void  ReadImage(const char *file, EImageFileTypes type = kUnknown);
   void  WriteImage(const char *file, EImageFileTypes type = kUnknown);
   void  SetImage(const Double_t *imageData, UInt_t width, UInt_t height, TImagePalette *palette = 0);
   void  SetImage(const TArrayD &imageData, UInt_t width, TImagePalette *palette = 0);
   void  SetImage(const TVectorD &imageData, UInt_t width, TImagePalette *palette = 0);
   void  SetImage(Pixmap_t pxm, Pixmap_t mask = 0); // {}

   // Pad conversions
   void  FromPad(TVirtualPad *pad, Int_t x = 0, Int_t y = 0,
                 UInt_t w = 0, UInt_t h = 0);
   void  Draw(Option_t *option = "");
   void  Paint(Option_t *option = "");
   Int_t DistancetoPrimitive(Int_t px, Int_t py);
   void  ExecuteEvent(Int_t event, Int_t px, Int_t py);
   char *GetObjectInfo(Int_t px, Int_t py) const;

   // Transformations
   // TAttImage overloaded virtrual method 
   void  StartPaletteEditor(); // *MENU*
   void  SetPalette(const TImagePalette *palette);
   void  Scale(UInt_t toWidth, UInt_t toHeight);
   void  Zoom(UInt_t offX, UInt_t offY, UInt_t width, UInt_t height);  //*MENU*
   void  UnZoom();                     //*MENU*
   void  Flip(Int_t flip = 180);       //*MENU*
   void  Gray(Bool_t on = kTRUE);
   Bool_t IsGray() const;
   void  Mirror(Bool_t vert = kTRUE);  //*MENU*
   void  Tile(UInt_t /*width*/, UInt_t /*height*/); // {}
   void  Crop(Int_t /*x*/ = 0, Int_t /*y*/ = 0, UInt_t /*width*/ = 0, UInt_t /*height*/ = 0); //  {}
   virtual void Pad(const char * /*color*/ = "#FFFFFFFF", UInt_t /*left*/ = 0, 
                   UInt_t /*right*/ = 0, UInt_t /*top*/ = 0, UInt_t /*bottom*/ = 0) {}
   virtual void Blur(Double_t /*horizontal*/ = 3, Double_t /*vertical*/ = 3) {}
#ifndef __CINT__
#if ROOT_VERSION_CODE < ROOT_VERSION(5,11,3)
   virtual void Vectorize(UInt_t /*max_colors*/ = 256, UInt_t /*dither*/ = 4, Int_t /*opaque_threshold*/ = 0) {}
#else
   virtual Double_t *Vectorize(UInt_t /*max_colors*/ = 256, UInt_t /*dither*/ = 4, Int_t /*opaque_threshold*/ = 0) { return 0; }
#endif
#endif
   virtual void HSV(UInt_t /*hue*/ = 0, UInt_t /*radius*/ = 360, Int_t /*H*/ = 0, Int_t /*S*/ = 0, Int_t /*V*/ = 0, 
                    Int_t /*x*/ = 0, Int_t /*y*/ = 0, UInt_t /*width*/ = 0, UInt_t /*height*/ = 0) {}
   virtual void Gradient(UInt_t /*angle*/ = 0, const char * /*colors*/ = "#FFFFFF #000000", const char * /*offsets*/ = 0,
                         Int_t /*x*/ = 0, Int_t /*y*/ = 0, UInt_t /*width*/ = 0, UInt_t /*height*/ = 0) {}
   virtual void Merge(const TImage * /*im*/, const char * /*op*/ = "alphablend", Int_t /*x*/ = 0, Int_t /*y*/ = 0) {}
   virtual void Append(const TImage * /*im*/, const char * /*option*/ = "+", const char * /*color*/ = "#00000000") {}
   virtual void Bevel(Int_t /*x*/ = 0, Int_t /*y*/ = 0, UInt_t /*width*/ = 0, UInt_t /*height*/ = 0,
                      const char * /*hi*/ = "#ffdddddd", const char * /*lo*/ = "#ff555555", 
                      UShort_t /*thick*/ = 1, Bool_t /*pressed*/ = kFALSE) {}

   virtual void DrawText(Int_t x = 0, Int_t y = 0, const char * text = "", Int_t size = 12, 
                         const char * color = 0, const char * font = "fixed",
                         EText3DType type = TImage::kPlain, const char * fore_file = 0) 
                        { DrawText(x,y,text,size,color,font,type,fore_file, (float) 0.0); }
   
   virtual void DrawText(Int_t /*x*/, Int_t /*y*/, const char * /*text*/, Int_t /*size*/, 
                         const char * /*color*/ , const char * /*font*/,
                         EText3DType /*type*/ , const char * /*fore_file*/, Float_t /*angle*/ ) { }

   // vector graphics
   virtual void BeginPaint(Bool_t /*fast*/ = kTRUE) {}
   virtual void EndPaint() {}
   virtual void DrawLine(UInt_t /*x1*/, UInt_t /*y1*/, UInt_t /*x2*/, UInt_t /*y2*/, 
                         const char * /*col*/ = "#000000", UInt_t /*thick*/ = 1) {}
   virtual void DrawDashLine(UInt_t /*x1*/, UInt_t /*y1*/, UInt_t /*x2*/, UInt_t /*y2*/, UInt_t /*nDash*/,
                             const char * /*pDash*/, const char * /*col*/ = "#000000", UInt_t /*thick*/ = 1) {}
   virtual void DrawBox(Int_t /*x1*/, Int_t /*y1*/, Int_t /*x2*/, Int_t /*y2*/, 
                         const char * /*col*/ = "#000000", UInt_t /*thick*/ = 1, Int_t /*mode*/ = 0) {}
   virtual void DrawRectangle(UInt_t /*x*/, UInt_t /*y*/, UInt_t /*w*/, UInt_t /*h*/, 
                              const char * /*col*/ = "#000000", UInt_t /*thick*/ = 1) {}
   virtual void FillRectangle(const char * /*col*/ = 0, Int_t /*x*/ = 0, Int_t /*y*/ = 0, 
                              UInt_t /*width*/ = 0, UInt_t /*height*/ = 0) {}
   virtual void DrawPolyLine(UInt_t /*nn*/, TPoint * /*xy*/, const char * /*col*/ = "#000000", 
                             UInt_t /*thick*/ = 1, TImage::ECoordMode /*mode*/ = kCoordModeOrigin) {}
   virtual void PutPixel(Int_t /*x*/, Int_t /*y*/, const char * /*col*/ = "#000000") {}
   virtual void PolyPoint(UInt_t /*npt*/, TPoint * /*ppt*/, const char * /*col*/ = "#000000", 
                          TImage::ECoordMode /*mode*/ = kCoordModeOrigin) {}
   virtual void DrawSegments(UInt_t /*nseg*/, Segment_t * /*seg*/, const char * /*col*/ = "#000000", UInt_t /*thick*/ = 1) {}
   virtual void FillPolygon(UInt_t /*npt*/, TPoint * /*ppt*/, const char * /*col*/ = "#000000",
                           const char * /*stipple*/ = 0, UInt_t /*w*/ = 16, UInt_t /*h*/ = 16) {}
   virtual void FillPolygon(UInt_t /*npt*/, TPoint * /*ppt*/, TImage * /*tile*/) {}
   virtual void CropPolygon(UInt_t /*npt*/, TPoint * /*ppt*/) {}
   virtual void DrawFillArea(UInt_t /*npt*/, TPoint * /*ppt*/, const char * /*col*/ = "#000000",
                           const char * /*stipple*/ = 0, UInt_t /*w*/ = 16, UInt_t /*h*/ = 16) {}
   virtual void DrawFillArea(UInt_t /*npt*/, TPoint * /*ppt*/, TImage * /*tile*/) {}
   virtual void FillSpans(UInt_t /*npt*/, TPoint * /*ppt*/, UInt_t * /*widths*/,  const char * /*col*/ = "#000000",
                         const char * /*stipple*/ = 0, UInt_t /*w*/ = 16, UInt_t /*h*/ = 16) {}
   virtual void FillSpans(UInt_t /*npt*/, TPoint * /*ppt*/, UInt_t * /*widths*/, TImage * /*tile*/) {}
   virtual void CropSpans(UInt_t /*npt*/, TPoint * /*ppt*/, UInt_t * /*widths*/) {}
   virtual void CopyArea(TImage * /*dst*/, Int_t /*xsrc*/, Int_t /*ysrc*/, UInt_t /*w*/, UInt_t /*h*/,
                         Int_t /*xdst*/ = 0, Int_t /*ydst*/ = 0, Int_t /*gfunc*/ = 3, EColorChan /*chan*/ = kAllChan) {}
   virtual void DrawCellArray(Int_t /*x1*/, Int_t /*y1*/, Int_t /*x2*/, Int_t /*y2*/, Int_t /*nx*/, Int_t /*ny*/, UInt_t * /*ic*/) {}

   virtual void FloodFill(Int_t /*x*/, Int_t /*y*/, const char * /*col*/, const char * /*min_col*/, const char * /*max_col*/ = 0) {}
   virtual void DrawCubeBezier(Int_t /*x1*/, Int_t /*y1*/, Int_t /*x2*/, Int_t /*y2*/, Int_t /*x3*/, Int_t /*y3*/, const char * /*col*/ = "#000000", UInt_t /*thick*/ = 1) {}
   virtual void DrawStraightEllips(Int_t /*x*/, Int_t /*y*/, Int_t /*rx*/, Int_t /*ry*/, const char * /*col*/ = "#000000", UInt_t /*thick*/ = 1) {}
   virtual void DrawCircle(Int_t /*x*/, Int_t /*y*/, Int_t /*r*/, const char * /*col*/ = "#000000", UInt_t /*thick*/ = 1) {}
   virtual void DrawEllips(Int_t /*x*/, Int_t /*y*/, Int_t /*rx*/, Int_t /*ry*/, Int_t /*angle*/, const char * /*col*/ = "#000000", UInt_t /*thick*/ = 1) {}
   virtual void DrawEllips2(Int_t /*x*/, Int_t /*y*/, Int_t /*rx*/, Int_t /*ry*/, Int_t /*angle*/, const char * /*col*/ = "#000000", UInt_t /*thick*/ = 1) {}
         
   virtual void SetEditable(Bool_t /*on*/ = kTRUE) {}
   virtual Bool_t IsEditable() const { return kFALSE; }

   
   // Utilities
   UInt_t GetWidth()  const;
   UInt_t GetHeight() const;
   Bool_t IsValid()   const;
   const QImage *GetImage() const { return fImage; }
   TImage *GetScaledImage() const { return fScaledImage; }

   TArrayL  *GetPixels(Int_t /*x*/= 0, Int_t /*y*/= 0, UInt_t /*w*/ = 0, UInt_t /*h*/ = 0) { return 0; }
   TArrayD  *GetArray(UInt_t w= 0, UInt_t h= 0, TImagePalette *palette = 0); // gWebImagePalette);
   Pixmap_t  GetPixmap();
   Pixmap_t  GetMask();
   UInt_t   *GetArgbArray() { return 0; }
   UInt_t   *GetScanline(UInt_t /*y*/) { return 0; }

   virtual void      GetImageBuffer(char ** /*buffer*/, int* /*size*/, EImageFileTypes /*type*/ = TImage::kPng) {}
   virtual Bool_t    SetImageBuffer(char ** /*buffer*/, EImageFileTypes /*type*/ = TImage::kPng) { return kFALSE; }
   virtual void      PaintImage(Drawable_t /*wid*/, Int_t /*x*/, Int_t /*y*/) { }
   virtual void      FromWindow(Drawable_t /*wid*/, Int_t /*x*/ = 0, Int_t /*y*/ = 0, UInt_t /*w*/ = 0, UInt_t /*h*/ = 0) {}
   virtual void      FromGLBuffer(UChar_t* /*buf*/, UInt_t /*w*/, UInt_t /*h*/) {}

   virtual Double_t  MaxValue()   const { return fMaxValue;   }   // max value in image
   virtual Double_t  MinValue()   const { return fMinValue;   }   // min value in image
   virtual UInt_t    ZoomOffX()   const { return fZoomOffX;   }   // X - offset for zooming
   virtual UInt_t    ZoomOffY()   const { return fZoomOffY;   }   // Y - offset for zooming
   virtual UInt_t    ZoomWidth()  const { return fZoomWidth;  }   // width of zoomed image
   virtual UInt_t    ZoomHeight() const { return fZoomHeight; }   // hight of zoomed image

   ClassDef(TQtImage,1)  // Image display class
};

#endif
