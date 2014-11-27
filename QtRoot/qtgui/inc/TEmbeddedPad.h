// @(#)root/base:$Name:  $:$Id: TEmbeddedPad.h,v 1.8 2013/08/30 16:00:19 perev Exp $
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


#ifndef ROOT_TEmbeddedPad
#define ROOT_TEmbeddedPad

/////////////////////////////////////////////////////////////////////////////
//                                                                         //
// TEmbeddedPad                                                            //
//                                                                         //
// Reimplementation of the TPad class to create the stand-alone offscreen  //
// TPad  QPixmap (without any mother TCanvas)                              //
//                                                                         //
/////////////////////////////////////////////////////////////////////////////

#ifndef ROOT_TPad
#include "TPad.h"
#endif

class TImage;
class TVirtualPadPainter;

class TEmbeddedPad  : public TPad {

private:
   TEmbeddedPad(const TEmbeddedPad&):TPad() {;}
   void operator=(const TEmbeddedPad&){ }
   
protected:

   UInt_t        fPw;              //Width of the pad along X (pixels)
   UInt_t        fPh;              //Height of the pad along Y (pixels)
   Int_t         fFullPixmapID;    //The pixmap Id that conatins all childldren pads images
   TVirtualPadPainter *fPainter;   // Painter (ROOT > = 5.24 )

protected:

    // Re-implement TCanvas related methods
    // Protect the base class methods those are usefull for "interactive" mode only
    virtual Int_t    GetCanvasID() const;
    virtual Int_t    GetEvent()    const;
    virtual Int_t    GetEventX()   const;
    virtual Int_t    GetEventY()   const;
    virtual Color_t  GetHighLightColor()   const;
    virtual TObject *GetSelected()        const;
    virtual TVirtualPad *GetSelectedPad() const;
    virtual TVirtualPad *GetPadSave()     const;
    virtual void SetBatch(Bool_t batch);
    virtual void SetCrosshair(Int_t crhair);
    virtual void SetCanvasSize(UInt_t ww, UInt_t wh);
    virtual void SetCursor(ECursor cursor);
    virtual void SetDoubleBuffer(Int_t mode);
    virtual void SetSelected(TObject *obj);
    virtual Bool_t IsRetained() const;
    virtual Bool_t OpaqueMoving()         const;
    virtual Bool_t OpaqueResizing()       const;
    virtual void RecursiveRemove(TObject *obj);
    virtual TVirtualViewer3D *GetViewer3D(Option_t *type);
    virtual Int_t GetGLDevice();
    virtual TVirtualPadPainter *GetPainter();

public:

    TEmbeddedPad(TPad *mother=0);
    TEmbeddedPad(const char *name, const char *title, 
                 UInt_t width=200, UInt_t height=200,
                 Color_t color=-1, Short_t bordersize=-1, Short_t bordermode=0);
    virtual ~TEmbeddedPad();
    virtual void   Close(Option_t *option="");

    virtual ULong_t GetHandle() const;
    virtual Int_t   GetHandle( Int_t id, double m11, double m12, double m21, double m22, double dx, double dy) const;
    virtual ULong_t GetHandle( ULong_t handle, double m11, double m12, double m21, double m22, double dx, double dy) const;
    virtual Int_t   GetHandleRotate( Int_t id, double angle) const;
    virtual ULong_t GetHandleRotate( ULong_t handle,  double angle) const;
	 virtual TImage *CreateImage( Int_t x=0, Int_t y=0, UInt_t w=0, UInt_t h=0);

    virtual UInt_t GetWh()      const;
    virtual UInt_t GetWw()      const;
    virtual Bool_t IsBatch()    const;

    virtual TObject *Pick(Int_t px, Int_t py);
    virtual TPad    *Pick(Int_t px, Int_t py, TObjLink *&pickobj);

    virtual void  Print(const char *filenam, Option_t *option);
    virtual void  Print(Option_t *option="") const;
    virtual void  ResizePad(UInt_t width, UInt_t height);
    virtual void  ResizePad(Option_t *option="");
    virtual void  Update();
    virtual void  Flush();
    virtual Int_t HasChildren() const;

    ClassDef(TEmbeddedPad,0)  //stand-alone TPad  class to create off-screen QPixmap objects
};

#endif
