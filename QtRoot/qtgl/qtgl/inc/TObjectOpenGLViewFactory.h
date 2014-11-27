// @(#)root/gtgl:$Name:  $:$Id: TObjectOpenGLViewFactory.h,v 1.6 2013/08/30 16:00:17 perev Exp $
// Author: Valery Fine      24/04/05

#ifndef ROOT_TObjectOpenGLViewFactory
#define ROOT_TObjectOpenGLViewFactory

/****************************************************************************
**
** TObjectOpenGLViewFactory

** An unterface of the class visitor to convert the ROOT 3D objets into 
** the concrete "viewer" representation like OpenGL, OpenInventor, x3d etc
**
** Copyright (C) 2005 by Valeri Fine.  Brookhaven National Laboratory All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
**
*****************************************************************************/

#include "TObject3DViewFactory.h"
#include "TBoundBoxEstimator.h"

class TShape3DPolygonView;


class TObjectOpenGLViewFactory : public TObject3DViewFactory {
protected:
  TBoundBoxEstimator    fBoundBox;
  virtual TObject3DView *MakeShape(TShape3DPolygonView &shapeView, const Float_t *rgba);
public:
   TObjectOpenGLViewFactory();
   virtual ~TObjectOpenGLViewFactory();
   virtual void AddNormal(TObject3DView *, const Double_t *normal);
   virtual void AddChild(TObject3DView * parent, TObject3DView *child);
   virtual TObject3DView *BeginModel(TObject3DView *);
   virtual TObject3DView *EndModel();
   virtual void           PushMatrix();
   virtual void           PushMatrixEstimate(const Double_t *traslation,const Double_t *rotation);
   virtual void           PopMatrix();
   virtual void           PopMatrixEstimate();
   virtual TObject3DView *CreateCoinNode(const TObject *){ return 0; }
   virtual TObject3DView *CreateMatrix(const Double_t *traslation, const Double_t *rotation, Bool_t isReflection);
   virtual TObject3DView *CreateNormal(const Double_t *normal);
   virtual TObject3DView *CreatePosition(UInt_t Id);
   virtual TObject3DView *CompileSelection(TObject3DView *view);
   virtual void           CompileViewLevel(TObject3DView *,ERenderType type=kNormal);
   virtual void           GetBoundBox(Double_t *min, Double_t *max) const;
   virtual ULong_t GetViewerId(TObject3DView *) const;
   virtual Bool_t NeedCompilation()             const;
   virtual void   Release(TObject3DView *);
};
#endif
