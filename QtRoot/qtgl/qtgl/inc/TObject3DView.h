// @(#)root/gtgl:$Name:  $:$Id: TObject3DView.h,v 1.7 2013/08/30 16:00:17 perev Exp $
// Author: Valery Fine      23/10/03

#ifndef ROOT_TObject3DView
#define ROOT_TObject3DView

/****************************************************************************
** TObject3DView
**
** Copyright (C) 2005 by Valeri Fine.  Brookhaven National Laboratory All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
**
*****************************************************************************/

#ifndef ROOT_TQObject
#include "TQObject.h"
#endif

#include "TObjectSet.h"
#include "TAttLine.h"
#include "TAttFill.h"
#include "TObject3DViewFactoryABC.h"
#include <map>

class TGeoVolume;
class TVolume;
class TVolumeView;
class TNode;
class TMarker3DBox;
class TPolyMarker3D;
class TPolyLine3D;
class TPolyLineShape;
class TGeoCompositeShape;

typedef  std::map<TObject *,TObject3DView *>  OBJECT_MAP;
class TObject3DView : public TObjectSet, public TAttLine, public TAttFill, public TQObject{
// Update mode
// Ligthing / No lighting
private:
   ULong_t  f3DViewID;
   ULong_t  f3DViewIDSelectable;
   ULong_t  f3DViewIDSelected;
   OBJECT_MAP *fMap;
   Int_t    fRefCounter;
   Int_t    fLevel;
   Int_t    fType;  // 0 - wired, 1 - solid
   Float_t  fTranslucentFactor;
   Bool_t   fIsShape;
   Bool_t   fIsSelectable;
   Bool_t   fIsSelected;
   Bool_t   fIsMapOwner;

   // to do. The base class methods below must be reimplemented
   virtual void Add(TDataSet* ) {}
   virtual void AddAt(TDataSet* , Int_t) {}
   virtual void AddAtAndExpand(TDataSet* , Int_t ) {}
   virtual void AddFirst(TDataSet*) {}
   virtual void AddLast(TDataSet* ) {}
   virtual void Remove(TDataSet* )  {}
   virtual TDataSet* RemoveAt(Int_t) {return 0; }
   virtual void Update(TDataSet*, UInt_t) {}
   virtual void Update() {}
   const OBJECT_MAP *Map() const { return fMap;}


protected:
    TObject3DViewFactoryABC  *fView3DFactory;
    TObject3DView();
    TObject3DView(TObject *root3DObject,std::map<TObject *,TObject3DView *> *volumeMap, TObject3DViewFactoryABC  *aFactory,  Int_t level=1,Int_t maxlevel=3);
    virtual void BeginModel();
    virtual void EndModel();
    virtual void AddChild(TObject3DView *child);

public:
    TObject3DView(TObject3DViewFactoryABC  *aFactory);
    TObject3DView(TObject *root3DObj, Option_t *depth, TObject3DViewFactoryABC  *aFactory);
    TObject3DView(TObject *root3DObj, TObject3DViewFactoryABC  *aFactory, Int_t thisLevel=1,Int_t maxlevel=3);
    virtual ~TObject3DView();
    Int_t Counter();
    virtual void Delete(Option_t *opt="");
    virtual void CompileViewLevel();
    virtual TObject3DView *CompileSelection();
    Int_t IncCounter(Int_t inc=1);
    Int_t DecCounter(Int_t dec=1);
    const TObject3DView *FindView(const TObject *node) const;
    const TObject       *FindObjectByView(const TObject3DView *view) const;
    TObject3DView *MakeMatrix(const Double_t *traslation=0, const Double_t *rotation=0, Bool_t isReflection=kFALSE);
    TObject3DView *AddNodeByDefinition(const TObject *descriptor);
    void   MakeVolumeView(Int_t maxlevel);
    Int_t IsSolid()      const { return fType;         }
    Int_t IsSelectable() const { return fIsSelectable; }
    Int_t IsSelected()   const { return fIsSelected;   }
    Int_t IsShape()      const { return fIsShape;      }
    
    void  MakeVolumeView(TGeoVolume  *volume,Int_t maxlevel=3);
    void  MakeVolumeView(TVolume     *volume,Int_t maxlevel=3);
    void  MakeVolumeView(TVolumeView *volume,Int_t maxlevel=3);
    void  MakeVolumeView(TNode       *node,  Int_t maxlevel=3);

    void  MakeVolumeView(TMarker3DBox   *node,  Int_t maxlevel=3);
    void  MakeVolumeView(TPolyMarker3D  *node,  Int_t maxlevel=3);
    void  MakeVolumeView(TPolyLine3D    *node,  Int_t maxlevel=3);
    void  MakeVolumeView(TPolyLineShape *node,  Int_t maxlevel=3);

    void  MakeShape(const TObject *shape);            //*SIGNAL*
    void  MakeCompositeShape(const TGeoCompositeShape *shape);            //*SIGNAL*
    ULong_t GetViewId(TObject3DViewFactoryABC::ERenderType type=TObject3DViewFactoryABC::kNormal) const;
    void  GetBoundBox(Double_t *min, Double_t *max) const;
    void  MarkModified(TObject *modefiedObject);
    void  MarkVisible(TObject *modefiedObject, Bool_t visibility= kTRUE);
    void  PushMatrix();
    void  PopMatrix();
    TObject3DViewFactoryABC *SetViewFactory(TObject3DViewFactoryABC *factory);
    void  SetShape(Bool_t yes=kTRUE)      { fIsShape      = yes; }
    void  SetSelectable(Bool_t yes=kTRUE) { fIsSelectable = yes; }
    void  SetSelected(Bool_t yes=kTRUE)   { fIsSelected   = yes; }
    void  SetType(Int_t type=1)           { fType=type;          }
    void  SetTranslucentFactor(Float_t factor = 0.5) { fTranslucentFactor = factor; }
    void  SetViewID(ULong_t id,TObject3DViewFactoryABC::ERenderType type=TObject3DViewFactoryABC::kNormal);
    Float_t TranslucentFactor() const { return fTranslucentFactor; }
    virtual void  Shunt(TDataSet *newParent);
    TObject3DViewFactoryABC *ViewFactory() const;

    ClassDef(TObject3DView,0)

//  protected:
   // Int_t ThisLevel()
    // Int_t TotalRenderingLevels();

#if 0
//________________________
void TViewer:: Add3Dview (TObjectView3D *o)
#endif

};

//___________________________________________________________________________
inline  Int_t TObject3DView::Counter()             { return fRefCounter; }
//___________________________________________________________________________
inline  Int_t TObject3DView::IncCounter(Int_t inc) { fRefCounter += inc; return Counter(); }
//___________________________________________________________________________
inline  Int_t TObject3DView::DecCounter(Int_t dec) { IncCounter(-dec);   return Counter(); }
//___________________________________________________________________________
inline  void  TObject3DView::SetViewID(ULong_t id,TObject3DViewFactoryABC::ERenderType type)
{ 
   switch (type) {
      case TObject3DViewFactoryABC::kSelectable:
        f3DViewIDSelectable = id; break;
      case TObject3DViewFactoryABC::kSelected:
        f3DViewIDSelected  = id; break;
     default:
        f3DViewID = id; break;
     };
}
//___________________________________________________________________________
inline  TObject3DViewFactoryABC *TObject3DView::ViewFactory() const {return fView3DFactory; }
//___________________________________________________________________________
inline  TObject3DViewFactoryABC *TObject3DView::SetViewFactory(TObject3DViewFactoryABC *factory)
{ TObject3DViewFactoryABC *s = fView3DFactory; fView3DFactory=factory;  return s; }
#endif

