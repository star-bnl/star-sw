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
#include <stdio.h>
#include <stdlib.h>
    
#include "TObject3DView.h"
#include "TROOT.h"
#include "TColor.h"
#include "TVolume.h"
#include "TVolumeView.h"
#include "TVolumePosition.h"
#include "TDataSetIter.h"
#include "TGeoManager.h"
#include "TGeoVolume.h"
#include "TGeoNode.h"
#include "TGeoMatrix.h"
#include "TGeoCompositeShape.h"
#include "TGeoBoolNode.h"
#include "TNode.h"
#include "TPolyMarker3D.h"
#include "TPolyLine3D.h"
#include "TMarker3DBox.h"
#include "TPolyLineShape.h"

#include "TStopwatch.h"
// Update mode
// Ligthing / No lighting

#define GLLIST 
// -- This slow down the rendering vf 31.05.2005
/////////////////////////////////////////////////////////////////////////////////////////////
//
//  class TObject3DView: Convert ROOT3D object to the generic 3D view
//
// TObject (3d) --> View(TObject, opt)
//                   |
//                   +--- BeginModel ( use the factory)
//                   |
//                   +--- Shape (use the factory)
//                   |
//                   +--- Position . . . Position . . . Postion 
//                   |       |
//                   |       +-----CreateMatrix (use the factory)
//                   |       |
//                   |       +-----View(TObject, map)
//                   |               |
//                   |               +-------- Shape 
//                   |               |
//                   |               +-------- Position  . . .  Position . . .  Position 
//                   |                             
//                   +--- EndModel (use the factory)
//
/////////////////////////////////////////////////////////////////////////////////////////////
 
ClassImp(TObject3DView)

// #define STARONLINE

//_____________________________________________________________________________
TObject3DView::TObject3DView()
: TObjectSet(0,kFALSE),f3DViewID(0),f3DViewIDSelectable(0),f3DViewIDSelected(0)
      , fMap(0),fRefCounter(0), fLevel(0),fType(1)
      ,fTranslucentFactor(0.5),fIsShape(kFALSE),fIsSelectable(kFALSE),fIsSelected(kFALSE),fIsMapOwner(kFALSE)
      ,fView3DFactory(0)
{
   // Ctor
}

//_____________________________________________________________________________
TObject3DView::TObject3DView(TObject3DViewFactoryABC  *aFactory)
: TObjectSet(0,kFALSE),f3DViewID(0),f3DViewIDSelectable(0),f3DViewIDSelected(0)
      ,fMap(0), fRefCounter(0), fLevel(0),fType(1)
      ,fTranslucentFactor(0.5),fIsShape(kFALSE),fIsSelectable(kFALSE),fIsSelected(kFALSE),fIsMapOwner(kFALSE)
      ,fView3DFactory(aFactory)
{
   // Ctor
}

//_____________________________________________________________________________
TObject3DView::TObject3DView(TObject *root3DObj, Option_t *depth, TObject3DViewFactoryABC  *aFactory)
: TObjectSet(root3DObj,kFALSE),f3DViewID(0),f3DViewIDSelectable(0),f3DViewIDSelected(0)
      , fMap(0),fRefCounter(0), fLevel(1),fType(1)
      ,fTranslucentFactor(0.5),fIsShape(kFALSE),fIsSelectable(kFALSE),fIsSelected(kFALSE),fIsMapOwner(kFALSE)
      ,fView3DFactory(aFactory)
{
   Int_t maxlevel = 3;
   SetName(root3DObj->GetName());
   if (depth && depth[0]) {
       maxlevel = atoi(depth);
   }
   if (maxlevel == 0) maxlevel = 5;
   fMap = new OBJECT_MAP; fIsMapOwner = kTRUE;
   BeginModel();
   // fprintf(stderr,"--------------START-- Top --Class:  %s --Name: %s --------\n",root3DObj->ClassName(),root3DObj->GetName());
   MakeVolumeView(maxlevel);

   EndModel();
#ifdef GLLIST
   CompileViewLevel();
#endif
  //  ls(0);
}
//_____________________________________________________________________________
TObject3DView::TObject3DView(TObject *root3DObj, TObject3DViewFactoryABC  *aFactory, Int_t thisLevel,Int_t maxlevel)
: TObjectSet(root3DObj,kFALSE),f3DViewID(0),f3DViewIDSelectable(0),f3DViewIDSelected(0)
      ,fMap(0),fRefCounter(0), fLevel(thisLevel),fType(1)
      ,fTranslucentFactor(0.5),fIsShape(kFALSE),fIsSelectable(kFALSE),fIsSelected(kFALSE),fIsMapOwner(kFALSE)
      ,fView3DFactory(aFactory)
{
   maxlevel = 6;
   SetName(root3DObj->GetName());
   fMap = new OBJECT_MAP; fIsMapOwner = kTRUE;
   BeginModel();
   // fprintf(stderr,"--------------START--   --Class:  %s --Name: %s --------\n",root3DObj->ClassName(),root3DObj->GetName());
   MakeVolumeView(maxlevel);

   EndModel();
#ifdef GLLIST
   CompileViewLevel();
#endif
  // ls(0);
}
//_____________________________________________________________________________
TObject3DView::TObject3DView(TObject *root3DObj,std::map<TObject *,TObject3DView *> *volumeMap,  TObject3DViewFactoryABC  *aFactory, Int_t level,Int_t maxlevel)
:  TObjectSet(root3DObj,kFALSE), f3DViewID(0),f3DViewIDSelectable(0),f3DViewIDSelected(0)
      ,fMap(volumeMap),fRefCounter(0), fLevel(level),fType(1)
      ,fTranslucentFactor(0.5),fIsShape(kFALSE),fIsSelectable(kFALSE),fIsSelected(kFALSE),fIsMapOwner(kFALSE)
      ,fView3DFactory(aFactory)
{
  if (fMap) {
      typedef std::pair <TObject *, TObject3DView *> VOLUME_PAIR;
      fMap->insert(VOLUME_PAIR(root3DObj,this));
   }
   if (level == 2) {
      // Make the second level object "selectable"
      SetSelectable(); 
     //  printf("set selectable %s %p\n",root3DObj->GetName(), this);
   }
   MakeVolumeView(maxlevel);
#ifdef  GLLIST   
   CompileViewLevel();
#endif   
}
//_____________________________________________________________________________
TObject3DView::~TObject3DView()
{ 
	//std::cout << "~TObject3DView";
   if (fView3DFactory) 
       fView3DFactory->Release(this);

   Delete();
   if (fIsMapOwner)  { delete fMap; fMap = 0; }
}

//______________________________________________________________________________
void TObject3DView::AddChild(TObject3DView *child)
{
  if (fView3DFactory) {
       fView3DFactory->AddChild(this,child); 
  }
  TDataSet::AddLast(child);
}

//______________________________________________________________________________
void TObject3DView::Delete(Option_t *opt)
{
//
// Delete - deletes the list of the TDataSet objects and all "Structural Members"
//          as well
//          This method doesn't affect the "Associated Members"
//
  if(opt){/*unused*/}

//      Delete list of the TDataSet
  TSeqCollection     *thisList = GetCollection();
  if (!thisList) return;
  fList = 0;
  TIter next(thisList);
  TDataSet *son = 0;
  //  Delete the "Structural Members" of this TDataSet only
  while ((son = (TDataSet *)next())) {
    if ( (!son->TObject::IsOnHeap()) || (this != son->TDataSet::GetParent()) ) continue;
    // mark the object is deleted from the TDataSet dtor or Delete method
    son->TDataSet::SetParent(0); 
    if (son->TDataSet::Last()) { son->Delete(); }
    son->TObject::SetBit(kCanDelete);
    delete son;
  }
  //  Cleare list
  thisList->Clear("nodelete");
  delete thisList;
}

//_____________________________________________________________________________
void TObject3DView::BeginModel()
{
   TObject3DView *beginView = 0; // new TObject3DView(0,fMap,fLevel+1);
   if (fView3DFactory) {
      beginView = fView3DFactory->BeginModel(this);
   }
   if (beginView) {
      TString nodeName = GetName(); nodeName += "_Begin";
      beginView->SetName((const char *)nodeName);
      AddChild(beginView);
#ifndef  GLLIST   
      SetViewID(beginView->GetViewId());
#endif
      beginView->IncCounter();
   }
}
//_____________________________________________________________________________
void TObject3DView::EndModel()
{
#ifdef GLLIST
   TObject3DView *endView = 0; // new TObject3DView(0,fMap,fLevel+1);
   if (fView3DFactory) {
      endView = fView3DFactory->EndModel();
   }
   if (endView) {
      TString nodeName = GetName(); nodeName += "_End";
      endView->SetName((const char *)nodeName);
      AddChild(endView);
      endView->IncCounter();
   }
#else
   if (fView3DFactory)
      fView3DFactory->EndModel();
#endif
}
//_____________________________________________________________________________
TObject3DView *TObject3DView::CompileSelection()
{
     // Ask factory to create the "highlight selection image"
     // Return the top level object of the view
     TObject3DView *top = 0;
     if (fView3DFactory && fView3DFactory->NeedCompilation() ) {
       top = fView3DFactory->CompileSelection(this);
     }
     return top;
}
//_____________________________________________________________________________
void TObject3DView::CompileViewLevel()
{ 
    // Compile the 3D object if any
   if (fView3DFactory && fView3DFactory->NeedCompilation() ) {
      fView3DFactory->CompileViewLevel(this);
#ifdef EXTRASELECT      
      if (fLevel <= 2)  {
         fprintf(stderr,"TObject3DView::CompileViewLevel this %p object = %p name=%s\n", this, GetObject(), GetName());
         fView3DFactory->CompileViewLevel(this,TObject3DViewFactoryABC::kSelectable);
      }
#endif
   }
}
//_____________________________________________________________________________
void  TObject3DView::GetBoundBox(Double_t *min, Double_t *max) const
{
   if (fView3DFactory) 
      fView3DFactory->GetBoundBox(min, max);
}

//_____________________________________________________________________________
void TObject3DView::MakeVolumeView(Int_t maxlevel)
{ 
   if (fObj) {
      SetTitle(fObj->GetTitle());
      if (!fMap) {
         fprintf(stderr," TObject3DView::MakeVolumeView no map for the 3Dobject %s %s\n", GetName(), GetTitle());
         assert (fMap);// fMap = new std::map<TObject *,TObject3DView *>;
      }
      if (fObj->InheritsFrom(TGeoVolume::Class())) 
         MakeVolumeView((TGeoVolume *)fObj,gGeoManager->GetVisLevel()+1);
      else if (fObj->InheritsFrom(TVolume::Class()))
         MakeVolumeView((TVolume *)fObj,maxlevel);
      else if (fObj->InheritsFrom(TNode::Class()))
         MakeVolumeView((TNode *)fObj,maxlevel);
      else if (fObj->InheritsFrom(TVolumeView::Class()))
         MakeVolumeView((TVolumeView *)fObj,maxlevel);
      else if (fObj->InheritsFrom(TMarker3DBox ::Class()))
         MakeVolumeView((TMarker3DBox  *)fObj,maxlevel);
      else if (fObj->InheritsFrom(TPolyMarker3D::Class())) {
         SetType(0); // It is wired object
         MakeVolumeView((TPolyMarker3D *)fObj,maxlevel);
         ((TObject3DView *)Last())->SetSelectable(); 
         ((TObject3DView *)Last())->SetObject(GetObject(),kFALSE);
      } else if (fObj->InheritsFrom(TPolyLine3D::Class())) {
         SetType(0); // It is wired object
         MakeVolumeView((TPolyLine3D *)fObj,maxlevel);
         ((TObject3DView *)Last())->SetSelectable(); 
         ((TObject3DView *)Last())->SetObject(GetObject(),kFALSE);
      } else if (fObj->InheritsFrom(TPolyLineShape::Class())) {
         SetType(0); // It is wired object
         MakeVolumeView((TPolyLineShape *)fObj,maxlevel);
         ((TObject3DView *)Last())->SetSelectable(); 
         ((TObject3DView *)Last())->SetObject(GetObject(),kFALSE);
#ifdef TOBEDONEYET
      else if (fObj->InheritsFrom(TShape::Class()))
         MakeVolumeView((TShape  *)fObj,maxlevel);
      else if (fObj->InheritsFrom(TParticle::Class()))
         MakeVolumeView((TParticle *)fObj,maxlevel);
      else if (fObj->InheritsFrom(TPrimary::Class()))
         MakeVolumeView((TPrimary *)fObj,maxlevel);
      else if (fObj->InheritsFrom(TGeoOverlap::Class()))
         MakeVolumeView((TGeoOverlap *)fObj,maxlevel);
#endif       
      } else {
         fprintf(stderr," ** ERROR ** TObject3DView::MakeVolumeView Can not render the  %s class objects yet\n", fObj->ClassName());
         assert(0);
      }
   }
}
//_____________________________________________________________________________
TObject3DView *TObject3DView::MakeMatrix(const Double_t *traslation, const Double_t *rotation, Bool_t isReflection)
{
   TObject3DView *matrixNode = 0; // new TObject3DView(0,fMap,fLevel+1);
   
   if (fView3DFactory) 
         matrixNode =  fView3DFactory->CreatePosition(0);
   assert (matrixNode);
   TString nodeName = "MatrixNode_"; nodeName += GetName(); ;
   matrixNode->SetName((const char *)nodeName);

   if (fView3DFactory) {
       TObject3DView *matrixView = fView3DFactory->CreateMatrix(traslation, rotation, isReflection);
       if (matrixView) {
          matrixView->SetTitle("transformation");
          matrixView->IncCounter();
          matrixNode->AddChild(matrixView);
       }
         //matrixView->SetName("MatrixView");
   }
   return matrixNode;
}

//_____________________________________________________________________________
TObject3DView *TObject3DView::AddNodeByDefinition(const TObject *descriptor)
{
   // THis method allows mixins ROOT and Coin3D node if Coin3D factor is present
   // Add the new Coin node by its defininition
   // Non-coin factory would return zero and no insancts for the "descriptor"
   // is to be added to the TObject3DView

   TObject3DView *coinNode = 0; // new TObject3DView(0,fMap,fLevel+1);
   
   if (fView3DFactory && descriptor) {
         coinNode =  fView3DFactory->CreateCoinNode(descriptor);
         if (coinNode) AddChild(coinNode);
   }
   return coinNode;
}


//_____________________________________________________________________________
void  TObject3DView::MakeShape(const TObject *shape)
{
    TObject3DView *shapeView = 0; // new TObject3DView(0,fMap,fLevel+1);
    if (fView3DFactory) {
        // Emit signal to allow the final touch up

        Float_t rgba[4] = {0, 0, 0, 1.0} ;
        Style_t style = GetFillStyle();
        // 4000 - means 100% transparent
        // 4100 - means 100% opaque
        // OpenGL = 0 - 100% opaque
        // OpenGL = 1 - 100% transparent
        if (4000 <= style && style <= 4100) 
           rgba[3] = (style-4000)/float(100.);

        // Multiply the original transparency to the translucent factor
        //rgba[3]  = TMath:: Min(1.0f,TMath::Max(0.0f,rgba[3]));        
        rgba[3] *= TMath::Max(0.2f,TMath::Min(1.0f,fTranslucentFactor));
        rgba[3] = 1 - rgba[3];
        TColor *c = gROOT->GetColor(GetFillColor());
        if (c->GetNumber() == kBlack) c = gROOT->GetColor(GetLineColor());
        if (c->GetNumber() == kBlack) c = 0;
        if (!c) c = gROOT->GetColor(17);
        c->GetRGB(rgba[0],rgba[1],rgba[2]);
       // if (!strstr(shape->ClassName(),"Line3D") )
       // fprintf(stderr, " TObject3DView::MakeShape %s %d %f %f %f \n", shape->GetName(), c->GetNumber(),rgba[0],rgba[1],rgba[2]);
        shapeView = fView3DFactory->CreateShape(shape,rgba);
   }
   if (shapeView) {
      AddChild(shapeView);
      shapeView->IncCounter();
   }
}
//_____________________________________________________________________________
void TObject3DView::MakeCompositeShape(const TGeoCompositeShape *top)
{
   // Create the composite shape as a trivial superposition of two shapes
   // No bool opeation is provided yet.

   if (!top) return ;
   // fprintf(stderr,"1. TObject3DView::MakeCompositeShape %s \"%s\"\n",  top->GetName(),top->GetTitle());
   //   top->ls();

   // if (!top->IsVolumeMulti())    
   TString title = top->GetTitle();
   if (title.IsNull() ) title = top->GetName();

   TGeoBoolNode *shapeNode = top->GetBoolNode();
   if (shapeNode) {
      TGeoMatrix *geoMatrice[2] =  { shapeNode->GetLeftMatrix(), shapeNode->GetRightMatrix()};
      TGeoShape  *geoShape[2]   =  { shapeNode->GetLeftShape(),  shapeNode->GetRightShape() };
              
      for (Int_t i = 0;  i < 2  ; i++) {
         if (!geoShape[i]) continue;
         //  Add transformation
         TGeoMatrix *geoMatrix       = geoMatrice[i];
         const Double_t   *trans     = geoMatrix->GetTranslation();
         const Double_t   *rotation  = geoMatrix->IsIdentity() ? 0 : geoMatrix->GetRotationMatrix();
         Bool_t isReflection  = geoMatrix->IsReflection();
         Double_t mx[9];
         if (rotation) {
            mx[0] = rotation[0]; mx[1] = rotation[3]; mx[2] = rotation[6];
            mx[3] = rotation[1]; mx[4] = rotation[4]; mx[5] = rotation[7];
            mx[6] = rotation[2]; mx[7] = rotation[5]; mx[8] = rotation[8];
            rotation = &mx[0];
         }
         TObject3DView *position =  MakeMatrix(trans,rotation,isReflection);
         position->AddNodeByDefinition(geoMatrix);
         if ( geoShape[i]->IsComposite() )  position->MakeCompositeShape((TGeoCompositeShape *)geoShape[i]);
         else position->MakeShape(geoShape[i]);
         AddChild(position);
         // ls(0);
         position->CompileViewLevel();
      }  
   }
}

//_____________________________________________________________________________
void TObject3DView::MakeVolumeView(TGeoVolume *top, Int_t maxlevel)
{
   if (!top) return ;
   //fprintf(stderr,"1. TObject3DView::MakeVolumeView %s Visible %d maxlevel=%d, currentLevel=%d\n",  top->GetName(),top->IsVisible(),maxlevel,fLevel);
   //   top->ls();
   TObjArray *nodes = 0;

   // if (!top->IsVolumeMulti()) 
   if ( top->IsVisible() && strcmp(top->GetName(),"TOP") ) {
      // fprintf(stderr,"2. TObject3DView::MakeVolumeView %s Visible %d \n",  top->GetName(),top->IsVisible());
      SetLineColor(top->GetLineColor()); SetLineStyle(top->GetLineStyle());
      SetLineWidth(top->GetLineWidth()); SetFillColor(top->GetLineColor());
      if (top->GetTransparency()) SetFillStyle( 4100-top->GetTransparency() );
      TGeoShape *thisShape = top->GetShape();
      if (thisShape) {
         if (thisShape->IsComposite()) MakeCompositeShape((TGeoCompositeShape *)thisShape);
         else MakeShape(thisShape);
      }
   }
   TString title = top->GetTitle();
   if (title.IsNull() ) title = top->GetName();


   if ( (maxlevel > fLevel) && (nodes = top->GetNodes()) ) {
      TGeoNode *geoNode = 0;
      TGeoVolume *geoVolume = 0;
      Int_t nNodes = top->GetNdaughters();
      for (Int_t i = 0; (i < nNodes)  ; i++) {
         geoNode   = top->GetNode(i);
         //  Add transformation
         TGeoMatrix *geoMatrix       = geoNode->GetMatrix();
         const Double_t   *trans     = geoMatrix->GetTranslation();
         const Double_t   *rotation  = geoMatrix->IsIdentity() ? 0 : geoMatrix->GetRotationMatrix();
         Bool_t isReflection  = geoMatrix->IsReflection();
         Double_t mx[9];
         if (rotation) {
            mx[0] = rotation[0]; mx[1] = rotation[3]; mx[2] = rotation[6];
            mx[3] = rotation[1]; mx[4] = rotation[4]; mx[5] = rotation[7];
            mx[6] = rotation[2]; mx[7] = rotation[5]; mx[8] = rotation[8];
            rotation = &mx[0];
         }
         TObject3DView *position =  MakeMatrix(trans,rotation,isReflection);
         position->AddNodeByDefinition(geoMatrix);

         geoVolume = geoNode ? geoNode->GetVolume(): 0;
         if (geoVolume) {
            TObject3DView *nextVolume = 0;
#ifdef GLLIST
            std::map <TObject *,TObject3DView *> :: const_iterator volumeFinder;
            volumeFinder = fMap->find(geoVolume);
            if (volumeFinder != fMap->end( ) ) 
               nextVolume = volumeFinder->second;

            if (!nextVolume) {
#  if ROOT_VERSION_CODE < ROOT_VERSION(5,16,0)
               gGeoManager->CdDown(i);
#  endif
               nextVolume= new TObject3DView(geoVolume,fMap,fView3DFactory,fLevel+1,maxlevel);
#  if ROOT_VERSION_CODE < ROOT_VERSION(5,16,0)
               gGeoManager->CdUp();
#  endif
            } 
#else
#  if ROOT_VERSION_CODE < ROOT_VERSION(5,16,0)
               gGeoManager->CdDown(i);
#  endif
               nextVolume= new TObject3DView(geoVolume,fMap,fView3DFactory,fLevel+1,maxlevel);
#  if ROOT_VERSION_CODE < ROOT_VERSION(5,16,0)
               gGeoManager->CdUp();
#  endif
#endif
           position->AddChild(nextVolume);
            AddChild(position);
            nextVolume->IncCounter();
            position->CompileViewLevel();
   }  }  }
}
//_____________________________________________________________________________
void TObject3DView::MakeVolumeView(TVolume *top, Int_t maxlevel)
{
   if (!top || ( top->GetVisibility() == TVolume::kNoneVisible)) return;

   // printf("TObject3DView::MakeVolumeView  name=%s  title=%s \n",GetName(), GetTitle());
   if ( !( top->GetVisibility() & TVolume::kThisUnvisible) ) {
      TShape *shape = top->GetShape();
      if (shape) {
         SetLineColor(shape->GetLineColor()); SetLineStyle(shape->GetLineStyle());
         SetLineWidth(shape->GetLineWidth()); SetFillColor(shape->GetLineColor());
         SetFillStyle(shape->GetFillStyle() );
         MakeShape(shape);
         // printf("TObject3DView::MakeVolumeView  colors line = %d fill=%d \n",GetLineColor(), GetFillColor());
     }
   }

   const TList *nodes = 0;
   if ( (maxlevel > fLevel) && (nodes = top->GetListOfPositions()) ) {
      TVolumePosition *node = 0;
      TVolume *geoVolume = 0;
      TIter nextPosition(nodes);
      while ( (node = (TVolumePosition*) nextPosition())  ) {
         geoVolume  = node->GetNode();
         //  Add transformation 
         TRotMatrix *matrix         = node->GetMatrix();
         const Double_t   *trans    = node->GetXYZ();
         const Double_t   *rotation = 0;
         Bool_t isReflection        = kFALSE;
         if (matrix) {
            rotation =  matrix->GetMatrix();
            isReflection = matrix->IsReflection();
         }
         TObject3DView *position = MakeMatrix(trans,rotation, isReflection);
         position->AddNodeByDefinition(matrix);

         if (geoVolume) {
            TObject3DView *nextVolume = 0;
#ifdef GLLIST
            std::map <TObject *,TObject3DView *> :: const_iterator volumeFinder;
            volumeFinder = fMap->find(geoVolume);
            if (volumeFinder != fMap->end( ) ) 
               nextVolume = volumeFinder->second;

            if (!nextVolume) {
               nextVolume= new TObject3DView(geoVolume,fMap,fView3DFactory,fLevel+1,maxlevel);
            }
#else
            nextVolume= new TObject3DView(geoVolume,fMap,fView3DFactory,fLevel+1,maxlevel);
#endif
            position->AddChild(nextVolume);
            AddChild(position);
            if (nextVolume) nextVolume->IncCounter();
            position->CompileViewLevel();
   }  }  }
}
//_____________________________________________________________________________
void TObject3DView::MakeVolumeView(TVolumeView *top, Int_t maxlevel)
{
   if (!top || ( top->GetVisibility() == TVolume::kNoneVisible)) return;

   if ( !( top->GetVisibility() & TVolume::kThisUnvisible) ) 
   {
      TVolume *thisNode  = 0;
      TVolumePosition *position = top->GetPosition();

      // UpdatePosition does change the current matrix and it MUST be called FIRST !!!
      if (position) {
         thisNode  = position->GetNode();
         position->UpdatePosition("");
      }

//    if (level >= iFirst) 
      {
          TShape *shape = top->GetShape();
          if ( shape || ( thisNode && thisNode->GetShape()) ) {
             SetLineColor(top->GetNode()->GetLineColor()); SetLineStyle(top->GetNode()->GetLineStyle());
             SetLineWidth(top->GetNode()->GetLineWidth()); SetFillColor(top->GetNode()->GetLineColor());
             SetFillStyle(top->GetNode()->GetFillStyle() );
          }
          if (shape) MakeShape(shape);
          if (thisNode) {
             shape = thisNode->GetShape();
             if(shape)  {
#ifdef  STARONLINE
                 shape->Paint("MakeShape");
                 if ( shape->GetVisibility() ) {
                    SetLineColor(shape->GetLineColor()); SetLineStyle(shape->GetLineStyle());
                    SetLineWidth(shape->GetLineWidth()); SetFillColor(shape->GetLineColor());
                    SetFillStyle(shape->GetFillStyle() );
#endif
                    MakeShape(shape);
#ifdef  STARONLINE
                 }
#endif
             }
         }
      }
   }

   // fprintf(stderr," MakeVolumeView Shape maxlevel %d level %d \n", maxlevel , fLevel );
   if ( (maxlevel > fLevel) ) {
      TVolumeView *geoVolume = 0;
      TDataSetIter nextVolume(top);
      while ( (geoVolume = (TVolumeView*) nextVolume())  ) {
         TVolumePosition *node = geoVolume->GetPosition();
         // geoVolume  = node->GetNode();
         //  Add transformation 
         
         TRotMatrix      *matrix   = node->GetMatrix();
         const Double_t  *trans    = node->GetXYZ();
         const Double_t  *rotation = 0;
         Bool_t isReflection       = kFALSE;
         if (matrix) {
            rotation =    matrix->GetMatrix();
            isReflection = matrix->IsReflection();
         }
 
         TObject3DView *position = MakeMatrix(trans,rotation, isReflection);
         position->AddNodeByDefinition(matrix);

         TObject3DView *nextVolume = 0;
#ifdef GLLIST
         std::map <TObject *,TObject3DView *> :: const_iterator volumeFinder;
         volumeFinder = fMap->find(geoVolume);
         if (volumeFinder != fMap->end( ) ) 
            nextVolume = volumeFinder->second;

         if (!nextVolume) {
            nextVolume= new TObject3DView(geoVolume,fMap,fView3DFactory,fLevel+1,maxlevel);
         } else {
            Error("MakeVolumeView","Each TVolumeView must be unique");
            assert(0);
         }
#else 
         nextVolume= new TObject3DView(geoVolume,fMap,fView3DFactory,fLevel+1,maxlevel);
#endif                  
         position->AddChild(nextVolume);
         AddChild(position);
         nextVolume->IncCounter();
         position->CompileViewLevel();
   }  }
}
//_____________________________________________________________________________
void TObject3DView::MakeVolumeView(TNode *top, Int_t maxlevel)
{
   // printf("TObject3DView::MakeVolumeView(TNode *top = %p)\n", top);
   if (!top) return;

   switch ( top->GetVisibility()) {
      case 1: case 3: case -2:  
         {
            SetLineColor(top->GetLineColor()); SetLineStyle(top->GetLineStyle());
            SetLineWidth(top->GetLineWidth()); SetFillColor(top->GetLineColor());
            SetFillStyle( top->GetFillStyle() );
            MakeShape(top->GetShape());
         }
   };

   if ( (maxlevel > fLevel) ) {
      switch ( top->GetVisibility()) {
        case 0: case 1: case 2: case 3: case -3: case -4: 
           {
              TNode *node = 0;
              TIter nextVolume(top->GetListOfNodes());
              while ( (node = (TNode*) nextVolume())  ) {
                 //  Add transformation 
                 TRotMatrix      *matrix    = node->GetMatrix();
                 const Double_t  trans[3]  ={ node->GetX(),node->GetY(),node->GetZ()};
                 const Double_t  *rotation = 0;
                 Bool_t isReflection       = kFALSE;
                 if (matrix) {
                    rotation =  matrix->GetMatrix();
                    isReflection = matrix->IsReflection();
                 }
                 
                 TObject3DView *position = MakeMatrix(trans,rotation, isReflection);
                 position->AddNodeByDefinition(matrix);

 
                 TObject3DView *nextVolume = 0;

                 OBJECT_MAP::const_iterator volumeFinder;
                 volumeFinder = fMap->find(node);
                 if (volumeFinder != fMap->end( ) ) 
                    nextVolume = volumeFinder->second;

                 if (!nextVolume) {
                    nextVolume= new TObject3DView(node,fMap,fView3DFactory,fLevel+1,maxlevel);
                 }
          nextVolume->SetName("NextVolume-child");
          position->AddChild(nextVolume);
          AddChild(position);
          nextVolume->IncCounter();
          position->CompileViewLevel();
     }  }  };  }
}
//_____________________________________________________________________________
static void PrintToDoMessage(TObject *obj, Int_t maxlevel)
{
   fprintf(stderr," TObject3DView can no render %s class woth %d levels\n"
      , obj->ClassName(),maxlevel);
}
//_____________________________________________________________________________
void TObject3DView::MakeVolumeView(TMarker3DBox  *obj,Int_t maxlevel)
{ PrintToDoMessage(obj,maxlevel);}
//_____________________________________________________________________________
void TObject3DView::MakeVolumeView(TPolyMarker3D *polymarker,Int_t maxlevel)
{ 
   if (polymarker && maxlevel > 0 ) {
      SetLineColor(polymarker->GetMarkerColor());         SetLineStyle(polymarker->GetMarkerStyle());
      SetLineWidth(Width_t(polymarker->GetMarkerSize())); SetFillColor(polymarker->GetMarkerColor());
      MakeShape(polymarker);
   }
}
//_____________________________________________________________________________
void TObject3DView::MakeVolumeView(TPolyLine3D *polyline, Int_t  maxlevel)
{ 
   if (polyline  && maxlevel > 0 ) {
      SetLineColor(polyline->GetLineColor()); SetLineStyle(polyline->GetLineStyle());
      SetLineWidth(polyline->GetLineWidth()); SetFillColor(polyline->GetLineColor());
      MakeShape(polyline);
   }
}
//_____________________________________________________________________________
void TObject3DView::MakeVolumeView(TPolyLineShape *polyline, Int_t  maxlevel)
{ 
   if (polyline  && maxlevel > 0 ) {
      SetLineColor(polyline->GetColorAttribute());          SetLineStyle(polyline->GetStyleAttribute());
      SetLineWidth(Width_t (polyline->GetSizeAttribute())); SetFillColor(polyline->GetColorAttribute());
      assert(polyline->GetSizeAttribute());
      MakeShape(polyline);
   }
}

//_____________________________________________________________________________
void  TObject3DView::MarkModified(TObject * /*modefiedObject*/) 
{ }
//_____________________________________________________________________________
void  TObject3DView::MarkVisible(TObject *modefiedObject, Bool_t /*visibility*/) 
{    MarkModified(modefiedObject); }
//_____________________________________________________________________________
void TObject3DView::PopMatrix()
{
    if (fView3DFactory) fView3DFactory->PopMatrix();
}

//_____________________________________________________________________________
void TObject3DView::PushMatrix()
{
    if (fView3DFactory) fView3DFactory->PushMatrix();
}

//_____________________________________________________________________________
const TObject3DView *TObject3DView::FindView(const TObject *node) const
{
   const TObject3DView *nextVolume = 0;
   if (fMap) {
      OBJECT_MAP :: const_iterator volumeFinder;
      volumeFinder = fMap->find((TObject *)node);
      if (volumeFinder != fMap->end( ) ) 
         nextVolume = volumeFinder->second;
   }
   return nextVolume;
}
//_____________________________________________________________________________
const TObject *TObject3DView::FindObjectByView(const TObject3DView *view) const
{
   const TObject *obj = 0;
   if (fMap) 
      obj = (*fMap)[(TObject3DView *)view];
   return obj;
}

//_____________________________________________________________________________
ULong_t TObject3DView::GetViewId(TObject3DViewFactoryABC::ERenderType viewType) const
{
   ULong_t res = f3DViewID;
   switch (viewType) {
      case TObject3DViewFactoryABC::kSelectable:
        res = f3DViewIDSelectable;
        break;
      case TObject3DViewFactoryABC::kSelected:
        res = f3DViewIDSelected;
        break;
      default:
        res = f3DViewID;
        break;
     };

     return res; // (fView3DFactory) ? fView3DFactory->GetViewerId(this):0;
}
//_____________________________________________________________________________
void  TObject3DView::Shunt(TDataSet *newParent)
{
   TDataSet::Shunt(newParent);
}

#if 0
//________________________
void TViewer:: Add3Dview (TObjectView3D *o)

#endif 

