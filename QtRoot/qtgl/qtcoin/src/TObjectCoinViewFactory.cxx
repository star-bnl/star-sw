// @(#)root/gtgl:$Name:  $:$Id: TObjectCoinViewFactory.cxx,v 1.12 2013/08/30 16:00:15 perev Exp $
// Author: Valery Fine      24/09/06

/****************************************************************************
**
** TObjectCoinViewFactory
** An unterface of the class visitor to convert the ROOT 3D objets into 
** the concrete "viewer" representation like OpenGL, OpenInventor, x3d etc
**
** Copyright (C) 2006 by Valeri Fine.  Brookhaven National Laboratory All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
**
*****************************************************************************/

#include "TObjectCoinViewFactory.h"
#include "TCoinShapeBuilder.h"
#include "TSystem.h"
#include "TQtCoin3DDefInterface.h"
#include "TShape3DPolygonView.h"
#include "TObject3DView.h"
#include "TDataSetIter.h"
#include "TStopwatch.h"
#include "TGeometry.h"
#include "TEnv.h"

#include <stack>

#include <Inventor/nodes/SoGroup.h>
#include <Inventor/nodes/SoSeparator.h>
#include <Inventor/nodes/SoMatrixTransform.h>
#include <Inventor/lists/SbStringList.h> 

#include <Inventor/nodes/SoTransform.h>
#include <Inventor/nodes/SoTransformSeparator.h> 
#include <Inventor/nodes/SoTranslation.h>
#include <qstring.h>
#include <qstringlist.h>
#include <qdir.h>

#include <qfileinfo.h>


#include "assert.h"
// #define OWNNORMALS 1

//______________________________________________________________________________
static inline void SetView(TObject3DView *view, const SoGroup *node)
{
     node->ref(); // this node to be release by Release method
     view->SetViewID(ULong_t ( node ) );
}
//____________________________________________________________________________________________________________________
static void SetName(SoNode *node, const char *name) 
{
    // Make sure the name has no "invalid symbols"
   if (node && name && name[0]) {
      TString n  = name;
      n.ReplaceAll(" ","_");
      node->setName(n.Data());
   }
}
//____________________________________________________________________________________________________________________
static inline TObject3DView *OpenView(TObject3DViewFactoryABC  *aFactory) 
{  
   // Instantiate the OpenGL list and TObjectView
   // If one can not create OpenGL list the object is to dle
   TObject3DView *view = new TObject3DView(aFactory); 
   SetView(view , new SoGroup() );
   return view;
}

//____________________________________________________________________________________________________________________
TObjectCoinViewFactory::TObjectCoinViewFactory() : TObject3DViewFactory()
{ }
//____________________________________________________________________________________________________________________
TObjectCoinViewFactory::~TObjectCoinViewFactory()
{ }

//____________________________________________________________________________________________________________________
void TObjectCoinViewFactory::AddChild(TObject3DView * parent, TObject3DView *child)
{
   // Connect Coin3D node in the parent-child tree
   if (parent && child) {
      if (parent->GetViewId() == 0) {
         SoGroup * p = new SoGroup();
         SetName(p,parent->GetTitle());
         p->setUserData((void*)(TObject *)parent);
         SetView(parent , p );
      }
      if (child->GetViewId()  == 0) {
         // FIX ME:  This is a dummy node. It must be deleted in fact
         // fprintf(stderr,"\tnew child %s \n", child->GetTitle());
         // assert(0);
         SoGroup * c = new SoGroup();
         SetName(c,child->GetTitle());
         c->setUserData((void*)(TObject *)child);
         SetView(child , c );
      }
      SoGroup *p = (SoGroup*)(parent->GetViewId());
      p->setUserData((void*)(TObject *)parent); // FIXME: one has to check this first
      
      SoGroup *c = (SoGroup*)(child->GetViewId() );
      c->setUserData((void*)(TObject *)child);  // FIXME: one has to check this first
      p->addChild(c);
   } else {
      assert(0);
   }
}

//____________________________________________________________________________________________________________________
void TObjectCoinViewFactory::AddNormal(TObject3DView *, const Double_t * )
{ }

//____________________________________________________________________________________________________________________
TObject3DView *TObjectCoinViewFactory::BeginModel(TObject3DView *rootView)
{
   if (rootView) {}
   TObject3DView *view = new TObject3DView(this);
   SoSeparator *separator = new SoSeparator();
   SetName(separator, rootView->GetObject()->GetTitle());
//   separator->renderCaching = SoSeparator::ON;
//   separator->boundingBoxCaching = SoSeparator::ON;
   SetView(view , separator );
   return view;
}
//____________________________________________________________________________________________________________________
TObject3DView *TObjectCoinViewFactory::EndModel()
{   return 0;                                     }

//____________________________________________________________________________________________________________________
TObject3DView *TObjectCoinViewFactory::CreateNormal(double const*)
{ return 0; }

//______________________________________________________________________________
static SoNode *ReadInputFile(QString fileName)
{ 	
   // Read in the external scene in the "OpenInventor" format
    SoNode *exObj = 0;
    SoInput viewDecor;
    SbString thisFile = SoInput::searchForFile((const char *)fileName
       ,SoInput::getDirectories ()
       ,SbStringList());
    QFileInfo info(thisFile.getString());
    if (info.isReadable() ) {
       QString saveWorkingDir = QDir::currentDirPath();
       TString ivDir = (const char*)info.dirPath();
       gSystem->ExpandPathName(ivDir);
       gSystem->ChangeDirectory((const char*)ivDir);
       if ( viewDecor.openFile(info.fileName() ) ) {
          if (!SoDB::read(&viewDecor,exObj)) {
             qWarning(" Can not open the Coin3D file <%s>",thisFile.getString());
             exObj = 0; // FIX ME. Print something for user
          }
       } else {
          qWarning(" Can not open the file %s",(const char*)fileName);
       }
       gSystem->ChangeDirectory((const char*)saveWorkingDir);
    } else { 
       qWarning(" Can not read the file <%s>",thisFile.getString());
    }
    return exObj;
}


//____________________________________________________________________________________________________________________
TObject3DView *TObjectCoinViewFactory::CreateCoinNode(const TObject *descriptor)
{
   // Create the Coin node from the external descriptor
   const TQtCoin3DDefInterface *coinDescriptor = dynamic_cast<const TQtCoin3DDefInterface *>(descriptor);
   SoNode *thisNode = 0;
   if (coinDescriptor) {
      switch (coinDescriptor->GetType() ) {
         case TQtCoin3DDefInterface::kStringNode:
            {
               const char *definition = coinDescriptor->GetNodeDescriptor();
               int len = strlen(coinDescriptor->GetNodeDescriptor());
               if (definition && len > 0) {
                  SoInput coinDb;
                  coinDb.setBuffer((void *)definition,len);
                  if (!SoDB::read(&coinDb,thisNode)){
                     thisNode = 0; // FIX ME. Print something for user
                  }
               }
            }
            break;
         case TQtCoin3DDefInterface::kFileNode:
            // Read the fist SoNode defintion from the file provided
            thisNode = ReadInputFile(coinDescriptor->GetFileName());
            break;
         case TQtCoin3DDefInterface::kMemoryNode:
            thisNode =  ((TQtCoin3DDefInterface *)coinDescriptor)->GetNode();
            break;
         default:
            break;
      };
   }
   TObject3DView *view = 0;
   if (thisNode) {
         view = OpenView(this);
         SoGroup *shapeGroup = (SoGroup *) view->GetViewId();
         shapeGroup->setName("ExternalObject");
         shapeGroup->addChild(thisNode);
   }

   return view;
}
//____________________________________________________________________________________________________________________
TObject3DView *TObjectCoinViewFactory::CreateMatrix( const Double_t *translation
                                                      ,const Double_t *rotation
                                                      ,Bool_t isReflection )
{
   SoTranslation     *trans = 0;
   SoMatrixTransform *rot   = 0;
   TObject3DView     *view  = 0;
   Double_t  bombTranslation[3] = {1.0,1.0,1.0};
   const Double_t *thisTranslation = translation;
   {
      Float_t bombFactor[3] =  { 1.0, 1.0, 1.0 };
      if (gGeometry) {
         bombFactor[0] = bombFactor[1] = bombFactor[2] = gGeometry->GetBomb();
      }
      //SoShapeHints *hints   = new SoShapeHints;
      //hints->shapeType      = SoShapeHints::SOLID;
      //hints->vertexOrdering = isReflection  ?
      //                          SoShapeHints::CLOCKWISE :
      //                          SoShapeHints::COUNTERCLOCKWISE;
      // shapeGroup->addChild(hints);
      // TVirtualGeoPainter* TVirtualGeoPainter::GeoPainter()
      if (TMath::Abs( bombFactor[0] - 1.0) > 0.01) {
         thisTranslation = bombTranslation;
         for (int i=0;i<3;i++) bombTranslation[i] = bombFactor[i]*translation[i];
      }
      if (thisTranslation) {
          if ( (   TMath::Abs(thisTranslation[0])
                 + TMath::Abs(thisTranslation[1])
                 + TMath::Abs(thisTranslation[2])
                ) > 1.0E-7) {
            trans = new  SoTranslation();
            trans->translation.setValue( thisTranslation[0]
                                        ,thisTranslation[1]
                                        ,thisTranslation[2] );
      }  }
      if (rotation) {
         // Check whether this is the unity:
         Double_t diagonal = rotation[0] + rotation[ 4] + rotation[8];
         Bool_t doRotation = true;
         if ( TMath::Abs(diagonal - 3) < 0.00001 ) {
            Double_t members = TMath::Abs(rotation[1]) + TMath::Abs(rotation[2])
                             + TMath::Abs(rotation[3]) + TMath::Abs(rotation[5])
                             + TMath::Abs(rotation[6]) + TMath::Abs(rotation[7]);
            // Second check 
            doRotation = ( members > 1.0E-7);
         }
         if (doRotation) {
            rot = new SoMatrixTransform();
            rot->matrix.setValue(    rotation[0] , rotation[ 1] , rotation[ 2] , 0.
                                   , rotation[3] , rotation[ 4] , rotation[ 5] , 0.
                                   , rotation[6] , rotation[ 7] , rotation[ 8] , 0.
                                   ,     0.      ,     0.       ,     0.       , 1.
                                );
            if (isReflection)
               rot->setName("reflection");
            else
               rot->setName("straight");
         }
      }
      if ( trans || rot ) {
         view =  OpenView(this);
         SoGroup *shapeGroup = (SoGroup *) view->GetViewId();
         shapeGroup->setName("VolumeTransformation");
         if (trans) shapeGroup->addChild(trans);
         if (rot)   shapeGroup->addChild(rot);
      }
   }
   return view;
}

//____________________________________________________________________________________________________________________
TObject3DView *TObjectCoinViewFactory::CreatePosition(UInt_t Id)
{
    // Create the new position node with Id
   if (Id) {}
   TObject3DView *view = new TObject3DView(this);
   // SoTransformSeparator *separator =  new SoTransformSeparator();
   SoSeparator *separator =  new SoSeparator();
   separator->setName("Position");
//   SoSeparator *separator =  new SoSeparator();
//   separator->renderCaching      = SoSeparator::ON;
//   separator->boundingBoxCaching = SoSeparator::ON;
   SetView(view , separator);
   return view;
}

//____________________________________________________________________________________________________________________
TObject3DView *CreateNormal(const Double_t * /*normal*/)
{  return 0;  }

//____________________________________________________________________________________________________________________
void TObjectCoinViewFactory::CompileViewLevel(TObject3DView *,ERenderType )
{
   // No compile view level is needed for Coin Factory
}

//____________________________________________________________________________________________________________________
TObject3DView *TObjectCoinViewFactory::MakeShape(TShape3DPolygonView &shapeView, const Float_t *rgba)
{
    TObject3DView *view = new TObject3DView(this);

    TCoinShapeBuilder coinShape(shapeView, rgba);
    SetView(view, coinShape());

    return view;
} 
//____________________________________________________________________________________________________________________
TObject3DView *TObjectCoinViewFactory::CompileSelection(TObject3DView *view)
{
   // No compile selection is needed for Coin Factory
   return view;
}
 
//____________________________________________________________________________________________________________________
void  TObjectCoinViewFactory::GetBoundBox(Double_t *min, Double_t *max) const
{
   const Coord3D &maxBox = fBoundBox.GetMaxBounds();
   const Coord3D &minBox = fBoundBox.GetMinBounds();
   for (int i=0; i < 3; i++ ){
      min[i] =  minBox[i];
      max[i] =  maxBox[i];
   }
}

//____________________________________________________________________________________________________________________
ULong_t TObjectCoinViewFactory::GetViewerId(TObject3DView *view) const
{       assert(0);   return view ? view->GetViewId():0;                      }
//____________________________________________________________________________________________________________________
Bool_t TObjectCoinViewFactory::NeedCompilation() const
{   
   // Coin view needs no compilation step to be performed
   return kFALSE; 
}
//____________________________________________________________________________________________________________________
void   TObjectCoinViewFactory::Release(TObject3DView *view)
{ 
   if (view) {
      SoGroup *id = 0;
      if ( (id = (SoGroup *)view->GetViewId()) )  
         id->unref();
      if ( (id = (SoGroup *)view->GetViewId(TObject3DViewFactoryABC::kSelectable)) ) 
         id->unref();
      if ( (id = (SoGroup *)view->GetViewId(TObject3DViewFactoryABC::kSelected))  ) 
         id->unref();
   }
}

