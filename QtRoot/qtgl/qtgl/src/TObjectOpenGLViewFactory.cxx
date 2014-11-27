// @(#)root/gtgl:$Name:  $:$Id: TObjectOpenGLViewFactory.cxx,v 1.9 2013/08/30 16:00:18 perev Exp $
// Author: Valery Fine      24/04/05

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

#include "TObjectOpenGLViewFactory.h"
#include "TShape3DPolygonView.h"
#include "TObject3DView.h"
#include "TDataSetIter.h"
#include "TStopwatch.h"
#include "TGeometry.h"
#include <stack>

#include  "TQtGLIncludes.h"

#include "assert.h"

// #define DEBUG_OPENGL 1
//-- This slow down the rendering vf 31.05.2005
#define GLLIST 1  

#define IFGLE  ErrorHandling(__FILE__, __LINE__)
//______________________________________________________________________________
static inline Bool_t ErrorHandling(const char *file, int line) {
   Bool_t noErrorFound = kTRUE;
   if (file && line > 0) {
#ifdef  DEBUG_OPENGL
      GLenum errCode;
      const GLubyte *errString;
      // this slow down the apllication in orders !!! of the magnitude !!!
      noErrorFound = (errCode = glGetError()) == GL_NO_ERROR ;
      if (!noErrorFound ) {
         errString = gluErrorString(errCode);
         fprintf(stderr, "OpenGL Error: %s at file %s : line %d\n", errString, file, line);
     }
#endif
   }
   return noErrorFound;
}

//______________________________________________________________________________
inline static GLuint GetNextGLList() {
   static GLuint nGLFirst = 0;
   static GLuint nGLTotal = 1;
   static GLuint nCurrent = 0;
   if (!nGLFirst) {
      nGLFirst = glGenLists(nGLTotal);
      IFGLE;
      nCurrent = 0;
   }
   GLuint res = nGLFirst+nCurrent;
   nCurrent++; if (nCurrent == nGLTotal)  nGLFirst = 0;
   return res;
}

static Bool_t gIsLightingOn = kTRUE;
//______________________________________________________________________________
static inline void TurnLighting(bool on=true) 
{
   gIsLightingOn = on;
   if (!on) {
           glDisable(GL_BLEND);
           glDisable(GL_CULL_FACE);
           glDisable(GL_LIGHTING);
           glEnable(GL_COLOR_MATERIAL);
   }
   return;
   if (on){ 
      //Restoring the lighing  if any
      glPopAttrib();
   //   glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    } else {
      // disable the lighing
      glPushAttrib(GL_LIGHTING_BIT ); // | GL_COLOR_BUFFER_BIT );
      // glPushAttrib(GL_ALL_ATTRIB_BITS);
      glDisable(GL_LIGHTING);
      glEnable(GL_COLOR_MATERIAL);
      glBlendFunc(GL_SRC_COLOR, GL_ZERO);

//      glDisable(GL_BLEND);
   }
}
//______________________________________________________________________________
static inline void CallGLList(const TObject3DView *view
      , TObject3DViewFactoryABC::ERenderType  type=TObject3DViewFactoryABC::kNormal)
{
//
// type = 0 - "normal" list
//        1 - "Selectable" list
//        2 - "selected" list
//
   if (type==TObject3DViewFactoryABC::kSelected) fprintf(stderr," SELECTED\n");
    ULong_t id = 0;
    if (view ) { 
       id = view->GetViewId(type);
       // No "selectable id yet, get the normal one instead
//       if (!id && (type==TObject3DViewFactoryABC::kSelectable) ) {
       if (!id)     id = view->GetViewId();
       if (id) {
          switch(type) {
            default:  // "normal" list
#ifdef EXTRASELECT
               glCallList(id);
              break;
#endif
            case TObject3DViewFactoryABC::kSelectable: // "Selectable" list
                if (view->IsSelectable() ) {
                 // fprintf(stderr," 1. Selectable %d object %p view = %p  %s\n",id, view->GetObject(), view, view->GetObject()->GetName());
// #ifdef NAMESELECTOR                 
// tm vf 15.91.2007                  glPushName( GLuint(view));
// #endif
                     glCallList(id);
// #ifdef NAMESELECTOR
                  glPopName();
// #endif
                  
                } else {
                 //  fprintf(stderr,"2. Selectable %d object %p view = %p  %s\n",id, view->GetObject(), view
                 //        , view->GetObject() ? view->GetObject()->GetName():0);
                   glCallList(id);
                }
                break;
            case TObject3DViewFactoryABC::kSelected: // "selected" list
               if (!view->IsShape() || view->IsSelected() )
                 glCallList(id);
               fprintf(stderr," id = %ld\n",id);
               break;
         };
      } else {
         assert(0);
         fprintf(stderr, " !!0!! CallGLList problem for id = %ld view = %p \n, ", id, view );
      }
   } else {
      fprintf(stderr, " !!1!!  CallGLList problem for id = %ld view = %p \n, ", id, view );
   }
   if (type==TObject3DViewFactoryABC::kSelected) fprintf(stderr,"---\n");
}
//______________________________________________________________________________
static inline void SetCurrentColor(const Float_t *rgba)
{
#if 0   
   if ( true && rgba[3] < 0.98)  {
      glEnable(GL_BLEND);
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      IFGLE;
      //        glBlendFunc(GL_SRC_COLOR, GL_DST_ALPHA);
   }
   else {
      // glDisable(GL_BLEND);
//      alpha = 0.5;       
      glEnable(GL_BLEND);
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
   }
#endif   
   if (gIsLightingOn) 
      glColor4f(rgba[0],rgba[1],rgba[2],rgba[3]);
   else {
//      glLineWidth(3.0);
//      glPointSize(3.0);
      glColor3f(rgba[0],rgba[1],rgba[2]);
   }
   IFGLE;
   // fprintf(stderr,"SetCurrentColor %f %f %f %f \n",  rgba[0],rgba[1],rgba[2],rgba[3]);
}
//____________________________________________________________________________________________________________________
static inline void CreateGLNormal(const Double_t *norm)
{
   if (gDebug & 4 )   glNormal3dv(norm);
}
//____________________________________________________________________________________________________________________
static inline void CreateGLVertex(const Double_t *vertex) 
{
    glVertex3dv(vertex);
}

//____________________________________________________________________________________________________________________
static inline void MultGLMatrix(const Double_t *matrix) 
{
   // if (gDebug & 1  ) 
      glMultMatrixd(matrix);
}

//____________________________________________________________________________________________________________________
static inline void MakeShape(const TPolygone3DFaceBindingView &shape
               ,const std::vector<Coord3D> &vertices
               ,const std::vector<Coord3D> &normals)
{
   GLenum mode=GLenum(-1); 
   switch (shape.fType) {
      case TPolygone3DView::kTriangle:
         mode = GL_TRIANGLES;
         assert(0); break;
      case TPolygone3DView::kQuadeStrip:
         mode = GL_QUAD_STRIP;
         break;
      case TPolygone3DView::kQuade:
         mode = GL_QUADS;
         break;
      case TPolygone3DView::kPolygon:
         mode = GL_POLYGON;
         break;
      case TPolygone3DView::kLines:
         mode = GL_LINE_STRIP;
         break;
      case TPolygone3DView::kPoints:
         mode = GL_POINTS;
         break;
      default: fprintf(stderr, "ERROR **  MakeShape  unknown shape type %x\n",shape.fType);
          mode=GLenum(-1);
   }
  if (mode==GLenum(-1) ) return;
  // fprintf(stderr," ------- MakeShape BEGIN shape.fType = %d size %d ------ \n", shape.fType,normals.size());
  glBegin(mode); {
      // set the normal first
     if (normals.size()) {
        glNormal3dv(&normals[shape.fNormalIndex].fX);
        IFGLE;
        //fprintf(stderr," -- MakeShape -- normals %d = %f %f %f \n", shape.fNormalIndex
        //   , normals[shape.fNormalIndex].fX
        //   , normals[shape.fNormalIndex].fY
        //   , normals[shape.fNormalIndex].fZ); 
        // loop over vertices
        const std::vector<Int_t>  &indices = shape.fVertexIndices;
        std::vector <Int_t>::const_iterator vertex = indices.begin();
        for (;vertex != indices.end(); ++vertex) {
           CreateGLVertex(&vertices[*vertex].fX);
           IFGLE;
		   // fprintf(stderr," MakeShape vertex  %d: %d  = %f %f %f \n", *vertex, index++
           //    , vertices[*vertex].fX
           //   , vertices[*vertex].fY
           //   , vertices[*vertex].fZ); 
        }
     } else {
        // This is  the lines or dots
        std::vector <Coord3D>::const_iterator vertex = vertices.begin();
        // fprintf(stderr," --  MakeShape vertex type =%d mode=%d, LINE_STRIP=%d\n",shape.fType, mode, GL_LINE_STRIP );
        for (; vertex != vertices.end(); ++vertex) {
           CreateGLVertex(&((*vertex).fX));
           // fprintf(stderr," MakeShape vertex %f %f %f \n"
           //   , (*vertex).fX
           //   , (*vertex).fY
           //   , (*vertex).fZ ); 
           IFGLE;
        }
      }
   } glEnd();    
    //fprintf(stderr," MakeShape TPolygone3DFaceBindingView RealTime %f CPU time %f; total vertices=%d %f\n"
    //  ,watch.RealTime(),watch.CpuTime(),shape.fVertexIndices.size(),shape.fVertexIndices.size()/watch.RealTime());
 //  fprintf(stderr," ============= MakeShape END ======================\n\n");
}

//____________________________________________________________________________________________________________________
static inline void MakeShape(const TPolygone3DVertexBindingView &shape
               ,const std::vector<Coord3D> &vertices
               ,const std::vector<Coord3D> &normals)
{
   GLenum mode = 0;; 
   switch (shape.fType) {
      case TPolygone3DView::kTriangle:
         mode = GL_TRIANGLES;
         assert(0); break;
      case TPolygone3DView::kQuadeStrip:
         mode = GL_QUAD_STRIP;
         break;
      case TPolygone3DView::kQuade:
         mode = GL_QUADS;
         break;
      case TPolygone3DView::kPolygon:
         mode = GL_POLYGON;
         break;
   }
   if (!mode) return;
   // TStopwatch watch;
   glBegin(mode); {
      // loop over vertices
      const std::vector<Int_t>  &indices = shape.fVertexIndices; 
      std::vector <Int_t>::const_iterator vertex = indices.begin();
      std::vector <Int_t>::const_iterator normal_iter = shape.fNormalIndices.begin();
      for (;vertex != indices.end(); ++vertex, ++normal_iter ) {
         // set the normal first
         glNormal3dv(&normals[*normal_iter].fX);
         IFGLE;
       //fprintf(stderr," MakeShape normal to vertex %d = %f %f %f \n", *normal_iter
       //  , normals[*normal_iter ].fX
       //  , normals[*normal_iter ].fY
       //  , normals[*normal_iter ].fZ); 
      // set the vertex coordinate
         CreateGLVertex(&vertices[*vertex].fX);
      }
   } glEnd(); 
   // fprintf(stderr," MakeShape TPolygone3DVertexBindingView RealTime %f CPU time %f; total vertices=%d %f\n"
   //   ,watch.RealTime(),watch.CpuTime(),shape.fVertexIndices.size(),shape.fVertexIndices.size()/watch.RealTime());
}

//____________________________________________________________________________________________________________________
static inline const Double_t *GlMatrix(const Double_t *matrix,Double_t *glrows) 
{
//
//  Convert this matrix to the OpenGL [4x4]
//
//  [  matrix[0]   matrix[1]   matrix[2]    0  ]
//  [  matrix[3]   matrix[4]   matrix[5]    0  ]
//  [  matrix[6]   matrix[7]   matrix[8]    0  ]
//  [     0           0           0         1  ]
//
//  Input:
//  -----
//  Double_t *glmatrix - pointer to Double_t 4x4 buffer array
//
//  Return:
//  ------
//  Double_t pointer to the input buffer
//
   Double_t *glmatrix = glrows;
   if (glmatrix && matrix) {
       for (Int_t i=0;i<3;i++,glmatrix+=4,matrix+=3)
       {
          memcpy(glmatrix,matrix,3*sizeof(Double_t));
          glmatrix[3] = 0.0;
       }
       memset(glmatrix,0,3*sizeof(Double_t));
       glmatrix[3] = 1.0;
   }
   return glrows;
}

//____________________________________________________________________________________________________________________
static inline TObject3DView *OpenView(TObject3DViewFactoryABC  *aFactory) 
{  
   // Instantiate the OpenGL list and TObjectView
   // If one can not create OpenGL list the object is to dle
   TObject3DView *view = 0; 
#ifdef GLLIST   
   GLuint gllist = GetNextGLList();
   if ( gllist ) {
      view = new TObject3DView(aFactory); 
      view->SetViewID(gllist);
      // fprintf(stderr," OpenView for %d list\n", gllist);
      glNewList(gllist, GL_COMPILE);
      IFGLE;
   } 
#else
      view = new TObject3DView(aFactory);
#endif   
   return view;
}
//____________________________________________________________________________________________________________________
static inline void CloseView() 
{ 
#ifdef GLLIST   
     glEndList();   IFGLE;
#endif     
}

//____________________________________________________________________________________________________________________
TObjectOpenGLViewFactory::TObjectOpenGLViewFactory() : TObject3DViewFactory()
{ }
//____________________________________________________________________________________________________________________
TObjectOpenGLViewFactory::~TObjectOpenGLViewFactory()
{ }

//____________________________________________________________________________________________________________________
void TObjectOpenGLViewFactory::AddChild(TObject3DView * parent, TObject3DView *child)
{
      // There is no GL specific for this method. Call the base method
    if (parent && child) {}
}

//____________________________________________________________________________________________________________________
TObject3DView *TObjectOpenGLViewFactory::MakeShape(TShape3DPolygonView &shapeView, const Float_t *rgba)
{
    TObject3DView *view = OpenView(this);
#ifdef GLLIST
    if ( view )
#endif     
    {
       // Create GL list
       // shapeView.Print();
   //     glLineWidth(shapeView.GetLineWidth());
   //     glPointSize(shapeView.GetLineWidth());

       const std::vector<Coord3D> &normals  = shapeView.fNormals;
       const std::vector<Coord3D> &vertices = shapeView.fVertex;
       TurnLighting(normals.size() > 0 );
       SetCurrentColor(rgba);
       // int testCounter = 0;
       if (shapeView.fPolygonsFaceBinding.size()) {
          std::vector <TPolygone3DFaceBindingView>::const_iterator face_binding_iter = shapeView.fPolygonsFaceBinding.begin();
          for (; face_binding_iter != shapeView.fPolygonsFaceBinding.end(); ++face_binding_iter) {
             //  fprintf(stderr, " TPolygone3DFaceBindingView  %d\n",++testCounter);
             // SetCurrentSize(rgba);
             if ( (*face_binding_iter).fType ==  TPolygone3DView::kLines ) glLineWidth(shapeView.GetLineWidth()+0.4);
             if ( (*face_binding_iter).fType ==  TPolygone3DView::kPoints) glPointSize(2.66*shapeView.GetLineWidth() + 0.5);
             ::MakeShape(*face_binding_iter,vertices,normals);
             IFGLE;
          }
       }
       // testCounter  = 0;
       if (shapeView.fPolygonsVertexBinding.size() ) {
          std::vector <TPolygone3DVertexBindingView>::const_iterator vertex_binding_iter = shapeView.fPolygonsVertexBinding.begin();;
          for (; vertex_binding_iter != shapeView.fPolygonsVertexBinding.end(); ++vertex_binding_iter) {
              // fprintf(stderr, " fPolygonsVertexBinding  %d\n",++testCounter);
             ::MakeShape(*vertex_binding_iter,vertices,normals);
          }
       }
       CloseView();
       // shapeView.Print();
    } 
    return view;
}
//____________________________________________________________________________________________________________________
void TObjectOpenGLViewFactory::AddNormal(TObject3DView *, const Double_t * /*normal*/ )
{
}

//____________________________________________________________________________________________________________________
TObject3DView *TObjectOpenGLViewFactory::BeginModel(TObject3DView *rootView)
{
   TObject3DView *view = OpenView(this);
#ifdef GLLIST
    if (rootView && (rootView->GetViewId(TObject3DViewFactoryABC::kNormal) == 0) )
    {
      rootView->SetViewID(GetNextGLList());
    }
    if ( view )
#endif     
{    
#ifndef GLLIST      
      GLuint gllist = GetNextGLList();
      if ( gllist ) 
      {
        view->SetName("BeginModel");
        view->SetTitle("Attributes");
        view->SetViewID(gllist);
        // fprintf(stderr,"  -----Begin Model --- list %d \n", gllist);
        glNewList(gllist, GL_COMPILE);
        IFGLE;
#endif 
        // fprintf(stderr,"  -----Begin Model --- list %ld \n", view->GetViewId());
        PushMatrix();
        glPushAttrib(GL_LIGHTING_BIT | GL_COLOR_BUFFER_BIT | GL_ENABLE_BIT );
     } 
     CloseView();

#ifndef GLLIST      
   } 
#endif 
   return view;
}
static int pushcounter=0;
//____________________________________________________________________________________________________________________
TObject3DView *TObjectOpenGLViewFactory::EndModel()
{
   TObject3DView *view = 0;
#ifdef GLLIST          
    if ( (view=OpenView(this)) )
#endif     
    {
#if 0       
//   move to TGLAttributiteList::RestoreAttributes()
          glPopAttrib();
#endif          
//   move to TGLAttributiteList::RestoreAttributes()
          glPopAttrib();
    PopMatrix();
#ifndef GLLIST
    glEndList();   IFGLE;
#else 
    CloseView();
#endif
    }
    // fprintf(stderr,"  ----- End Model --- list. Current stack deep = %d\n",pushcounter);
    return view;
}

//____________________________________________________________________________________________________________________
TObject3DView *TObjectOpenGLViewFactory::CreateNormal(double const*)
{ return 0; }
//____________________________________________________________________________________________________________________
void  TObjectOpenGLViewFactory::PushMatrix()
{
   glPushMatrix(); pushcounter++;IFGLE;
}
//____________________________________________________________________________________________________________________
void  TObjectOpenGLViewFactory::PopMatrix()
{
   glPopMatrix();  IFGLE;
   pushcounter--;
   assert(pushcounter >=0);
}
//____________________________________________________________________________________________________________________
void  TObjectOpenGLViewFactory::PushMatrixEstimate(const Double_t *traslation,const Double_t *rotation)
{
  fBoundBox.Push(traslation,rotation);
#ifndef GLLIST
     PushMatrix();
#endif 
}
//____________________________________________________________________________________________________________________
void  TObjectOpenGLViewFactory::PopMatrixEstimate()
{
#ifndef GLLIST
      PopMatrix();
#endif
      fBoundBox.Pop();
}
//____________________________________________________________________________________________________________________
TObject3DView *TObjectOpenGLViewFactory::CreateMatrix( const Double_t *translation 
                                                      ,const Double_t *rotation
                                                      ,Bool_t isReflection)
{ 
   TObject3DView *view = OpenView(this);
   Double_t  bombTranslation[3] = {1.0,1.0,1.0};
   const Double_t *thisTranslation = translation;
   if ( view ) {
      Float_t bombFactor[3] =  { 1.0, 1.0, 1.0 };
      if (gGeometry) {
         bombFactor[0] = bombFactor[1] =bombFactor[2] =gGeometry->GetBomb();
      }
      // TVirtualGeoPainter* TVirtualGeoPainter::GeoPainter() 
      if (TMath::Abs( bombFactor[0] - 1.0) > 0.01) {
         thisTranslation = bombTranslation;
         for (int i=0;i<3;i++) bombTranslation[i] = bombFactor[i]*translation[i];
      }


//      PushMatrixEstimate(traslation,rotation);
      PushMatrixEstimate(thisTranslation,rotation);
      // fprintf(stderr," TObjectOpenGLViewFactory::CreateMatrix id = %d \n", view->GetViewId());
      if (thisTranslation) {
         glTranslated(thisTranslation[0],thisTranslation[1],thisTranslation[2]);
         // fprintf(stderr," Translation %f %f %f \n", traslation[0],traslation[1],traslation[2]);
         IFGLE;
      }
      if (rotation)
      {
         Double_t glmatrix[16];
         if (isReflection)
            glFrontFace(GL_CW);   // kCW stands for the CLOCKWISE
         else
            glFrontFace(GL_CCW);  // kCCW stands for the COUNTERCLOCKWISE

         MultGLMatrix(GlMatrix(rotation,glmatrix));
         IFGLE;
      }
      CloseView();
   }

   return view;
}
//____________________________________________________________________________________________________________________
TObject3DView *TObjectOpenGLViewFactory::CreatePosition(UInt_t Id)
{
    // Create the new position node with Id
   if (Id) {}
   return new TObject3DView(this);
} 
 //____________________________________________________________________________________________________________________
TObject3DView *CreateNormal(const Double_t * /*normal*/)
{  return 0;  }
//____________________________________________________________________________________________________________________
void TObjectOpenGLViewFactory::CompileViewLevel(TObject3DView *view,ERenderType type)
{
   if (view) {
      PopMatrixEstimate();
#ifdef GLLIST
      GLuint gllist = view->GetViewId(type);
      if ( !gllist ) {
         gllist =  GetNextGLList();
         view->SetViewID(gllist,type);
      }
      // Create GL list
      glNewList(gllist, GL_COMPILE); {
         //fprintf(stderr, " TObjectOpenGLViewFactory::CompileViewLevel %d %p %s %s \n"
         //   , gllist, view, view->GetName(), view->GetTitle() );
         TDataSetIter next(view);
         TObject3DView *obj =0;
         PushMatrix();
         while ( (obj = (TObject3DView *)next()) ) {
                CallGLList(obj, type);
         }
         // fprintf(stderr, " \n -------  FINISH level %d \n", gllist);
         PopMatrix();
      } glEndList();
#endif
   }
}
//____________________________________________________________________________________________________________________
TObject3DView *TObjectOpenGLViewFactory::CompileSelection(TObject3DView *view)
{
   if (view) {
#ifdef GLLIST
       ERenderType type =  TObject3DViewFactoryABC::kSelected;
       std::stack<GLuint> highLightSelection;
       GLuint gllist = view->GetViewId(type);
       if ( !gllist ) {
          gllist =  GetNextGLList();
          view->SetViewID(gllist,type);
       }
       highLightSelection.push(view->GetViewId());
       // Define the parent
       TObject3DView *parent =  (TObject3DView *)view->GetParent();
       while (parent) { 
          // find the transformation node
          TObject3DView *transformation=0;
          TDataSetIter next(parent);
          while ((transformation = (TObject3DView *)next()) 
                 && !strstr(transformation->GetTitle(),"transformation" ) )
          {   transformation = (TObject3DView *)next();                  }

          if (transformation) {
              highLightSelection.push(transformation->GetViewId());
          }
          parent =  (TObject3DView *)parent->GetParent();
       }
       // Create GL list
       glNewList(gllist, GL_COMPILE); {
       //fprintf(stderr, " TObjectOpenGLViewFactory::CompileSelection %d %p %s %s \n"
       //   , gllist, view, view->GetName(), view->GetTitle() );
          PushMatrix();
          // Repeat until stack is empty
          while (!highLightSelection.empty()) {
             const GLuint &id=highLightSelection.top();
             //fprintf(stderr, " TObjectOpenGLViewFactory::CompileSelection %d \n"
             //, id );
             glCallList(id); 
             highLightSelection.pop();
          }
          // fprintf(stderr, " \n -------  FINISH level %d \n", gllist);
          PopMatrix();
       } glEndList();
#endif
   }
   return view;
}
 
//____________________________________________________________________________________________________________________
void  TObjectOpenGLViewFactory::GetBoundBox(Double_t *min, Double_t *max) const
{
   const Coord3D &maxBox = fBoundBox.GetMaxBounds();
   const Coord3D &minBox = fBoundBox.GetMinBounds();
   for (int i=0; i < 3; i++ ){
      min[i] =  minBox[i];
      max[i] =  maxBox[i];
   }
}

//____________________________________________________________________________________________________________________
ULong_t TObjectOpenGLViewFactory::GetViewerId(TObject3DView *view) const
{       assert(0);   return view ? view->GetViewId():0;                      }
//____________________________________________________________________________________________________________________
Bool_t TObjectOpenGLViewFactory::NeedCompilation() const
{   
   // OpenGL view needs an extra compilation step to be performed
   return kTRUE; 
}
//____________________________________________________________________________________________________________________
void   TObjectOpenGLViewFactory::Release(TObject3DView *view)
{ 
   if (view) {
      GLuint id = 0;
       if ( (id = view->GetViewId()) )  
          glDeleteLists(id,1);
       if ( (id = view->GetViewId(TObject3DViewFactoryABC::kSelectable)) ) 
          glDeleteLists(id,1);
       if ( (id = view->GetViewId(TObject3DViewFactoryABC::kSelected))  ) 
         glDeleteLists(id,1);
   }
}

#if 0
// TO DO YET !!!
//______________________________________________________________________________
void TGLKernel::SetLineAttr(Color_t color, Int_t width)
{
    Color_t c = color;
    TGLKernel::SetGLColorIndex(c);
    TGLKernel::SetGLLineWidth((Float_t)width);
}

//______________________________________________________________________________
void TGLKernel::PaintGLPointsObject(const TPoints3DABC *points, Option_t *option)
{
  // PaintGLPointsObject - draws the points
  //
  // option = "L"        - connect all points with straight lines
  //          "P"        - draw the 3d points at each coordinate (by default)
  //          "LP"       - draw the 3D points and connect them with
  //                       straight lines
    if (!points) return;
    // check whether we need a special inventor metafile
    Int_t n = points->Size();
    if (n <=0) return;

    TurnLighting(false);

    GLenum mode = GL_POINTS;
    Int_t pass = 0;
    if (option && strchr(option,'P')) { pass++;}
    if (option && strchr(option,'L')) { mode = GL_LINE_STRIP; pass++;}
    do {
      pass--;
      glBegin(mode);
      int i;
      for (i=0; i < n; i++) {
        GLfloat x = points->GetX(i);
        GLfloat y = points->GetY(i);
        GLfloat z = points->GetZ(i);
        glVertex3f(x,y,z);
      }
      glEnd();
    } while ((mode=GL_POINTS) && pass);
    TurnLighting();
}
//______________________________________________________________________________
void TGLKernel::SetCurrentColor(Int_t color)
{
//    if (fRootLight)
    if (gVirtualGL->GetTrueColorMode())
    {
        Float_t rgba[4];
        Float_t red;
        Float_t green;
        Float_t blue;
        
        Float_t alpha = 1.0;
        // Define the alpha component 
        Style_t style =  gVirtualX->GetFillStyle();
        if (style > 4000) {
           glEnable(GL_BLEND);
           glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
 //        glBlendFunc(GL_SRC_COLOR, GL_DST_ALPHA);           
           alpha = (style - 4000)/100.;   
        }
        else {
           // glDisable(GL_BLEND);
           alpha = 0.5;       
           // glEnable(GL_BLEND);
           // glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        }
           
        TColor *c = gROOT->GetColor(color);
        if (c->GetNumber() == kBlack ) c = 0;
        if (!c) c = gROOT->GetColor(17);
        c->GetRGB(red,green,blue);
        rgba[0] = red;
        rgba[1] = green;
        rgba[2] = blue;
        rgba[3] = alpha;
        TGLKernel::SetGLColor(rgba);
    }
    else
        glIndexi(color+ColorOffset);
}
#endif
