// @(#)root/qtgl:$Name:  $:$Id: TQtGLViewerWidget.cxx,v 1.10 2013/08/30 16:00:18 perev Exp $
// Author: Valery Fine(fine@vxcern.cern.ch)   12/11/02
 
/****************************************************************************
** $Id: TQtGLViewerWidget.cxx,v 1.10 2013/08/30 16:00:18 perev Exp $
**
** Copyright (C) 2002 by Valeri Fine. Brookhaven National Laboratory.
**                                    All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
**
*****************************************************************************/
  
//////////////////////////////////////////////////////////////////////////
//                                                                      //
//    This the ROOT GL veiwer implmentation based on                    //
//                l i b Q G L V i e w e r                               //
//           Copyright c Gilles Debunne 2002-03                         //
//                Gilles.Debunne@imag.fr                                //
//   http://www-imagis.imag.fr/Membres/Gilles.Debunne/CODE/QGLViewer/   //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include "TQPadOpenGLView.h"
#include "TQVirtualGL.h"

#include "TVirtualPad.h"
// #include "Buttons.h"
#include "TROOT.h"
#include "TColor.h"
#include "TView.h"
#include "TMath.h"
#include "TVector3.h"

#include "TGQt.h"
#include "TQtGLViewerWidget.h"
#include "TObject3DView.h"
#include <QApplication>
#include <QEvent>
#include <QMessageBox> 
#include <QColor> 

#ifdef TEST_GETGFERROR_PEFORMANCE
#  include <QDateTime> 
#endif

#include <QKeyEvent>

#include <QGLViewer/manipulatedFrame.h> 
#include <QFontMetrics> 
#include <QGLViewer/quaternion.h>

// TWin32GLViewer* TQtGLViewerWidget::fgCurrent = 0;
using namespace qglviewer;  
// ClassImp(TQtGLViewerWidget)
#define IFGLE  ErrorHandling(__FILE__, __LINE__)

//______________________________________________________________________________
static inline Bool_t ErrorHandling(const char *file, int line) {
   Bool_t noErrorFound = kTRUE;
   if (file && line) {}
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
   return noErrorFound;
}


// Prdefine lists

//____________________________________________________________________________________________________________________
//
//                        class  TGLAttributiteList
//____________________________________________________________________________________________________________________
void TGLAttributiteList::SaveAttributes() const
{  glPushAttrib(GL_LIGHTING_BIT | GL_COLOR_BUFFER_BIT | GL_ENABLE_BIT ); }

//____________________________________________________________________________________________________________________
void TGLAttributiteList::RestoreAttributes() const
{   glPopAttrib();  }

//____________________________________________________________________________________________________________________
void TGLAttributiteList::RunObjectList(const std::list<unsigned int> &objectList) const
{
   // run the selected object list with the current attributes
   if (!objectList.empty()) {
      std::list<unsigned int>::const_iterator gllist = objectList.begin();
      for (;gllist!=objectList.end(); ++gllist) 
      {  glCallList(*gllist);     IFGLE;     }
   }
}

//____________________________________________________________________________________________________________________
//
//                        class  TGLSolidAttribute
//____________________________________________________________________________________________________________________
class TGLSolidAttribute : public  TGLAttributiteList
{
  private:
     bool fCullFace;
     bool fTranslucent;
  public: 
     TGLSolidAttribute():fCullFace(TRUE),fTranslucent(TRUE){ MakeAttributeList();}
     virtual ~TGLSolidAttribute(){ }
     void MakeAttribute();
     bool ToggleCullFace()    { fCullFace    = !fCullFace;    MakeAttributeList(); return fCullFace;    }
     bool ToggleTranslucent() { fTranslucent = !fTranslucent; MakeAttributeList(); return fTranslucent; }
};

//____________________________________________________________________________________________________________________
void TGLSolidAttribute::MakeAttribute()
{
    glPolygonMode(GL_FRONT ,GL_FILL);
    glFrontFace(GL_CW);
    
    if (fCullFace) glCullFace(GL_BACK);
    else           glDisable(GL_CULL_FACE);
    
    if (fTranslucent) {
      glEnable(GL_BLEND);
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    } else {
      glDisable(GL_BLEND);
    }
}

//____________________________________________________________________________________________________________________
//
//                        class  TGLWiredAttribute
//____________________________________________________________________________________________________________________
class TGLWiredAttribute  : public  TGLAttributiteList
{
  private:
     bool fCullFace;
  public: 
     TGLWiredAttribute():fCullFace(FALSE){ MakeAttributeList();}
     virtual ~TGLWiredAttribute(){ }
     void MakeAttribute();
     bool ToggleCullFace() {fCullFace = !fCullFace; MakeAttributeList(); return fCullFace; }
};

//____________________________________________________________________________________________________________________
void TGLWiredAttribute::MakeAttribute()
{
   glDisable(GL_BLEND);
   
   if (fCullFace) glCullFace(GL_BACK);
   else           glDisable(GL_CULL_FACE);
   
   glDisable(GL_LIGHTING);
   glPolygonMode(GL_FRONT_AND_BACK ,GL_LINE);
   glEnable(GL_LINE_SMOOTH);
   glEnable(GL_POINT_SMOOTH);
   glLineWidth(3);
   // glPointSize(4);

   glEnable (GL_COLOR_MATERIAL);
}

//____________________________________________________________________________________________________________________
//
//                        class  TGLSelectingAttribute
//____________________________________________________________________________________________________________________
class TGLSelectingAttribute  : public  TGLAttributiteList
{
  public: 
     TGLSelectingAttribute() { MakeAttributeList();}
     virtual ~TGLSelectingAttribute(){ }
     void MakeAttribute();
};

//____________________________________________________________________________________________________________________
void TGLSelectingAttribute::MakeAttribute()
{
   glPolygonMode(GL_FRONT_AND_BACK ,GL_FILL);
   glFrontFace(GL_CW);
   glDisable(GL_BLEND);
   glDisable(GL_CULL_FACE);
 
   glDisable(GL_LIGHTING);
//   glEnable (GL_COLOR_MATERIAL);
}
//____________________________________________________________________________________________________________________
//
//                        class  TGLSlicePlaneAttribute
//____________________________________________________________________________________________________________________
class TGLSlicePlaneAttribute  : public  TGLAttributiteList
{
  private:
     const TQtGLViewerWidget *fMaster;
     float fSliceWidth;
  protected:
     TGLSlicePlaneAttribute() : fMaster (0) {}
  public: 
     TGLSlicePlaneAttribute(const TQtGLViewerWidget *master) : fMaster (master), fSliceWidth(0.002) { MakeAttributeList();}
     virtual ~TGLSlicePlaneAttribute(){ }
     void MakeAttribute();
};

//____________________________________________________________________________________________________________________
void TGLSlicePlaneAttribute::MakeAttribute()
{
       double plane[4] ={0,0,1,0};
       glClipPlane(GL_CLIP_PLANE0,plane);
       glEnable(GL_CLIP_PLANE0);
       plane[2] = -1; plane[3] =fSliceWidth*fMaster->sceneRadius();
       glClipPlane(GL_CLIP_PLANE1,plane);
       glEnable(GL_CLIP_PLANE1);
}

//____________________________________________________________________________________________________________________
//
//                        class  TObjectOpenGLViewFactory
//____________________________________________________________________________________________________________________

//______________________________________________________________________________
 TQtGLViewerWidget::TQtGLViewerWidget(QWidget *parent, const char *name,const QGLWidget *shareWidget, Qt::WindowFlags f) :
  QGLViewer(parent,shareWidget,f)
 ,fGLView(0),fPad(0),fClipPlane(-1),fPadSynch(kFALSE)
 ,fSolidAttributes(0),fWiredAttributes(0),fSelectedAttributes(0),fSlicingAttributes(0),fSolidSelectable(kTRUE),fWiredSelectable(kTRUE) 
 , fFrameAxisFactor(-1)
{
   if (name && name[0]) setObjectName(name);
   memset(fGLLights,0,sizeof(fGLLights));
 }
//______________________________________________________________________________
TQtGLViewerWidget::TQtGLViewerWidget(TPadOpenGLView *view, const char *title,QWidget *parent,Qt::WindowFlags f):
      QGLViewer(parent,0,f)
      ,fGLView(view),fPad(0),fClipPlane(-1), fPadSynch(kFALSE)
      , fSolidAttributes(0),fWiredAttributes(0),fSelectedAttributes(0),fSlicingAttributes(0),fSolidSelectable(kTRUE),fWiredSelectable(kTRUE)
      , fFrameAxisFactor(-1)
{  
   if (title && title[0]) setWindowTitle(title);
   memset(fGLLights,0,sizeof(fGLLights));
}
//______________________________________________________________________________
TQtGLViewerWidget::TQtGLViewerWidget(TVirtualPad *pad, const char *title,QWidget *parent,Qt::WindowFlags f):
      QGLViewer(parent,0,f)
      ,fGLView(0),fPad(pad),fClipPlane(-1), fPadSynch(kFALSE)
      ,fSolidAttributes(0),fWiredAttributes(0),fSelectedAttributes(0),fSlicingAttributes(0),fSolidSelectable(kTRUE),fWiredSelectable(kTRUE)
      , fFrameAxisFactor(-1)
{ 
    if (title && title[0]) setWindowTitle(title);
    memset(fGLLights,0,sizeof(fGLLights));
}
//______________________________________________________________________________
 TQtGLViewerWidget::~TQtGLViewerWidget()
 {
    makeCurrent();
    emit viewerAbout2Close();
    saveStateToFile();
    fGLView = 0;
    delete fSolidAttributes;     fSolidAttributes     = 0;
    delete fWiredAttributes;     fWiredAttributes     = 0;
    delete fSelectingAttributes; fSelectingAttributes = 0;
    delete fSlicingAttributes;   fSlicingAttributes   = 0;
 }
 
//  QGLViewer virtual methods;
//______________________________________________________________________________
 void TQtGLViewerWidget::init  ( ) 
 {
    
    // this method is called by QGLViewer::initializeGL() 
    
    if (fGLView) {
          if ( format ().rgba () )  gVirtualGL->SetTrueColorMode(); // jus flag ugnore 
          fGLView->MapOpenGL();
          gVirtualGL->SetRootLight(kFALSE);  // redundant 
    }
 
    Float_t minBound[3]={-1000,-1000,-1000};
    Float_t maxBound[3]={ 1000, 1000, 1000};
    Float_t  phi   = 0;//-M_PI/2; // -90.;
    Float_t  theta = 0;//M_PI/2; //  90.;
    Float_t  psi   =  0;//  M_PI/2; // 90. ;
    TVirtualPad *thisPad  = fGLView ? fGLView->GetPad() : fPad;
    fStateFileName = "."; 
    if (thisPad) {
       fStateFileName += thisPad->GetName();
       fStateFileName += ".xml";
       TView *padView = thisPad->GetView();
       if (padView)  {
          padView->GetRange(minBound,maxBound);
          phi   = M_PI*(padView->GetLatitude())/180;  // (phi)
          theta = M_PI*(padView->GetLongitude())/180; // (Theta)
          psi   = M_PI*padView->GetPsi()/180.;

       }
       SetBackgroundColor(thisPad->GetFillColor());
    } else {
       fStateFileName += "nopad.xml";
    }
    setStateFileName (fStateFileName );

   // calculate the raduis
    Float_t raduis = TMath::Sqrt(
       (maxBound[2]-minBound[2])*(maxBound[2]-minBound[2]) 
       + (maxBound[1]-minBound[1])*(maxBound[1]-minBound[1]) 
       + (maxBound[0]-minBound[0])*(maxBound[2]-minBound[0]) 
       );
    setSceneRadius (raduis);
    // calculate the center
    Float_t xCenter = 0.5*(maxBound[0]+minBound[0]);
    Float_t yCenter = 0.5*(maxBound[1]+minBound[1]);
    Float_t zCenter = 0.5*(maxBound[2]+minBound[2]);
#if QGLVIEWER_VERSION <  0x020000
    setSceneCenter (xCenter, yCenter, zCenter);
#else         
    setSceneCenter (qglviewer::Vec(xCenter, yCenter, zCenter));
#endif         

    // setSceneBoundingBox (minBound, maxBound);
  
    // setDrawAxis(true);
    setGridIsDrawn(true);
    // set manipulatin
    setManipulatedFrame(new qglviewer::ManipulatedFrame() );
    showEntireScene();
#if QGLVIEWER_VERSION <  0x020000
    Float_t x,y,z;
    const Vec &pos= camera()->getPosition(x,y,z);
#else
    const Vec &pos= camera()->position();
    Float_t x=pos.x;
    Float_t y=pos.y;
    Float_t z=pos.z;
#endif
    Double_t padPhi   =    M_PI/2 + phi;
    Double_t padTetha =   -M_PI/2 + theta;
    // Double_t padPsi    =   M_PI/2 + psi;

    Double_t  mLatitude[9] = { 1,      0,         0        , 
                               0, cos(padPhi), -sin(padPhi),
                               0, sin(padPhi),  cos(padPhi)
    };

   
    Double_t  mLongitude[9] = { cos(padTetha), 0, sin(padTetha),
                                   0,          1,   0           ,
                               -sin(padTetha), 0, cos(padTetha)
    };


    //Double_t  mPsi[9]      =  { cos(padPsi), -sin(padPsi), 0,
    //                            sin(padPsi),  cos(padPsi), 0,
    //                               0,          0,          1
    //};

      // parallel view
    Double_t xNew1 = mLatitude[0]*x     + mLatitude[1]*y     + mLatitude[2]*z - xCenter;
    Double_t yNew1 = mLatitude[3]*x     + mLatitude[4]*y     + mLatitude[5]*z - yCenter;
    Double_t zNew1 = mLatitude[6]*x     + mLatitude[7]*y     + mLatitude[8]*z - zCenter;

    Double_t xNew2 = mLongitude[0]*xNew1 + mLongitude[1]*yNew1 + mLongitude[2]*zNew1;
    Double_t yNew2 = mLongitude[3]*xNew1 + mLongitude[4]*yNew1 + mLongitude[5]*zNew1;
    Double_t zNew2 = mLongitude[6]*xNew1 + mLongitude[7]*yNew1 + mLongitude[8]*zNew1;

    //Double_t xNew3 = mPsi[0]*xNew2      + mPsi[1]*yNew2      + mPsi[2]*zNew2;
    //Double_t yNew3 = mPsi[3]*xNew2      + mPsi[4]*yNew2      + mPsi[5]*zNew2;
    //Double_t zNew3 = mPsi[6]*xNew2      + mPsi[7]*yNew2      + mPsi[8]*zNew2;
 

#if QGLVIEWER_VERSION <  0x020000
    camera()->setPosition(xNew2 + xCenter,yNew2 + yCenter,zNew2 + zCenter);
    getSceneCenter(x,y,z);
    camera()->setUpVector(0,0,1);
    camera()->lookAt(x,y,z);
#else
    camera()->setPosition(Vec(xNew2 + xCenter,yNew2 + yCenter,zNew2 + zCenter));
    camera()->setUpVector(Vec(0,0,1));
    camera()->lookAt(sceneCenter());
#endif
   // camera()->lookAt(0,0,0);

    // Set default ligts and materials
    GLfloat ambientFactor[4] = {0.2,0.2,0.2,1.0};
    glLightfv(GL_LIGHT0,GL_AMBIENT,ambientFactor);

    GLfloat diffuseFactor[4] = {0.8,0.8,0.8,1.0};
    glLightfv(GL_LIGHT0,GL_DIFFUSE,diffuseFactor);

    GLfloat speculartFactor[4] = {0.3,0.3,0.3,1.0};
    glMaterialfv(GL_FRONT, GL_SPECULAR, speculartFactor);
    glMateriali(GL_FRONT,  GL_SHININESS, 45);
    fSolidAttributes = new TGLSolidAttribute();
    fSolidAttributes->SetActive();
    //fGLAttributesLists.push_back(fSolidAttributes );
    
    fWiredAttributes =  new TGLWiredAttribute();
    fSelectingAttributes =  new TGLSelectingAttribute();
    fGLAttributesLists.push_back(fWiredAttributes);
    fGLAttributesLists.push_back(fSolidAttributes );
    fSlicingAttributes  = new TGLSlicePlaneAttribute(this);
    // Set key descriptions
    setKeyDescription(Qt::Key_E,"<b>Cut off</b> the view with the XY plane");
    setKeyDescription(Qt::Key_R,"Toggles the <b>Hidden</b> surface mode");
    setKeyDescription(Qt::Key_N, "Toggles the <b>\"Solid\" /   \"Translucent\" </b> color mode on ");
    setKeyDescription(Qt::Key_W, "Toggles the <b>wireframe</b> mode");
//    text += tableLine("c", "Turns the cull-face mode");
    setKeyDescription(Qt::Key_P, "Toggles the <b>perpective </b> / <b> orthographics</b> projection");
    
// Remove the "Z" key description
    setKeyDescription(Qt::Key_Z, "");
#ifdef TEST_GETGFERROR_PEFORMANCE
//  -- test the server performance by calling glGetError in loop
     QTime bench; bench.start();
     for (int tt =0; tt<1000; tt++) {
          glGetError();
     }
     fprintf(stderr,"Testing the GLX-server peformance: %f \n", bench.elapsed()/1000.);
#endif
    if (!restoreStateFromFile() )
          showEntireScene();
    setCursor(Qt::CrossCursor);
 } 
//______________________________________________________________________________
void TQtGLViewerWidget::SetSceneLayout()
{
    Float_t minBound[3]={-1000,-1000,-1000};
    Float_t maxBound[3]={ 1000, 1000, 1000};
//    Float_t  angles[3] = { 90.,-90.,90. };
    Color_t padColor = kBlack;
    TVirtualPad *thisPad  = fGLView ? fGLView->GetPad() : fPad;
    if (thisPad) {
       TView *padView = thisPad->GetView();
       if (padView)  {
          padView->GetRange(minBound,maxBound);
          //angles[0] = M_PI*(-90-padView->GetLatitude())/180;  // (phi)
          //angles[1] = M_PI*(90-padView->GetLongitude())/180; // (Theta)
          //angles[2] = padView->GetPsi();
          //fprintf(stderr,"TQtGLViewerWidget::SetSceneLayout view angles = %f %f %f\n",angles[0],angles[1],angles[2]);
       }
       padColor = thisPad->GetFillColor();
    }
    SetSceneLayout(minBound,maxBound,padColor);
}

//______________________________________________________________________________
void TQtGLViewerWidget::SetSceneLayout(Float_t *minBound,Float_t *maxBound,Color_t color)
{

    SetBackgroundColor(color);
    // calculate the raduis
    Float_t raduis = TMath::Sqrt(
       (maxBound[2]-minBound[2])*(maxBound[2]-minBound[2]) 
       + (maxBound[1]-minBound[1])*(maxBound[1]-minBound[1]) 
       + (maxBound[0]-minBound[0])*(maxBound[2]-minBound[0]) 
       );
    setSceneRadius (raduis);
    // calculate the center
    Float_t x = 0.5*(maxBound[0]+minBound[0]);
    Float_t y = 0.5*(maxBound[1]+minBound[1]);
    Float_t z = 0.5*(maxBound[2]+minBound[2]);
#if QGLVIEWER_VERSION <  0x020000
    setSceneCenter (x, y, z);
#else
    setSceneCenter (Vec(x, y, z));
#endif
}
#ifdef DEBUGVIEWER
//______________________________________________________________________________
 static void testCube() {
   // - > 0-th face binded polygon
 glBegin(GL_QUADS);
   glNormal3f( -1.000000, 0.000000, 0.000000);
     glVertex3f(-200.000000, -150.000000, -150.000000);
     glVertex3f(-200.000000, 150.000000, -150.000000);
     glVertex3f(-200.000000, 150.000000, 150.000000);
     glVertex3f(-200.000000, -150.000000, 150.000000);

 //-- > 1-th face binded polygon
     glNormal3f(0.000000, 1.000000, 0.0000000);
      glVertex3f(-200.000000, 150.000000, -150.000000);
      glVertex3f(200.000000, 150.000000, -150.000000);
      glVertex3f(200.000000, 150.000000, 150.000000);
      glVertex3f(-200.000000, 150.000000, 150.000000);
 // -- > 2-th face binded polygon
   glNormal3f( -1.000000, 0.000000, 0.000000);
      glVertex3f(200.000000,  150.000000, -150.000000);
      glVertex3f(200.000000, -150.000000, -150.000000);
      glVertex3f(200.000000, -150.000000,  150.000000);
      glVertex3f(200.000000,  150.000000,  150.000000);
// -- > 3-th face binded polygon
   glNormal3f(0.000000, -1.000000, 0.000000);
      glVertex3f(-200.000000, -150.000000, -150.000000);
      glVertex3f(-200.000000, -150.000000,  150.000000);
      glVertex3f( 200.000000, -150.000000,  150.000000);
      glVertex3f( 200.000000, -150.000000, -150.000000);
// -- > 4-th face binded polygon
   glNormal3f(0.000000, 0.000000, +1.000000);
       glVertex3f(-200.000000, -150.000000, 150.000000);
       glVertex3f(-200.000000,  150.000000, 150.000000);
       glVertex3f( 200.000000,  150.000000, 150.000000);
       glVertex3f( 200.000000, -150.000000, 150.000000);
 //-- > 5-th face binded polygon
    glNormal3f(0.000000, 0.000000, -1.000000);
       glVertex3f( 200.000000, -150.000000, -150.000000);
       glVertex3f( 200.000000,  150.000000, -150.000000);
       glVertex3f(-200.000000,  150.000000, -150.000000);
       glVertex3f(-200.000000, -150.000000, -150.000000);
  glEnd();
 }
#endif
//______________________________________________________________________________
QDomElement TQtGLViewerWidget::domElement(const QString &name, QDomDocument &document) const
{
   // Returns an XML QDomElement that represents the TQtGLViewerWidget
   // Get the DOM element from the base class
   QDomElement res = QGLViewer::domElement(name, document);
#if 0
   // Light on option 
   QDomElement de = document.createElement("Light");
   de.setAttribute("State", (lightIsOn()?"on":"off"));
   de.appendChild(lightManipulatedFrame()->domElement("LightFrame", document));
   // Get default state domElement and append custom node
   res.appendChild(de);
#endif
   // Slicing plane on option 
   QDomElement de = document.createElement("Slicing");
   de.setAttribute("state", (IsSlicingSelected()?"on":"off"));
   // Get default state domElement and append custom node
   res.appendChild(de);

   // Selection modes
   QDomElement selections = document.createElement("Selection");
   res.appendChild(selections);

   // Wired view  on option 
   de = document.createElement("Detector");
   de.setAttribute("state", (IsSolidSelectable()?"on":"off"));
   selections.appendChild(de);
  // Get default state domElement and append custom node

   // Solid view  on option 
   de = document.createElement("Event");
   de.setAttribute("state", (IsWiredSelectable()?"on":"off"));
   selections.appendChild(de);

   // Frame Axis 
   de = document.createElement("FrameAxisScale");
   de.setAttribute("state", FrameAxisScale() > 0 ? "on":"off");
   if ( FrameAxisScale() > 0) 
      de.setAttribute("scale", QString::number(FrameAxisScale()));
   res.appendChild(de);
   return res;
}
//______________________________________________________________________________
void TQtGLViewerWidget::draw  ( )
{
#if 0
         glEnable(GL_LIGHT0); 
         glEnable(GL_LIGHTING);
         glEnable(GL_COLOR_MATERIAL);

            GLfloat ambientFactor[4] = {0.2,0.2,0.2,1.0};
            glLightfv(GL_LIGHT0,GL_AMBIENT,ambientFactor);

            GLfloat diffuseFactor[4] = {0.8,0.8,0.8,1.0};
            glLightfv(GL_LIGHT0,GL_DIFFUSE,diffuseFactor);

            GLfloat speculartFactor[4] = {0.3,0.3,0.3,1.0};
            glMaterialfv(GL_FRONT, GL_SPECULAR, speculartFactor);
            glMateriali(GL_FRONT,  GL_SHININESS, 45);


    SetDefaultGL(); // fGLView->MapOpenGL();  
#endif
   glEnable(GL_LIGHTING);
   drawGLLights();
   // Paint G3D objects  
   Int_t myGLIst = fGLView? fGLView->GetGLList()-1 : 0;
   // assert(myGLIst );
   
   if (myGLIst || !fGLSolidLists.empty() || !fGLWiredLists.empty()) {
      glPushAttrib(GL_ALL_ATTRIB_BITS); {
         //  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT); // done by QGLViewer::preDraw()
         
         const GLdouble *matrix = manipulatedFrame()->matrix();
         // Add object axis as needed
         if (fFrameAxisFactor > 0 ) {
            glPushMatrix(); {
               glMultMatrixd(matrix);
               drawAxis(sceneRadius() / fFrameAxisFactor );
            }     glPopMatrix();
         }
         
         // Render the wired object first
         if ( !fGLWiredLists.empty() ) {
            glPushMatrix(); {
               glMultMatrixd(matrix);
               // Pick the current TPad rotation
               // Draw external components if any
               fWiredAttributes->RunList(fGLWiredLists);
            } glPopMatrix();
         }
         
         // Render the selected object (if any)
         drawSelectedObject();
         
         // Render the slicing plane         
         if (fClipPlane > 0)  fSlicingAttributes->RunList();
         
         // Render the solid object 
         glPushMatrix(); {
            glMultMatrixd(matrix);
            // Pick the current TPad rotation
            if (myGLIst) {
               if (fPadSynch) {}
               gVirtualGL->RunGLList(myGLIst + TPadOpenGLView::kView);      
               //   gVirtualGL->RunGLList(myGLIst + TPadOpenGLView::kUpdateView);
               // Draw G3D objects
               gVirtualGL->RunGLList(myGLIst + TPadOpenGLView::kModel);
            }
            // Draw external components if any
            if ( !(fGLSolidLists.empty() || fGLAttributesLists.empty() ) )  {
               std::list<TGLAttributiteList *>::const_iterator gllist = fGLAttributesLists.begin();
               for (;gllist!=fGLAttributesLists.end(); ++gllist) 
               {
               //  fprintf(stderr,"\t 3. TQtGLViewerWidget::draw %d\n", *gllist);
                //testCube();
                  if ((*gllist)->IsActive() ){
                     (*gllist)->RunList(fGLSolidLists);
                  IFGLE;
                  }
               }
            }
         }  glPopMatrix();

         drawFooter();
      }  glPopAttrib();
   }
}
//______________________________________________________________________________
void TQtGLViewerWidget::drawSelectedObject()
{
   // render the selected objects. To be called from draw() 
   if ( !fGLSelectedLists.empty() ) {
      glPushAttrib(GL_ALL_ATTRIB_BITS);
       glPushMatrix(); {
          glMultMatrixd(manipulatedFrame()->matrix());
          fWiredAttributes->RunList(fGLSelectedLists);
      } glPopMatrix();
      glPopAttrib();
   }
} 

//______________________________________________________________________________
void TQtGLViewerWidget::drawWithNames()
{
#ifdef EXTRASELECT
   if (!fGLSelectingLists.empty() ) {
        // fprintf(stderr," TQtGLViewerWidget::drawWithNames %d \n", fGLSelectingLists.size());
         glPushAttrib(GL_ALL_ATTRIB_BITS);
         if (fClipPlane > 0)  fSlicingAttributes->RunList();
         glPushMatrix(); 
            glMultMatrixd(manipulatedFrame()->matrix());
               fSelectingAttributes->RunList(fGLSelectingLists);
//               fSolidAttributes->RunList(fGLSelectingLists);
         glPopMatrix();
      glPopAttrib();
   }
#else
      glPushAttrib(GL_ALL_ATTRIB_BITS); {
         //  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT); // done by QGLViewer::preDraw()
         
         // Render the wired object first
         
         if ( !fGLWiredLists.empty() &&  fWiredSelectable) {
            glPushMatrix(); {
               glMultMatrixd(manipulatedFrame()->matrix());
                  fWiredAttributes->RunList(fGLWiredLists);
            } glPopMatrix();
         }

         // Render the solid object 
         
         if ( !fGLSolidLists.empty()  && fSolidSelectable )  {
           if (fClipPlane > 0)  fSlicingAttributes->RunList();
           glPushMatrix(); {
              glMultMatrixd(manipulatedFrame()->matrix());
              fSolidAttributes->RunList(fGLSolidLists);
           }  glPopMatrix();
        }
      }  glPopAttrib();
#endif
//    QGLViewer::drawWithNames();
}
//______________________________________________________________________________
void TQtGLViewerWidget::postSelection(const QPoint &point)
{
   if (selectedName() == -1) {
     // qWarning("No object selected under pixel " + QString::number(point.x()) + "," + QString::number(point.y()));
   } else {
      TObject3DView *view =  (TObject3DView *) selectedName();
      TObject *obj = 0;
      if (view && (view != (TObject3DView *)1) && (obj = view->GetObject()) ) {
          emit objectSelected(obj,point);
         // qWarning(" Selected object: %p %s %s\n", obj, obj->ClassName(), obj->GetObjectInfo(point.x(),point.y()));
      }
   }
}
//______________________________________________________________________________
void   TQtGLViewerWidget::drawFooter()
{
   if (! fFooterText.isEmpty() ) {
      QFontMetrics m(fFooterFont);
#if QT_VERSION < 0x40000
      QSize tsize = m.size(Qt::SingleLine, fFooterText);
#else /* QT_VERSION */
      QSize tsize = m.size(Qt::TextSingleLine, fFooterText);
#endif /* QT_VERSION */
      QSize wsize = size();
      if ( 0.6*wsize.width() > tsize.width()|| tsize.width() > 0.8*wsize.width()) {
            float newSize = 1;
            newSize = 0.7*float(wsize.width())/tsize.width();
            fFooterFont.setPixelSize(int(fFooterFont.pixelSize()*newSize+0.5));
            // Estimate the new size
            QFontMetrics n(fFooterFont);
#if QT_VERSION < 0x40000
            tsize = n.size(Qt::SingleLine, fFooterText);
#else /* QT_VERSION */
            tsize = n.size(Qt::TextSingleLine, fFooterText);
#endif /* QT_VERSION */
      }

//      fprintf(stderr,"TQtGLViewerWidget::drawFooter() ration %f textSize = %d, widget width %d, pixels= %d %s\n",
//      float(wsize.width())/tsize.width(), tsize.width(),wsize.width(),fFooterFont.pixelSize(),(const char *)fFooterText);

      int leftmargin = (wsize.width() - tsize.width()+1)/2;
      fWiredAttributes->SaveAttributes();fWiredAttributes->RunList();
        const QColor white_color("white");
        qglColor(white_color);
        drawText(leftmargin, height()- tsize.height(), fFooterText,fFooterFont);
      fWiredAttributes->RestoreAttributes();
   }
}
#if 0
//______________________________________________________________________________
void TQtGLViewerWidget::drawGLLights()
{
   // Rotate the camera accroding the TPAd view angles
   fAngles[0] = view->GetLatitude();
   fAngles[1] = view->GetLongitude();
   fAngles[2] = view->GetPsi();
   setFromRotationMatrix()
   camera().setOrientation();
   TGLKernel::RotateGL(-(-90+angles[0]),
                        -( 90+angles[1]),
                           90+angles[2]);
//*-*  Move model to the center of the "view" box ??????????
    // TGLKernel::TranslateGL(delta[0],delta[1],delta[2]);
}
}
#endif

//______________________________________________________________________________
void TQtGLViewerWidget::drawGLLights()
{
   for (unsigned int i =0; i < sizeof(fGLLights)/sizeof(GLLIGHT);i++) {
      if (fGLLights[i].fOn) 
          drawLight(fGLLights[i].fGLValue,fGLLights[i].fScale);
   }
}
//______________________________________________________________________________
void  TQtGLViewerWidget::initFromDOMElement(const QDomElement &element)
{
   // Restores the TQtGLViewerWidget state from a QDomElement created by domElement().
   // Restore standard state
   QGLViewer::initFromDOMElement(element);
   QDomElement child=element.firstChild().toElement();
   while (!child.isNull())
   {
     if (child.tagName() == "Slicing")
     {
       if (child.hasAttribute("state"))
         setSlicing(child.attribute("state").toLower() == "on");
     } else if ( child.tagName() == "Selection") {
        QDomElement selection=child.firstChild().toElement();
        while (!selection.isNull())
        {
           if (selection.tagName() == "Detector")
           {
              if (selection.hasAttribute("State"))
                 SetSolidSelectable(selection.attribute("state").toLower() == "on");
           } else if (selection.tagName() == "Event") 
              if (selection.hasAttribute("state"))
                 SetWiredSelectable(selection.attribute("state").toLower() == "on");
           selection = selection.nextSibling().toElement();
        }
     } else if ( child.tagName() == "FrameAxisScale" ) {
          // Frame Axis 
        if ( (child.hasAttribute("state")) && child.hasAttribute("scale") )
           SetFrameAxisFactor(child.attribute("scale").toFloat());
        else 
           SetFrameAxisFactor(-1.0);
     }
     child = child.nextSibling().toElement();
   }
}

//______________________________________________________________________________
void TQtGLViewerWidget::postDraw ( )
{
   QGLViewer::postDraw();
}
//______________________________________________________________________________
static QString tableLine(const QString& left, const QString& right)
{
   // borrowed from QGLViewer
  static bool even = false;
  const QString tdtd("</b></td><td>");
  const QString tdtr("</td></tr>\n");

  QString res("<tr bgcolor=\"");

  if (even)
    res += "#eeeeff\">";
  else
    res += "#ffffff\">";
  res += "<td><b>" + left + tdtd + right + tdtr;
  even = !even;

  return res;
}

//______________________________________________________________________________
QString TQtGLViewerWidget::helpString()  const
{
   //  helpString() is displayed by the help() function 
   // when the HELP_KEY (default is H) is pressed. 

//      "<b> Moves the  object:</b>"
//      "<b>u</b>  Moves down \n"
//      "<b>i</b> Moves up<br>"
//      "<br>"
//      "<b>o<b>  Shifts right<br>"
//       "<b>l</b> Shifts left<br>"
//      "<br>"
//      "<b>j</b>  Pulls the object backward<br>"
//      "<b>k</b>  Pushes the object forward<br>"
//      "<b>m</b>  turns \"SMOOTH\" color mode off<br>"
//      "<b>xX</b><br>"
//      "<b>yY</b>   rotates the object along x,y,z axis<br>"
//      "<b>zZ</b>"
    QString text("<table border=\"1\" cellspacing=\"0\">\n");
    text += "<tr bgcolor=\"#aaaacc\"><th align=\"center\">Key</th><th align=\"center\">Description</th></tr>\n";

    text += tableLine("e", "<b>Cut off</b> the view with the XY plane");
    text += tableLine("r", "Toggles the <b>Hidden</b> surface mode");
    text += tableLine("n", "Toggles the <b>\"Solid\" /   \"Translucent\" </b> color mode on ");
    text += tableLine("w", "Toggles the <b>wireframe</b> mode");
//    text += tableLine("c", "Turns the cull-face mode");
    text += tableLine("p", "Toggles the <b>perpective </b> / <b> orthographics</b> projection");

    text += "</table>";
    return text;
}

//______________________________________________________________________________
Int_t findint(const Int_t *array,  Int_t key)
{
   while (*array && ( key != *array )){ ++array; }
   return *array;
}

//______________________________________________________________________________
void TQtGLViewerWidget::keyPressEvent(QKeyEvent * e)
{
#if 0
   // Map Qt keyPressEvent to ROOT kKeyPress event
   const Int_t keys[] = {   Qt::Key_U, Qt::Key_I, Qt::Key_J, Qt::Key_K
                         ,  Qt::Key_O, Qt::Key_X, Qt::Key_Y, 0 };
   int key =  e->ascii();
   
   if (!findint(keys,key))
#endif
   {
      makeCurrent();
      switch ( e->key() ) {

         case Qt::Key_E:
            e->accept();
            fClipPlane = -fClipPlane;
            updateGL();
            break;

         case Qt::Key_R: 
            e->accept();
                 // toggle the solid view
            fSolidAttributes->SetActive(!fSolidAttributes->IsActive());
            if (!(fSolidAttributes->IsActive() || fWiredAttributes->IsActive())  )
                fWiredAttributes->SetActive();
            updateGL();
            break;

         case Qt::Key_W: 
            e->accept();
            // toggle the solid view
            fWiredAttributes->SetActive(!fWiredAttributes->IsActive());
            if (!(fSolidAttributes->IsActive() || fWiredAttributes->IsActive())  )
               fSolidAttributes->SetActive();
            updateGL();
            break;

         case Qt::Key_C: 
            e->accept();
            // toggle the cull face view
            fSolidAttributes->ToggleCullFace();
            fWiredAttributes->ToggleCullFace();
            updateGL();
            break;

         case Qt::Key_N: 
            e->accept();
            // toggle the cull face view
            fSolidAttributes->ToggleTranslucent();
            updateGL();  
            break;

        case Qt::Key_P:
           e->accept();
           // toggle the perspective/ orthographic projection
#if QGLVIEWER_VERSION <  0x020000
#  define QGL_ORTHO_CAMERA  ORTHO
#else         
#  define QGL_ORTHO_CAMERA  ORTHOGRAPHIC
#endif         
           if (camera()->type() == Camera::QGL_ORTHO_CAMERA) 
              camera()->setType(Camera::PERSPECTIVE );
           else 
              camera()->setType(Camera::QGL_ORTHO_CAMERA); 
           updateGL();
           break;
#undef QGL_ORTHO_CAMERA
       case Qt::Key_Z:
          // disable Key_Z for a while. Cause crash
          break;
       default:
 //          fprintf(stderr," TQtGLViewerWidget::keyPressEvent ascii %c \n", key);
           QGLViewer::keyPressEvent(e);
           break;
       };      
   }
#if 0
   // preserve the code for the sake of the bakcward compatibility. to be removed later
   else if (fGLView) {
      // Map "arrows" to the "ASCII" and handle it
//      Int_t asciikeymap[] = {'z','x','Z','X' };
//      Int_t indx = e->key()- Qt::Key_Left;
//      if (indx >= 0 && e->key() <= Qt::Key_Down ) key = asciikeymap[indx];
      makeCurrent();
      fGLView->ExecuteEvent(kKeyPress,key,1);
      e->accept();
   }
#endif
}
//______________________________________________________________________________
//
//    public slots:
//______________________________________________________________________________
void TQtGLViewerWidget::actionGLView(char option, int count) 
{
   // mimic keyboard action
   QKeyEvent *e = new QKeyEvent(QEvent::KeyPress,0,Qt::KeyboardModifiers(Qt::NoModifier),QChar(option)
                               ,false,ushort(1));
   QApplication::postEvent(this,e);
}
//______________________________________________________________________________
void TQtGLViewerWidget::addGLList(unsigned int list,TQtGLViewerWidget::EObjectType type)
{
   // Add one GL list to be rendered
   switch (type) {
     default: case  kSolid:
         fGLSolidLists.push_back(list);
         break;
     case  kWired:
        fGLWiredLists.push_back(list);
        break;
     case  kSelecting:
        fGLSelectingLists.push_back(list);
        break;
     case  kSelected:
        // fprintf(stderr," TQtGLViewerWidget::addGLList list=%d\n", list);
        fGLSelectedLists.push_back(list);
        break;
   };
}
//______________________________________________________________________________
void TQtGLViewerWidget::removeGLList(unsigned int list)
{   fGLSolidLists.remove(list);  fGLSolidLists.remove(list);  fGLSelectingLists.remove(list); }
//______________________________________________________________________________
void TQtGLViewerWidget::clearGLList()
{
  //  Clear all lists at once
  clearGLList(kSolid);
  clearGLList(kWired);
  clearGLList(kSelecting);
  clearGLList(kSelected);
}
//______________________________________________________________________________
void TQtGLViewerWidget::clearGLList(EObjectType type)
{
     //  Clear the selected list
    switch (type) {
      default: case kSolid:
            fGLSolidLists.clear();
            break;
      case kWired:
            fGLWiredLists.clear();
            break;
      case kSelecting:
            fGLSelectingLists.clear();
            break;
      case kSelected:
            fGLSelectedLists.clear();
            break;
   };
}

//______________________________________________________________________________
void TQtGLViewerWidget::setPadSynchronize(bool on)
{
   fPadSynch = on;
}
//______________________________________________________________________________
void TQtGLViewerWidget::setSlicing(Bool_t on)
{
  fClipPlane = on ? +1 : -1;
}
//______________________________________________________________________________
void TQtGLViewerWidget::setRotationAxisAngle(const float  x, const float  y, const float  z, const   float a)
{
   // Set the current rotation of the frame. Parameters are the rotation axis vector and its angle (in radians).   
#if QGLVIEWER_VERSION <  0x020000
   manipulatedFrame()->setRotationAxisAngle(x,y,z,a);
#else
   manipulatedFrame()->setRotation(  Quaternion (Vec(x,y,z),a)) ;
#endif
   // remember the current value to compare later
   fX = x; fY =y; fZ = z; fAngle = a;
}
//______________________________________________________________________________
void TQtGLViewerWidget::SetBackgroundColor(Color_t color)
{
   SetBackgroundColor ( gROOT->GetColor(color) ) ;

}
//______________________________________________________________________________
void TQtGLViewerWidget::SetBackgroundColor(const TColor *color)
{
   if (color) {
       makeCurrent();
       float rgb[3];
       color->GetRGB(rgb[0],rgb[1],rgb[2]);
       setBackgroundColor(QColor(int(255*rgb[0]),int(255*rgb[1]),int(255*rgb[2])));
   }
}
//______________________________________________________________________________
void TQtGLViewerWidget::setDrawLight(bool on , int light, float scale)
{
   fGLLights[light].fOn = on;
   fGLLights[light].fGLValue = GL_LIGHT0 + light;
   fGLLights[light].fScale = scale;
}

//______________________________________________________________________________
void TQtGLViewerWidget::ViewAll() 
{
   showEntireScene();
}

