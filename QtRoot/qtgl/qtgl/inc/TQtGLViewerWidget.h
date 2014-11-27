
// @(#)root/gtgl:$Name:  $:$Id: TQtGLViewerWidget.h,v 1.9 2013/08/30 16:00:17 perev Exp $
// Author: Valery Fine      23/10/03

#ifndef ROOT_TQtGLViewerWidget_
#define ROOT_TQtGLViewerWidget_

/****************************************************************************
** $Id: TQtGLViewerWidget.h,v 1.9 2013/08/30 16:00:17 perev Exp $
**
** Copyright (C) 2002 by Valeri Fine.  All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
**
*****************************************************************************/


//////////////////////////////////////////////////////////////////////////
//                                                                      //
// TQtGLViewerWidget                                                    //
//                                                                      //
//  TQtGLViewerWidget specifies window system independent openGL        //
//  interface.                                                          //
//                                                                      //
//    This the ROOT GL veiwer implmentation based on                    //
//                l i b Q G L V i e w e r                               //
//           Copyright c Gilles Debunne 2002-03                         //
//                Gilles.Debunne@imag.fr                                //
//   http://www-imagis.imag.fr/Membres/Gilles.Debunne/CODE/QGLViewer/   //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "Rtypes.h"
#include "Gtypes.h"

#ifndef __CINT__
#  include  "TQtGLIncludes.h"
// Hide the Qt header files from CINT to build the RootCint Dictionary
#  include "QGLViewer/qglviewer.h"
#  include <QString>
#  include <QFont>
#else
  class QWidget;
  class QGLWidget;
  class QString;
  class QFont;
  class QKeyEvent;
  class QPoint;
  typedef int GLuint;
  class QGLViewer;
  class QDomElement;
# define slots
#endif
#include <list>

class TColor;
class TPadOpenGLView;
class TVirtualPad;
class TObject;

//____________________________________________________________________________________________________________________
class TGLAttributiteList {
  private:
    GLuint fGList;
    bool   fIsActive;
    bool   fUseObjects;

  protected:
    virtual void MakeAttribute() = 0;
    inline  void OpenList()  const  { glNewList(fGList, GL_COMPILE);     }
    inline  void CloseList() const  { glEndList();                       }

  public:
    TGLAttributiteList() : fGList(glGenLists(1)),fIsActive(FALSE),fUseObjects(TRUE){}
    virtual ~TGLAttributiteList()  { glDeleteLists(fGList,1);           }
    inline  bool IsActive()  const { return fIsActive;                  }
    inline  void RunList()   const {     glCallList(fGList);            }
    inline  void RunList(const std::list<unsigned int> &objectList)   const
    { 
       {
          SaveAttributes();
            RunList();
            if (fUseObjects) RunObjectList(objectList);
          RestoreAttributes();
       }
    }
    virtual void RestoreAttributes() const;
    virtual void SaveAttributes()    const ;
    inline  void SetActive (bool on = TRUE)     {fIsActive   = on;          }
    inline  void SetUseObjects (bool on = TRUE) {fUseObjects = on;          }
    inline  void MakeAttributeList(){OpenList();MakeAttribute();CloseList();}
    virtual void RunObjectList(const std::list<unsigned int> &objectList) const;
};

class TGLSolidAttribute;
class TGLWiredAttribute;
class TGLSelectingAttribute;
class TGLSlicePlaneAttribute;
class QDomDocument;
//____________________________________________________________________________________________________________________
class TQtGLViewerWidget : public QGLViewer {
   Q_OBJECT
private:
   TQtGLViewerWidget(const TQtGLViewerWidget&):QGLViewer() {}
   void operator=(const TQtGLViewerWidget&){}
protected:
      struct GLLIGHT {
        bool         fOn; // light on
        unsigned int fGLValue;
        float        fScale;
      };
   TPadOpenGLView        *fGLView;         // Abstract view pointer.
   TVirtualPad           *fPad;            // Altranative the current TPad pointer.
   Int_t                  fClipPlane;      // Slicing plane flag;
   Bool_t                 fPadSynch;       // GL Widget view should follow TPad rotation
   std::list<unsigned int> fGLWiredLists;       // The array of the glList to render the wired objects;
   std::list<unsigned int> fGLSolidLists;       // The array of the glList to render the solid objects;
   std::list<unsigned int> fGLSelectingLists;   // The array of the glList to select some objects;
   std::list<unsigned int> fGLSelectedLists;    // The array of the selected objects glList ;
   std::list<TGLAttributiteList *>  fGLAttributesLists; // The array of the glList to set the GL attributes
   //--  rotation parameters to quire later --- temporary
   float fX;
   float fY;
   float fZ;
   float fAngle;
   GLLIGHT fGLLights[8];
   // Footer parameters:
   QString fFooterText;
   QFont   fFooterFont;
   TGLSolidAttribute      *fSolidAttributes;         // Define the GL attributes to render the solid objects
   TGLWiredAttribute      *fWiredAttributes;         // Define the GL attributes to render the wired objects
   TGLSelectingAttribute  *fSelectingAttributes;     // Define the GL attributes to select the objects.
   TGLSelectingAttribute  *fSelectedAttributes;      // Define the GL attributes to render the selected objects.
   TGLSlicePlaneAttribute *fSlicingAttributes;       // Define the GL attributes to render "slicing" plane.
   QString  fStateFileName;
   Bool_t   fSolidSelectable;     // flag whether to objects from fGLSolidLists are selectable
   Bool_t   fWiredSelectable;     // flag whether to objects from fGLWiredLists are selectable
   Float_t  fFrameAxisFactor;     // the scale of the frame axis ( <0 - no axis at all)
   bool     fEnableObjectPick;    // Should we pick the TObject ?
   
protected:
   void    drawFooter();
   void    drawGLLights();
   virtual void init();
   virtual QDomElement domElement(const QString &name, QDomDocument &document) const;
   virtual void draw();
   virtual void drawWithNames();
   virtual void drawSelectedObject();
   virtual void postDraw();
   virtual void postSelection (const QPoint &point);
    
public:
   // kWired is applied to the dots and lines, kSolid for the rest kind of the objects
   enum  EObjectType { kWired, kSolid, kSelecting, kSelected, kRaw };
//   TQtGLViewerWidget();
#ifndef __CINT__
   TQtGLViewerWidget(QWidget *parent=0, const char *name=0,const QGLWidget *shareWidget=0, Qt::WindowFlags f=0);
   TQtGLViewerWidget(TPadOpenGLView *view, const char *title="OpenGL Viewer",QWidget *parent=0,Qt::WindowFlags f = 0);
   TQtGLViewerWidget(TVirtualPad *pad, const char *title="OpenGL Viewer",QWidget *parent=0,Qt::WindowFlags f = 0);
#else
//MOC_SKIP_BEGIN
   TQtGLViewerWidget(QWidget *parent=0, const char *name=0,const QGLWidget *shareWidget=0);
   TQtGLViewerWidget(TPadOpenGLView *view, const char *title="OpenGL Viewer",QWidget *parent=0);
   TQtGLViewerWidget(TVirtualPad *pad, const char *title="OpenGL Viewer",QWidget *parent=0);
//MOC_SKIP_END
#endif
   virtual ~TQtGLViewerWidget();

   //   virtual void ShowHelp();
   //  QGLWidget virtual methods;

         virtual QString helpString()  const;
   //
   virtual void keyPressEvent(QKeyEvent * e);
   const QString &footerText() const { return fFooterText;       }
   const QFont   &footerFont() const { return fFooterFont;       }
   inline Bool_t  IsWiredSelectable() const { return fWiredSelectable;  }
   inline Bool_t  IsSolidSelectable() const { return fSolidSelectable;  }
   inline Bool_t  IsSlicingSelected() const { return fClipPlane > 0 ? true: false;  }
   inline Float_t FrameAxisScale()    const { return fFrameAxisFactor;  }
          bool    ObjectPickEnabled() const { return  fEnableObjectPick;}
   
public slots:
   virtual void actionGLView(char option, int count=1);
   virtual void setDrawLight(bool on = true, int light = 0, float scale = 1.0);
   virtual void addGLList(unsigned int list, EObjectType type=kSolid);
   virtual void clearGLList();
   virtual void clearGLList(EObjectType type);
   virtual void EnableObjectPick(bool enable=true)  { fEnableObjectPick = enable; }
   virtual void initFromDOMElement(const QDomElement &element);
   virtual void removeGLList(unsigned int list);
   virtual void setFooter(QString &text){ fFooterText = text;}
   virtual void setFooter(QFont &font)  { fFooterFont = font;}
   virtual void setPadSynchronize(bool on=true);
   virtual void setRotationAxisAngle(const float  x, const float  y, const float  z, const   float a);
           float RotationAxisAngle(float  &x, float  &y, float  &z) const;
   virtual void setSlicing(Bool_t on=kTRUE);
   virtual void SetBackgroundColor(Color_t color);
   virtual void SetBackgroundColor(const TColor *color); 
   virtual void SetFrameAxisFactor(Float_t axisScale=5.0);
   virtual void SetSceneLayout();
   virtual void SetSceneLayout(Float_t *minBound,Float_t *maxBound, Color_t color=kBlack);
   virtual void SetWiredSelectable(Bool_t on=kTRUE);
   virtual void SetSolidSelectable(Bool_t on=kTRUE);
   virtual void ViewAll();


#ifndef __CINT__
signals:
      void viewerAbout2Close();
      void objectSelected(TObject *,const QPoint &);
#endif 
};
//_____________________________________________________________________________________________________
inline   float TQtGLViewerWidget::RotationAxisAngle(float  &x, float  &y, float  &z) const 
{ x = fX; y=fY; z=fZ; return fAngle;  }

//_____________________________________________________________________________________________________
inline void TQtGLViewerWidget::SetWiredSelectable(Bool_t on) { fWiredSelectable = on; }
//_____________________________________________________________________________________________________
inline void TQtGLViewerWidget::SetSolidSelectable(Bool_t on) { fSolidSelectable = on; }
//_____________________________________________________________________________________________________
inline void TQtGLViewerWidget::SetFrameAxisFactor(Float_t axisScale) { fFrameAxisFactor = axisScale; }

// $log$
#endif
