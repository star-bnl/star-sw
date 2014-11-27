#ifndef ROOT_TQtZoomPadWidget
#define ROOT_TQtZoomPadWidget
// Author: Valeri Fine   16/03/2006
/****************************************************************************
** $Id: TQtZoomPadWidget.h,v 1.6 2013/08/30 16:00:22 perev Exp $
**
** Copyright (C) 2006 by Valeri Fine. Brookhaven National Laboratory.
**                                    All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
**
*****************************************************************************/

class TVirtualPad;
class TCanvas;
class QEvent;
class QWidget;
class TQtWidget;
class QMouseEvent;
class TObject;
class QResizeEvent;

#ifdef __CINT__
#ifndef Q_MOC_RUN
   class QWidget;
#endif
#else
#include <QWidget>
#endif

#include "Buttons.h"

/////////////////////////////////////////////////////////////////////////////////////
//                                                                                 //
//  TQtZoomPadWidget                                                               //
//  TQtZoomPadWidget - create the temporary "splash" TCanvas with the magnified    //
//                     image of the selected TPad  connected to the TQtZoomPadWidget //
//                     via either ROOT or Qt signal/slot                           //
//                     One instance of the class is sufficient to serve            //
//                     the unlimited number of the TPad's                          //
//                                                                                 //
// Begin_html <img src="png/ZoomPad.png"> End_html                                 //
//                                                                                 //
// For example:                                                                    //
// ------------                                                                    //
//   TQtZoomPadWidget *zoomer = new TQtZoomPadWidget();                            //
//   c1 = new TCanvas("c1","Histogram Drawing Options",200,10,700,900);            //
//   zoomer->Connect(c1);                                                          //
//                                                                                 //
//   c2 = new TCanvas("c2","Geometry objects ",200,10,700,900);                    //
//   zoomer->Connect(c2);                                                          //
//                                                                                 //
//  Note: There is no need to create any special zoomer to connect the TCanvas     //
//  ----- to the global one. It is enough simply select the "zoomer" menu from     //
//        the "View" dropdown TCanvas menu (see picture above)                     //
//                                                                                 //
//  Slots:                                                                         //
//    virtual void Selected(TVirtualPad *pad, TObject *obj, int event);            //
//          This slot is convinient to be connected                                //
//          to the TCanvas::Selected ROOT signal                                   //
//                                                                                 //
//    virtual void RootEventProcessed(TObject *selected, unsigned int event, TCanvas *c) //
//          This slot is convinient to be connected                                //
//          to the TQtWidget::RootEventProcessed  Qt signal                        //
//                                                                                 //
/////////////////////////////////////////////////////////////////////////////////////


#ifndef __CINT__
  class  TQtZoomPadWidget : public QWidget
{   
  Q_OBJECT
#else
#ifndef Q_MOC_RUN
class  TQtZoomPadWidget
{
#endif
#endif
  private:
     TQtZoomPadWidget(const TQtZoomPadWidget &);
     void operator=(const TQtZoomPadWidget &);
     
     EEventType   fSelectingButton;  // The mlouse button to use to activa the zoomer
     bool         fSetPadInProgress; // semaphore to avoid double setting
     bool         fSmartZoomFactor;  // Flag to adjust the zoom factor on resize event
     bool         fJustOpen;         // Flag to see whther the zommer has been opened
     int          fOldWidth;         // Previous canvas size for smart zooming
     int          fOldHieght;        // Previous canvas size for smart zooming
     
  protected:
     TVirtualPad *fPad;         // The source TPad to be zoomed.
     bool         fHideOnLeave; // Hide the widget on mouse leave.
     TCanvas     *fCanvas;      // The destination TCanvas.
     float        fZoomFactor;  // The initial zoom factor.
     TQtWidget   *fSrcWidget;   // The signaling TCanvas QWidget
     unsigned int fMouseBits;   // Keep the fSrcWidget mouse event mask to restore
     TVirtualPad *fLastZoomed;  // Last TPad to be zoomed;
     bool         fIgnoreNextMotion; // The flag wehther to igomre the mouse motion events
     void         DefineTooTip(bool hideOnLeave);
 public:
#ifndef __CINT__
     TQtZoomPadWidget(TVirtualPad *pad =0, QWidget *parent=0, const char *name=0, Qt::WindowFlags f=0);
#else
#ifndef Q_MOC_RUN
     TQtZoomPadWidget(TVirtualPad *pad =0, QWidget *parent=0, const char *name=0);
#endif  
#endif  
     virtual ~TQtZoomPadWidget(){;}
     TCanvas *GetCanvas() const;
     TVirtualPad *GetPad() const;
     virtual void Show();
             bool HasSmartZoom() const;
             void ResetLastZoomed(TVirtualPad *pad=0);

protected:

     virtual void leaveEvent(QEvent *e);
     virtual void mousePressEvent(QMouseEvent *e);
     virtual void resizeEvent(QResizeEvent *e);
     
#ifndef __CINT__
   protected slots:
      virtual void CanvasEvent(TObject *, unsigned int, TCanvas *);

   public slots:
     void RootEventProcessed(TObject *selected, unsigned int event, TCanvas *c); //SLOT
#else
   public:
#endif
     void Connect(const char *canvasName);          // SLOT
     void Connect(TQtWidget  *wid);                 // SLOT
     void Connect(int        wid);                  // SLOT
     void Connect(TCanvas    *c);                   // SLOT
     void Disconnect();                             // SLOT
     void Disconnect(const char *canvasName);       // SLOT
     void Disconnect(TQtWidget  *wid);              // SLOT
     void Disconnect(int        wid);               // SLOT
     void Disconnect(TCanvas    *c);                // SLOT
     void HideOnLeave(bool on=true);                // SLOT
     void SmartZoomEnable(bool on=true);            // SLOT
     void PadModified(bool on);                     // SLOT
     void Resize(unsigned int w, unsigned int h);   // SLOT
     void SetPad(TVirtualPad *pad, bool toBeShown=true); // SLOT
     void SetSelectingButton(EEventType  button=kButton2Down);  // SLOT
     virtual void Selected(TVirtualPad *pad, TObject *obj, int event);
     void SetZoomFactor(float f);                   // SLOT
#ifndef __CINT__
   signals:
     void zoomChanged(float zoomFactor);            // *SIGNAL*
     void madeHidden();                             // *SIGNAL*
#endif         
};

inline TCanvas  *TQtZoomPadWidget::GetCanvas() const
{ return fCanvas;                                         }

inline TVirtualPad *TQtZoomPadWidget::GetPad() const
{ return fPad;                                            }

inline  bool TQtZoomPadWidget::HasSmartZoom() const
{  return fSmartZoomFactor;                              }

inline void TQtZoomPadWidget::SmartZoomEnable(bool on)
{ fSmartZoomFactor = on;                                  }

#endif
