#ifndef ROOT_TQtMarkerSelectButton
#define ROOT_TQtMarkerSelectButton

#include "Gtypes.h"
#include <TObject.h>
#include <TQObject.h>

#include <QPoint>
#include <QToolButton>
#ifndef __CINT__
#  include  <QDialog>
#  include  <QFrame>
#else
  class QMenu;
  class QPopupMenu;
#endif  

/////////////////////////////////////////////////////////////////////////////////////////////////
// TQtMarkerFrame                                                                              //
/////////////////////////////////////////////////////////////////////////////////////////////////
class TQtMarkerFrame : public QToolButton {
Q_OBJECT

private :

   Style_t   fStyle;  // each pixmap is associated to a Style_t value
   QPixmap   fPixmap;

public :
   TQtMarkerFrame ( QWidget *p, const char * name, Style_t style );
   virtual ~TQtMarkerFrame(){}

   void SetStyle ( const Style_t style );
   Style_t GetStyle ( ) const { return fStyle ; }

protected slots :
   void clickedSlot () { emit selected(this); }

signals :
   void selected ( TQtMarkerFrame * selectedMarkerFrame );
}; // class TQtMarkerFrame


/////////////////////////////////////////////////////////////////////////////////////////////////
// TQt18MarkerSelector                                                                         //
/////////////////////////////////////////////////////////////////////////////////////////////////
class TQt18MarkerSelector : public QDialog {
Q_OBJECT

public :
   TQt18MarkerSelector( QWidget *p,Qt::WindowFlags f=  Qt::WStyle_NoBorder |
#if QT_VERSION < 0x50000
   Qt::WStyle_Customize | Qt::WStyle_StaysOnTop
#else
   Qt::WindowStyle_Customize | Qt::WindowStyle_StaysOnTop
#endif   
);
   virtual ~TQt18MarkerSelector(){}
   void showSelector ( const QPoint & position );

protected slots :
   void selectedSlot ( TQtMarkerFrame * selectedMarkerFrame );

signals :
   void selected ( TQtMarkerFrame * selectedMarkerFrame );

}; // class TQt18MarkerSelector

class QMenu;
/////////////////////////////////////////////////////////////////////////////////////////////////
// TQtMarkerSelectButton                                                                              //
/////////////////////////////////////////////////////////////////////////////////////////////////
class TQtMarkerSelectButton : public QFrame {
Q_OBJECT
private :
   TQtMarkerFrame      * fSelected ;
   TQt18MarkerSelector * fPopup ;
   QMenu               * fFakeMenu;
   TQtMarkerSelectButton(const TQtMarkerSelectButton&);
   TQtMarkerSelectButton &operator=(const TQtMarkerSelectButton&);

protected slots :

   void selectedSlot ( TQtMarkerFrame * selectedMarkerFrame );

   void showPopup    ();

public :
   TQtMarkerSelectButton ( QWidget * p = 0, const char * name = "" , Style_t style = 1 );
   virtual ~TQtMarkerSelectButton () {}

   virtual void MarkerStyleEmit(Style_t style);  // *SIGNAL*

   Style_t GetStyle();
   void    SetStyle(Style_t style);

signals :
   void StyleSelected ( Style_t markerStyle );
}; // class TQtMarkerSelectButton 
#endif // #ifndef ROOT_TQtMarkerSelectButton
