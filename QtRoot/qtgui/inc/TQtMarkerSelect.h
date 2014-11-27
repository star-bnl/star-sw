#ifndef ROOT_TQtMarkerSelect
#define ROOT_TQtMarkerSelect

#include "Gtypes.h"
#include <TObject.h>
#include <TQObject.h>



#ifndef __CINT__
#include <QPoint>
#include <QObject>

#endif // ifndef __CINT__

class TQtMarkerSelectButton;

/////////////////////////////////////////////////////////////////////////////////////////////////
// TQtMarkerEmit                                                                               //
/////////////////////////////////////////////////////////////////////////////////////////////////
class TMarkerStyleEmit {
public:
   TMarkerStyleEmit(){}
   virtual ~TMarkerStyleEmit(){}
   virtual  void MarkerStyleEmit(Style_t ) { }
};


/////////////////////////////////////////////////////////////////////////////////////////////////
// TQtStyleSelect                                                                              //
/////////////////////////////////////////////////////////////////////////////////////////////////
class TQtMarkerSelect :
#ifndef __CINT__
   public QObject, 
#endif
   public TObject, public TQObject, public TMarkerStyleEmit {
#ifndef __CINT__
Q_OBJECT
#endif // #ifndef __CINT__
private :
   TQtMarkerSelectButton *fSelectButton; 
   TQtMarkerSelect(const TQtMarkerSelect&);
   TQtMarkerSelect &operator=(const TQtMarkerSelect&);

#ifndef __CINT__
protected slots :
#endif // #ifndef __CINT__

   virtual void MarkerStyleEmit(Style_t style);  // *SIGNAL*

public :
   TQtMarkerSelect ( QWidget * p = 0, const char * name = "" , Style_t style = 1 );
   virtual ~TQtMarkerSelect () {}

   QWidget *GetMarkerSelectButton() const;

   Style_t GetStyle();
   void    SetStyle(Style_t style);

#ifndef __CINT__
signals :
   void StyleSelected ( Style_t markerStyle );
#endif // #ifndef __CINT__
#ifndef Q_MOC_RUN
//MOC_SKIP_BEGIN
   ClassDef ( TQtMarkerSelect, 0 ) // style selection checkbutton
//MOC_SKIP_END
#endif
}; // class TQtMarkerSelectButton 

#endif
 
