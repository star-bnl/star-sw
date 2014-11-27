#include "TQtMarkerSelect.h"
#include "TQtMarkerSelectButton.h"

#include "TQtRootAction.h"
#include <qstring.h>


#if QT_VERSION < 0x40000
#  define  QBUTTONGROUP QButtonGroup
#  include <qbuttongroup.h>
#  include <qhbox.h>

#else /* QT_VERSION */
#  define  QBUTTONGROUP Q3ButtonGroup
#  include <q3buttongroup.h>
#  include <q3hbox.h>
#endif /* QT_VERSION */


ClassImp( TQtMarkerSelect )

/////////////////////////////////////////////////////////////////////////////////////////////////
// TQtMarkerSelect                                                                             //
/////////////////////////////////////////////////////////////////////////////////////////////////
//_____________________________________________________________________________
TQtMarkerSelect::TQtMarkerSelect ( QWidget * p, const char *name, Style_t style )
   : fSelectButton(0)
{
   //QBUTTONGROUP *group =  new  QBUTTONGROUP (2, Qt::Horizontal , p, "markerGroup");
   //group->setSizePolicy(QSizePolicy( QSizePolicy::Minimum, QSizePolicy::Fixed ));
  fSelectButton = new   TQtMarkerSelectButton(p,name,style);
  connect(fSelectButton, SIGNAL(StyleSelected (Style_t)), this, SLOT(MarkerStyleEmit( Style_t)));
}

//_____________________________________________________________________________
QWidget *TQtMarkerSelect::GetMarkerSelectButton() const { return fSelectButton;}

//_____________________________________________________________________________
Style_t TQtMarkerSelect::GetStyle()
{
   return fSelectButton ? fSelectButton->GetStyle() : 1 ;
}

//_____________________________________________________________________________
void TQtMarkerSelect::SetStyle(Style_t style)
{
   if ( fSelectButton ) fSelectButton->SetStyle(style);
}

//_____________________________________________________________________________
void TQtMarkerSelect::MarkerStyleEmit(Style_t style)
{
  emit StyleSelected (style);
  Emit("StyleSelected(style);", style);
}

#undef QBUTTONGROUP
