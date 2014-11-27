#include "TQtMarkerSelectButton.h"

#include "TQtGui.h"
#include "TAttMarker.h"
#include <qstring.h>

#if QT_VERSION < 0x40000
#  include <qbuttongroup.h>
#  include <qlayout.h>
#  include <qtooltip.h>
#else /* QT_VERSION */
#  include <QHBoxLayout>
#  include <QVBoxLayout>
#  include <QMenu>
#  include <QTimer>
#endif /* QT_VERSION */

/////////////////////////////////////////////////////////////////////////////////////////////////
// TQtMarkerFrame                                                                              //
/////////////////////////////////////////////////////////////////////////////////////////////////
//__________________________________________________________________________________
TQtMarkerFrame::TQtMarkerFrame ( QWidget *p, const char * name, Style_t style )
   : QToolButton (p, name),
     fStyle (-1),
     fPixmap()
{
//   std::cout << "TQtMarkerFrame constructor "<< (QString("marker")  + QString::number(shape) + ".xpm" )).ascii() << std::endl ;
   SetStyle(style);
   connect (this , SIGNAL(clicked()), this, SLOT(clickedSlot()));
}

//__________________________________________________________________________________
void TQtMarkerFrame::SetStyle ( const Style_t style )
{
   if ( fStyle != style )
   {
      fStyle  = style;
      fPixmap = TQtGui::GetPicture( QString("marker")  + QString::number(fStyle) + ".xpm" ) ;
      // set Tool tip
#if QT_VERSION < 0x40000
      setPixmap ( fPixmap );
      QToolTip::add(this,QString("ROOT marker style %1").arg(style));
#else
      setIcon(fPixmap);
      setToolTip(QString("ROOT marker style %1").arg(style));
#endif            
   }
}

/////////////////////////////////////////////////////////////////////////////////////////////////
// TQt18MarkerSelector                                                                         //
/////////////////////////////////////////////////////////////////////////////////////////////////
//__________________________________________________________________________________
TQt18MarkerSelector::TQt18MarkerSelector( QWidget * p,Qt::WindowFlags f) :
#if QT_VERSION < 0x40000
   QDialog (p,0,0,f)
#else
   QDialog (p,f)
#endif
{
   setModal(true);
   QFrame *inter = new QFrame(this);
   QVBoxLayout *vLayout = new QVBoxLayout(this);
   vLayout->addWidget(inter);
   vLayout->setMargin(1);
   inter->setFrameShape(QFrame::Panel);
   QWidget *group            = inter;
   QGridLayout *gridLayout   = new QGridLayout(group);
   
   gridLayout->setMargin   ( 1 );  
   gridLayout->setSpacing  ( 0 ); 
   EMarkerStyle styles[] = {kDot,           kPlus,          kStar
                          , kCircle,        kMultiply,      kFullDotSmall
                          , kFullDotMedium, kFullDotLarge,  kFullCircle
                          , kFullSquare,    kFullTriangleUp,kFullTriangleDown
                          , kOpenCircle,    kOpenSquare,    kOpenTriangleUp
                          , kOpenDiamond,   kOpenCross,     kFullStar
                          , kOpenStar,      kPlus,          kStar
                           };
   
   TQtMarkerFrame * frame = 0;
   int nStyles =  sizeof(styles)/sizeof(EMarkerStyle);
   int i = 0; int j = 0;
   while (i< nStyles) { 
     for (int k =0; k < 3 && i< nStyles ;k++,i++) {
        frame = new TQtMarkerFrame ( group, "", styles[i]); 
        connect ( frame, SIGNAL (selected( TQtMarkerFrame * )), this, SLOT( selectedSlot ( TQtMarkerFrame * )) );
        frame->setSizePolicy(QSizePolicy( QSizePolicy::Fixed, QSizePolicy::Minimum ));
        gridLayout->addWidget(frame,j,k);
     }
     j++;
   }
}

//__________________________________________________________________________________
void TQt18MarkerSelector::selectedSlot ( TQtMarkerFrame * selectedMarkerFrame )
{
   // close();
   accept();
   emit selected(selectedMarkerFrame);
}

//__________________________________________________________________________________
void TQt18MarkerSelector::showSelector( const QPoint & position)
{
   // popup(position);
   move(position);
   exec();
}

//__________________________________________________________________________________
TQtMarkerSelectButton::TQtMarkerSelectButton ( QWidget * p, const char *, Style_t style )
   : QFrame(p)
   , fSelected(0)
   , fPopup   (0)
   , fFakeMenu(0)
{ 
   QHBoxLayout *hbox = new QHBoxLayout(this);
   hbox->setMargin (0);
   hbox->setSpacing(0);
   
   setSizePolicy(QSizePolicy( QSizePolicy::Fixed, QSizePolicy::Minimum ));

   fSelected = new TQtMarkerFrame (this,"selectedMarker",style);
   hbox->addWidget(fSelected);
   connect ( fSelected , SIGNAL ( clicked ( ))                  , this , SLOT ( showPopup()    )) ;
#if QT_VERSION >= 0x40000
                 fSelected->setPopupMode(QToolButton::MenuButtonPopup);
#else   
   QToolButton * arrow     = new QToolButton( Qt::DownArrow,this,"arrowDownToolButton" );
   hbox->addWidget(arrow);
   connect ( arrow     , SIGNAL ( clicked ( ))                  , this , SLOT ( showPopup()    )) ;
   arrow->setFixedWidth(arrow->sizeHint().width()+4);
   arrow->setSizePolicy(QSizePolicy( QSizePolicy::Fixed, QSizePolicy::Minimum ));
#endif

   fPopup    = new TQt18MarkerSelector(this); //,"18markerSelector");

   connect ( fPopup    , SIGNAL ( selected ( TQtMarkerFrame * )), this , SLOT ( selectedSlot(TQtMarkerFrame * ) )) ;
   fSelected->setSizePolicy(QSizePolicy( QSizePolicy::Fixed, QSizePolicy::Minimum ));
#if QT_VERSION >= 0x40000
   if (!fFakeMenu) {
       // Add fake menu to force the "real" Dialog popup
       fFakeMenu = new QMenu(this);
       fSelected->setMenu(fFakeMenu);
       connect(fFakeMenu,SIGNAL(aboutToShow()), this, SLOT(showPopup()));
    }
#endif
}

//__________________________________________________________________________________
void TQtMarkerSelectButton::selectedSlot( TQtMarkerFrame * selectedMarkerFrame )
{
   Style_t style = selectedMarkerFrame->GetStyle();
   if ( style != fSelected->GetStyle() ) {
      fSelected->SetStyle ( style );
      MarkerStyleEmit(style);
   }
}

//__________________________________________________________________________________
Style_t TQtMarkerSelectButton::GetStyle()
{
   return fSelected ? fSelected->GetStyle() : 1 ;
}

//__________________________________________________________________________________
void TQtMarkerSelectButton::SetStyle(Style_t style)
{
   if ( fSelected )   fSelected->SetStyle(style);
}

//__________________________________________________________________________________
void TQtMarkerSelectButton::showPopup()
{
   fPopup->adjustSize();
#if QT_VERSION < 0x40000
   fPopup->showSelector( fSelected->mapToGlobal(fSelected->pos()+QPoint(fSelected->width(),fSelected->height())) ); // QWidget::mapToGlobal().
#else
   fPopup->showSelector( fSelected->mapToGlobal(fSelected->pos()+QPoint(-10,fSelected->height())) ); // QWidget::mapToGlobal().
   if (fFakeMenu && (sender() == fFakeMenu) ) QTimer::singleShot(0,fFakeMenu, SLOT(close()) );
#endif
}

//__________________________________________________________________________________
void TQtMarkerSelectButton::MarkerStyleEmit(Style_t style)
{
  emit StyleSelected (style);
}
