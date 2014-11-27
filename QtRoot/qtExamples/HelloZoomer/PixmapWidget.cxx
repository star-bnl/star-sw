#include "PixmapWidget.h"
#include <QPixmap>
#include <QPainter>
#include <QEvent>
#include <QTimer>
#include <QWheelEvent>

//_____________________________________________________________________
PixmapWidget::PixmapWidget( const QPixmap &pixmap, QWidget *parent , Qt::WFlags f)
: QWidget( parent,f ), m_pm(0)
{
	ResetPixmap(pixmap);
}
 
//_____________________________________________________________________
void PixmapWidget::Init()
{
#if QT_VERSION < 0x40000
   setBackgroundMode(Qt::NoBackground);
#endif   
	zoomFactor = 0.1f; // 1.2;
	fZoomStep = 0.05f; //fTime/fSmoothFactor;
	setMinimumSize( int(m_pm->width()*zoomFactor), int(m_pm->height()*zoomFactor) );
   resize(400,400);
}

//_____________________________________________________________________
PixmapWidget::~PixmapWidget()
{
	delete m_pm;
}
//_____________________________________________________________________
void PixmapWidget::ResetPixmap(const QPixmap &pixmap)
{
   if (m_pm) { delete m_pm; m_pm = 0; }
   m_pm = new QPixmap(pixmap);
   Init();

}

//_____________________________________________________________________
void PixmapWidget::setZoomFactor( float f )
{
	int w, h;
	
	if( f == zoomFactor )
		return;

	zoomFactor = f;
	emit( zoomFactorChanged( zoomFactor ) );

	w = (int)(m_pm->width()*zoomFactor);
	h = (int)(m_pm->height()*zoomFactor);
	setMinimumSize( w, h );
	
	QWidget *p = dynamic_cast<QWidget*>( parent() );
	if( p )
		resize( p->width(), p->height() );
	
	repaint();
}

//_____________________________________________________________________
void PixmapWidget::paintEvent( QPaintEvent * /*event*/  )
{
   int xoffset, yoffset;
   bool drawBorder = false;
   if( width() > m_pm->width()*zoomFactor )
   {
      xoffset = (int)(width()-m_pm->width()*zoomFactor)/2;
      drawBorder = true;
   }
   else
   {
      xoffset = 0;
   }

   if( height() > m_pm->height()*zoomFactor )
   {
      yoffset = (int)(height()-m_pm->height()*zoomFactor)/2;
      drawBorder = true;
   }
   else
   {
      yoffset = 0;
   }

   QPainter p( this );
   p.save();
   p.translate( xoffset, yoffset );
   p.scale( zoomFactor, zoomFactor );
   p.drawPixmap( 0, 0, *m_pm );
   p.restore();

   if( drawBorder )
   {
      p.setPen( Qt::black );
      p.drawRect( xoffset-1, yoffset-1, (int)(m_pm->width()*zoomFactor+1), (int)(m_pm->height()*zoomFactor+1) );
   }
   if (zoomFactor < 1.) QTimer::singleShot(50,this,SLOT(Magnify()));
}

//_____________________________________________________________________
void PixmapWidget::leaveEvent (QEvent *)
{
   hide();
   // close(true);
}


//_____________________________________________________________________
void PixmapWidget::wheelEvent( QWheelEvent *event )
{
	float f;

	f = zoomFactor + 0.001*event->delta();
	if( f < 32.0/m_pm->width() )
		f = 32.0/m_pm->width();

	setZoomFactor( f );
}

//_____________________________________________________________________
void PixmapWidget::Magnify() 
{
    setZoomFactor(zoomFactor+fZoomStep);
}
