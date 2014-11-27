#ifndef PIXMAPWIDGET_H
#define PIXMAPWIDGET_H

#include <QWidget>
#include <QString>

class QPixmap;

class PixmapWidget : public QWidget
{
	Q_OBJECT
	
public:
	PixmapWidget( const QPixmap &pixmap, QWidget *parent=0, Qt::WFlags f=0 );
	~PixmapWidget();

public slots:
	void setZoomFactor( float );
	void Magnify( );
   void ResetPixmap(const QPixmap &pixmap);
	
signals:
	void zoomFactorChanged( float );
   void Zoommed();

protected:
   void Init();

	void paintEvent( QPaintEvent* );
	void wheelEvent( QWheelEvent* );
   void leaveEvent(QEvent *);

private:
	QPixmap *m_pm;
	float zoomFactor;
   float fTime; // millisecond
   float fSmoothFactor;
   float fZoomStep;
};

#endif // PIXMAPWIDGET_H
