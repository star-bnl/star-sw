/*  Dark !!!
QObject::disconnect: No such slot XFormCenter::cleanupEventFilter(QObject*)
QObject::disconnect:  (sender name:   'unnamed')
QObject::disconnect:  (receiver name: 'unnamed')
*/

/****************************************************************************
** $Id: xform.cpp,v 1.3 2013/08/30 16:00:30 perev Exp $
**
** Copyright (C) 1992-2000 Trolltech AS.  All rights reserved.
**
** This file is part of an example program for Qt.  This example
** program may be used, distributed and modified without limitation.
**
*****************************************************************************/

#include <qapplication.h>

#include <qdialog.h>
#include <qlabel.h>
#include <qlineedit.h>
#include <qpushbutton.h>
#include <qcheckbox.h>
#include <qradiobutton.h>
#include <qbuttongroup.h>
#include <qlcdnumber.h>
#include <qslider.h>
#include <qmenubar.h>
#include <qfontdialog.h>
#include <qlayout.h>
#include <qvbox.h>
#include <qwidgetstack.h>
#include <qtooltip.h> 
#include <qmessagebox.h>
#include <qcombobox.h>
#include <qpainter.h>
#include <qpixmap.h>
#include <qpicture.h>

#include <stdlib.h>

#include "TROOT.h"
#include "TApplication.h"
#include "TPad.h"
#include "TGQt.h"
#include "TSystem.h"
#include "TCanvas.h"
#include "TQtWidget.h"
#include "ProgramPath.h"
class ModeNames {
public:
  enum Mode { Text, Image, Picture, RootCanvas, RootPad };
};


class XFormControl : public QVBox, public ModeNames
{
  Q_OBJECT
public:
  XFormControl( const QFont &initialFont, QWidget *parent=0, const char *name=0 );
  ~XFormControl() {}
  
  QWMatrix matrix();
  
signals:
  void newMatrix( QWMatrix );
  void newText( const QString& );
  void newFont( const QFont & );
  void newMode( int );
private slots:
  void newMtx();
  void newTxt(const QString&);
  void selectFont();
  void fontSelected( const QFont & );
  void changeMode(int);
  void timerEvent(QTimerEvent*);
  void execRoot();
  void updateMirror(bool);
private:
  Mode mode;
  QSlider	 *rotS;		       // Rotation angle scroll bar
  QSlider	 *shearS;	       // Shear value scroll bar
  QSlider	 *magS;		       // Magnification value scroll bar
  QLCDNumber	 *rotLCD;	       // Rotation angle LCD display
  QLCDNumber	 *shearLCD;	       // Shear value LCD display
  QLCDNumber	 *magLCD;	       // Magnification value LCD display
  QCheckBox	 *mirror;	       // Checkbox for mirror image on/of
  QWidgetStack* optionals;
  QLineEdit	 *textEd;	       // Inp[ut field for xForm text
  QComboBox	 *rootCommand;	   // Inp[ut field for ROOT command

  QPushButton  *fpb;		       // Select font push button
  QRadioButton *rb_txt;	       // Radio button for text
  QRadioButton *rb_img;	       // Radio button for image
  QRadioButton *rb_pic;	       // Radio button for picture
  QRadioButton *rb_can;	       // Radio button for TCanvas
  QRadioButton *rb_pad;	       // Radio button for TPad
  QFont currentFont;
};

/*
ShowXForm displays a text or a pixmap (QPixmap) using a coordinate
transformation matrix (QWMatrix)
*/

class ShowXForm : public QWidget, public ModeNames
{
  Q_OBJECT
public:
  ShowXForm( const QFont &f, QWidget *parent=0, const char *name=0 );
  ~ShowXForm() {}
  void showIt();			// (Re)displays text or pixmap
  
  Mode mode() const { return m; }
public slots:
  void setText( const QString& );
  void setMatrix( QWMatrix );
  void setFont( const QFont &f );
  void setPixmap( QPixmap );
  void setPicture( const QPicture& );
  void setMode( int );
  void updatePad(QPixmap*);
private:
  QSizePolicy sizePolicy() const;
  QSize sizeHint() const;
  void paintEvent( QPaintEvent * );
  void resizeEvent( QResizeEvent * );
  QWMatrix  mtx;			// coordinate transform matrix
  QString   text;			// text to be displayed
  QPixmap   pix;			// pixmap to be displayed
  QPicture  picture;			// text to be displayed
  QRect     eraseRect;		// covers last displayed text/pixmap
  Mode      m;
};

XFormControl::XFormControl( const QFont &initialFont,
                           QWidget *parent, const char *name )
                           : QVBox( parent, name )
{
  setSpacing(6);
  setMargin(6);
  currentFont = initialFont;
  mode = Image;
  
  rotLCD	= new QLCDNumber( 4, this, "rotateLCD" );
  rotS	= new QSlider( QSlider::Horizontal, this,
    "rotateSlider" );
  shearLCD	= new QLCDNumber( 5,this, "shearLCD" );
  shearS	= new QSlider( QSlider::Horizontal, this,
    "shearSlider" );
  mirror	= new QCheckBox( this, "mirrorCheckBox" );
  rb_txt = new QRadioButton( this, "text" );
  rb_img = new QRadioButton( this, "image" );
  rb_pic = new QRadioButton( this, "picture" );

  rb_pad = new QRadioButton( this, "pad" );
  QToolTip::add(rb_pad,"Select and click any ROOT tool button from the ROOT ToolBar to activate ROOT");

  rb_can = new QRadioButton( this, "canvas" );
  QToolTip::add(rb_can,"Select \"canvas\" to activate \"Embedded\" canvas");
  connect(rb_can,SIGNAL(toggled(bool)),this,SLOT(updateMirror(bool)));
 
  optionals = new QWidgetStack(this);
  QVBox* optionals_text = new QVBox(optionals);
  optionals_text->setSpacing(6);

  QVBox* optionals_other = new QVBox(optionals);
  optionals_other->setSpacing(6);

  QVBox* optionals_root = new QVBox(optionals);
  optionals_root->setSpacing(6);
  
  optionals->addWidget(optionals_text,0);
  optionals->addWidget(optionals_other,1);
  optionals->addWidget(optionals_root,2);
  fpb		= new QPushButton( optionals_text, "text" );
  textEd	= new QLineEdit( optionals_text, "text" );
  textEd->setFocus();
 
  
  rootCommand	= new QComboBox( optionals_root, "rootCommand" );
  rootCommand->setInsertionPolicy (QComboBox::AtTop);
  rootCommand->setEditable(TRUE);
  rootCommand->setDuplicatesEnabled(FALSE);
  rootCommand->setFocus();
  QToolTip::add(rootCommand,"Type any ROOT (C++ statement) command here");

  rotLCD->display( "  0'" );
  
  rotS->setRange( -180, 180 );
  rotS->setValue( 0 );
  connect( rotS, SIGNAL(valueChanged(int)), SLOT(newMtx()) );
  
  shearLCD->display( "0.00" );
  
  shearS->setRange( -25, 25 );
  shearS->setValue( 0 );
  connect( shearS, SIGNAL(valueChanged(int)), SLOT(newMtx()) );
  
  mirror->setText( tr("Mirror") );
  connect( mirror, SIGNAL(clicked()), SLOT(newMtx()) );
  
  QButtonGroup *bg = new QButtonGroup(this);
  bg->hide();
  bg->insert(rb_txt,0);
  bg->insert(rb_img,1);
  bg->insert(rb_pic,2);
  
  bg->insert(rb_can,3);
  bg->insert(rb_pad,4);
  
  rb_txt->setText( tr("Text") );
  rb_img->setText( tr("Image") );
  rb_img->setChecked(TRUE);
  rb_pic->setText( tr("Picture") );
  
  rb_can->setText( tr("TCanvas") );
  rb_pad->setText( tr("gPad - current TPad") );
  
  connect( bg, SIGNAL(clicked(int)), SLOT(changeMode(int)) );
  
  fpb->setText( tr("Select font...") );
  connect( fpb, SIGNAL(clicked()), SLOT(selectFont()) );
  
  textEd->setText( "Troll" );
  connect( textEd, SIGNAL(textChanged(const QString&)),
    SLOT(newTxt(const QString&)) );

  rootCommand->insertItem("gROOT->Macro(\"qzdemo.CC(gPad)\");");
  rootCommand->insertItem("gROOT->Macro(\"qcanvas.CC(gPad)\");");
  
  connect(rootCommand->lineEdit(),SIGNAL(returnPressed ()), SLOT( execRoot()) );
  
  magLCD = new QLCDNumber( 4,optionals_other, "magLCD" );
  magLCD->display( "100" );
  magS = new QSlider( QSlider::Horizontal, optionals_other,
    "magnifySlider" );
  magS->setRange( 0, 800 );
  connect( magS, SIGNAL(valueChanged(int)), SLOT(newMtx()) );
  magS->setValue( 0 );
  connect( magS, SIGNAL(valueChanged(int)), magLCD, SLOT(display(int)));
  
  optionals_text->adjustSize();
  optionals_other->adjustSize();
  optionals_root->adjustSize();

  changeMode(Image);
  
  startTimer(20); // start an initial animation
}

void XFormControl::timerEvent(QTimerEvent*)
{
  int v = magS->value();
  v = (v+2)+v/10;
  if ( v >= 200 ) {
    v = 200;
    killTimers();
  }
  magS->setValue(v);
}

//________________________________________________________
void XFormControl::updateMirror(bool canvasOn)
{
  // disable "mirror"/ "shear and rotation check boxes  when TCanvas option is selected
  mirror->setEnabled (!canvasOn);
  rotS  ->setEnabled (!canvasOn);
  shearS->setEnabled (!canvasOn);
}


/*
    Called whenever the user has changed one of the matrix parameters
    (i.e. rotate, shear or magnification)
*/
void XFormControl::newMtx()
{
  emit newMatrix( matrix() );
}

/*
    Called whenever the user hits "return" key 
    to complete ROOT command
*/
void XFormControl::execRoot()
{
  // rootCanvas->GetCanvas()->cd();
  gROOT->ProcessLine(rootCommand->lineEdit()->text());
  gROOT->ProcessLine("gPad->Update();");
}

void XFormControl::newTxt(const QString& s)
{
  emit newText(s);
  changeMode(Text);
}

/*
    Calculates the matrix appropriate for the current controls,
    and updates the displays.
*/
QWMatrix XFormControl::matrix()
{
  QWMatrix m;
  if (mode != Text) {
    double magVal = 1.0*magS->value()/100;
    m.scale( magVal, magVal );
  }
  double shearVal = 1.0*shearS->value()/25;
  m.shear( shearVal, shearVal );
  m.rotate( rotS->value() );
  if ( mirror->isChecked() ) {
    m.scale( 1, -1 );
    m.rotate( 180 );
  }
  
  QString tmp;
  tmp.sprintf( "%1.2f", shearVal  );
  if ( shearVal >= 0 )
    tmp.insert( 0, " " );
  shearLCD->display( tmp );
  
  int rot = rotS->value();
  if ( rot < 0 )
    rot = rot + 360;
  tmp.sprintf( "%3i'", rot );
  rotLCD->display( tmp );
  return m;
}


void XFormControl::selectFont()
{
  bool ok;
  QFont f = QFontDialog::getFont( &ok, currentFont );
  if ( ok ) {
    currentFont = f;
    fontSelected( f );
  }
}

void XFormControl::fontSelected( const QFont &font )
{
  emit newFont( font );
  changeMode(Text);
}

/*
    Sets the mode - Text, Image, or Picture.
*/

void XFormControl::changeMode(int m)
{
  mode = (Mode)m;
  
  emit newMode( m );
  newMtx();
  if ( mode == Text ) {
    optionals->raiseWidget(0);
    rb_txt->setChecked(TRUE);
  } else if (mode == RootCanvas) {
    optionals->raiseWidget(2);
    rb_can->setChecked(TRUE); 
  } else {
    optionals->raiseWidget(1);
    switch (mode) {
    case Image:      rb_img->setChecked(TRUE); break;
    case RootPad:    rb_pad->setChecked(TRUE); break;
    case Picture:
    default:  rb_pic->setChecked(TRUE);
    };
  }
  qApp->flushX();
}

ShowXForm::ShowXForm( const QFont &initialFont,
                     QWidget *parent, const char *name )
                     : QWidget( parent, name, WResizeNoErase )
{
  setFont( initialFont );
  setBackgroundColor( white );
  m = Text;
  eraseRect = QRect( 0, 0, 0, 0 );
}

QSizePolicy ShowXForm::sizePolicy() const
{
  return QSizePolicy( QSizePolicy::Expanding, QSizePolicy::Expanding );
}

QSize ShowXForm::sizeHint() const
{
  return QSize(400,400);
}

void ShowXForm::paintEvent( QPaintEvent * )
{
  showIt();
}

void ShowXForm::resizeEvent( QResizeEvent * )
{
  eraseRect = QRect( width()/2, height()/2, 0, 0 );
  repaint(rect());
}

void ShowXForm::setText( const QString& s )
{
  text = s;
  showIt();
}

void ShowXForm::setMatrix( QWMatrix w )
{
  mtx = w;
  showIt();
}

void ShowXForm::setFont( const QFont &f )
{
  m = Text;
  QWidget::setFont( f );
}

void ShowXForm::setPixmap( QPixmap pm )
{
  pix	 = pm;
  m    = Image;
  showIt();
}

void ShowXForm::setPicture( const QPicture& p )
{
  picture = p;
  m = Picture;
  showIt();
}

void ShowXForm::setMode( int mode )
{
  m = (Mode)mode;
  if ( m == RootPad ) {
    connect(((TGQt *)gVirtualX)->Emitter(),SIGNAL(padPainted(QPixmap*)), SLOT(updatePad(QPixmap*))); 
  } else {
    disconnect(((TGQt *)gVirtualX)->Emitter(),SIGNAL(padPainted(QPixmap*)), this,SLOT(updatePad(QPixmap*))); 
  }
}

void ShowXForm::updatePad( QPixmap *pixPad )
{
  if ( m == RootPad) {
    if ( pixPad == (QPixmap *)TGQt::iwid(gPad->GetPixmapID())) showIt();
  }
}

void ShowXForm::showIt()
{
  if (mode() == RootCanvas) 
  {
    // Canvas will show itself alone

    return;
  }
  QPixmap rootPm;
  QPainter p;
  QRect r;	  // rectangle covering new text/pixmap in virtual coordinates
  QWMatrix um;  // copy user specified transform
  int textYPos = 0; // distance from boundingRect y pos to baseline
  int textXPos = 0; // distance from boundingRect x pos to text start
  QRect br;
  QFontMetrics fm( fontMetrics() );	// get widget font metrics
  switch ( mode() ) {
  case Text:
    br = fm.boundingRect( text );	// rectangle covering text
    r  = br;
    textYPos = -r.y();
    textXPos = -r.x();
    br.moveTopLeft( QPoint( -br.width()/2, -br.height()/2 ) );
    break;
  case Image:
    r = pix.rect();
    break;
  case RootCanvas:
        break;
  case RootPad:
    if (gPad) {
      rootPm = *(QPixmap *)TGQt::iwid(gPad->GetPixmapID());
      r = rootPm.rect();
    }
    break;
  case Picture:
    // ### need QPicture::boundingRect()
    r = QRect(0,0,1000,1000);
    break;
  }
  r.moveTopLeft( QPoint(-r.width()/2, -r.height()/2) );
  // compute union of new and old rect
  // the resulting rectangle will cover what is already displayed
  // and have room for the new text/pixmap
  eraseRect = eraseRect.unite( mtx.map(r) );
  eraseRect.moveBy( -1, -1 ); // add border for matrix round off
  eraseRect.setSize( QSize( eraseRect.width() + 2,eraseRect.height() + 2 ) );
  int pw = QMIN(eraseRect.width(),width());
  int ph = QMIN(eraseRect.height(),height());
  QPixmap pm( pw, ph );		// off-screen drawing pixmap
  pm.fill( backgroundColor() );
  
  p.begin( &pm );
  um.translate( pw/2, ph/2 );	// 0,0 is center
  um = mtx * um;
  p.setWorldMatrix( um );
  switch ( mode() ) {
  case Text:
    p.setFont( font() );		// use widget font
    p.drawText( r.left() + textXPos, r.top() + textYPos, text );
#if 0
    p.setPen( red );
    p.drawRect( br );
#endif
    break;
  case Image:
    p.drawPixmap( -pix.width()/2, -pix.height()/2, pix );
    //QPixmap rotated = pix.xForm(mtx);
    //bitBlt( &pm, pm.width()/2 - rotated.width()/2,
    //pm.height()/2 - rotated.height()/2, &rotated );
    break;
  case RootCanvas:
    break;
  case RootPad:
    p.drawPixmap( -rootPm.width()/2, -rootPm.height()/2, rootPm );
    break;
  case Picture:
    // ### need QPicture::boundingRect()
    p.scale(0.25,0.25);
    p.translate(-230,-180);
    p.drawPicture( picture );
  }
  p.end();
  int xpos = width()/2  - pw/2;
  int ypos = height()/2 - ph/2;
  bitBlt( this, xpos, ypos,			// copy pixmap to widget
    &pm, 0, 0, -1, -1 );
  eraseRect =	 mtx.map( r );
}


/*
    Grand unifying widget, putting ShowXForm and XFormControl
    together.
*/

class XFormCenter : public QHBox, public ModeNames
{
  Q_OBJECT
public:
  XFormCenter( QWidget *parent=0, const char *name=0 );
public slots:
  void setFont( const QFont &f ) { sx->setFont( f ); }
  void newMode( int );
private:
  QWidgetStack      *wStack;
  ShowXForm	        *sx;
  XFormControl      *xc;
  TQtWidget *rt; // ROOT embedded TCanvas

};

void XFormCenter::newMode( int m )
{
  static bool first_i = TRUE;
  static bool first_p = TRUE;
  static bool first_r = TRUE;
  if ( sx->mode() == m )
    return;
  if ( m == Image && first_i) {
    first_i = FALSE;
    QPixmap pm;
//    QString fileName(gSystem->Getenv("ROOTSYS"));
//    fileName += "/test/qt/image.any";
    QString fileName(::ProgramPath("image.any","libxform"));
    if ( pm.load( fileName ) )
      sx->setPixmap( pm );
    else {
#ifndef WIN32
       if ( QMessageBox::warning(0," Open file error"
                            ,"Can not open \"image.any\" picture file",
			    QMessageBox::Cancel ,QMessageBox::Ignore ) ==
             QMessageBox::Cancel) {
             gApplication->Terminate(1);
       }
#else
       fprintf(stderr," Open file error: Can not open \"image.any\" picture file\n");
#endif
    }  
    wStack->raiseWidget(0);
    return;
  }  
  if ( m == RootCanvas && first_r ) {
    first_r = FALSE;
    rt = new TQtWidget(wStack,"EmbeddedCanvas");
    wStack->addWidget(rt,1);
    setStretchFactor(rt,1);
    wStack->raiseWidget(1);
    rt->adjustSize();
    sx->setMode(m);
    rt->GetCanvas()->Resize();
    return;
  }
  if ( m == Picture && first_p ) {
    first_p = FALSE;
    QPicture p;
    QString fileName(gSystem->Getenv("ROOTSYS"));
    fileName += "/test/qt/picture.any";
    if (p.load( fileName ))
      sx->setPicture( p );
    wStack->raiseWidget(0);
    return;
  }
  if (m == RootCanvas) {
    rt->GetCanvas()->cd();
    wStack->raiseWidget(1);

  } else {
    wStack->raiseWidget(0);
  }
  sx->setMode(m);
}

XFormCenter::XFormCenter( QWidget *parent, const char *name )
: QHBox( parent, name )
{
  QFont f( "Charter", 36, QFont::Bold );
  
  xc = new XFormControl( f, this );

  wStack = new QWidgetStack(this);
     sx  = new ShowXForm( f, wStack );
  wStack->addWidget(sx,0);


  setStretchFactor(sx,1);

  xc->setFrameStyle( QFrame::Panel | QFrame::Raised );
  xc->setLineWidth( 2 );
  connect( xc, SIGNAL(newText(const QString&)), sx,
    SLOT(setText(const QString&)) );
  connect( xc, SIGNAL(newMatrix(QWMatrix)),
    sx, SLOT(setMatrix(QWMatrix)) );
  connect( xc, SIGNAL(newFont(const QFont&)), sx,
    SLOT(setFont(const QFont&)) );
  connect( xc, SIGNAL(newMode(int)), SLOT(newMode(int)) );
  sx->setText( "Troll" );
  newMode( Image );
  sx->setMatrix(xc->matrix());
}

int start()
{  
  XFormCenter *xfc = new XFormCenter;
  xfc->setCaption("Qt Example - XForm");
  xfc->show();
  return 0;
}

class mainClass 
{
public:
  mainClass(){ start();}
};

mainClass  __mainProgram__;

#include "xform.moc"		      // include metadata generated by the moc
