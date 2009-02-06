/****************************************************************************
** $Id: TextEdit.cxx,v 1.5 2009/02/06 00:05:00 fine Exp $
**
** Copyright (C) 1992-2000 Trolltech AS.  All rights reserved.
**
** This file is part of an example program for Qt.  This example
** program may be used, distributed and modified without limitation.
**
*****************************************************************************/

#include "TextEdit.h"

#include <QTextEdit>
#include <QAction>
#include <QMenu>
#include <QToolBar>
#include <QTabWidget>
#include <QApplication>
#include <QFontDatabase>
#include <QComboBox>
#include <QLineEdit>
#include <QFileInfo>
#include <QFile>
#include <QFileDialog>
#include <QPrinter>
//#include <QPaintDeviceMetrics>
// #include <QSimpleRichText>
#include <QColorDialog>
#include <QPainter>
#include <QMessageBox>

TextEdit::TextEdit( QWidget *parent, const char *name )
    : QMainWindow( parent, name )
{
#if 0
    setupFileActions();
    setupEditActions();
    setupTextActions();

    tabWidget = new QTabWidget( this );
    connect( tabWidget, SIGNAL( currentChanged( QWidget * ) ),
	     this, SLOT( editorChanged( QWidget * ) ) );
    setCentralWidget( tabWidget );

#if 0
    if ( qApp->argc() == 1 ) {
	load( "example.html" );
    } else {
	for ( int i = 1; i < qApp->argc(); ++i )
	    load( qApp->argv()[ i ] );
    }
#endif    
#endif
}

void TextEdit::setupFileActions()
{
#if 0
    QToolBar *tb = new QToolBar( this );
    tb->setLabel( "File Actions" );
    QPopupMenu *menu = new QPopupMenu( this );
    menuBar()->insertItem( tr( "&File" ), menu );

    QAction *a;
    a = new QAction( QPixmap::fromMimeSource( "filenew.xpm" ), tr( "&New..." ), CTRL + Key_N, this, "fileNew" );
    connect( a, SIGNAL( activated() ), this, SLOT( fileNew() ) );
    a->addTo( tb );
    a->addTo( menu );
    a = new QAction( QPixmap::fromMimeSource( "fileopen.xpm" ), tr( "&Open..." ), CTRL + Key_O, this, "fileOpen" );
    connect( a, SIGNAL( activated() ), this, SLOT( fileOpen() ) );
    a->addTo( tb );
    a->addTo( menu );
    menu->insertSeparator();
    a = new QAction( QPixmap::fromMimeSource( "filesave.xpm" ), tr( "&Save..." ), CTRL + Key_S, this, "fileSave" );
    connect( a, SIGNAL( activated() ), this, SLOT( fileSave() ) );
    a->addTo( tb );
    a->addTo( menu );
    a = new QAction( tr( "Save &As..." ), 0, this, "fileSaveAs" );
    connect( a, SIGNAL( activated() ), this, SLOT( fileSaveAs() ) );
    a->addTo( menu );
    menu->insertSeparator();
    a = new QAction( QPixmap::fromMimeSource( "fileprint.xpm" ), tr( "&Print..." ), CTRL + Key_P, this, "filePrint" );
    connect( a, SIGNAL( activated() ), this, SLOT( filePrint() ) );
    a->addTo( tb );
    a->addTo( menu );
    a = new QAction( tr( "&Close" ), 0, this, "fileClose" );
    connect( a, SIGNAL( activated() ), this, SLOT( fileClose() ) );
    a->addTo( menu );
    a = new QAction( tr( "E&xit" ), 0, this, "fileExit" );
    connect( a, SIGNAL( activated() ), this, SLOT( fileExit() ) );
    a->addTo( menu );
#endif	
}

void TextEdit::setupEditActions()
{
#if 0
    QToolBar *tb = new QToolBar( this );
    tb->setLabel( "Edit Actions" );
    QPopupMenu *menu = new QPopupMenu( this );
    menuBar()->insertItem( tr( "&Edit" ), menu );

    QAction *a;
    a = new QAction( QPixmap::fromMimeSource( "editundo.xpm" ), tr( "&Undo" ), CTRL + Key_Z, this, "editUndo" );
    connect( a, SIGNAL( activated() ), this, SLOT( editUndo() ) );
    a->addTo( tb );
    a->addTo( menu );
    a = new QAction( QPixmap::fromMimeSource( "editredo.xpm" ), tr( "&Redo" ), CTRL + Key_Y, this, "editRedo" );
    connect( a, SIGNAL( activated() ), this, SLOT( editRedo() ) );
    a->addTo( tb );
    a->addTo( menu );
    menu->insertSeparator();
    a = new QAction( QPixmap::fromMimeSource( "editcopy.xpm" ), tr( "&Copy" ), CTRL + Key_C, this, "editCopy" );
    connect( a, SIGNAL( activated() ), this, SLOT( editCopy() ) );
    a->addTo( tb );
    a->addTo( menu );
    a = new QAction( QPixmap::fromMimeSource( "editcut.xpm" ), tr( "Cu&t" ), CTRL + Key_X, this, "editCut" );
    connect( a, SIGNAL( activated() ), this, SLOT( editCut() ) );
    a->addTo( tb );
    a->addTo( menu );
    a = new QAction( QPixmap::fromMimeSource( "editpaste.xpm" ), tr( "&Paste" ), CTRL + Key_V, this, "editPaste" );
    connect( a, SIGNAL( activated() ), this, SLOT( editPaste() ) );
    a->addTo( tb );
    a->addTo( menu );
#endif	
}

void TextEdit::setupTextActions()
{
#if 0
    QToolBar *tb = new QToolBar( this );
    tb->setLabel( "Format Actions" );
    QPopupMenu *menu = new QPopupMenu( this );
    menuBar()->insertItem( tr( "F&ormat" ), menu );

    comboFont = new QComboBox( TRUE, tb );
    QFontDatabase db;
    comboFont->insertStringList( db.families() );
    connect( comboFont, SIGNAL( activated( const QString & ) ),
	     this, SLOT( textFamily( const QString & ) ) );
    comboFont->lineEdit()->setText( QApplication::font().family() );
#if 0
    comboSearch = new QComboBox(TRUE,tb);
    connect( comboSearch, SIGNAL( activated( const QString & ) ),
	     this, SLOT( searchActivated( const QString & ) ) );
    connect( comboSearch, SIGNAL( textChanged( const QString & ) ),
	     this, SLOT( searchContinue( const QString & ) ) );
#endif    
    comboSize = new QComboBox( TRUE, tb );
    QValueList<int> sizes = db.standardSizes();
    QValueList<int>::Iterator it = sizes.begin();
    for ( ; it != sizes.end(); ++it )
	 comboSize->insertItem( QString::number( *it ) );
    connect( comboSize, SIGNAL( activated( const QString & ) ),
	     this, SLOT( textSize( const QString & ) ) );
    comboSize->lineEdit()->setText( QString::number( QApplication::font().pointSize() ) );

    actionTextBold = new QAction( QPixmap::fromMimeSource( "textbold.xpm" ), tr( "&Bold" ), CTRL + Key_B, this, "textBold" );
    connect( actionTextBold, SIGNAL( activated() ), this, SLOT( textBold() ) );
    actionTextBold->addTo( tb );
    actionTextBold->addTo( menu );
    actionTextBold->setToggleAction( TRUE );
    actionTextItalic = new QAction( QPixmap::fromMimeSource( "textitalic.xpm" ), tr( "&Italic" ), CTRL + Key_I, this, "textItalic" );
    connect( actionTextItalic, SIGNAL( activated() ), this, SLOT( textItalic() ) );
    actionTextItalic->addTo( tb );
    actionTextItalic->addTo( menu );
    actionTextItalic->setToggleAction( TRUE );
    actionTextUnderline = new QAction( QPixmap::fromMimeSource( "textunder.xpm" ), tr( "&Underline" ), CTRL + Key_U, this, "textUnderline" );
    connect( actionTextUnderline, SIGNAL( activated() ), this, SLOT( textUnderline() ) );
    actionTextUnderline->addTo( tb );
    actionTextUnderline->addTo( menu );
    actionTextUnderline->setToggleAction( TRUE );
    menu->insertSeparator();

    QActionGroup *grp = new QActionGroup( this );
    connect( grp, SIGNAL( selected( QAction* ) ), this, SLOT( textAlign( QAction* ) ) );

    actionAlignLeft = new QAction( QPixmap::fromMimeSource( "textleft.xpm" ), tr( "&Left" ), CTRL + Key_L, grp, "textLeft" );
    actionAlignLeft->setToggleAction( TRUE );
    actionAlignCenter = new QAction( QPixmap::fromMimeSource( "textcenter.xpm" ), tr( "C&enter" ), CTRL + Key_E, grp, "textCenter" );
    actionAlignCenter->setToggleAction( TRUE );
    actionAlignRight = new QAction( QPixmap::fromMimeSource( "textright.xpm" ), tr( "&Right" ), CTRL + Key_R, grp, "textRight" );
    actionAlignRight->setToggleAction( TRUE );
    actionAlignJustify = new QAction( QPixmap::fromMimeSource( "textjustify.xpm" ), tr( "&Justify" ), CTRL + Key_J, grp, "textjustify" );
    actionAlignJustify->setToggleAction( TRUE );

    grp->addTo( tb );
    grp->addTo( menu );

    menu->insertSeparator();

    QPixmap pix( 16, 16 );
    pix.fill( black );
    actionTextColor = new QAction( pix, tr( "&Color..." ), 0, this, "textColor" );
    connect( actionTextColor, SIGNAL( activated() ), this, SLOT( textColor() ) );
    actionTextColor->addTo( tb );
    actionTextColor->addTo( menu );
#endif	
}

void TextEdit::load( const QString &f )
{
#if 0
   if ( QFile::exists( f ) ) {
      QTextEdit *edit = new QTextEdit( tabWidget );
      doConnections( edit );
      tabWidget->addTab( edit, QFileInfo( f ).fileName() );
      edit->setTextFormat(PlainText );
      edit->setFamily("Courier New");
      fontChanged(edit->font());
      QFile file( f );
      if ( file.open( IO_ReadOnly ) ) {
        QTextStream ts( &file );
        QString txt = ts.read();
#if 0    
        if ( !QStyleSheet::mightBeRichText( txt ) )
         txt = QStyleSheet::convertFromPlainText( txt, QStyleSheetItem::WhiteSpacePre );
#endif    
        edit->setText( txt );
        tabWidget->showPage( edit );
        edit->viewport()->setFocus();
        filenames.replace( edit, f );
      } else {
         QMessageBox::critical(
                this,
                tr("Open failed"),
                tr("Could not open file for reading: %1").arg( qApp->translate("QFile",file.errorString()) )
                );
      }
   }
#endif   
}

QTextEdit *TextEdit::currentEditor() const
{
#if 0
    if ( tabWidget->currentPage() &&
	 tabWidget->currentPage()->inherits( "QTextEdit" ) )
	return (QTextEdit*)tabWidget->currentPage();
#endif	
    return 0;
}

void TextEdit::doConnections( QTextEdit *e )
{
#if 0
    connect( e, SIGNAL( currentFontChanged( const QFont & ) ),
	     this, SLOT( fontChanged( const QFont & ) ) );
    connect( e, SIGNAL( currentColorChanged( const QColor & ) ),
	     this, SLOT( colorChanged( const QColor & ) ) );
    connect( e, SIGNAL( currentAlignmentChanged( int ) ),
	     this, SLOT( alignmentChanged( int ) ) );
    connect( e, SIGNAL(  textChanged () ),
	     this, SLOT( textChanged() ) );
#endif		 
}

void TextEdit::fileNew()
{
#if 0
    QTextEdit *edit = new QTextEdit( tabWidget );
    edit->setTextFormat( PlainText );
    edit->setFamily("Courier");
    fontChanged(edit->font());
    doConnections( edit );
    tabWidget->addTab( edit, tr( "noname" ) );
    tabWidget->showPage( edit );
    edit->viewport()->setFocus();
#endif	
}

//______________________________________________________________
void TextEdit::fileOpen()
{
#if 0
   QString fn = QFileDialog::getOpenFileName( QString::null, tr( "STAR Geometry (*.g);;All Files (*)" ), this );
   if ( !fn.isEmpty() ) load( fn );
#endif   
}

//______________________________________________________________
void TextEdit::fileSave()
{
#if 0
   if ( currentEditor() ) {
      QString fn;
      if ( filenames.find( currentEditor() ) == filenames.end() ) {
         fileSaveAs();
      } else {
         QString &fileName = *filenames.find( currentEditor() );
         QFile file(fileName);
         if ( file.open( IO_WriteOnly ) ) {
            QTextStream ts( &file );
            ts << currentEditor()->text();
            currentEditor()->setModified(false);
            emit textSaved(fileName);
         } else {
            QMessageBox::critical(
                this,
                tr("Save failed"),
                tr("Could not save file: %1").arg( qApp->translate("QFile",file.errorString()) )
                );
         }
      }
   }
#endif   
}

//______________________________________________________________
void TextEdit::fileSaveAs()
{
#if 0
   if ( currentEditor() ) {
      QString fn = QFileDialog::getSaveFileName( QString::null, tr( "HTML-Files (*.htm *.html);;All Files (*)" ), this );
      if ( !fn.isEmpty() ) {
         filenames.replace( currentEditor(), fn );
         fileSave();
         tabWidget->setTabLabel( currentEditor(), QFileInfo( fn ).fileName() );
      }
   }
#endif   
}

void TextEdit::filePrint()
{
#if 0
    if ( !currentEditor() )
	return;
#ifndef QT_NO_PRINTER
    QPrinter printer( QPrinter::HighResolution );
    printer.setFullPage(TRUE);
    if ( printer.setup( this ) ) {
	QPainter p( &printer );
	// Check that there is a valid device to print to.
	if ( !p.device() ) return;
	QPaintDeviceMetrics metrics( p.device() );
	int dpiy = metrics.logicalDpiY();
	int margin = (int) ( (2/2.54)*dpiy ); // 2 cm margins
	QRect body( margin, margin, metrics.width() - 2*margin, metrics.height() - 2*margin );
	QFont font( currentEditor()->QWidget::font() );
 	font.setPointSize( 10 ); // we define 10pt to be a nice base size for printing

	QSimpleRichText richText( currentEditor()->text(), font,
				  currentEditor()->context(),
				  currentEditor()->styleSheet(),
				  currentEditor()->mimeSourceFactory(),
				  body.height() );
	richText.setWidth( &p, body.width() );
  	QRect view( body );
	int page = 1;
	do {
	    richText.draw( &p, body.left(), body.top(), view, colorGroup() );
	    view.moveBy( 0, body.height() );
	    p.translate( 0 , -body.height() );
	    p.setFont( font );
	    p.drawText( view.right() - p.fontMetrics().width( QString::number( page ) ),
			view.bottom() + p.fontMetrics().ascent() + 5, QString::number( page ) );
	    if ( view.top()  >= richText.height() )
		break;
	    printer.newPage();
	    page++;
	} while (TRUE);
    }
#endif
#endif
}

//______________________________________________________________________
void TextEdit::fileClose()
{
#if 0
    delete currentEditor();
    if ( currentEditor() )	currentEditor()->viewport()->setFocus();
#endif	
}

//______________________________________________________________________
void TextEdit::fileExit()
{
#if 0
   setEnabled(false);
   while (currentEditor()) delete currentEditor();
   hide();
   setEnabled(true);
#endif   
}

//______________________________________________________________________
void TextEdit::editUndo()
{
#if 0
    if (currentEditor()) currentEditor()->undo();
#endif	
}

//______________________________________________________________________
void TextEdit::editRedo()
{
#if 0
    if ( currentEditor() ) currentEditor()->redo();
#endif	
}

//______________________________________________________________________
void TextEdit::editCut()
{
#if 0
    if (currentEditor())  currentEditor()->cut();
#endif	
}

//______________________________________________________________________
void TextEdit::editCopy()
{
#if 0

    if (currentEditor())
       currentEditor()->copy();
#endif   
}

//______________________________________________________________________
void TextEdit::editPaste()
{
#if 0
   if (currentEditor() )
      currentEditor()->paste();
#endif   
}

//______________________________________________________________________
void TextEdit::textBold()
{
#if 0
    if (currentEditor())
       currentEditor()->setBold( actionTextBold->isOn() );
#endif   
}

//______________________________________________________________________
void TextEdit::textChanged()
{
#if 0
   QTextEdit *w = currentEditor();
   if ( w )
   {
      QString label = tabWidget->tabLabel(w);
      if ( w->isModified() ) {
         // add "*" if needed
         if  (!label.endsWith("*")) {
            label += "*";
            tabWidget->setTabLabel(w,label);
         }
      } else if  (label.endsWith("*")) {
         // remove  "*" if present
         label.remove("*");
         tabWidget->setTabLabel(w,label);
      }
   }
#endif   
}

//______________________________________________________________________
void TextEdit::textUnderline()
{
#if 0
   if (currentEditor())
      currentEditor()->setUnderline( actionTextUnderline->isOn() );
#endif   
}

//______________________________________________________________________
void TextEdit::textItalic()
{
#if 0
    if (currentEditor())
       currentEditor()->setItalic( actionTextItalic->isOn() );
#endif   
}

//______________________________________________________________________
void TextEdit::textFamily(const QString &f)
{
#if 0
   if (currentEditor()) {
      currentEditor()->setFamily( f );
      currentEditor()->viewport()->setFocus();
   }
#endif   
}

//______________________________________________________________________
void TextEdit::textSize( const QString &p )
{
#if 0
   if (currentEditor()) {
      currentEditor()->setPointSize( p.toInt() );
      currentEditor()->viewport()->setFocus();
   }
#endif   
}

void TextEdit::textColor()
{
#if 0
    if ( !currentEditor() )
       	return;
    QColor col = QColorDialog::getColor( currentEditor()->color(), this );
    if ( !col.isValid() )
       	return;
    currentEditor()->setColor( col );
    QPixmap pix( 16, 16 );
    pix.fill( black );
    actionTextColor->setIconSet( pix );
#endif   
}

void TextEdit::textAlign( QAction *a )
{
#if 0
    if ( !currentEditor() )
	return;
    if ( a == actionAlignLeft )
	currentEditor()->setAlignment( AlignLeft );
    else if ( a == actionAlignCenter )
	currentEditor()->setAlignment( AlignHCenter );
    else if ( a == actionAlignRight )
	currentEditor()->setAlignment( AlignRight );
    else if ( a == actionAlignJustify )
	currentEditor()->setAlignment( AlignJustify );
#endif   
}

void TextEdit::fontChanged( const QFont &f )
{
#if 0
    comboFont->lineEdit()->setText( f.family() );
    comboSize->lineEdit()->setText( QString::number( f.pointSize() ) );
    actionTextBold->setOn( f.bold() );
    actionTextItalic->setOn( f.italic() );
    actionTextUnderline->setOn( f.underline() );
#endif   
}

void TextEdit::colorChanged( const QColor &c )
{
#if 0
    QPixmap pix( 16, 16 );
    pix.fill( c );
    actionTextColor->setIconSet( pix );
#endif   
}

void TextEdit::alignmentChanged( int a )
{
#if 0
    if ( ( a == AlignAuto ) || ( a & AlignLeft ))
	actionAlignLeft->setOn( TRUE );
    else if ( ( a & AlignHCenter ) )
	actionAlignCenter->setOn( TRUE );
    else if ( ( a & AlignRight ) )
	actionAlignRight->setOn( TRUE );
    else if ( ( a & AlignJustify ) )
	actionAlignJustify->setOn( TRUE );
#endif   
}

//______________________________________________________________________
void TextEdit::editorChanged( QWidget * )
{
#if 0
   if (currentEditor()) {
     fontChanged( currentEditor()->currentFont() );
     colorChanged( currentEditor()->color() );
     alignmentChanged( currentEditor()->alignment() );
   }
#endif   
}
//______________________________________________________________________
void TextEdit::findBlock(const QString &expr) 
{
#if 0
   int para=0;
   int index=0; 
   if (QTextEdit *e = currentEditor())
   {
      e->setCursorPosition(0,0); 
      while (1) {
         if (e->find("block",false,true,true,&para,&index))
         {   
            int paraFrom = para;
            int indexFrom = index; 
            if (e->find(expr, false, true,true,&para,&index)) {
               if ( (para > paraFrom) || (index - indexFrom) > 6+(int)expr.length()) { 
                  para = paraFrom; index = indexFrom+6;
                  continue;
               }
               if (e->find("EndBlock", false, true,true,&para,&index)) {
                  e->setSelection(paraFrom,indexFrom, para, index);
                  break;
               }
            }
         }
         break;
      } 
   }
#endif   
}
//______________________________________________________________________
void  TextEdit::searchActivated( const QString &)
{
}
//______________________________________________________________________
void  TextEdit::searchContinue( const QString &)
{
}
