#include "TQtMmlTextProxy.h"
#include "qtmmlwidget.h"
#include <QMessageBox>
#include <QFile>
#include "TQtRConfig.h"
#include <assert.h>
//___________________________________________________________________________________________
TQtMmlTextProxy::TQtMmlTextProxy():TQtTextProxy(), fMmlDoc(0), fTextFont(-1)
{ 
    if (!TGQt::TextProxy()) TGQt::SetTextProxy(this);
}
//___________________________________________________________________________________________
QtMmlDocument *TQtMmlTextProxy::MmlDoc() const
{
   // QtMMlDocyment intantiation
	if (!fMmlDoc) ((TQtMmlTextProxy *)this)->fMmlDoc =  new QtMmlDocument;
   return fMmlDoc;
}

//___________________________________________________________________________________________
TQtMmlTextProxy::~TQtMmlTextProxy()
{
   if (TGQt::TextProxy()== this) TGQt::SetTextProxy(0);
   delete fMmlDoc; fMmlDoc  = 0;
}
//___________________________________________________________________________________________
TQtTextProxy *TQtMmlTextProxy::Clone() 
{
   // Create an instance of the proxy
   return new TQtMmlTextProxy;
}

//___________________________________________________________________________________________
void TQtMmlTextProxy::clear()
{
   if (MmlDoc()) fMmlDoc->clear();
}

//___________________________________________________________________________________________
bool TQtMmlTextProxy::setContent(const QString &text, QString *errorMsg,
                    int *errorLine, int *errorColumn)
{
   // Treat the leading '@' as a sign of the indirect file.
   // The @ is to follow the the filename thast can be read.
   // otherwise the string  "text" is treated as the regular string

   bool mmlType = false;
   QString mmlFile = text.trimmed();
   if (mmlFile.startsWith("@") ) {
      QFile file(mmlFile.mid(1));
      if (file.open(QIODevice::ReadOnly)) {
         QTextStream stream(&file);
         QString mmText = stream.readAll();
         mmlType = setMmlContent(mmText,errorMsg,errorLine,errorColumn);
      } else {
         qDebug() << " Error:  TQtMmlTextProxy::setContent(. . .) : File error:"
                  << " Could not open \""<< file.fileName()
                  << "\": " <<  file.errorString();
      }
   }
   if (!mmlType) mmlType = setMmlContent(text,errorMsg,errorLine,errorColumn);
   return mmlType;
}
//___________________________________________________________________________________________
bool TQtMmlTextProxy::setMmlContent(const QString &text, QString *errorMsg,
                    int *errorLine, int *errorColumn)
{
   // Look for <math> tag 
   bool result = false;
   if (text.startsWith("<math") && MmlDoc())  {
      int *error_line    = errorLine;
      int *error_column  = errorColumn;
      QString *error_msg = errorMsg;
      QString error_msg_own;
      int error_line_own, error_column_own;
      if (!error_msg   ) error_msg    = &error_msg_own;
      if (!error_line  ) error_line   = &error_line_own;
      if (!error_column) error_column = &error_column_own;
      result = fMmlDoc->setContent(text, error_msg, error_line,
						error_column);
      if (!result) {
         qDebug()  << "TQtMmlTextProxy::setContent: " 
                   <<"Parse error: line " 
                   << QString::number(*error_line)
                   << ", col " << QString::number(*error_column)
                   << ": " << *error_msg;
      }
   }
   return result;
}
//___________________________________________________________________________________________
void TQtMmlTextProxy::paint(QPainter *p,unsigned int x, unsigned int y) const
{
   if (MmlDoc()) {
#ifndef QT4BUGFIXED      
      // workaround of Qt 4.x QPainter for QPixmap bug :-(
      if ( p->device()->devType() == QInternal::Pixmap ) {
         QImage im(width(), height(),QImage::Format_ARGB32_Premultiplied);
         {
            im.fill(0);
            QPainter pi(&im);
            fMmlDoc->paint(&pi,QPoint(0,0));
         }
         p->drawImage(QPoint(x,y),im);
      } else  {
         fMmlDoc->paint(p,QPoint(x,y));
      }
#else
      // On Win32 everything works fine with no extra trick :-)
      fMmlDoc->paint(p,QPoint(x,y));
#endif
   }
}
//___________________________________________________________________________________________
unsigned int TQtMmlTextProxy::width() const
{
   return
      MmlDoc() ? fMmlDoc->size().width():0;
}
//___________________________________________________________________________________________
unsigned int TQtMmlTextProxy::height()    const
{
   return
      MmlDoc() ? fMmlDoc->size().height():0;
}

//___________________________________________________________________________________________
void TQtMmlTextProxy::setFont(Font_t )
{
   assert(0 && "TQtMmlTextProxy::setFont method to be implemented yet");
}

//___________________________________________________________________________________________
int TQtMmlTextProxy::baseFontPointSize() const
{
   return
      MmlDoc() ? fMmlDoc->baseFontPointSize() : 0;
}
//___________________________________________________________________________________________
void TQtMmlTextProxy::setBaseFontPointSize(int size)
{
   // qDebug() << "TQtMmlTextProxy::setBaseFontPointSize: " << size;
   if (MmlDoc() && size > 1) fMmlDoc->setBaseFontPointSize(size);
} 
//___________________________________________________________________________________________
void TQtMmlTextProxy::setForegroundColor(const QColor &color)
{
    if (MmlDoc()) fMmlDoc->setForegroundColor(color);
}

//___________________________________________________________________________________________
TQtMmlTextProxy * TQtMmlTextProxy::Create() {
   // create the top level instance:
   qDebug() << "TQtMmlTextProxy activated";
   return new  TQtMmlTextProxy;
}


static TQtMmlTextProxy *gTQtMmlTextProxy = TQtMmlTextProxy::Create();
