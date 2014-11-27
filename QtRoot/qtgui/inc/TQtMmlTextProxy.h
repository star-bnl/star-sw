// @(#)root/qt:$Id: TQtMmlTextProxy.h,v 1.2 2013/08/30 16:00:21 perev Exp $
// Author: Valeri Fine   21/01/2002
/****************************************************************************
**
** Copyright (C) 2009 by Valeri Fine.  All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
*****************************************************************************/

#ifndef ROOT_TQTMMLTEXTPROXY
#define ROOT_TQTMMLTEXTPROXY


//////////////////////////////////////////////////////////////////////////
//                                                                      //
// TQtMmlTextProxy                                                      //
//                                                                      //
// Proxy to render the text with QMmlDocument                           //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "TGQt.h" 

class QtMmlDocument;
class QPainter;
class QString;

class TQtMmlTextProxy : public TQtTextProxy {
private:
    QtMmlDocument *fMmlDoc;
    Font_t         fTextFont;
    TQtMmlTextProxy(const TQtMmlTextProxy&);
    void operator=(const TQtMmlTextProxy&);
protected:
    QtMmlDocument *MmlDoc() const;
    TQtMmlTextProxy();
public:
    virtual  ~TQtMmlTextProxy();
    virtual void clear();

    virtual bool setContent(const QString &text, QString *errorMsg   = 0,
                    int *errorLine = 0, int *errorColumn = 0);
    virtual bool setMmlContent(const QString &text, QString *errorMsg   = 0,
                    int *errorLine = 0, int *errorColumn = 0);
    virtual void paint(QPainter *p,unsigned int x, unsigned int y) const;
    virtual unsigned int width()   const; 
    virtual unsigned int height()  const;

    virtual void setFont(Font_t fontnumber);

    virtual int baseFontPointSize() const;
    virtual void setBaseFontPointSize(int size);
    virtual void setForegroundColor(const QColor &);
    virtual TQtTextProxy *Clone();
    static TQtMmlTextProxy *Create();
};
#endif
