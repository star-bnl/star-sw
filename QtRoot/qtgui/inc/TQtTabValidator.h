// Author: Valeri Fine   10/08/2004
/****************************************************************************
** $Id: TQtTabValidator.h,v 1.6 2013/08/30 16:00:22 perev Exp $
**
** Copyright (C) 2003 by Valeri Fine.  All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
*****************************************************************************/

#ifndef ROOT_TQtTabValidator
#define ROOT_TQtTabValidator

#ifndef __CINT__
#  include <qvalidator.h> 
#else
  class QObject;
  class QValidator;
  typedef int State;
#endif

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// TQtTabValidator                                                      //
//                                                                      //
// A Qt QValidator to use the ROOT TTabCom class to validate the string //
//                                                                      //
//////////////////////////////////////////////////////////////////////////


class QString;
class TTabCom;

class TQtTabValidator  : public QValidator {
private:
    TQtTabValidator(const TQtTabValidator &t) : QValidator (0) {}
    void operator=(const TQtTabValidator &) {}
protected:
   static TTabCom *fgTabCom;  
   TQtTabValidator() : QValidator(0) {}
public:
   TQtTabValidator(QObject *parent, const char *name=0) :
#if QT_VERSION >= 0x040000
      QValidator(parent){ if (name && name[0]) {} }
#else
      QValidator(parent,name){}
#endif
   static void Clear();

   virtual State validate(QString &input, int &pos) const;
};
#endif
