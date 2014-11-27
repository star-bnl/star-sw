#include "TQtTabValidator.h"
#include "TTabCom.h"
#include <sstream>
/****************************************************************************
** $Id: TQtTabValidator.cxx,v 1.9 2013/08/30 16:00:25 perev Exp $
**
** Copyright (C) 2003 by Valeri Fine.  All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
*****************************************************************************/

TTabCom *TQtTabValidator::fgTabCom = 0;
//_________________________________________________________________________________________________________
void TQtTabValidator::Clear()  
{
   if (fgTabCom) fgTabCom->ClearAll();
}
//_________________________________________________________________________________________________________
QValidator::State TQtTabValidator::validate(QString &input, int &pos) const {
   if (!fgTabCom) fgTabCom = new TTabCom();
#if ROOT_VERSION_CODE >= ROOT_VERSION(5,30,0)
   std::stringstream out;
   fgTabCom->Hook(input.toLatin1().data(), &pos,out);
   input = out.str().c_str();
#else
   char buffer[2048];
   qstrcpy(buffer,(const char *)input);
   fgTabCom->Hook(buffer, &pos);
   input = buffer;
#endif   
   return QValidator::Acceptable; // Intermediate;
}
