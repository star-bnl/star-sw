#ifndef STAR_StEventDisplayInfo
#define STAR_StEventDisplayInfo

//*-- Author :    Valery Fine   14/08/01  (E-mail: fine@bnl.gov)
// Copyright (C)  Valery Fine, Brookhaven National Laboratory, 1999. All right reserved
// $Id: StEventDisplayInfo.h,v 1.1 2003/01/08 03:16:32 fine Exp $

#include "TObject.h"
#ifdef R__QT
// Qt header files
#include <qtextview.h>
#endif

class StEventDisplayInfo
#ifdef R__QT
   : public QTextView 
#endif
{
public:
   StEventDisplayInfo(StEventDisplayInfo **kaddr, const char* title, UInt_t w=100, UInt_t h=50);
   virtual ~StEventDisplayInfo(){*fKAddr=0;}
private:
   StEventDisplayInfo **fKAddr;	//!
public:
   void SetText(const char *info);
   void AddText(const char *info);
   void Popup();
};

#endif 
