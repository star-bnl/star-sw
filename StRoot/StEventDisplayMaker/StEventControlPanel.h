//*-- Author :    Valery Fine   14/08/01  (E-mail: fine@bnl.gov)
//
// Copyright (C)  Valery Fine, Brookhaven National Laboratory, 1999. All right reserved
//
// $Id: StEventControlPanel.h,v 1.1 2003/01/08 03:16:32 fine Exp $
//


///////////////////////////////////////////////////////////////////////
//
//  PadControl panel is the set of the static methods to control 
//  TView of any "current" TPad with some "primitive"
//  operation:
//
///////////////////////////////////////////////////////////////////////
#ifdef R__QT
#ifndef STAR_StEventControlPanel
#define STAR_StEventControlPanel

#include "TObject.h"

class StChain;
class StMaker;
class StEventDisplayMaker;
class StEventDisplayInfo;

class QButtonGroup;
 
#if !defined(__CINT__)
# include <qobject.h>
#endif

class StEventControlPanel
#if !defined(__CINT__)
   : public QObject 
#endif
{
#if !defined(__CINT__)
Q_OBJECT
#endif
protected:
   QButtonGroup *fBar;  


public:
   static StMaker             *fgChain;
   static StEventDisplayMaker *fgDispMk;
   static StEventDisplayInfo  *fgHlp;  
public slots:
     void Clicked(int id);

protected:
   void AddButt(const Char_t *buttonName, const Char_t *command);
   void Build();

public:
   StEventControlPanel();
   virtual ~StEventControlPanel();
   QButtonGroup *Bar() const;

   static void PrintNames();
   void Refresh();
   void AddFilter(TObject *filter);
   void Show();
};

// StEventControlPanel __StEventControlPanel__;

#endif
#endif
