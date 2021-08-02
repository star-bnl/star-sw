//DGHelp.h
#ifndef DGHELP_H
#define DGHELP_H

#include <iostream>
#include <cstdlib>
using namespace std;

//#ifdef STANDALONE
#  include <TObjArray.h>
#  include <TGFrame.h>
#  include <TGTextView.h>
#  include <TGButton.h>
//#endif

#include "RQ_OBJECT.h"

//*****************************************************************************
class DGHelp : public TGTransientFrame
{

  RQ_OBJECT() ;

private:
    TObjArray     *fMemList;           // list containing all members
    TGTextView    *fTextView;
    TGLayoutHints *fL0;
    TGLayoutHints *fL1;
    TGTextButton  *fCloseButton;
    void Exit(void) { std::exit(0); };
public:
    DGHelp(char *filename, bool exitOnClose=false, Pixel_t color=(Pixel_t)(-1));
    ~DGHelp();
    Bool_t ProcessMessage(Long_t msg, Long_t parm1, Long_t parm2);
    

    ClassDef(DGHelp,0) ;
};
//*****************************************************************************

#endif






/***************************************************************************
 *
 * $Id: DGHelp.h,v 1.1 2009/01/23 16:10:53 jeromel Exp $
 *
 * Author: Frank Laue, laue@bnl.gov
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: DGHelp.h,v $
 * Revision 1.1  2009/01/23 16:10:53  jeromel
 * Import from online/RTS/src/
 *
 * Revision 1.2  2008/03/06 16:50:04  fine
 * Fix the compilation error with the mewer gcc
 *
 * Revision 1.1  2007/02/27 15:23:36  laue
 * Initial version
 *
 * Revision 1.1  2006/10/04 20:31:15  laue
 * Initial Version
 *
 *
 ***************************************************************************/

