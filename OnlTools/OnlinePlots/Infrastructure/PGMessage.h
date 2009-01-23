//PGMessage.h
#ifndef PGMessage_H
#define PGMessage_H

#include <iostream>
#include <cstdlib>
using namespace std;

#include <TObjArray.h>
#include <TGFrame.h>
#include <TGTextView.h>
#include <TGButton.h>
#include <TString.h>
#include "RQ_OBJECT.h"

//*****************************************************************************
class PGMessage : public TGTransientFrame {

  RQ_OBJECT() 

private:
    TObjArray     *fMemList;           // list containing all members
    TGTextView    *fTextView;
    TGLayoutHints *fL0;
    TGLayoutHints *fL1;
    TGTextButton  *fCloseButton;
 public: 
    PGMessage(const char*, bool exitOnClose=false, Pixel_t color=Pixel_t(-1));
    ~PGMessage();
    void AddLine(const char* line);
    void Exit(void);
    Bool_t ProcessMessage(Long_t msg, Long_t parm1, Long_t parm2);

    ClassDef(PGMessage,0) ;

};
//*****************************************************************************

#endif






/***************************************************************************
 *
 * $Id: PGMessage.h,v 1.1 2009/01/23 16:10:57 jeromel Exp $
 *
 * Author: Frank Laue, laue@bnl.gov
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: PGMessage.h,v $
 * Revision 1.1  2009/01/23 16:10:57  jeromel
 * Import from online/RTS/src/
 *
 * Revision 1.2  2008/03/06 17:21:15  fine
 * Fix the compilation error: integer constant is too large
 *
 * Revision 1.1  2007/02/27 15:23:38  laue
 * Initial version
 *
 * Revision 1.1  2006/10/04 20:31:16  laue
 * Initial Version
 *
 *
 ***************************************************************************/

