/***************************************************************************
 *
 * $Id: InputDialog2.hh,v 1.1 2004/02/06 02:30:33 munhoz Exp $
 *
 * Author: Marcelo Munhoz (adapted from ROOT)
 ***************************************************************************
 *
 * Description: Input Dialogs for 2 entries
 *
 ***************************************************************************
 *
 * $Log: InputDialog2.hh,v $
 * Revision 1.1  2004/02/06 02:30:33  munhoz
 * inserting SVT online monitor
 *
 **************************************************************************/

///////////////////////////////////////////////////////////////////////////
//                                                                       //
// Input Dialog Widget                                                   //
//                                                                       //
///////////////////////////////////////////////////////////////////////////
 
class TGTransientFrame;
class TGTextEntry;
class TList;

class InputDialog2 {
 
private:
   TGTransientFrame *fDialog;   //! transient frame, main dialog window
   TGTextEntry      *fTE;       //! text entry widget containing
   TGTextEntry      *fTE2;      //! text entry widget containing
   TList            *fWidgets;  //! keep track of widgets to be deleted in dtor
   char             *fRetStr;   //! address to store return string
   char             *fRetStr2;  //! address to store return string
 
public:
   InputDialog2(const char *prompt, const char *prompt2, const char *defval, const char *defval2, char *retstr, char *retstr2);
   virtual ~InputDialog2();
   void ProcessMessage(Long_t msg, Long_t parm1, Long_t parm2);

   ClassDef(InputDialog2,1)
};
 
