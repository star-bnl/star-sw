/***************************************************************************
 *
 * $Id: InputDialog.hh,v 1.1 2004/02/06 02:30:33 munhoz Exp $
 *
 * Author: Marcelo Munhoz (adapted from ROOT)
 ***************************************************************************
 *
 * Description: Input Dialogs
 *
 ***************************************************************************
 *
 * $Log: InputDialog.hh,v $
 * Revision 1.1  2004/02/06 02:30:33  munhoz
 * inserting SVT online monitor
 *
 **************************************************************************/

//
// This file contains the class InputDialog.
// An InputDialog object prompts for an input string using a simple
// dialog box. The InputDialog class is also a good example of how
// to use the ROOT GUI classes via the interpreter. Since interpreted
// classes can not call virtual functions via base class pointers, all
// GUI objects are used by composition instead of by inheritance.
//
// This file contains also some utility functions that use
// the InputDialog class to either get a string, integer or
// floating point number. There are also two functions showing
// how to use the file open and save dialogs. The utility functions are:
//
// const char *GetStringDialog(const char *prompt, const char *defval)
// Int_t GetIntegerDialog(const char *prompt, Int_t defval)
// Float_t GetFloatDialog(const char *prompt, Float_t defval)
//
 
///////////////////////////////////////////////////////////////////////////
//                                                                       //
// Input Dialog Widget                                                   //
//                                                                       //
///////////////////////////////////////////////////////////////////////////

class TGTransientFrame;
class TGTextEntry;
class TList;
 
class InputDialog {
 
private:
   TGTransientFrame *fDialog;  //! transient frame, main dialog window
   TGTextEntry      *fTE;      //! text entry widget containing
   TList            *fWidgets; //! keep track of widgets to be deleted in dtor
   char             *fRetStr;  //! address to store return string
 
public:
   InputDialog(const char *prompt, const char *defval, char *retstr);
   virtual ~InputDialog();
   void ProcessMessage(Long_t msg, Long_t parm1, Long_t parm2);

   ClassDef(InputDialog,1)
};

 
