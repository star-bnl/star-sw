//*-- Author :    Valery Fine(fine@bnl.gov)  04/01/2006
//
// $Id: DialogsQ.C,v 1.3 2007/01/19 19:36:20 fine Exp $
//
// This file contains the set of functions to use class QInputDialog
//     http://doc.trolltech.com/3.3/qinputdialog.html
// static methods  to either get a string, integer or floating point number. 
//
// (see $ROOTSYS/macros/Dialogs.C for the ROOT GUI-based counterpart )
//
//
// The functions  prompt for an input string using a simple dialog box. 
// There are also two functions showing how to use the file open and save dialogs.
// see: http://doc.trolltech.com/3.3/qfiledialog.html#getOpenFileName 
//
// The utility functions are:
// --------------------------
// const char *OpenFileDialog()
// const char *SaveFileDialog()
// const char *GetStringDialog(const char *prompt, const char *defval)
// Int_t GetIntegerDialog(const char *prompt, Int_t defval)
// Float_t GetFloatDialog(const char *prompt, Float_t defval)
//
// To use the QInputDialog Qt class and the utility functions you just
// have to load the DialogsQ.C file as follows:
//
// .x DialogsQ.C
//
// Now you can use them like:
// {
//    const char *file = OpenFileDialog();
//    Int_t run   = GetIntegerDialog("Give run number:", 0);
//    Int_t event = GetIntegerDialog("Give event number:", 0);
//    printf("analyse run %d, event %d from file %s\n", run ,event, file);
// }
//
#ifndef __CINT__
#  include <qapplication.h> 
#  include <qstyle.h> 
#  include <qfiledialog.h> 
#  include <qstringlist.h> 
#  include <qstring.h> 
#  include "TObjString.h"
#  include "TList.h"
#  include "TSystem.h"
#  include <qinputdialog.h> 
#endif

//--- Utility Functions --------------------------------------------------------
//______________________________________________________________________
void DialogsQ() {
   // Load the Qt ROOT dictionary
#ifdef __CINT__
   gSystem->Load("qtcint");
#endif   
}
//______________________________________________________________________
const char *OpenFileDialog()
{
   // Prompt for file to be opened. 
   // http://doc.trolltech.com/3.3/qfiledialog.html#getOpenFileName
   
   QString filter =
       "Macro files (*.C);"
      ";ROOT files (*.root);"
      ";PostScript (*.ps);"
      ";Encapsulated PostScript (*.eps);"
      ";Gif files (*.gif);"
      ";All files (*)";

   static QString fFilename;
   fFilename = QFileDialog::getOpenFileName(gSystem->WorkingDirectory()
        , filter);

   return (const char*)fFilename;
}

//______________________________________________________________________
const char *SaveFileDialog()
{
   // Prompt for file to be saved.
   // http://doc.trolltech.com/3.3/qfiledialog.html#getSaveFileNamehttp://doc.trolltech.com/3.3/qfiledialog.html#getSaveFileName 
   
   QString filter =
      ";Macro files (*.C);"
      ";ROOT files (*.root);"
      ";PostScript (*.ps);"
      ";Encapsulated PostScript (*.eps);"
      ";Gif files (*.gif);"
      ";All files (*);";

   static QString fFilename;
   fFilename = QFileDialog::getSaveFileName(gSystem->WorkingDirectory()
        , filter);

   return (const char*)fFilename;
}

//______________________________________________________________________
const char *GetStringDialog(const char *prompt, const char *defval)
{
   // Prompt for string. The typed in string is returned.
   // http://doc.trolltech.com/3.3/qinputdialog.html#getText
   
   static QString answer;

   answer = QInputDialog::getText(
            "Enter text", prompt, QLineEdit::Normal,defval);

   return (const char *)answer;
}

//______________________________________________________________________
Int_t GetIntegerDialog(const char *prompt, Int_t defval
                      ,int minValue = -2147483647, int maxValue = 2147483647
                      , int step = 1)
{
    // Prompt for integer. The typed in integer is returned.
    // http://doc.trolltech.com/3.3/qinputdialog.html#getInteger
   
    return QInputDialog::getInteger("Enter integer", prompt, defval
                                    ,minValue, maxValue, step);
}

//______________________________________________________________________
Double_t GetFloatDialog(const char *prompt, Double_t defval
                       , double minValue = -2147483647, double maxValue = 2147483647
                       , int decimals = 1)
{
   // Prompt for float. The typed in float is returned.
   // http://doc.trolltech.com/3.3/qinputdialog.html#getDouble
   //
   return QInputDialog::getDouble("Enter double", prompt, defval
                                  ,minValue, maxValue, decimals);
}
