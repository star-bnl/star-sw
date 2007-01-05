//*-- Author :    Valery Fine(fine@bnl.gov)  04/01/2006
//
// $Id: DialogsQ.C,v 1.1 2007/01/05 01:28:04 fine Exp $
//
// This file contains the set of function to use class QInputDialog class
// See $ROOTSYS/macros/Dialogs.C for the ROOT GUI-based counterpart
//
// This file contains also some utility functions that use
// the QInputDialog class static methods 
//     http://doc.trolltech.com/3.3/qinputdialog.html
// to either get a string, integer or floating point number. 
//
// The QInputDialog static methods prompts for an input string 
// using a simple dialog box. 
// There are also two functions showing
// how to use the file open and save dialogs.
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

//--- Utility Functions --------------------------------------------------------
void DialogsQ() {
   // Load the Qt dictionary
   gSystem->Load("qtcint");
}
const char *OpenFileDialog()
{
   // Prompt for file to be opened. Depending on navigation in
   // dialog the current working directory can be changed.
   // The returned file name is always with respect to the
   // current directory.
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

const char *SaveFileDialog()
{
   // Prompt for file to be saved. Depending on navigation in
   // dialog the current working directory can be changed.
   // The returned file name is always with respect to the
   // current directory.

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

const char *GetStringDialog(const char *prompt, const char *defval)
{
   // Prompt for string. The typed in string is returned.

   static QString answer;

   answer = QInputDialog::getText(
            "Enter text", prompt, QLineEdit::Normal,
            defval);

   return (const char *)answer;
}

Int_t GetIntegerDialog(const char *prompt, Int_t defval
                      ,int minValue = -2147483647, int maxValue = 2147483647
                      , int step = 1)
{
    // Prompt for integer. The typed in integer is returned.
    int res = QInputDialog::getInteger("Enter integer", prompt, defval
                                       , minValue,maxValue, int step);
    return res;
}

Double_t GetFloatDialog(const char *prompt, Double_t defval
                       , double minValue = -2147483647, double maxValue = 2147483647
                       , int decimals = 1)
{
   // Prompt for float. The typed in float is returned.
   Double_t res = QInputDialog::getDouble("Enter double", prompt, defval
                                           ,minValue, maxValue, decimals);
   return res;
}
