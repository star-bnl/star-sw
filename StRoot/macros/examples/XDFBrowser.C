// $Id: XDFBrowser.C,v 1.4 1999/05/21 15:33:55 kathy Exp $
// $Log: XDFBrowser.C,v $
// Revision 1.4  1999/05/21 15:33:55  kathy
// made sure Log & Id are in each file and also put in standard comment line with name of owner
//
//=======================================================================
// owner: Valery Fine
// what it does: 
//=======================================================================
//*-- Author :    Valery Fine   21/01/99
class St_XDFFile;
TBrowser *xdfbrow;
St_XDFFile *xdf=0;
//
// This is a macro to browse the XDF files with ROOT TBrowser
// (It uses St_XDFFile::Browse method)
// To get this picture:
// Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/XDFBrowser.gif"> </P> End_Html 
// just load this macro: 
//
//  root[0] .L XDFBrowser.C
//
// Now you can "browse" your file with the statement:
//
// root[1] XDFBrowser("XDF file name here")
//           That's all !!!
// To get another file just re-call XDFBRowser("with another file name here")
//
Load(){
 gSystem->Load("St_base");
 gSystem->Load("xdf2root");
 gSystem->Load("St_Tables");
}
XDFBrowser(const Char_t *fileName="\0") {
   if (gClassTable->GetID("St_DataSet") < 0) {
      Load(); 
      cout << "Usage: XDFBrowser(\"XDF file name here\")" << endl;
   }
   Char_t filename[512]="\0";
   if (fileName && strlen(fileName)) strcpy(filename,fileName);
   while (!(filename && strlen(filename)) ) {
    // Prompt user:
    cout << "Input XDF file name, please: ";
    cin >> filename ;
//    gets( filename );
//    cout << endl;
   }
   if (xdf) delete xdf;
   xdf = new St_XDFFile();
   if (!xdf->OpenXDF(filename)) {
       if (xdfbrow) delete xdfbrow;
       xdfbrow = new TBrowser(filename,xdf);
   }
}
