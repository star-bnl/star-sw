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
XDFBrowser(const Char_t *filename) {
   if (gClassTable->GetID("St_DataSet") < 0) Load(); 
   if (xdf) delete xdf;
   xdf = new St_XDFFile();
   if (!xdf->OpenXDF(filename)) {
       if (xdfbrow) delete xdfbrow;
       xdfbrow = new TBrowser(filename,xdf);
   }
}
