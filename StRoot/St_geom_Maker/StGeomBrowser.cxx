#include "TSystem.h"
#include "StGeomBrowser.h"
#include "GeomBrowser.h"
#include "TObjString.h"

//----------------------------
//  STAR geom browser:
//----------------------------
// ZEBRA files        - .fz   extension
// ROOT  files        - .root extension
// ROOT  macros       - .C    extension
// OpenInvetor2 scene - .iv   extension
// VRML scene         - .wrl  extension
// STAR  geometry descriptor - 
// as defined by http://www.star.bnl.gov/STAR/comp/prod/MCGeometry.html
//______________________________________________________________
StGeomBrowser::StGeomBrowser(const char *fileName)
: fFileName(fileName), fBrowser(0) {}
//______________________________________________________________
StGeomBrowser::~StGeomBrowser()
{
    delete fBrowser; fBrowser=0;
}
//______________________________________________________________
void StGeomBrowser::SetFile(const char *fileName) 
{
   fFileName = fileName;
   gSystem->ExpandPathName(fFileName);    
   if (!fBrowser) fBrowser = new GeomBrowser();
   if ( fFileName.EndsWith(".C") || fFileName.EndsWith(".h")) {
      if (!gSystem->AccessPathName(fFileName.Data()))
           fBrowser->fileOpenMacro(fFileName.Data()); 
   } else if ( fFileName.EndsWith(".root") ) {         
      if (!gSystem->AccessPathName(fFileName.Data()))
                fBrowser->fileOpenRoot(fFileName.Data()); 
   } else if ( fFileName.EndsWith(".fz") ) {         
      if (!gSystem->AccessPathName(fFileName.Data())) fBrowser->fileOpenZebra(fFileName.Data()); 
   } else if ( fFileName.EndsWith(".iv") ) {         
      if (!gSystem->AccessPathName(fFileName.Data())) fBrowser->fileOpenInventor(fFileName.Data()); 
   } else if ( fFileName.EndsWith(".wrl") ) {         
      if (!gSystem->AccessPathName(fFileName.Data())) fBrowser->fileOpenInventor(fFileName.Data()); 
   }  else if ( fFileName.Length() <= 8) {
      // STAR geometry version
      fBrowser->SelectGeometry(fFileName.Data());
   } else  {
        // The last STAR geometry
        //fBrowser->fileOpenZebra(fFileName.Data());
   }
}

//______________________________________________________________
void StGeomBrowser::SetSize(Int_t w,Int_t h) {
   if (!fBrowser) fBrowser = new GeomBrowser();
   fBrowser->resize(w,h);
}

//______________________________________________________________
void StGeomBrowser::Show() {
   if (!fBrowser) fBrowser = new GeomBrowser();
   fBrowser->show();
}
