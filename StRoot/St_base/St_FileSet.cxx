//*CMZ :          12/07/98  18.27.27  by  Valery Fine(fine@mail.cern.ch)
//*-- Author :    Valery Fine(fine@mail.cern.ch)   03/07/98
 
#include "St_FileSet.h"
#include "TBrowser.h"
#include "TSystem.h"
 
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_FileSet                                                           //
//                                                                      //
// St_FileSet class is a class to convert the                           // 
//      "native file system structure"                                  //
// into an instance of the St_DataSet class                             //
//                                                                      //
//  Example:                                                            //
//    How to convert your home directory into the OO dataset           //
//                                                                      //
//  root [0] TString home = "$HOME";                                    //
//  root [1] St_FileSet set(home);                                      //
//  root [2] TBrowser b("MyHome",&set);                                 //
//  root [3] set.ls("*");                                               //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
 
ClassImp(St_FileSet)
//______________________________________________________________________________
  St_FileSet::St_FileSet()
    : St_DataSet(){}
//______________________________________________________________________________
St_FileSet::St_FileSet(const TString &dirname,const Char_t *setname,Bool_t expand)
           : St_DataSet()
{
  //
  // Creates St_FileSet  
  // Convert the "opearting system" file system tree into the memory resided St_FileSet
  //
  //  Parameters:
  //  -----------
  //  dirname  - the name of the "native file system" directory
  //             to convert into St_FileSet
  //  setname  - the name of this St_FileSet ("." by default)
  //  expand   - flag whether the "dirname" must be "expanded 
  //             (kTRUE by default)
  //
  St_FileSet *set = 0;
  Long_t id, size, flags, modtime;
  TString dirbuf = dirname;
  if (expand) gSystem->ExpandPathName(dirbuf);
  const Char_t *name = dirbuf.Data();
  if (gSystem->GetPathInfo(name, &id, &size, &flags, &modtime)==0) {
    TString nextobj = name;
    if (!setname) SetName(name);
    else          SetName(setname);

    // Check if "dirname" is a directory.
    void *dir = 0;
    if (flags & 2 ) 
       dir = gSystem->OpenDirectory(name);
    if (dir) {   // this is a directory
      SetTitle("directory");
      while (name = gSystem->GetDirEntry(dir)) {
         // skip some "special" names
         if (strcmp(name,"..")!=0 && strcmp(name,".")!=0) {
           Char_t *file = gSystem->ConcatFileName(dirbuf,name);
           TString nextdir = file;
           delete [] file;
           Add(new St_FileSet(nextdir,name,kFALSE));
         }
      }
    }
    else 
       SetTitle("file");
  }
}
//______________________________________________________________________________
St_FileSet::~St_FileSet(){}
//______________________________________________________________________________
Bool_t St_FileSet::IsFolder()
{
 // If the title of this St_FileSet is "file" it is NOT folder
 // see: St_FileSet(TString &dirname,const Char_t *setname,Bool_t expand)
 //
 return strcmp(GetTitle(),"file")!=0;
} 

