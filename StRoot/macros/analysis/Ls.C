// $Id: Ls.C,v 3.2 2009/03/27 19:06:15 fine Exp $
// Author: Valeri Fine (fine@bnl.gov) 27.03.2009

///This macro prints the list of the object name from the /a dir TDirectory
// if its name match the /a pattern,
// where /a pattern can be either the TRegexp or "wildcard" as
// the parameter /a wildcard defines
//
// Example: to print all histograms with the names
// =======  those contain the "hpy" substring
// type:  Ls("hpy");

#ifndef __CINT__
#  include "TDirectory.h"
#  include "StFileIter.h"
#  include "TRegexp.h"
#  include "TKey.h"
#endif 
int Ls(TDirectory *dir, const char *pattern="*", Bool_t wildcard=kTRUE)
{ 
   int counter = 0;
   if (!dir) dir = gDirectory;
   if (dir) {
      StFileIter iter(dir);
      TRegexp cmp(pattern,wildcard);
      TKey *key = 0;
      Ssiz_t  len=0;
      while (key = iter.NextEventKey()) {
         const char *name = key->GetName();
         Ssiz_t test = cmp.Index(name,&len);
         if ((test>=0) && len) {
             Printf("- %s class: %s", name, key->GetClassName()); 
             counter++;
         }
      }
      Printf(" ------------");
      Printf(" %d object%s ha%s been found", counter
            , ((counter>1) ? "s" : "")
            , ((counter>1) ? "ve": "s")
            );
   }
   return counter;
}

///This macro prints the list of the object name from the current TDirectory
// if its name match the /a pattern.
// where /a pattern can be either the TRegexp or "wildcard" as
// the parameter /a wildcard defines
// 
int Ls(const char *pattern="*", Bool_t wildcard=kTRUE)
{
   return Ls(gDirectory,pattern, wildcard);
}
