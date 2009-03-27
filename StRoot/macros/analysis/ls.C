// $Id: ls.C,v 3.1 2009/03/27 16:31:37 fine Exp $
// Author: Valeri Fine (fine@bnl.gov) 27.03.2009

///This macro prints the list of the object name from the /a dir TDirectory
// if its name match the /a pattern,
// where /a pattern can be either the TRegexp or "wildcard" as
// the parameter /a wildcard defines
// 
int ls.C(TDirectory *dir, const char *pattern="*", Bool_t wildcard=kTrue);
{
   if (!dir) dir = gDirectory;
   if (dir) {
      StFileIter iter(dir);
      TRegexp cmp(patter,wildcard);
      int counter = 0;
      while (key = itex.NextEventKey()) {
         const char *name = key->GetName() 
         if (cmp.Index(name)) {
             Printf("- %s class: %s \n", name, key->GetClassName); 
             counter++;
         }
      }
      Prinf(" %d  ha%s been found\n", counter>1 ? "ve": "s");
   }
}

///This macro prints the list of the object name from the current TDirectory
// if its name match the /a pattern.
// where /a pattern can be either the TRegexp or "wildcard" as
// the parameter /a wildcard defines
// 
int ls.C(const char *pattern="*", Bool_t wildcard=kTrue);
{
   return ls(gDirectory,pattern, wildcard);
}
