// $Id: MakeHtmlTables.cxx,v 1.12 2006/08/15 21:43:16 jeromel Exp $
// $Log: MakeHtmlTables.cxx,v $
// Revision 1.12  2006/08/15 21:43:16  jeromel
// Fix rhic -> rhic.bnl.gov
//
// Revision 1.11  2001/09/02 00:03:22  fine
// type fixed
//
// Revision 1.10  1999/11/19 21:20:23  fine
// Extra path to /afs/rhic.bnl.gov/star/packages/SL99i include dirictory was added
//
// Revision 1.9  1999/09/14 15:30:39  fine
// makedocs adjusted to the new source tree
//
// Revision 1.8  1999/09/12 01:09:57  fine
// Adjusted to the new source tree
//
// Revision 1.7  1999/05/21 15:33:51  kathy
// made sure Log & Id are in each file and also put in standard comment line with name of owner
//
//=======================================================================
// owner: Valery Fine
// what it does: Create the HTML page for STAF tables
//=======================================================================
{
  // Create the HTML page for STAF tables

// Load the share libraries

  Char_t *libs[] = {"St_base","St_Tables"};
  Char_t *suffix=0;
  Int_t nlist = 2;
  Bool_t NT=kFALSE;

  if (strcmp(gSystem->GetName(),"WinNT") == 0 ) {
     NT = kTRUE;
     gSystem.Load("Root_html.dll");
  }
  
  gSystem->Load("St_base");
  gSystem->Load("St_Tables");
  
  cout << "Dynamic libraries have been loaded" << endl;

  void *dirhandle = 0;
  const Char_t *affix="_st";
  TString sourcedir;
  if (NT) 
    sourcedir = "//sol/afs_rhic/star/packages/dev/.share/tables";
  else
    sourcedir = "$STAR/include/tables";

  gSystem->ExpandPathName(sourcedir);
  if (!(dirhandle = gSystem->OpenDirectory(sourcedir.Data()))) return;

  // Create THtml object
  THtml *html = new THtml;

  TString lookup = 0;
  if (NT) 
    lookup = "//sol/afs_rhic/star/packages/dev/.share/tables;../base;//sol/afs_rhic/star/packages/dev/.share/base";
  else {
    lookup  = "$STAF/inc:";
    lookup += "$STAR:";
    lookup += sourcedir;
    lookup += ":";
    lookup += "$STAR/include:";
    lookup += "$STAR/include/tables:";
    lookup += "$STAR/StRoot/St_base";
    lookup += "$STAR/.share/tables:";
  }

  html->SetSourceDir(lookup.Data());
  if (NT) 
     html->SetOutputDir("J:/Public/STAF/draft/base/html");
  else
     html->SetOutputDir("$STAR/StRoot/html");

  html.MakeClass("St_Table",kTRUE);

  char *n = 0;
  
  while (n = gSystem->GetDirEntry(dirhandle)) {
    Char_t *name = StrDup(n);
    // test the prefix
     if (strstr(name,"St_") == name) {      
       // test the tail
        Char_t *tail = 0;
        if (tail = strstr(name,"_Table.h")) {
          *tail = '\0';

          // Make class description
          html.MakeClass(name,kTRUE);

          // Make C structure name
          Int_t i;
          for (i=0; i < strlen(name)-3; i++) name[i] = name[i+3];
          name[i] = '\0';
          strcat(name,affix);
          html.MakeClass(name,kTRUE);
         } 
     }
     delete [] name ;
   }
   gSystem->FreeDirectory(dirhandle);
   // Make html for itself
   html->Convert("$STAR/StRoot/html/MakeHtmlTables.cxx","How to create the HTML doc for the STAF table wrappers");
}
