// $Id: MakeHtmlTables.cxx,v 1.8 1999/09/12 01:09:57 fine Exp $
// $Log: MakeHtmlTables.cxx,v $
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
  char *sourcedir = 0;
  if (NT) 
    sourcedir = "//sol/afs_rhic/star/packages/dev/.share/tables";
  else
    sourcedir = "/afs/rhic/star/packages/dev/include/tables";
    //    sourcedir = "/afs/rhic/star/packages/dev/.share/tables";

  if (!(dirhandle = gSystem->OpenDirectory(sourcedir))) return;

  // Create THtml object
  THtml *html = new THtml;

  TString lookup = 0;
  if (NT) 
    lookup = "//sol/afs_rhic/star/packages/dev/.share/tables;../base;//sol/afs_rhic/star/packages/dev/.share/base";
  else {
    lookup  = "/afs/rhic/star/packages/StAF/dev/inc:";
    lookup += "/afs/rhic/star/packages/dev/:";
    lookup += "/afs/rhic/star/packages/dev/include/tables:";
    lookup += "/afs/rhic/star/packages/dev/.share/tables:";
    lookup += "/afs/rhic/star/packages/dev/StRoot/St_base";
  }

  html->SetSourceDir(lookup.Data());
  if (NT) 
     html->SetOutputDir("J:/Public/STAF/draft/base/html");
  else
     html->SetOutputDir("/afs/rhic/star/packages/dev/StRoot/html");

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
     free(name);
   }
   gSystem->FreeDirectory(dirhandle);
   // Make html for itself
   html->Convert("./MakeHtmlTables.cxx","How to create the HTML doc for the STAF table wrappers");
}
