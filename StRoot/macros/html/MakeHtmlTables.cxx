{
  // Create the HTML page for STAF tables

  // Load the dynamic library for the STAF base classes
  gSystem.Load("../base/St_base.dll");

  // Load the dynamic library for the STAF table wrappers and STAF tables
  gSystem.Load("St_tables.dll");

  // Load the ROOT dynamic library for the THtml class
  gSystem.Load("Root_Html.dll");

  void *dirhandle = 0;
  const Char_t *affix="_st";
  char *sourcedir = "//sol/afs_rhic/star/packages/new/.share/tables";
  if (!(dirhandle = gSystem->OpenDirectory(sourcedir))) return;

  // Create THtml object
  THtml *html = new THtml;

  char *lookup = "//sol/afs_rhic/star/packages/new/.share/tables;../base;//sol/afs_rhic/star/packages/new/.share/base";
  html->SetSourceDir(lookup);
  html->SetOutputDir("J:/Public/STAF/draft/base/html");
  html.MakeClass("St_Table",kTRUE);

  char *n = 0;
  
  while (n = gSystem.GetDirEntry(dirhandle)) {
    Char_t *name = StrDup(n);
    // test the prefix
     if (strstr(name,"St_") == name) {

       // test the tail
        Char_t *tail = 0;
        if (tail = strstr(name,"_Table.h")) {
          *tail = '\0';

          // Make class description
          html.MakeClass(name);

          // Make C structure name
          Int_t i;
          for (i=0; i < strlen(name)-3; i++) name[i] = name[i+3];
          name[i] = '\0';
          strcat(name,affix);
          html.MakeClass(name);
         } 
     }
     free(name);
   }  
   // Make html for itself
    html.Convert("./MakeHtmlTables.cxx","How to create the HTML doc for the STAF table wrappers");
}
