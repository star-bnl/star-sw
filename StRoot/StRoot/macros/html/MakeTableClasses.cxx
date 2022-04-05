// $Id: MakeTableClasses.cxx,v 1.3 2006/08/15 21:43:17 jeromel Exp $
// $Log: MakeTableClasses.cxx,v $
// Revision 1.3  2006/08/15 21:43:17  jeromel
// Fix rhic -> rhic.bnl.gov
//
// Revision 1.2  1999/05/21 15:33:51  kathy
// made sure Log & Id are in each file and also put in standard comment line with name of owner
//
//=======================================================================
// owner: Valery Fine
// what it does: 
//=======================================================================
void MakeTableClasses(){
//  gROOT->Reset();
//#pragma Ccomment on
  // Create the St_<table> classes, where <table> is a name of the STAF 
  // table.
  //
  // Input: The directory with stic-generated C+structure descritions
  // Output: St_%_Table.cxx and St_%_Table.h files

// Load the share libraries

  Char_t *libs[] = {"St_base"};
  Char_t *suffix=0;
  Int_t nlist = 1;
  Bool_t NT=kFALSE;
  if (strcmp(gSystem.GetName(),"WinNT") == 0 ) {
     NT = kTRUE;
//     gSystem.Load("Root_html.dll");
     suffix = ".dll";
  }
  else
     suffix =".so";
// Load the extra share /dynamic libraries if any
   Char_t buffer[256];
   for(Int_t i=0;i<nlist;i++) {
      strcpy(buffer,libs[i]);strcat(buffer,suffix);
      if (gSystem.Load(buffer))  printf(" Loading DLL \"%s\" failed \n",buffer);
  }

  void *dirhandle = 0;
  char *sourcedir = 0;
  if (NT) 
    sourcedir = "//sol/afs_rhic/star/packages/dev/.share/tables";
  else
    sourcedir = "/afs/rhic.bnl.gov/star/packages/dev/.share/tables";
  NT = kFALSE;
  dirhandle = gSystem->OpenDirectory(sourcedir);
  if (dirhandle) {
    char *na = 0; 
    Int_t count=0;  
    Char_t name[200];
    Char_t *structname = 0;
    while (na = gSystem->GetDirEntry(dirhandle) && count < 999993) {
      char *dot = 0;
      strcpy(name,na);
      if (!strstr(name,"St_") && (dot = strstr(name,".h")) ){
            Char_t *filename =  gSystem->ConcatFileName(sourcedir,name);
            printf("G__loadfile(\"%s\");\n",filename);
            G__loadfile(filename);
            *dot = 0;
            structname = strrchr(name,'/');

            if (structname) 
               structname++;
            else if (structname = strrchr(name,':'))
               structname++;
            else
               structname = name;

           printf("St_Table tabs(\"%s\",1);\n",structname);
           printf("tabs.StafStreamer();\n");
           St_Table *tablest = new St_Table(structname,1);
           if (tablest) {
//            tablest->StafStreamer();           
              delete tablest;
              count++;
           }
           else
              printf(" CINT Error. The \"%s\" was not done!\n",structname);
           if (filename) delete [] filename;
      }  
    }
    printf(" %d files have been converted\n",count);
  }
}
