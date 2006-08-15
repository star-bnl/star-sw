// $Id: make1doc.C,v 1.4 2006/08/15 21:43:18 jeromel Exp $
// $Log: make1doc.C,v $
// Revision 1.4  2006/08/15 21:43:18  jeromel
// Fix rhic -> rhic.bnl.gov
//
// Revision 1.3  1999/05/21 15:33:59  kathy
// made sure Log & Id are in each file and also put in standard comment line with name of owner
//
//=======================================================================
// owner: Valery Fine
// what it does: 
//=======================================================================
void make1doc(const Char_t *docname,const Char_t *doctitle)
{
  gROOT.Reset();
  Char_t *libs[] = {"St_base","xdf2root","St_Tables", 
  "libmsg","libtls","tpc.sl","St_tpc","svt.sl","St_svt","StChain"};

 Char_t *suffix=0;
  Int_t nlist = 10;
  Bool_t NT=kFALSE;
  if (strcmp(gSystem.GetName(),"WinNT") == 0 ) {
     NT=kTRUE;
     gSystem.Load("Root_html.dll");
     suffix = ".dll";
  }
  else 
     suffix =".so";

   Char_t buffer[256];
   if (NT) {
    for(Int_t i=0;i<nlist;i++) {
       strcpy(buffer,libs[i]);
       if (!strchr(libs[i],'.')) strcat(buffer,suffix);
       if (gSystem.Load(buffer))  printf(" Loading DLL \"%s\" failed \n",buffer);
    }
    
    }
   else {
     gSystem->Load("St_base.so");
     gSystem->Load("xdf2root.so");
     gSystem->Load("St_Tables.so");
  
     gSystem->Load("libmsg.so");
     gSystem->Load("libtls.so");
     gSystem->Load("tpc.sl");
     gSystem->Load("St_tpc.so");
     gSystem->Load("svt.sl");
     gSystem->Load("St_svt.so");
     gSystem->Load("global.sl");
     gSystem->Load("St_global.so");
     gSystem->Load("ftpc.sl");
     gSystem->Load("St_ftpc.so");
     gSystem->Load("StChain.so");
   }
   
   
  //Create the object of the THtml class
  THtml *html = new THtml();

  char *sourcedir = 0;
  if (NT) 
    sourcedir = "//sol/afs_rhic/star/packages/dev/StRoot/base";
  else
    sourcedir = "/afs/rhic.bnl.gov/star/packages/dev/StRoot/base:/afs/rhic.bnl.gov/star/packages/dev/StRoot/xdf2root:/afs/rhic.bnl.gov/star/packages/dev/.share/tables:/afs/rhic.bnl.gov/star/packages/dev/inc";

  char *lookup = 0;
  if (NT) 
    lookup = "//sol/afs_rhic/star/packages/dev/.share/tables;../base;//sol/afs_rhic/star/packages/dev/.share/base";
  else
    lookup = "/afs/rhic.bnl.gov/star/packages/dev/StRoot/StChain:/afs/rhic.bnl.gov/star/packages/dev/StRoot/xdf2root:/afs/rhic.bnl.gov/star/packages/dev/.share/tables:/afs/rhic.bnl.gov/star/packages/dev/StRoot/base";

  html->SetSourceDir(lookup);

  if (NT) 
     html->SetOutputDir("J:/Public/STAF/draft/base/html");
  else
     html->SetOutputDir("/afs/rhic.bnl.gov/star/packages/dev/StRoot/html");

  // Create the list of the classes defined with the loaded DLL's to be documented

  Char_t *classes[] = {"St_XDFFile",  "St_Module",   "St_Table"
                       ,"St_DataSet", "St_DataSetIter","St_FileSet"
                       ,"StParticleView",
                       ,"StMaker",     "StChain"
                       ,"table_head_st", "St_TableSorter"
                        };
  Int_t nclass = 11;
  // Creat the definitions of the classes not derived from TObjects
  if (NT) {
     gROOT->LoadMacro("//sol/afs_rhic/star/packages/dev/inc/table_header.h");
  }
  else
     gROOT->LoadMacro("/afs/rhic.bnl.gov/star/packages/dev/inc/table_header.h");

  TClass header1("table_head_st",1,"table_header.h","table_header.h");
  // Make class descriptions
  Int_t i=0;

//  for (i=0;i<nclass;i++) 
//                   html.MakeClass(classes[i]);

  // Make HTML docs for the "plain" text files those are not in the dictionaries

  html.Convert(docname,doctitle);
  
//  html.MakeClass("EModuleTypes");
//  html.MakeIndex();
}
