{
  gSystem->Load("libStar");
  new THtml;
  TString src = gHtml->GetSourceDir();
  src += ":../:$ROOTSYS/include";
  gHtml->SetSourceDir(src.Data());
  const char *clNames[] = 
  {
           "TChair"
         , "TColumnView"
         , "TDataSet"
         , "TDataSetIter"
         , "TDsKey"
         , "TFileSet"
         , "TFileIter"
         , "TGenericTable"
         , "TObjectSet"
         , "TTable"
         , "TTableDescriptor"
         , "TTableIter"
         , "TTableSorter"
         , "TTableMap"
         , "TModule"
       };
  int nClasses = sizeof(clNames)/sizeof(char *);
  int i;
  for (i=0;i<nClasses;i++) {
    gHtml->MakeClass(clNames[i]);
  }
  printf("\n Generate an example Web pages\n");
  const char *testSubdirectory = "test/";

   const char *macroNames[] = {
       "MapTableTest.C","Complex example of generating \"Event-like\" structure"
      ,"SmallMapTest.C","Reference from of table to another table" 
      ,"TestFileIter.C","How to loop over ROOT file event by event"
      ,"staffEx17.C",   "example of the table analysis a'la PAW example #17"  
      ,"GenericTable.C","Generic table demo / test"
      ,"TestDataSet.C", "TDataSet methods demo/test"
      ,"staff.C",       "ASCII file reading and analyzing"
      ,"staffEx18.C",   "Example of the table analysis a'la PAW example #18"  
      ,"P2Test.C",     "Example how to create a ROOT file for Pythia event generator"  
      ,"LoadPythia.C",  "Example how to load the share libraries one needs to read Pythia ROOT files"  
      ,"ReadPythia.C",  "Example how to read and loop over the events created by Pythia"  
      ,"MakeDoc.C",     "How to create HTML documentation"
   };
   int nMacros = sizeof(macroNames)/sizeof(char *);
   const char *htmlHead[] = {
    "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2//EN\">"
    ,"<HTML>"
    ,"<HEAD>"
    ,"   <TITLE>RootKernel Tutorials</TITLE>"
    ,"   <META NAME=\"rating\" CONTENT=\"General\">"
    ,"   <META NAME=\"objecttype\" CONTENT=\"Manual\">"
    ,"   <META NAME=\"keywords\" CONTENT=\"RootKernel software development, oo, object oriented, unix, x11, motif, windows nt, c++, html, valeri fine\">"
    ,"   <META NAME=\"description\" CONTENT=\"ROOT Kernel - C++ class library for an Object Oriented Framework For Large Scale Data Analysis ROOT\">"
    ,"</HEAD>"
    ,"<CENTER><P><LINK rev=made href=\"mailto:fine@bnl.gov\"></P></CENTER>"
    ,"<H1 ALIGN=CENTER><FONT SIZE=+4>RootKernel Tutorials</FONT></H1>"
    ,"<P>"
    ,"<HR></P>"
  };
   TString flName = gHtml->GetOutputDir();

  flName += "/examples/index.html";
  FILE *htmlTestIndex = fopen(flName.Data(),"w");
  if (htmlTestIndex) {
    int lHead = sizeof(htmlHead)/sizeof(char *);
    int i=0;
    for (i=0;i<lHead;i++) fprintf(htmlTestIndex,"%s\n",htmlHead[i]);

     for (i=0;i<nMacros;i++) {
      TString s = testSubdirectory;
      s += macroNames[i++];
      gHtml->Convert(s.Data(),macroNames[i]);
      fprintf(htmlTestIndex,"<LI><A HREF=\"%s.html\">%s</A></LI>\n",macroNames[i-1],macroNames[i]);
     }
     TDatime today;
     fprintf(htmlTestIndex,"<P>\n<HR></P>\n<ADDRESS><A HREF=\"mailto:fine@bnl.gov\">Valeri Fine</A>\n</A>Last update");
     fprintf(htmlTestIndex,"%s",today.AsString());
     fprintf(htmlTestIndex," by VF</ADDRESS>\n</BODY>\n</HTML>\n");
     fclose(htmlTestIndex);
  }
}
