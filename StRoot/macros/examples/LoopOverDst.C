//*CMZ :          01/04/99  18.27.27  by  Valery Fine(fine@mail.cern.ch)
// $Id: LoopOverDst.C,v 1.4 2000/04/13 21:46:21 kathy Exp $
// $Log: LoopOverDst.C,v $
// Revision 1.4  2000/04/13 21:46:21  kathy
// remove loading of libtpc_Tables since l3Track table is now dst_track type from global
//
// Revision 1.3  2000/04/12 16:13:40  kathy
// have changed so that macro loads only table libraries needed instead of all table libraries
//
// Revision 1.2  1999/05/21 15:33:51  kathy
// made sure Log & Id are in each file and also put in standard comment line with name of owner
//
// Revision 1.1  1999/04/01 19:39:29  fine
// Macro to lopp over the dst MDC@ files
//
//=======================================================================
// owner: Valery Fine
// what it does:  Loop over dst files created during MDC2
//=======================================================================
// Loop over dst files created during MDC2
TCanvas *QACanvas = 0;
TBrowser *QABrowser = 0;
class StChain;
class St_DataSet;
StChain  *chain=0;

void Analysis(St_DataSet *set);

//________________________________________________________
void Load()
{
    gSystem->Load("St_base");
    gSystem->Load("StChain");

  gSystem->Load("libglobal_Tables");
  gSystem->Load("libgen_Tables");
  gSystem->Load("libsim_Tables");

    gSystem->Load("St_io_Maker");
};

//________________________________________________________
void  MakeCustomMacro(const Char_t *fileName){
  // This function creates a "template" of the analysis code to be used in "loop"
  FILE *f = fopen(fileName,"w");
  if (!f) return;
  const Char_t *codePatter[] = {
          "void Analysis(St_DataSet *set){                                                  "
        , "  printf(\" Found \\\"void Analysis(St_DataSet *set)\\\" and put in there \\n\");"
        , "  printf(\" your favourite analysis\\n\");                     "
        , "  set->ls(\"*\");                                              "
        , "}"
  };
  Int_t lenOfPatter = sizeof(codePatter)/4;
  printf("\n %s:\n",fileName);
  for (int i=0;i<lenOfPatter;i++) {
    fprintf(f,"%s\n",codePatter[i]);
    printf("\t%s\n", codePatter[i]);
  }
  fclose(f);
  printf(" Please find \"%s\" file with function \n\n",fileName);
  printf("\t void Analysis(St_DataSet *set)\n\n");
  printf(" and put in there your favourite analysis code\n");         
  printf(" or just call LoopOverDst() function again \"AS IS\":\n");         
  printf(" root[1] LoopOverDst()\n");         
}

//________________________________________________________
void LoopOverDst(
//     const Char_t *fileName="/disk00001/star/auau200/venus412/default/b0_3/year_1b/hadronic_on/tss/psc0067_02_40evts.root",
     const Char_t *fileName="/disk00001/star/auau200/venus412/default/b0_3/year_1b/hadronic_on/tss",
     const Char_t *customMacroFile = "Analysis.C"
     )
{ 
    cout << endl   
         << " TURN the ROOT/CINT optimization OFF FIRST !!!" << endl
         << " with root[0] .O0 " << endl 
         << " command: \"dot\"-\"Ou\"-\"ZERO\" " << endl << endl       
         << " Usage:  LoopOverDst( const Char_t *fileName =\""     << fileName      << "\","   << endl
         << "                      const Char_t *customMacroFile =\""     << customMacroFile  << "\","   << endl
         << " Note: fileName      - may define either file or entire directory tree." << endl
         << " -----                 For the directory all files from that will be used." << endl
         << endl ;
  if (gClassTable->GetID("StChain") < 0) Load();
  // check whether the custom macro has been supplied 
  if (gSystem->AccessPathName(customMacroFile) == 0)
      gROOT->LoadMacro(customMacroFile);
  else {
      MakeCustomMacro(customMacroFile);
      return;
  }
  //  check file first
  Long_t id;
  Long_t size;
  Long_t flags; 
  Long_t modtime;
  Char_t *exFileName = gSystem->ExpandPathName(fileName);
  if (gSystem->GetPathInfo(exFileName, &id, &size, &flags, &modtime)) 
  {
   cerr << " *** Error ***  Can not find file: \"" << fileName << "\"" << endl;
   delete [] exFileName;
   exFileName=0;
   return;
  }

  if (!chain) {
    chain = new StChain("bfc");
    St_io_Maker *in    = new St_io_Maker("Input","all");
    Int_t  nFile = 0;
    if ((flags & 2)) {

//*-*  Add a files or files to the list of dst ones 

      St_FileSet dstDirs(exFileName);
      St_DataSetIter nextDataSet(&dstDirs,0);
      St_DataSet *set = 0;      
      while ( (set = nextDataSet()) ) {           
        if (strcmp(set->GetTitle(),"file") || !(strstr(set->GetName(),"evts.root"))) continue;
        TString p = set->Path();
        Char_t *rootfilename = gSystem->ConcatFileName(exFileName,p.Data());
        cout << "Including file " << rootfilename << " into list of the files " << endl;
        in->AddFile(rootfilename);
        nFile++;
      } 
    } else {
      in->AddFile(exFileName);
      nFile++;
    }
    cout << "Total: " << nFile << " files will be analysed" << endl ;

    in->SetMaxEvent(4);
  }

  chain->Init();
  chain->PrintInfo();
  int i;
  for (i=1;i<11111111;i++)  {
    if (!chain->Make(i)) {
     St_DataSet *ds =  chain.DataSet("dst");
     if (ds) Analysis(ds);
     chain->Clear();
    }
    else break;
  }
  chain->Finish();
  if (QABrowser) delete QABrowser;
//  QABrowser = new TBrowser;
  delete [] exFileName;
  delete [] exPsFile;
}
