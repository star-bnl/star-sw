// $Id: xdf2C.C,v 1.3 1999/05/21 15:34:02 kathy Exp $
// $Log: xdf2C.C,v $
// Revision 1.3  1999/05/21 15:34:02  kathy
// made sure Log & Id are in each file and also put in standard comment line with name of owner
//
// Revision 1.2  1999/04/01 00:55:17  perev
// xdf to .C converter
//=======================================================================
// owner:  Victor Perevoztchikov
// what it does: 
//=======================================================================
//
//
void xdf2C(const char *Input="$STAR/params",const char *Output="qweDB")
{
  int ierr;
  gSystem->Load("St_base");
  gSystem->Load("xdf2root");
  gSystem->Load("St_Tables");

  ofstream out; 
  St_DataSet *TOP = new St_DataSet("TOP");
  St_DataSet *Top = new St_DataSet("Top");
  TString TSInp = Input;
  TString TSOut = Output;
  gSystem->ExpandPathName(TSInp);
  gSystem->ExpandPathName(TSOut);


//  const char *Ipref = "/afs/rhic/star/packages/SL99c/";
  TString Ipref = gSystem->DirName((const char*)TSInp);Ipref +="/";
  const char *params =  "params" ;
  const char *params = gSystem->BaseName((const char*)TSInp);

//  const char *Ipref = "$ww/";
//  const char *Opref = "/afs/rhic/star/packages/SL99d/StDB/";
  TString Opref = TSOut; Opref +="/";
  gSystem->MakeDirectory((const char*)TSOut);

  TString OuputFile,CWD ;
  TString InputFile = Ipref; InputFile += params;

  St_FileSet *fs = new St_FileSet((char*)InputFile);
  TOP->Add(fs);
  St_DataSet *set;
  char path[100];
//  fs->ls(0);  

  St_DataSetIter next(TOP,9999);
  while (set = next()) { //loop over DIR 
    strcpy(path, (char*)(set->Path())+4);
    if (strstr(path,"CVS")) continue;
    InputFile = Ipref; InputFile.Append(path);
    OuputFile = Opref; OuputFile += path;

    printf("InputFile=%s\n",(const char*)InputFile);
    printf("OuputFile=%s\n",(const char*)OuputFile);


    if (strcmp("directory",set->GetTitle())==0) {	// directory
      ierr = gSystem->MakeDirectory((char*)OuputFile);
      printf("MakeDir=%s %d\n",(char*)OuputFile,ierr);
      CWD = OuputFile;
////      gSystem->ChangeDirectory((char*)OuputFile);
    } else {						// file
      if (!strstr((char*)InputFile,".xdf")) continue;
      printf("Process file %s -> %s\n",(char*)InputFile);
      St_XDFFile   *xdf_in   = new St_XDFFile((char*)InputFile,"r");

      St_DataSet *zet= xdf_in->NextEventGet();
      Top->Add(zet);
      St_DataSetIter Param(Top,9999);
      St_DataSet *t;

      while ((t = Param()))   {// xdf loop
        strcpy(path,(char*)t->Path()+4);

        TString FileC = CWD; FileC += "/"; FileC += path;
        printf("FileC=%s\n",(char*)FileC);
        if (t->HasData()){ // Table
          FileC +=".C";
          out.open((char*)FileC);
          printf("OpenOut %s \n",(char*)FileC);
          t->SavePrimitive(out,"");
          out.close();
        } else { // DataSet
          ierr=gSystem->MakeDirectory((char*)FileC);
          printf("MakeXdfDir=%s %d\n",(char*)FileC,ierr);
        }
 
        printf("%s\n",path);
      }// end of xdf loop
    delete zet;
    }// end if (DIR/FILE)
  }// end oop over DIR
}
