// Convert TDataSet from a root file to strucuted Cint files
void mkdir(const Char_t *path) {
  TString OutputDir(path);
  TString separator("/");
  TObjArray *array = OutputDir.Tokenize(separator);
  TIter nextD(array); 
  TString dir("");
  while ((objs = (TObjString *) nextD())) {
    dir +=  objs->GetString();
    cout << dir;
    if (! gSystem->AccessPathName(dir)) cout << " has been accessed" << endl;
    else {
      if (! gSystem->MakeDirectory(dir) ) cout << " has created" << endl;
      else {cout << " failed" << endl;}
    }
    //    cout << "Done with " << dir << endl;
    dir += "/";
  }
  delete array; array = 0;
}
//________________________________________________________________________________
void TD2C(const char *Input="$STAR/StarDb/VmcGeometry",const char *Output="./StarDb/VmcGeometry")
{
  int ierr;
  gSystem->Load("St_Tables");

  ofstream out; 
  TString OutputDir(Output);
  mkdir(OutputDir);
  TString sep(".");
  TFileSet *fs = new TFileSet(Input);
  TDataSet *set;
  TString Path("");
  TDataSetIter next(fs,9999);
  while (set = next()) { //loop over DIR 
    Path = set->Path();
    if (! Path.EndsWith(".root")) continue;
    cout << "next set " << Path << endl;
    TString InputFile(gSystem->DirName(Input));
    InputFile += "/";
    InputFile += Path;
    TString fName(gSystem->BaseName(InputFile));
    fName.ReplaceAll(".root","");
    array = fName.Tokenize(sep);
    TString TimeStamp("");
    Int_t i = 1;
    TObjString *string = 0;
    while ((string = (TObjString *) array->At(i))) {
      if (TimeStamp != "") TimeStamp += ".";
      TimeStamp += string->GetString();
      //      cout << "i " << i << "\t" << TimeStamp << endl;
      i++;
    }
    delete array; array = 0;
    cout << "TimeStamp = " << TimeStamp << endl;
    TFile *f = new TFile(InputFile,"read");
    if (! f) {cout << "Can't open " << InputFile << endl; continue;}
    else     {cout << InputFile << " has been opened" << endl;}
    
    TIter nextkey( gDirectory->GetListOfKeys() );
    TKey *key = 0;
    while ((key = (TKey*) nextkey())) {
      TObject *obj = key->ReadObj();
      if ( ! obj->IsA()->InheritsFrom( "TDataSet" ) ) continue;
      if (   obj->IsA()->InheritsFrom( "TObjectSet" ) ) continue;
      cout << "Found " << obj->GetName() << endl;
      TDataSet *tset = (TDataSet *) obj;
      cout << "OutputDir " << OutputDir << "\t" << tset->Path() << endl;
      TDataSetIter iter(tset,99);
      TDataSet *z = 0;
      while ((z = iter.Next())) {
	if (! z->HasData()) continue;
	cout << z->GetName() << "\t" << z->Path() << endl;
	TString FileC = OutputDir; FileC += "/"; FileC += z->Path();
	FileC += "."; FileC += TimeStamp; FileC += ".C";
	TString outputdir(gSystem->DirName(FileC));
	mkdir(outputdir);
	cout << "Create " << FileC << endl;
	out.open(FileC.Data());
	z->SavePrimitive(out,"");
	out.close();
      }
      delete obj;
    }// key loop 
    delete f;
  }// end oop over DIR
}
