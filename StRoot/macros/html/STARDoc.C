{
 // Macro to create the HTML doc of the STAR classes 
  gSystem->Load("libSTAR");

  const char *cls[] = {
                           "TCL",     "TChair",    "TDataSet", "TDataSetIter",
                           "TFileSet","TObjectSet","TPoints3D","TPointsArray3D",
                           "TPolyLineShape",  "TTable",        "TTable3Points",
                           "TTableDescriptor","TTableIter",    "TTablePoints",
                           "TTableSorter",    "TVolume",       "TVolumePosition",
                           "TVolumeView",     "TVolumeViewIter"
                          };

  const char *former[] = {
                           "StCL","TChair","St_DataSet","St_DataSetIter",
                           "St_FileSet","St_ObjectSet","St_Points3D","St_PointsArray3D",
                           "St_PolyLineShape","St_Table","St_Table3Points",
                           "St_tableDescriptor","St_TableIter","St_TablePoints",
                           "St_TableSorter","St_Node","St_NodePosition",
                           "St_NodeView","St_NodeViewIter"
                          };

  int lClasses = sizeof(cls)/sizeof(char *);
  THtml html;
  TString htmlDir = "$STAR/StRoot/html";
  html.SetOutputDir(htmlDir.Data());
  html.SetSourceDir("$ROOTSYS:$ROOTSYS/include:$ROOTSYS/ROOT/root/STAR:");
  for (int i=0; i < lClasses; i++) {
      html.MakeClass(cls[i],kTRUE);
      // Create links for the sake of the backward compatibility
      if (former[i] && *former[i] && strcmp(cls[i],former[i])) {
        TString thisName = cls[i];   thisName += ".html";
        TString formName = htmlDir; formName += '/'; formName += former[i]; formName += ".html";
        gSystem->ExpandPathName(formName);
        gSystem->Unlink(formName.Data());
        gSystem->Symlink(thisName.Data(),formName.Data());
     }
  }
}
//_____________________________________________
// $id: $
// $Log: STARDoc.C,v $
// Revision 1.4  2000/05/20 01:24:52  perev
// CleanUp
//
// Revision 1.3  2000/05/12 23:13:29  fine
// Macro to create HTML for ROOT-based STAR classes
//
// Revision 1.2  2000/04/28 19:47:00  fine
// Creates symbolik links for the obsolete names
//
//_____________________________________________

