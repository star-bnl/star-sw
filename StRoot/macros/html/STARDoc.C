{
 // Macro to create the HTML doc of the STAR classes 
  gSystem->Load("libSTAR");

  const char *cls[] = {
                           "TCL","TChair","TDataSet","TDataSetIter",
                           "TFileSet","TObjectSet","TPoints3D","TPointsArray3D",
                           "TPolyLineShape","TTable","TTable3Points",
                           "TTableDescriptor","TTableIter","TTablePoints",
                           "TTableSorter","TVolume","TVolumePosition",
                           "TVolumeView","TVolumeViewIter","Ttypes","point3",
                           "tableDescriptor"
                          };
  int lClasses = sizeof(cls)/sizeof(char *);
  THtml html;
  html.SetOutputDir("$STAR/StRoot/html");
  html.SetSourceDir("$ROOTSYS:$ROOTSYS/include");
  for (int i=0; i < lClasses; i++)  html.MakeClass(cls[i],kTRUE);
}
