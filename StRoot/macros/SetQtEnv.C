//*-- Author :    Valery Fine(fine@bnl.gov)   27/10/2006
// $Id: SetQtEnv.C,v 1.3 2006/10/30 19:45:14 fine Exp $
// This macro sets the Qt/Root environment "on fly" and 
// generates the correct ROOT resource ".rootrc" file also
//__________________________________________________________________
FILE *OpeFileName(const char *fileNamePrototype)
{
   // Open the file by the giverm file name
   return fopen( (const char*)GetNewFileName(fileNamePrototype),"w");
}
//__________________________________________________________________
TString GetNewFileName(const char *fileNamePrototype)
{
   // Find the filename for the given "fileNamePrototype"
   TString fileName = fileNamePrototype;
   gSystem->ExpandPathName(fileName);
   // Find the file extenstion if any
   Ssiz_t fileExtension = fileName.Last('.');
   TString fileNameHold = fileName;
   if ((fileExtension==0) || (fileExtension == -1)) {
      fileExtension = fileName.Length();
   }
   Int_t counter = 0;
   while (gSystem->AccessPathName(fileName.Data())==0) {
      fileName  = fileNameHold;
      fileName.Insert(fileExtension,Form(".%d",counter++));
   }
   return  fileName;     
}
  
//__________________________________________________________________
Int_t SetRootResource(FILE *file, const char *plugin, 
                       const char *lib,
                       const char *full=0,Bool_t append=kFALSE) 
{
   fprintf(stderr," plugin %s from the lib = %s", plugin, lib);
   Int_t success = 0;
   TString fullName;
   TString  fullValue;
   if (full) {
//      fprintf(stderr," full = %s", full);
      fullName  = Form("lib%s",lib);
      fullValue = Form(full,lib);
   } else {
      fullValue = lib;
   }
   fprintf(stderr,"\n ------- \n");
    

   if ( !full || gSystem->DynamicPathName(fullName.Data(),kTRUE) ) {
      // Check plugin
       success = 1;
       TString currentPlugin = gEnv->GetValue(plugin,"none");
       if ( !currentPlugin.Contains(lib) ) 
       {
          TString p = "+"; 
          if (append) {
             p +=plugin;
             plugin = p.Data();
            //  fprintf(stderr, "Appending %s\n", plugin);
          }
          gEnv->SetValue(plugin,fullValue);
          if (file) {
             fputs(Form("%s %s\n",plugin,fullValue.Data()), file);
          }
       }
    }
    return success;
}

//__________________________________________________________________
void SetQtEnv() {
  //------------------
  // Check GED library
  //------------------
  const char *plugins[] = {
                   "Plugin.TVirtualX"
                 , "GQt"
                 , " qt   TGQt %s  \"TGQt(const char*,const char*)\""
                                 
                 , "Gui.Backend", "qt", 0
                 
                 , "Plugin.TGuiFactory"                 
                 , "QtRootGui"
                 , " qtgui  TQtGUIFactory  %s  \"TQtGUIFactory()\""
                 
                 , "Gui.Factory", "qtgui", 0                  
                 
                 , "Gui.Factory", "qt"   , 0                 

                 , "Plugin.TVirtualPadEditor"
                 , "QtGed"
                 , " Ged TQtGedEditor   %s  \"TQtGedEditor(TCanvas*)\""

                 , "Plugin.TVirtualViewer3D"
                 , "RQTGL"
                 , " ogl    TQtRootViewer3D  %s \"TQtRootViewer3D(TVirtualPad*)\""

                 , "Plugin.TVirtualViewer3D"
                 , "RQIVTGL"
                 , " oiv   TQtRootCoinViewer3D  %s    \"TQtRootCoinViewer3D(TVirtualPad*)\""
                 , "", "", ""
          };
  Int_t iPlugin = 0;
  Int_t c = 0;
  TString fileName = GetNewFileName("rootrc");
  FILE *f =  fopen((const char *)fileName, "w");
  // Check Qt-layer 
  if (c+=SetRootResource(f,plugins[iPlugin++],plugins[iPlugin++],plugins[iPlugin++])) {
    c+=SetRootResource(f,plugins[iPlugin++],plugins[iPlugin++],plugins[iPlugin++]);  
    // Check Qt-extension
    if (c+=SetRootResource(f,plugins[iPlugin++],plugins[iPlugin++],plugins[iPlugin++])) {
      if (c+=SetRootResource(f,plugins[iPlugin++],plugins[iPlugin++],plugins[iPlugin++])) {
       // Set Qt extentsion
       // skip Qt-layer setting
        iPlugin++;iPlugin++;iPlugin++;
      } else {
         // Set Qt-layer
         c+=SetRootResource(f,plugins[iPlugin++],plugins[iPlugin++],plugins[iPlugin++]); 
      }  
   
      // Check QtGed 
      c+=SetRootResource(f,plugins[iPlugin++],plugins[iPlugin++],plugins[iPlugin++]);

      // Check QtGL 
     if (c+=SetRootResource(f,plugins[iPlugin++],plugins[iPlugin++],plugins[iPlugin++])) {
       // Check Open Inventor
       if (gSystem->DynamicPathName("libCoin",kTRUE)) {
          c+=SetRootResource(f,plugins[iPlugin++],plugins[iPlugin++],plugins[iPlugin++],kTRUE);
       } else {
          fprintf( stderr, "\"Coin3d\" shared libraies has not beed detected\n");
          fprintf( stderr, "Please, run \"source $STAR/QtRoot/qtgl/qtcoin/setup.csh\" script to set the Coin3D env.\n");
       }
     }
   }
 }
 
 fclose(f);
 if (c == 0) {
    fprintf(stderr," No shared library to actibvate the Qt-layter has been detected.\n Please talk with your SysAdmin\n");
    gSystem->Unlink(fileName.Data());
 }  else {
    fprintf(stderr," ----------------------------------------------------------\n");
    fprintf(stderr," The new version of ROOT resource file has been created: <%s>.\n",fileName.Data());
    fprintf(stderr," To active the Qt-layer - create a symlink \"ln -s %s .rootrc \" \n", fileName.Data());
    fprintf(stderr," and re-satrt your application\n");
    fprintf(stderr," ----------------------------------------------------------\n");    
 }
 //  gEnv->Print();
 //  gEnv->SaveLevel(kEnvLocal);
}
