//*-- Author :    Valery Fine(fine@bnl.gov)   27/10/2006
// $Id: SetQtEnv.C,v 1.2 2006/10/27 20:51:12 fine Exp $
// This macro sets the Qt/Root environment "on fly" and 
// generates the correct ROOT resource ".rootrc" file also

  Int_t SetRootResource(const char *plugin, 
                       const char *lib,
                       const char *full=0,Bool_t append=kFALSE) 
  {
    fprintf(stderr," plugin %s  lib = %s", plugin, lib);
    Int_t success = 0;
    TString fullName;
    TString  fullValue;
    if (full) {
      fprintf(stderr," full = %s", full);
      fullName  = Form("lib%s",lib);
      fullValue = Form(full,lib);
    } else {
      fullValue = lib;
    }
    fprintf(stderr,"\n ------- \n");
    

     if ( !full || gSystem->DynamicPathName(fullName.Data(),kTRUE) ) {
       // Check plugin
        TString currentPlugin = gEnv->GetValue(plugin,"none");
        success = 1;
        if ( !currentPlugin.Contains(lib) ) 
        {
           TString p = "+"; 
           if (append) {
             p +=plugin;
             plugin = p.Data();
             fprintf(stderr, "Appending %s\n", plugin);
           }
           gEnv->SetValue(plugin,fullValue);
        }
     }
     return success;
  }

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
  // Check Qt-layer 
  if (SetRootResource(plugins[iPlugin++],plugins[iPlugin++],plugins[iPlugin++])) {
    SetRootResource(plugins[iPlugin++],plugins[iPlugin++],plugins[iPlugin++]);  
    // Check Qt-extension
    if (SetRootResource(plugins[iPlugin++],plugins[iPlugin++],plugins[iPlugin++])) {
      if (SetRootResource(plugins[iPlugin++],plugins[iPlugin++],plugins[iPlugin++])) {
       // Set Qt extentsion
       // skip Qt-layer setting
        iPlugin++;iPlugin++;iPlugin++;
      } else {
         // Set Qt-layer
         SetRootResource(plugins[iPlugin++],plugins[iPlugin++],plugins[iPlugin++]); 
      }  
   
      // Check QtGed 
      SetRootResource(plugins[iPlugin++],plugins[iPlugin++],plugins[iPlugin++]);

      // Check QtGL 
     if (SetRootResource(plugins[iPlugin++],plugins[iPlugin++],plugins[iPlugin++])) {
       // Check Open Inventor
       if (gSystem->DynamicPathName("libCoin",kTRUE)) {
          SetRootResource(plugins[iPlugin++],plugins[iPlugin++],plugins[iPlugin++],kTRUE);
       } else {
          fprintf( stderr, "\"Coin3d\" shared libraies has not beed detected\n");
       }
     }
   }
 }
//  gEnv->Print();
  gEnv->SaveLevel(kEnvLocal);
}
