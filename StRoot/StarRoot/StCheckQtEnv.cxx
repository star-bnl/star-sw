//*-- Author :    Valery Fine(fine@bnl.gov)   27/10/2006
//
// $Id: StCheckQtEnv.cxx,v 1.10 2010/02/26 01:23:06 fine Exp $
// This class sets the Qt/Root environment "on fly" and 
// generates the correct ROOT resource ".rootrc" file 
// also

#include "StCheckQtEnv.h"
#include "TSystem.h"
#include "TApplication.h"
#if ROOT_VERSION_CODE >= ROOT_VERSION(5,00,0)
#include "TPRegexp.h"
#endif
#include "TEnv.h"
namespace {
class InitGraph { 
   public: 
     InitGraph() {
         gEnv->SetValue("Gui.Backend", "qt");
         gEnv->SetValue("Gui.Factory", "qtgui");
         gApplication->NeedGraphicsLibs(); 
         gApplication->InitializeGraphics();
      }
};
}

//__________________________________________________________________
FILE *StCheckQtEnv::OpeFileName(const char *fileNamePrototype)
{
   // Open the file by the given file name
   return fopen( (const char*)GetNewFileName(fileNamePrototype),"w");
}
//__________________________________________________________________
TString StCheckQtEnv::GetNewFileName(const char *fileNamePrototype)
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
//Int_t StCheckQtEnv::SetRootResource(FILE *file, const char *plugin, 
//                       const char *lib, const char *full,Bool_t append) 
Int_t StCheckQtEnv::SetRootResource(FILE *file, const char *plugin, 
                       const char *lib, const char *full,Bool_t append) 
{
   fprintf(stderr," Testing the plugin <%s> from the lib = <%s>\n", plugin, lib);
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
    
   if ( !full || gSystem->DynamicPathName(fullName.Data(),kTRUE) ) {
      // Check plugin
       success = 1;
       TString currentPlugin = gEnv->GetValue(plugin,"none");
#if ROOT_VERSION_CODE >= ROOT_VERSION(5,00,0)
        TPRegexp exp(Form("\\b%s\\b",lib));
#else
        TString exp(Form(" %s ",lib));
#endif                
       if ( !currentPlugin.Contains(exp) ) 
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
       } else {
         //  fprintf(stderr, "Found %s with <%s>\n", currentPlugin.Data(), Form("\\s+?%s\\s+?",lib));
       }
    }
    return success;
}

//__________________________________________________________________
Long_t  StCheckQtEnv::SetQtEnv(bool checkCoin) {
  //------------------
  // Check GED library
  // Return value:
  //        -1 - Wrong Qt env. need SysAdmit to fix it
  //         0 - Correct env. 
  //       > 0 - Wrong env. The user should fix his/her env.
  //------------------
  static InitGraph init_graph;
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
  Long_t c = 0;
  TString fileName = GetNewFileName("rootrc");
  FILE *f =  fopen((const char *)fileName, "w");
  if (f) {
  // Check Qt-layer 
  if ((c+=SetRootResource(f,plugins[iPlugin],plugins[iPlugin+1],plugins[iPlugin+2]))) {
    iPlugin+=3;
    c+=SetRootResource(f,plugins[iPlugin],plugins[iPlugin+1],plugins[iPlugin+2]);  
    iPlugin+=3;
    // Check Qt-extension
    if ((c+=SetRootResource(f,plugins[iPlugin],plugins[iPlugin+1],plugins[iPlugin+2]))) {
      iPlugin+=3;
      if ((c+=SetRootResource(f,plugins[iPlugin],plugins[iPlugin+1],plugins[iPlugin+2]))) {
 //         iPlugin+=3;
       // Set Qt extension
       // skip Qt-layer setting
          iPlugin+=3;
      } else {
         // Set Qt-layer
         iPlugin+=3;
         c+=SetRootResource(f,plugins[iPlugin],plugins[iPlugin+1],plugins[iPlugin+2]); 
      }  
   
      // Check QtGed 
      iPlugin+=3;
      c+=SetRootResource(f,plugins[iPlugin],plugins[iPlugin+1],plugins[iPlugin+2]);
      // Check QtGL 
      iPlugin+=3;
     
     if ((c+=SetRootResource(f,plugins[iPlugin],plugins[iPlugin+1],plugins[iPlugin+2]))) {
       // Check Open Inventor
       Long_t savedC = c;

       if (checkCoin) {
          if (gSystem->DynamicPathName("libCoin",kTRUE)) {
             iPlugin+=3;
             c+=SetRootResource(f,plugins[iPlugin],plugins[iPlugin+1],plugins[iPlugin+2],kTRUE);
          } else {
             fprintf(stderr," ----------------------------------------------------------\n");
             fprintf(stderr,"                        ATTENTION :                        \n");
             fprintf(stderr,"\"Coin3d\" shared libraries has not beed detected\n");
             fprintf(stderr," Please, run:\n\n"); 
             fprintf(stderr,"=====  \"source $STAR/QtRoot/qtgl/qtcoin/setup.csh\"  =====\n\n");
             fprintf(stderr," script to activated the advanced (recommended) Coin3D env.\n");
             fprintf(stderr," ----------------------------------------------------------\n");
             fprintf(stderr," and re-start your application\n");
             fprintf(stderr," ----------------------------------------------------------\n");    
             c = savedC; // no libCoin. we still can use it
             gSystem->Sleep(5000);
          } }
 } } }
 
 fclose(f);
 } else {
  c = 1;
 }
 if (c == 0) {
    fprintf(stderr," No shared library to activate the Qt-layer has been detected.\n");
    fprintf(stderr," Please talk to your SysAdmin\n");
    gSystem->Unlink(fileName.Data());
    c = -1;
 }  else {
    Long_t id; Long_t size; Long_t flags; Long_t modtime;
    gSystem->GetPathInfo(fileName.Data(), &id, &size, &flags, &modtime);
    c = size;
    if (size == 0) {
       fprintf(stderr," ----------------------------------------------------------\n");
       fprintf(stderr," The correct Qt/Root env has been detected.\n");
       fprintf(stderr," ----------------------------------------------------------\n");
       gSystem->Unlink(fileName.Data());
    } else {
       fprintf(stderr," ----------------------------------------------------------\n");
       fprintf(stderr,"                        ATTENTION :                        \n");
       fprintf(stderr," The new version of ROOT resource file has been created: <%s>.\n",fileName.Data());
       if (gSystem->AccessPathName(".rootrc")) {
          fprintf(stderr," To active the Qt-layer - create a symlink:\n"); 
          fprintf(stderr,"============  \"ln -s %s .rootrc \" ====================\n", fileName.Data());
       } else { 
          fprintf(stderr," To active the Qt-layer - **merge** the existen \".rootrc\" file with  \"%s\" \n", fileName.Data());
       }          
       fprintf(stderr," and re-start your application\n");
       fprintf(stderr," ----------------------------------------------------------\n");    
    }
 }
 return c;
 //  gEnv->Print();
 //  gEnv->SaveLevel(kEnvLocal);
}
