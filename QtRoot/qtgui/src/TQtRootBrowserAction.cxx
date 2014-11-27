// Author: Valeri Fine  08/03/2003
/****************************************************************************
** $Id: TQtRootBrowserAction.cxx,v 1.5 2013/08/30 16:00:25 perev Exp $
**
** Copyright (C) 2002 by Valeri Fine.  All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
*****************************************************************************/

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// TQtRootBrowserAction                                                 //
//                                                                      //
// define the ROOT browser actions.                                     //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "TQtRootBrowserAction.h"
#include "TQMimeTypes.h"
#include "TString.h"
#include "TSystem.h"
#include "TEnv.h"
#include "TSystemFile.h"
#include "TInterpreter.h"
#include "TApplication.h"
#include "TPad.h"

#include <QApplication>

TQtRootBrowserAction *TQtRootBrowserAction::gfRootBrowserAction = 0;
//______________________________________________________________________________
void TQtRootBrowserAction::IconList() 
{
   // Load GUI defaults from .rootrc
  char icon_path[1024];
  char line[1024];
  char mime_file[1024];
#ifndef R__VMS
# ifdef ROOTICONPATH
   sprintf(icon_path, "%s/icons:%s:.:",
           gSystem->Getenv("HOME"),
           ROOTICONPATH);
#  ifdef EXTRAICONPATH
   strcat(icon_path, gEnv->GetValue("Gui.IconPath", EXTRAICONPATH));
#  else
   strcat(icon_path, gEnv->GetValue("Gui.IconPath", ""));
#  endif
# else
   sprintf(icon_path, "%s/icons:%s/icons:.:", gSystem->Getenv("HOME"),
                                              gSystem->Getenv("ROOTSYS"));
   strcat(icon_path, gEnv->GetValue("Gui.IconPath", ""));
# endif
   sprintf(line, "%s/.root.mimes", gSystem->Getenv("HOME"));
#else
   sprintf(line,"[%s.ICONS]",gSystem->Getenv("ROOTSYS"));
   strcpy(icon_path, gEnv->GetValue("Gui.IconPath",line));
   sprintf(line,"%sroot.mimes",gSystem->Getenv("HOME"));
#endif

   strcpy(mime_file, gEnv->GetValue("Gui.MimeTypeFile", line));
   char *mf = gSystem->ExpandPathName(mime_file);
   if (mf) {
      strcpy(mime_file, mf);
      delete [] mf;
   }
   if (gSystem->AccessPathName(mime_file, kReadPermission))
#ifdef R__VMS
      sprintf(mime_file,"[%s.ETC]root.mimes",gSystem->Getenv("ROOTSYS"));
#else
# ifdef ROOTETCDIR
      sprintf(mime_file, "%s/root.mimes", ROOTETCDIR);
# else
      sprintf(mime_file, "%s/etc/root.mimes", gSystem->Getenv("ROOTSYS"));
# endif
#endif
   fMimeTypeList = new TQMimeTypes(icon_path, mime_file);
}

//______________________________________________________________________________
TQtRootBrowserAction::~TQtRootBrowserAction(){}
//______________________________________________________________________________
TQtRootBrowserAction *TQtRootBrowserAction::Instance()
{
   if (!gfRootBrowserAction) gfRootBrowserAction = new TQtRootBrowserAction();
   return gfRootBrowserAction;
}
//______________________________________________________________________________
// public slots:
//______________________________________________________________________________
void  TQtRootBrowserAction::ExecuteDefaultAction(TObject *obj) {
   // Execute default action for selected object (action is specified
   // in the $HOME/.root.mimes or $ROOTSYS/etc/root.mimes file.
   // Emits signal "ExecuteDefaultAction(TObject*)".

   char action[512];

   // Special case for file system objects...
   if (obj->IsA() == TSystemFile::Class()) {
      if (fMimeTypeList->GetAction(obj->GetName(), action)) {
         TString act = action;
         act.ReplaceAll("%s", obj->GetName());
         if (act[0] == '!') {
            act.Remove(0,1);
#ifdef WIN32
            Ssiz_t  delim = act.First(' ');
            if (delim != kNPOS) {
               // if the executable can not be found try to use the file name alone
               // that should work for Windows
               TString execName = act(0,delim);
               char *found = gSystem->Which("$PATH",execName);
               if (!found) { // try "exe" suffix
                  delete [] found;
                  execName += ".exe";
                  found = gSystem->Which("$PATH",execName);
               }
               if (!found) act.Remove(0,delim);
               delete [] found;
            }
            act.Prepend("start ");
#endif 
            gSystem->Exec(act.Data());
         } else {
            gApplication->ProcessLine(act.Data());
            if (gPad) gPad->Update();
         }
      }
      // Emit("ExecuteDefaultAction(TObject*)", obj);
      return;
   }

   // For other objects the default action is still hard coded in
   // their Browse() member function.
}

