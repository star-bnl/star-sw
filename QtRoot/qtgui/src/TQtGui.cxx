#include "TQtGui.h"
#include "TGClient.h"
#include "TGPicture.h"
#include "TSystem.h"
#include "TEnv.h"
#include <qpixmapcache.h> 
#include <qfileinfo.h> 

#include <QPixmap>
#include <QString>

namespace {
static QPixmap *gDummyIcon = 0;
//________________________________________________________________________
inline QString RootIconPath() {
   // See TGResourcePool::TGResourcePool(TGClient *client)
  QString icon_path;
#ifndef R__VMS
# ifdef ROOTICONPATH
   icon_path = QString("%1/icons:%2:.:")
               .arg(gSystem->HomeDirectory())
               .arg(ROOTICONPATH);
   icon_path += 
#  ifdef EXTRAICONPATH
   gEnv->GetValue("Gui.IconPath", EXTRAICONPATH);
#  else
   gEnv->GetValue("Gui.IconPath", "");
#  endif
# else
   icon_path = QString("%1/icons:%2/icons:.:")
         .arg(gSystem->HomeDirectory())
         .arg(gSystem->Getenv("ROOTSYS"));
   const char *guiicon = gEnv->GetValue("Gui.IconPath", "");
   if (guiicon && guiicon[0])icon_path += guiicon;
# endif
#else
   QString line = QString("[%1.ICONS]").arg(gSystem->Getenv("ROOTSYS");
   icon_path =  gEnv->GetValue("Gui.IconPath",line.toLatin1().data());
#endif
   return icon_path;
}

}
//________________________________________________________________________
bool TQtGui::AddPicture(const QPixmap &pic, const char *pictureName, bool checkCache)
{
   // Add the picture to the picture cache
   // Ignore the unamed and empty pictures
   // Return: true  - the picture was sussefully added
   //         false - wrong input parameters or the picture 
   //                 name has been taken
   bool  res = false;
   if (pictureName && pictureName[0] && !pic.isNull()) 
   {
     QString pname = QString(pictureName).stripWhiteSpace();
     bool found = false;
     if (!checkCache || ( checkCache && !(found = QPixmapCache::find(pname))))
     {
        QPixmap p(pic);
        QPixmapCache::insert(pictureName, p);
        res = true;
     }
   }
   return res;
}
//________________________________________________________________________
const QPixmap &TQtGui::GetPicture(QString &pictureName) 
{
   std::string pName = pictureName.toStdString();
   return GetPicture(pName.c_str());
}

//________________________________________________________________________
const QPixmap &TQtGui::GetPicture(const char *pictureName) 
{
   //
   // A'la const TGPicture *TGPicturePool::GetPicture(const char *name)
   // Warning: If valid, you should copy the pixmap immediately (this is fast). 
   // Subsequent insertions into the cache could cause the pointer 
   // to become invalid.
   //
   QPixmap* pp=0;
   QPixmap p;
   QString pname = QString(pictureName).stripWhiteSpace();
   if ( !(pp=QPixmapCache::find(pname)) ) {

      QString ext   = QFileInfo(pname).suffix().lower();

      if (!ext.isEmpty()) { // ".xpm", ".gif" etc

         std::string mstrname = pname.toStdString();
         char *pxname = gSystem->ExpandPathName(gSystem->UnixPathName(mstrname.c_str()));
         pname = pxname;
         delete [] pxname;
      }

      // Get ROOT Icon path:
      QString iconPath = ".";
      if (gClient) {
         // see: TGResourcePool::TGResourcePool
         TGPicturePool *pool  = gClient->GetPicturePool();
         iconPath = pool->GetPath();
         if (iconPath.isEmpty()) iconPath = ".";
      }  else {
         iconPath = RootIconPath();
      }
      char *picnam = 0;
      picnam = gSystem->Which(iconPath.toLatin1().data(), (const char *)pname, kReadPermission);
      if (picnam) {
         p.load(picnam);
         AddPicture(p,pname,kFALSE);
         pp = QPixmapCache::find(pname);
      }
   }
   if (!gDummyIcon && !pp) gDummyIcon = new QPixmap();
   return pp ? *pp : *gDummyIcon;
}
