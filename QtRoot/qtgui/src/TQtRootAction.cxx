#include "TQtRootAction.h"
#include "TQtGui.h"
#include <QPixmap>

//_________________________________________________________________________________________________________
TQtRootAction::TQtRootAction(QObject * parent, const TQtBrowserMenuItem_t  &data)
      : QAction (parent), fId(data.fId)
{
   setText(data.fMenuText);
   if (data.fToolTip && data.fToolTip[0]) 
            setToolTip(data.fToolTip);   
   if (data.fAccelerator) setShortcut (data.fAccelerator);
   if (data.iconName && data.iconName[0] /*&& gClient*/ ) {
      if (data.iconName[0] == ':') {
          // Use the Qt4 resource 
          setIcon(QIcon(data.iconName));
      } else {
          const QPixmap &pixmap = TQtGui::GetPicture(data.iconName);
          setIcon(pixmap);
      }
   }
}
