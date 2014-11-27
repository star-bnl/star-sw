#include "TQtObjectListItem.h"

//______________________________________________________________________________
#if QT_VERSION < 0x40000
TQtObjectListItemInterface *TQtObjectListItemInterface::Cast(QListViewItem *thisItem)
#else /* QT_VERSION */
TQtObjectListItemInterface *TQtObjectListItemInterface::Cast(Q3ListViewItem *thisItem)
#endif /* QT_VERSION */
{
   TQtObjectListItemInterface *item = 0;
#if QT_VERSION < 0x40000
   QCheckListItem *checkItem = dynamic_cast<QCheckListItem*>(thisItem);
#else /* QT_VERSION */
   Q3CheckListItem *checkItem = dynamic_cast<Q3CheckListItem*>(thisItem);
#endif /* QT_VERSION */
   if ( checkItem ) {
      item = (TQtObjectListItem *)checkItem ;
   } else {
      item = (TQtBrowserItem *)thisItem;
   }
   return item;
}
