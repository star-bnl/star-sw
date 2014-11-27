#ifndef STAR_STARGEOMTREE
#define STAR_STARGEOMTREE
// Author: Valeri Fine   11/01/2009
#include <QTreeWidget>
#include <QString>
#include "TVolume.h"

class TObject;
class TContextMenu;
class TGeoManager;
class QMenu;

//_____________________________________________________________________________
class TQtLockUpdateWidget {

private:

   QAbstractScrollArea *fView;
   bool         fHasBeenLocked;

public: 
   TQtLockUpdateWidget(QAbstractScrollArea *view) : fView (view),fHasBeenLocked(FALSE) {
      if (fView) {
         fHasBeenLocked = !fView->updatesEnabled();
         if (!fHasBeenLocked) {
            // We should lock the view
            fView->setCursor(Qt::WaitCursor);
            fView->setUpdatesEnabled( FALSE );
            fView->viewport()->setCursor(Qt::WaitCursor);
            fView->viewport()->setUpdatesEnabled( FALSE );
        }
      }
   }
   ~TQtLockUpdateWidget() {
       if (fView) {
         if (!fHasBeenLocked) {
           // we should unlock the view
            fView->viewport()->setUpdatesEnabled( TRUE );
            fView->setUpdatesEnabled( TRUE );
            fView->unsetCursor(); 
            fView->viewport()->unsetCursor();
            // fView->repaintContents();
         }
      }
   }
};

class StarGeomTreeWidget : public QTreeWidget
{
      Q_OBJECT
private:
   TContextMenu    *fContextMenu;
   TGeoManager     *fGeoManager2Delete;
   QTreeWidgetItem *fCurrentDrawn;
   bool             fNewItemCreating;
   QMenu           *fPopupContextMenu;

   protected:
      friend class GeomBrowser;
      void ConnectTreeSlots();
      void Init();
      void TreeView_selectionChanged(QTreeWidgetItem *i);
      void drawItem( QTreeWidgetItem *item, bool expanded=false);
      void SetVisibility( QTreeWidgetItem * item, TVolume::ENodeSEEN vis );
	   QTreeWidgetItem* AddModel2ListView( TObject *obj, const QString &title);
      QTreeWidgetItem *CreateTreeWidgetItem(TVolume  *obj, QTreeWidgetItem *parent=0, const QString &cnt=QString() );

   public:
      StarGeomTreeWidget(QWidget *parent = 0);
      virtual ~StarGeomTreeWidget();
      TObject *CurrentObject(QTreeWidgetItem *item=0);

   public slots:
   
      void ClearCB();
      void currentItemChangedCB ( QTreeWidgetItem * current, QTreeWidgetItem * previous );
      void itemActivatedCB ( QTreeWidgetItem * item, int column );
      void itemChangedCB ( QTreeWidgetItem * item, int column );
      void itemClickedCB ( QTreeWidgetItem * item, int column );
      void itemCollapsedCB ( QTreeWidgetItem * item );
      void itemDoubleClickedCB ( QTreeWidgetItem * item, int column );
      void itemEnteredCB ( QTreeWidgetItem * item, int column );
      void itemExpandedCB ( QTreeWidgetItem * item );
      void itemPressedCB ( QTreeWidgetItem * item, int column );
      void itemSelectionChangedCB ();
      void contextMenuRequestedCB(const QPoint &pos);
      void SelectByTObject( TObject *obj, const QPoint &);

signals:
      void DrawObject(TObject *o,bool expanded);
      void ObjectInfo(QString objInfo);
};
#endif
