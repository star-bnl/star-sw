// Author: Valeri Fine   11/01/2009
#include "StarGeomTreeWidget.h"
#include "TVolume.h"
#include "TROOT.h"
#include "TPad.h"
#include "TSystem.h"
#include "TContextMenu.h"
#include "TColor.h"
#include "TGeometry.h"
#include "TGeoManager.h"
#include "TVolumePosition.h" 
#include "TVolumeView.h"
#include "TDataSetIter.h"

#include "TQtIconBrowserImp.h"
#include "TQMimeTypes.h"

#include "QtGBrowserGeoDrawHelper.h"

#include <map>
#include <stack>
using namespace std;

#include <QAbstractScrollArea>
#include <QFile>
#include <QTextStream>
#include <QMessageBox>
#include <QColorDialog>
#include <QMenu>
#include <QDebug>
#include <QMouseEvent>
#include <QKeyEvent>
#include <QStyledItemDelegate>
#include <QApplication>

class GeomBrowseItemDeledate  : public  QStyledItemDelegate {
public:

/*!
    Constructs an item delegate with the given \a parent.
*/
    GeomBrowseItemDeledate(QObject *parent): QStyledItemDelegate(parent) {}
   ~GeomBrowseItemDeledate(){}
/*!
  \reimp
*/
bool editorEvent(QEvent *event,
                 QAbstractItemModel *model,
                 const QStyleOptionViewItem &option,
                 const QModelIndex &index)
{
    Q_ASSERT(event);
    Q_ASSERT(model);

    // make sure that the item is checkable
    Qt::ItemFlags flags = model->flags(index);
    if (!(flags & Qt::ItemIsUserCheckable) || !(option.state & QStyle::State_Enabled)
        || !(flags & Qt::ItemIsEnabled))
        return false;

    // make sure that we have a check state
    QVariant value = index.data(Qt::CheckStateRole);
    if (!value.isValid())
        return false;
    const QWidget *widget = 0;
//    const QWidget *widget = QStyledItemDelegatePrivate::widget(option);
    if (const QStyleOptionViewItemV3 *v3 = qstyleoption_cast<const QStyleOptionViewItemV3 *>(&option))
    widget = v3->widget;
    QStyle *style = widget ? widget->style() : QApplication::style();

    // make sure that we have the right event type
    if ((event->type() == QEvent::MouseButtonRelease)
        || (event->type() == QEvent::MouseButtonDblClick)) {
        QStyleOptionViewItemV4 viewOpt(option);
        initStyleOption(&viewOpt, index);
        QRect checkRect = style->subElementRect(QStyle::SE_ItemViewItemCheckIndicator, &viewOpt, widget);
        QMouseEvent *me = static_cast<QMouseEvent*>(event);
        if (me->button() != Qt::LeftButton || !checkRect.contains(me->pos()))
            return false;

        // eat the double click events inside the check rect
        if (event->type() == QEvent::MouseButtonDblClick)
            return true;

    } else if (event->type() == QEvent::KeyPress) {
        if (static_cast<QKeyEvent*>(event)->key() != Qt::Key_Space
         && static_cast<QKeyEvent*>(event)->key() != Qt::Key_Select)
            return false;
    } else {
        return false;
    }

    Qt::CheckState state = static_cast<Qt::CheckState>(value.toInt());
    switch (state) {
        case Qt::Checked:         state = Qt::Unchecked;        break;
        case Qt::Unchecked:       state = Qt::PartiallyChecked; break;
        case Qt::PartiallyChecked:state = Qt::Checked; break;
     };
    return model->setData(index, state, Qt::CheckStateRole);
}
};

//________________________________________________________________
//
//
//________________________________________________________________

//_____________________________________________________________________________
static QIcon GetGeoIcon(const char *className) {
   QString cN = QString(className).toLower();
   cN.remove(0,1);
   cN+="_t.xpm";
   TString iconsPath = "$ROOTSYS/icons";
   gSystem->ExpandPathName(iconsPath);
   QString fullPath = iconsPath.Data();
   fullPath += "/"; fullPath += cN;
   if (!gSystem->AccessPathName(fullPath.toAscii().data())) {
      return QIcon(fullPath);
   }
   return QIcon();
}

static QTreeWidgetItem *Find(QTreeWidgetItem *from, TObject *topObject)
{
   // Fine the child item by the TObject pointer
   return 0;
}
//________________________________________________________________
static pair<QString,QString> MakeVolumeDsc(const QString &s) 
{
  // First 4  symbols are the string key
  
  QString key = s.left(4);
  pair<QString, QString> volumeDiscriptor(key,s);
  return volumeDiscriptor;
}

//________________________________________________________________
static map<QString,QString> MakeVolumeMap(const QString &fileName)
{
    QFile file( fileName );
    map<QString,QString> thisMap;
    if ( file.open( IO_ReadOnly ) ) {
        QTextStream stream( &file );
        QString line;
        while ( !stream.atEnd() ) {
            line = stream.readLine(); // line of text excluding '\n'
            thisMap.insert(MakeVolumeDsc(line));
        }
        file.close();
    }
    return thisMap;
}


//_____________________________________________________________________________
static const QString  &GetVolumeDescriptor(const QString &volumeName,bool richText=false)
{
   static bool first = true;
   static map<QString,QString> volumeMap;
   static QString dsc;
   static bool errorMessage = false;// show it at once;
   if (first) {
      first = false;
      TString helpFile = "volumes.txt";
      const char *fullPath = gSystem->Which("./:./StDb/geometry:$STAR/StDb/geometry",helpFile);
      if (fullPath) {
        volumeMap = MakeVolumeMap(fullPath);
      } else if (!errorMessage) {
        errorMessage = true; // show it at once;
        QMessageBox::critical(0
              ,"STAR Geometry narrative description"
              ,QString("No file <%1> file under %2 was found")
                       .arg(helpFile.Data())
                       .arg("./:./StDb/geometry:$STAR/StDb/geometry"));
      }
      delete [] fullPath;  fullPath=0;
   }
   map<QString,QString>::iterator it;
   it = volumeMap.find(volumeName);
   if (it!=volumeMap.end())  {
      dsc = richText ? "<p><b>" : "";
      dsc += it->second;
   } else dsc = "";
   dsc.replace(" comment =",richText ? ":</b> ": ": ");
   return dsc;
}


//_____________________________________________________________________________
StarGeomTreeWidget::StarGeomTreeWidget(QWidget *parent) : QTreeWidget(parent)
, fContextMenu(0),fGeoManager2Delete(0),fCurrentDrawn(0),fNewItemCreating(false)
, fPopupContextMenu(0)
{
   Init();
}

//_____________________________________________________________________________
void StarGeomTreeWidget::Init()
{
   this->setContextMenuPolicy(Qt::CustomContextMenu); 
   QStringList labels;
      labels << "Name" << "Title" << "#" <<"Class";
   this->setHeaderLabels(labels);
   this->setColumnCount(4);
   this->setSelectionMode(QAbstractItemView::ExtendedSelection);
   this->setItemDelegate(new GeomBrowseItemDeledate(this));
   ConnectTreeSlots();
   setMouseTracking(true);
   setUniformRowHeights(true);
}

//_____________________________________________________________________________
void StarGeomTreeWidget::ConnectTreeSlots()
{
   connect(this,SIGNAL(currentItemChanged( QTreeWidgetItem *, QTreeWidgetItem *)),this,SLOT(currentItemChangedCB( QTreeWidgetItem *, QTreeWidgetItem *)));
   connect(this,SIGNAL(itemActivated(QTreeWidgetItem *, int)),                    this,SLOT(itemActivatedCB( QTreeWidgetItem *, int)   ));
   connect(this,SIGNAL(itemChanged( QTreeWidgetItem *, int)),                     this,SLOT(itemChangedCB( QTreeWidgetItem *, int)));
   connect(this,SIGNAL(itemClicked( QTreeWidgetItem *, int )),                    this,SLOT(itemClickedCB( QTreeWidgetItem *, int )));
   connect(this,SIGNAL(itemCollapsed( QTreeWidgetItem *)),                        this,SLOT(itemCollapsedCB( QTreeWidgetItem *)));
   connect(this,SIGNAL(itemDoubleClicked( QTreeWidgetItem * , int )),             this,SLOT(itemDoubleClickedCB( QTreeWidgetItem * , int )));
   connect(this,SIGNAL(itemEntered( QTreeWidgetItem *, int)),                     this,SLOT(itemEnteredCB( QTreeWidgetItem *, int)));
   connect(this,SIGNAL(itemExpanded( QTreeWidgetItem * )),                        this,SLOT(itemExpandedCB( QTreeWidgetItem * )));
   connect(this,SIGNAL(itemPressed( QTreeWidgetItem * , int  )),                  this,SLOT(itemPressedCB( QTreeWidgetItem * , int)));
   connect(this,SIGNAL(itemSelectionChanged()),                                   this,SLOT(itemSelectionChangedCB()));
   connect(this,SIGNAL(customContextMenuRequested(const QPoint & )),              this,SLOT(contextMenuRequestedCB(const QPoint & )));
}

//_____________________________________________________________________________
StarGeomTreeWidget::~StarGeomTreeWidget() 
{
   // delete the custom delegate
   delete itemDelegate();
}

//_____________________________________________________________________________
QTreeWidgetItem *StarGeomTreeWidget::CreateTreeWidgetItem(TVolume  *volume, QTreeWidgetItem *parent,const QString &nConter)
{
   // Create the tree item filled with the ROOT object
   // parent = 0 ; parent is the item is to be toplevel item
   QStringList strings;
       strings.append(volume->GetName());
       strings.append(volume->GetTitle());
       strings.append(nConter);
       strings.append(volume->ClassName());
   fNewItemCreating = true;
   QTreeWidgetItem* item = parent ?
                      new QTreeWidgetItem(parent,strings)
                      :
                      new QTreeWidgetItem(this,strings);
   item->setData(0,Qt::UserRole,qVariantFromValue((void *)volume));
   item->setChildIndicatorPolicy(
            volume->GetListSize() >0 ? QTreeWidgetItem::ShowIndicator
                                    : QTreeWidgetItem::DontShowIndicatorWhenChildless );
   SetVisibility(item, volume->GetVisibility());
   if (QString(volume->GetName()).startsWith("{")) {
      // make  it blue
      for (int i=0;i<4;i++)item->setData(i,Qt::ForegroundRole,Qt::blue);
   }
   // Set the icon if any
   TShape *sh = volume->GetShape(); 
   if (sh)  {
       const QIcon  *set = 0;
       QIcon geoIcon;
       const char *shapeClassName = sh->ClassName();
       set = TQtIconBrowserImp::Shape2GeoShapeIcon(shapeClassName);
       if (!set) {
          geoIcon = GetGeoIcon(shapeClassName);
          set = &geoIcon;
       }
       item->setIcon(0,*set);
       item->setIcon(2,*set);
   }
   fNewItemCreating = false;
   return item;
}

//_____________________________________________________________________________
void StarGeomTreeWidget::currentItemChangedCB ( QTreeWidgetItem * current, QTreeWidgetItem * previous ) {}
//_____________________________________________________________________________
void StarGeomTreeWidget::itemActivatedCB ( QTreeWidgetItem * item, int column ) 
{} 
//_____________________________________________________________________________
void StarGeomTreeWidget::itemChangedCB ( QTreeWidgetItem * item, int) 
{
   if (item) {
#if 0
      //  Rotate the visibility flag
      //qDebug() << " StarGeomTreeWidget::itemChangedCB  state:" << item->text(0) << item->checkState(0) << fNewItemCreating;
      if (!fNewItemCreating) {
         TObject *obj = CurrentObject(item);
         TVolume *volume = dynamic_cast<TVolume *>(obj);
         TVolume::ENodeSEEN vis = volume->GetVisibility();
         if (vis == TVolume::kNoneVisible) 
            vis = TVolume::kThisUnvisible;
         else if (vis == TVolume::kThisUnvisible)
            vis = TVolume::kBothVisible;
         else 
            vis = TVolume::kNoneVisible;
         SetVisibility(item, vis);
      }
#endif
   }
}

//_____________________________________________________________________________
void StarGeomTreeWidget::itemClickedCB ( QTreeWidgetItem * item, int column )
{
   if (item) {
      // qDebug() << " StarGeomTreeWidget::itemClickedCB  state:" << item->text(0) << item->checkState(0);
      TObject *obj = CurrentObject(item);
      TVolume *volume = dynamic_cast<TVolume *>(obj);

      // check visibility
      if (volume) {
         TVolume::ENodeSEEN s = TVolume::kThisUnvisible;
         switch (item->checkState (0)) {
             case Qt::Unchecked:        s = TVolume::kNoneVisible;   break;
             case Qt::PartiallyChecked: s = TVolume::kThisUnvisible; break;
             case Qt::Checked:          s = TVolume::kBothVisible;   break;
             default:                   s = TVolume::kThisUnvisible; break;
         };
         // set visibility
         if (volume->GetVisibility() != s) {
            volume->SetVisibility(s);

            QString m = "The ";
            if ( s & TVolume::kThisUnvisible) m += "in";
            m += "visible volume: ";
            const char *info = obj->GetObjectInfo(gPad->XtoPixel(0),gPad->YtoPixel(0));
            if (info) m += info;
            emit ObjectInfo(m);

            // adjust 1 level view
            if (item == this->currentItem() ) drawItem(item, false);

            // adjust multi-level  view
            if (fCurrentDrawn == item) drawItem(item, true);
            else {
               // check whether the item parent was drawn
               QTreeWidgetItem *lookup = item;
               int depth = 3;
               while( lookup && depth && (lookup != fCurrentDrawn))  {
                  lookup = lookup->parent(); 
                  depth--;
               }
               if (lookup && depth) drawItem(fCurrentDrawn, true);
            }
         }
#if 0
         QString dsc = GetVolumeDescriptor(volume->GetName(),true);
         if (dsc.isEmpty()) {
            dsc = QString("<p>No desciption was found for <b>%1</b><br>Edit the <code>volumes.txt</code> file")
                          .arg(volume->GetName());
         }
         // ???QWhatsThis::display(dsc, QCursor::pos()+QPoint(100,0)); 
#endif
      }
   }
}

//_____________________________________________________________________________
void StarGeomTreeWidget::itemCollapsedCB ( QTreeWidgetItem * item ) 
{
 if (item) {
      // check fCurrentDrawn
      TQtLockUpdateWidget listLock(this);

      QList<QTreeWidgetItem *> children = item->takeChildren();
      for (int i = 0; i< children.size(); i++) {
         if (fCurrentDrawn == children.at(i)) {fCurrentDrawn = 0; break;}
      }
      // clean the collapsed branch
      while(!children.isEmpty()) delete(children.takeLast());
   }
}

//_____________________________________________________________________________
void StarGeomTreeWidget::itemDoubleClickedCB ( QTreeWidgetItem * item, int column )
{
   if (item && fCurrentDrawn != item) {
      // remove the old mark
      { 
         TQtLockUpdateWidget listLock(this);
        
         TObject  *obj = 0;
         const QIcon  *set = 0;
         QIcon geoIcon;
         const char *shapeClassName = 0;
         if ( fCurrentDrawn ) {
            obj = CurrentObject();
            if (obj) {
               if (obj->InheritsFrom(TVolume::Class()) ) {
                  TShape *sh = ((TVolume *)obj)->GetShape(); 
                  if (sh)  {
                     shapeClassName = sh->ClassName();
                     set = TQtIconBrowserImp::Shape2GeoShapeIcon(shapeClassName);
                  }
               } else if (obj->InheritsFrom(TVolumeView::Class()) ) {
                  shapeClassName = ((TVolumeView *)obj)->GetShape()->ClassName();
                  set =  TQtIconBrowserImp::Shape2GeoShapeIcon(shapeClassName);
               } else if (obj->InheritsFrom(TShape::Class()) ) {
                  set =  TQtIconBrowserImp::Shape2GeoShapeIcon(((TShape *)obj)->ClassName());
               } else if (obj->InheritsFrom(TGeoVolume::Class()) ) 
                  set = TQtIconBrowserImp::IconList()->GetIcon(((TGeoVolume *)(obj))->GetShape()->GetName());
            }
            if (!set) {
              geoIcon = GetGeoIcon(shapeClassName);
              set = &geoIcon;
            }
            fCurrentDrawn->setIcon(0,set ? *set: QIcon());
            fCurrentDrawn->setIcon(1,QIcon());
            for (int i=0;i<4;i++) fCurrentDrawn->setData(i,Qt::ForegroundRole,Qt::black);
         }

         fCurrentDrawn = item;
         obj = CurrentObject();
         if (obj) {
            if (obj->InheritsFrom(TVolume::Class()) ) {
               TShape *sh = ((TVolume *)obj)->GetShape(); 
                  if (sh)  {
                     shapeClassName = sh->ClassName();
                     set = TQtIconBrowserImp::Shape2GeoShapeIcon(shapeClassName);
                  }
            } else if (obj->InheritsFrom(TVolumeView::Class()) )
               set =  TQtIconBrowserImp::Shape2GeoShapeIcon(((TVolumeView *)obj)->GetShape()->ClassName());
            else if (obj->InheritsFrom(TShape::Class()) )
               set =  TQtIconBrowserImp::Shape2GeoShapeIcon(((TShape *)obj)->ClassName());
            else if (obj->InheritsFrom(TGeoVolume::Class()) )
               set = TQtIconBrowserImp::IconList()->GetIcon(((TGeoVolume *)(obj))->GetShape()->GetName());
         }
         // highlight the new item
         if (!set) {
            geoIcon = GetGeoIcon(shapeClassName);
            set = &geoIcon;
         }
         fCurrentDrawn->setIcon(0,QIcon(":/wirebox.xpm")); 
//       fCurrentDrawn->setPixmap(0,QPixmap::fromMimeSource("arrow_right.xpm")); 
         fCurrentDrawn->setIcon(1,set ? *set : QIcon(":/arrow_left.xpm")); 
         for (int i=0;i<4;i++) fCurrentDrawn->setData(i,Qt::ForegroundRole,Qt::red);
      }
      drawItem(fCurrentDrawn, true); 
   }
}

//_____________________________________________________________________________
void StarGeomTreeWidget::itemEnteredCB ( QTreeWidgetItem * item, int column )
{
  if (item) {  
      TObject *obj = CurrentObject(item);
      if (obj && gPad) {
         QString m = "The ";
         TVolume::ENodeSEEN s = ((TVolume *)obj)->GetVisibility();
         if ( s & TVolume::kThisUnvisible) m += "in";
         m += "visible volume: ";
         QString dsc= GetVolumeDescriptor(obj->GetName());
         if (dsc.isEmpty() ) {
            const char *info = obj->GetObjectInfo(gPad->XtoPixel(0),gPad->YtoPixel(0));
            if (info) m += info;
         } else {
           m += dsc;
         }
         emit ObjectInfo(m);
      }
   }
}

//_____________________________________________________________________________
inline static Int_t CountInstances(TVolume *parent,  TVolume *child) 
{
   // Find the number of the positions of the child volume 
   Int_t counter = 0;
   TList *positions = parent->GetListOfPositions();
   TIter next(positions);
   while (TVolumePosition *pos = (TVolumePosition *)next() )
      if (pos->GetNode() == child ) counter++;
   return counter;   
}


//_____________________________________________________________________________
void StarGeomTreeWidget::itemExpandedCB ( QTreeWidgetItem * item ) 
{
   if (item) {
      // qDebug() << "  StarGeomTreeWidget::itemExpandedCB  state:" << item->text(0) << item->checkState(0);
      TObject *obj = CurrentObject(item);
      TVolume *volume = dynamic_cast<TVolume *>(obj);

      TQtLockUpdateWidget listLock(this);
      this->setSortingEnabled(false);

      TDataSetIter next(volume);
      TVolume *child = 0;
      while ( (child = (TVolume *)next()) ) {
         Int_t nVolume = CountInstances(volume,child);
         CreateTreeWidgetItem(child,item, (nVolume >1) ? QString("> #%1").arg(nVolume) : QString());  
      }
      this->setSortingEnabled(true);
      int currentWidth = columnWidth(0); 
      this->resizeColumnToContents(0);
      if (currentWidth > this->columnWidth(0) ) {
         // restore the wider dimension. Do no shrink to column
         this->setColumnWidth(0,currentWidth);
      }
   }
}
//_____________________________________________________________________________
void StarGeomTreeWidget::itemPressedCB ( QTreeWidgetItem * item, int column ) {}
//_____________________________________________________________________________
void StarGeomTreeWidget::itemSelectionChangedCB () 
{
  QTreeWidgetItem *i = this->currentItem();
  if (i && i->isSelected() ) TreeView_selectionChanged(i);
}

//_____________________________________________________________________________
void StarGeomTreeWidget::TreeView_selectionChanged(QTreeWidgetItem *item)
{
   int selectedItems  = this->selectedItems().size();
   if (item && (selectedItems == 1)) drawItem(item,false);
}

//_____________________________________________________________________________
void StarGeomTreeWidget::drawItem( QTreeWidgetItem *item, bool expanded)
{
  if (item) {
      TObject *rootObject =  CurrentObject(item);
      emit DrawObject(rootObject,expanded);
  }
}

//_____________________________________________________________________________
void StarGeomTreeWidget::contextMenuRequestedCB(const QPoint &pos)
{
   QList<QTreeWidgetItem *>lst = this->selectedItems();
   // Count the number of the selected items
   int nSelected = lst.size();
   if (nSelected == 1) {
      if (!fContextMenu) fContextMenu = new TContextMenu("BrowserContextMenu");
      QTreeWidgetItem *that = lst.first();
      QVariant model = that->data(0, Qt::UserRole);
      TObject *obj = (TObject *)model.value<void *>();
      if (obj) 
         fContextMenu->Popup(QCursor::pos().x(),QCursor::pos().y(), obj,(TBrowser *)0);
   } else {
      QAction *response = 0; //QMessageBox::question(listView1,"Change the volume visibility","Visible","Both","Child","none");
      static vector<QAction *> menus;
      if (!fPopupContextMenu) {
         menus.clear();
         fPopupContextMenu = new QMenu(this);
         fPopupContextMenu->setTitle("Visibility:");
         QAction *action =  fPopupContextMenu->menuAction();
         QFont af = action->font();
         af.setBold(true);
         action->setFont(af);
         QAction  *itemPosition = 0;
         // menu title :
         itemPosition = fPopupContextMenu->addAction("&Both");
         int j =0;
         itemPosition->setWhatsThis("Make the selected volumes and its children visible");
         menus.push_back(itemPosition);

         itemPosition=fPopupContextMenu->addAction("&Children");
         itemPosition->setWhatsThis("Make the selected the children of the selected volumes visible but the volume itself none");
         menus.push_back(itemPosition);

         itemPosition=fPopupContextMenu->addAction("&None");
         itemPosition->setWhatsThis("Make the selected the volumes invisible");
         menus.push_back(itemPosition);

         fPopupContextMenu->addSeparator();

         itemPosition=fPopupContextMenu->addAction("&Save");
         itemPosition->setWhatsThis("Save the selected object into ROOT file");
         menus.push_back(itemPosition);

         itemPosition=fPopupContextMenu->addAction("&Color");
         itemPosition->setWhatsThis("Change the color of the selected object");
         menus.push_back(itemPosition);
      }
      response = fPopupContextMenu->exec(QCursor::pos());
      if (response) {
         TQtLockUpdateWidget listLock(this);
         // bool saved = false;
         Color_t rootColor = -1;
         Style_t rootStyle = -1;
         // TVolume *topVolumeToSave = 0;
         // TFile  *file2Save = 0;
         for (int i = 0; i < nSelected; ++i) {
            QTreeWidgetItem* itemRoot =lst.at(i);
            QVariant model = itemRoot->data(0, Qt::UserRole);
            TObject *obj = (TObject *)model.value<void *>();
            TVolume *volume = dynamic_cast<TVolume *>(obj);

            // check visibility
            if (volume) {
               TVolume::ENodeSEEN s = volume->GetVisibility();
               if  ( response == menus[0] ) {
                  itemRoot->setCheckState(0,Qt::Checked);       s = TVolume::kBothVisible; 
               } else if (response == menus[1]) { 
                  itemRoot->setCheckState(0,Qt::PartiallyChecked); s = TVolume::kThisUnvisible;
               } else if (response == menus[2]) { 
                  itemRoot->setCheckState(0,Qt::Unchecked)     ; s = TVolume::kNoneVisible;
               } else if (response == menus[3]) { 
#if 0
                  if (topVolumeToSave) {
                     topVolumeToSave->Add(volume);
                  } else if (!saved) {
                     saved  = true;
                     // Save the object
                     QString filter = "ROOT file (*.root);";
                     QString selectedFilter;
                     QString dir = fSaveFileName;
                     if (dir.isEmpty()) dir = gSystem->WorkingDirectory(); 
                     else               dir = QFileInfo(dir).dirPath();

                     QString thatFile = QFileDialog::getSaveFileName(dir
                        , filter, this, "SaveAs"
                        , "Save the volulme  as"
                        , &selectedFilter);

                     if (thatFile.isEmpty()) {
                        response = 0;
                     } else {
                        TDirectory *save = gDirectory;
                        file2Save = TFile::Open((const char *)thatFile,"RECREATE");
                        topVolumeToSave = new TVolume("GeomBrowse","saved",(TShape *)0);
                        save->cd();
                     }
                  }
#endif
               } else if (response == menus[4]) { 
                  // Change the object color
                  Color_t vc = volume->GetLineColor();
                  Style_t vs = volume->GetFillStyle();
                  if (rootColor == -1)  {
                     float r,g,b;
                     int a = 0;
                     gROOT->GetColor(vc)->GetRGB(r,g,b);
                     if (4000 >= vs && vs < 5000) a = vs-4000;
                     QRgb initial = qRgba(int(r*255),int(g*255),int(b*255),a);
                     bool ok;
                     QRgb color = QColorDialog::getRgba(initial, &ok, this);
                     if (ok) {
                        int red   = qRed(color);
                        int green = qGreen(color);
                        int blue  = qBlue(color);
                        int alpha = qAlpha(color);
                        rootColor = TColor::GetColor(red, green, blue);
                        if ( (alpha > 0) && (alpha != vs-4000) ) rootStyle = 4000+alpha;
                     } else {
                        response = 0;
                     }
                  }
                  if (rootColor != -1 && vc != rootColor) volume->SetLineColor(rootColor);
                  if (rootStyle != -1 && vs != rootStyle) volume->SetFillStyle(rootStyle);
               } else { response = 0; }
               // set visibility
               if (volume->GetVisibility() != s) volume->SetVisibility(s);
            }
         }
#if 0
         if (file2Save) {
            TDirectory *save = gDirectory;
            file2Save->cd();
            topVolumeToSave->Write();
            file2Save->Close();
            delete file2Save; file2Save=0;
            delete topVolumeToSave; topVolumeToSave = 0;
            save->cd();
         }
#endif
      }
      //         if (response) RefreshCanvas(tQtWidget1);
   }
}

//_____________________________________________________________________________
void StarGeomTreeWidget::SetVisibility( QTreeWidgetItem * item, TVolume::ENodeSEEN vis )
{
   // Convert the the visbility status to the checkmark  
   if (item) {
      blockSignals(true);
      switch (vis) {
          case TVolume::kBothVisible:   item->setCheckState(0,Qt::Checked);         break;
          case TVolume::kSonUnvisible:  item->setCheckState(0,Qt::Checked);         break;
          case TVolume::kThisUnvisible: item->setCheckState(0,Qt::PartiallyChecked);break;
          case TVolume::kNoneVisible:   item->setCheckState(0,Qt::Unchecked);       break;
      };
      blockSignals(false);
   }
}
//_____________________________________________________________________________
QTreeWidgetItem* StarGeomTreeWidget::AddModel2ListView( TObject *obj, const QString &title)
{
   TVolume *volume = 0;
   TQtLockUpdateWidget listLock(this);
   this->setSortingEnabled(false);
   QTreeWidgetItem* item = 0;
   if (       obj->IsA() == TGeometry::Class()) {
      volume = new TVolume(*((TGeometry*)obj)->GetCurrentNode());
   } else if (obj->IsA() == TGeoManager::Class()) {
      volume = TGeoDrawHelper::MakeVolume(((TGeoManager*)obj)->GetTopVolume());
      fGeoManager2Delete = (TGeoManager*)obj;
   } else if (obj->IsA() == TGeoVolume::Class()) {
      volume = TGeoDrawHelper::MakeVolume((TGeoVolume*)obj);
   } else if (obj->IsA() == TVolume::Class()) {
      volume = (TVolume *)obj;
   }
   if(volume) {
      item = CreateTreeWidgetItem(volume);
      this->addTopLevelItem(item);
   }
   if (!title.isEmpty() && item)  item->setText(0,title);
   this->setSortingEnabled(true);
   return item;
}

//_____________________________________________________________________________
void StarGeomTreeWidget::SelectByTObject( TObject *obj, const QPoint &)
{
   // [slot] to accept the selected object to expand the ListTreeView
   if ( fCurrentDrawn && obj->InheritsFrom(TDataSet::Class()) ) {
      // suspend the list view update
      
      TQtLockUpdateWidget listLock(this);

      QVariant model = fCurrentDrawn->data(0, Qt::UserRole);
      TObject *topObject = (TObject *)model.value<void *>();

      stack<TObject *> items;
      items.push(obj);
      TDataSet *set = (TDataSet *)obj;
      while ( (set = set->GetParent()) ) {
         items.push(set);
         // fprintf(stderr," 1. GeomBrowser::ObjectSelected %s %p %p \n", set->GetName(),set,topObject );
         if (((TObject *)set) == topObject) break;
      }
      if (set) {
         // Found the whole path, now roll it back

         QTreeWidgetItem *selectedItem = fCurrentDrawn;
         while ( !items.empty() )  {
            topObject = items.top(); items.pop();
            QTreeWidgetItem *nextSelectedItem =  Find(selectedItem,topObject);
             // fprintf(stderr," 2. GeomBrowser::ObjectSelected %s %p %p\n", topObject->GetName(),topObject, nextSelectedItem);
            if (nextSelectedItem) {
#ifdef TOBE_DONE_YET
                if ( !nextSelectedItem->isOpen() ) ((TQtObjectListItem *)nextSelectedItem)->setOpen(true);
#endif
               selectedItem = nextSelectedItem;
            }
         }
         this->setCurrentItem(selectedItem,0,QItemSelectionModel::SelectCurrent);
         this->scrollToItem(selectedItem);
     } 
   }
}

//_____________________________________________________________________________
void StarGeomTreeWidget::ClearCB()
{
   // clear the view
   int nItem = this->topLevelItemCount();
   for (;nItem;nItem--) {
      QTreeWidgetItem * item = this->topLevelItem (nItem-1);
      delete  CurrentObject(item);
   }
   this->clear();
}

//_____________________________________________________________________________
TObject * StarGeomTreeWidget::CurrentObject(QTreeWidgetItem *item)
{
   // return the TObject accosiated wioth the current tree item
   if (!item) item = fCurrentDrawn;
   QVariant model = item->data(0, Qt::UserRole);
   TObject *obj = (TObject *)model.value<void *>();
   return obj;
}
