#ifndef STAR_QtObjectViewFrame
#define STAR_QtObjectViewFrame

#include <QTreeView>
#include <QFileIconProvider>

class QStandardItem;
class QString;
class QModelIndex;
class QStandardItemModel;
class TDirectory;
class TObject;

class TQtObjectViewFrame : public QTreeView {
   Q_OBJECT
private:
   QStandardItemModel *fModel;
   QStandardItem *fROOTFileNameItem;
   QStandardItem *fDaqBufferItem;
   TDirectory  *fDirectory;
   QString   fLastWorkingDir;// the last directory to open with the next file dialog
   static  QFileIconProvider *fgIcons;
   QString   fLastExpandedFile;  // the name of the file item that was expanded
   QString   fFullFileName;

protected:
   void ResetView();
   void IconList();

public:
   TQtObjectViewFrame(QWidget * parent=0);
   virtual ~TQtObjectViewFrame();
   static QStandardItem *CreateItem(TDirectory *dir, QStandardItem *parentItem);
   static const QIcon &Icon(QFileIconProvider::IconType it);
   static QStandardItem *ClearEditFlag(QStandardItem *item);

public slots:
   void ActivateObject(const QModelIndex &index);
   void OpenRootFile();
   void Clicked(const QModelIndex &index);
   void Expand(const QModelIndex &index);
   void SetRootFile(const QString &fileName);
   void SetDirectoryBuffer(TDirectory *buffer, const QString &header);
   void SetLastWorkingDir(const QString &dir="./");
signals: 
     void Activated(TObject *);
     void newRootFile(const QString &);
};
#endif
