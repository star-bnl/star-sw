#ifndef STAR_GEOMBROWSER
#define STAR_GEOMBROWSER

#include <QMainWindow>
#include <QString>

class TFile;
class TObject;
class St_geant_Maker;
class StChain;
class QComboBox;
class TVirtualViewer3D;
class StarGeomTreeWidget;
class TQtWidget;
class StarGeomTreeWidget;
class TQtRootCommandCombo;
class TQtRangeControl;
class TextEdit;
class QAction;
class QStatusBar;

class GeomBrowser : public QMainWindow {
  Q_OBJECT
  private:
   StarGeomTreeWidget  *fTreeWidget;
	TFile               *fFile;
	StChain             *fChain;
   St_geant_Maker      *fGeant;
	QComboBox           *fGeometrySelector;
	TVirtualViewer3D    *fCurrentViewer;
	QString              fOpenFileName;
	bool                 fGeometryLoaded;
   TQtWidget           *fSingleVolumeCanvas;
   TQtWidget           *fComplexVolumeCanvas;
   TQtRootCommandCombo *fRootCommand;
	TextEdit            *fTextEdit;
	 
	TQtWidget           *fCurrentWidget;
	 
	QString              fSaveFileName;

   QAction   *fFile_New,  *fFile_Open,  *fFile_Reload, *fFile_Save, *fFile_SaveAs
             , *fFile_Print, *fFile_Exit;
   QAction   *fView_Coin3DAction, *fView_GLAction;
   QAction   *fEditGeoSrc;
   QStatusBar *fStatusBar;
   TQtRangeControl *fDepthControl;

   protected:
     static int Geant3Init;

  protected:
	 void CleanGeoManager();
	 St_geant_Maker &Geant();
	 void Init();
	 void CreateActions();
	 void CreateMenu();
	 void CreateToolBar();
	 void CreateStatusBar();
	 void CreateGuiLayout();
    void Connect();


  public:
   GeomBrowser(QWidget *parent=0);
   virtual ~GeomBrowser();

 public slots:
	void fileOpenMacro( const QString &fileName );
   void fileOpenRoot ( const QString &fileName );
   void fileOpenZebra( const QString &fileName );
   void fileOpenInventor( const QString &fileName );
   void SelectGeometry(const QString &geomTag);
   void STAR_geometry_activated( const QString &geoVersion );
   void DrawObject(TObject *rootObject=0,bool expanded=true);
   void ObjectSelected( TObject *obj, const QPoint &);
   void ViewerDestroyed();
   
  public slots:
	void RemakeGeomSlot(const QString &);
	void fileOpenSlot();
   void fileReloadSlot();
	void fileSaveSlot();
	void fileSaveAsSlot();
	void filePrintSlot();
	void fileExitSlot();
   
   void viewGLSlot();
   TVirtualViewer3D *viewCoin3DSlot();
	
};

#endif
