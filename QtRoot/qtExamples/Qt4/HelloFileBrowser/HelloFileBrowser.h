#ifndef HelloFileBrowser_H
#define HelloFileBrowser_H

#include <ui_HelloFileBrowser.h>

class TObject;
class TFile;
class TCanvas;
class QStandardItem;
class QPoint;

class HelloFileBrowser : public QWidget, public Ui::HelloFileBrowser
{
    Q_OBJECT

public:
    HelloFileBrowser(QWidget* parent = 0);
    virtual ~HelloFileBrowser();

public slots:
    virtual void init();
    virtual void destroy();
    virtual void TreeView_Clicked(TObject *o);
    virtual void execRoot();
    virtual void widget_destroyed( QObject * );
    virtual void CanvasEvent( TObject * obj, unsigned int event, TCanvas * canvas );
    virtual void SetRootFile(const QString&);
    virtual void SetLastWorkingDir(const QString&);
protected:
    TQtWidget *currentWidget;

protected slots:
    virtual void languageChange();

};

#endif // HelloFileBrowser_H
