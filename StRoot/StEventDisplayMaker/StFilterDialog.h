#ifndef STAR_StFilterDialog
#define STAR_StFilterDialog

#ifdef R__QT
#  include <qwidget.h>

class QTable;
class QPushButton;
class QTableView;

class StFilterDialog : public     QWidget
{
   Q_OBJECT
private:
   const char **fNamVal;
   const float *fDefs;
   float       *fVals;
   int          fNVals;
   int         *fFlagg;
   bool        *fActive;
#if (QT_VERSION < 0x040000)
   QTable      *fTable;
#else
   QTableView   *fTable;
#endif
private:
   QPushButton *fOn;
public:
   StFilterDialog(const char *wName=0,const char **NamVal=0,const float *defs=0, float *vals=0,int *flagg=0,bool *active=0);
   ~StFilterDialog();
public slots:
   void     Update();
   void     Show  ();
   void     Reset ();	//Reset defaults
   void     Toggle();

};
#endif // R__QT
#endif // STAR_StFilterDialog
