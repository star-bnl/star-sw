#ifndef STAR_StFilterDialog
#define STAR_StFilterDialog

#ifdef R__QT
#include <qvbox.h>
class QTable;

class StFilterDialog : public QVBox
{
   Q_OBJECT
private:

   const char **fNamVal;
   const float *fDefs;
   float *fVals;
   int    fNVals;
   int    *fFlagg;
   QTable  *fTable; 
public:
   StFilterDialog(const char *wName=0,const char **NamVal=0,const float *defs=0, float *vals=0,int *flagg=0);
   ~StFilterDialog();
public slots:
   void     Update();
   void     Show  ();
   void     Reset ();	//Reset defaults
};
#endif // R__QT
#endif // STAR_StFilterDialog
