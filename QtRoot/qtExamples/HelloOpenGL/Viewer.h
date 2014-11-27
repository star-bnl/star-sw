#ifndef STAR_RootViewer
#define STAR_RootViewer

#include "TObject.h"


class  TF2;
class TQtFunViewer;
class QWidget;
class RootViewer : public TObject 
{
   private:
      TF2 *fFun;
      TQtFunViewer *fViewer;

   public:
        RootViewer(TF2 *fun=0, QWidget *parent=0);  
       ~RootViewer();
       void Draw(TF2 *fun);
       void SetColZ(Bool_t on=kTRUE);
       void SetTop(Bool_t on=kTRUE);
       void Show();
};

#endif
