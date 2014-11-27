#include "TQtFunViewer.h"
#include "Viewer.h"
//__________________________________________________________________________
RootViewer::RootViewer(TF2 *fun, QWidget *parent): fFun(fun)
{
  fViewer = new TQtFunViewer(fFun,parent); 
}
//__________________________________________________________________________
RootViewer::~RootViewer()
{
   delete fViewer;
}
//__________________________________________________________________________
void RootViewer::Draw(TF2 *fun)
{
  fFun = fun;
  if (!fViewer)  fViewer = new TQtFunViewer(fFun);
  else fViewer->SetFun(fFun);
}
//__________________________________________________________________________
void RootViewer::Show()
{
    if (fViewer) fViewer->show();
}
//__________________________________________________________________________
void RootViewer::SetColZ(Bool_t on)
{
   if  (fViewer) fViewer->SetColZ(on);
}
//__________________________________________________________________________
void RootViewer::SetTop(Bool_t on)
{
   if  (fViewer) fViewer->SetTop(on);
}
