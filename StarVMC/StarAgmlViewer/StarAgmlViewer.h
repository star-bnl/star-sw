#ifndef __StarAgmlViewer_h__
#define __StarAgmlViewer_h__

#include "StMaker.h"
#include "TEveManager.h"

class TGLViewer;

class StarAgmlViewer : public StMaker
{

 public:
  StarAgmlViewer( const Char_t *name="agmlViewer" );
  ~StarAgmlViewer(){ /* nada */ };
  
  Int_t Init();

  Bool_t addGlobalElement( const Char_t *vol, const Char_t *node );

  void   applyTransparency( const Char_t *TOP, Int_t mode=-1 );

 private:
 protected:
  ClassDef(StarAgmlViewer,1);

  TEveManager *mEve; // EVE manager
  

  Bool_t LoadGeometry();


};

#endif
