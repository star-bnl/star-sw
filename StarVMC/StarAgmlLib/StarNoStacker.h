#ifndef __StarNoStacker_h__


#include "StarAgmlStacker.h"

/**
   \class StarNoStacker
   \brief The Seinfeld of Geometry Bulders...

   Allows AgML structures to be created w/out instantiating a geometry.  Useful for debugging
   purposes, documentation, dependency analysis, etc... 

 */

class StarNoStacker : public StarAgmlStacker
{

 public:
  StarNoStacker( const Char_t *name="NotAStacker",
		 const Char_t *title="The stacker that does nothing" );
  ~StarNoStacker(){ /* nada */ };

  Bool_t Build( AgBlock *block ){ return true; }
  Bool_t Position( AgBlock *block, AgPlacement placement ){ return true; }
  Bool_t Position( AgBlock *block, AgPosition  placement ){ return true; }; 
  Bool_t SearchVolume( const AgShape &shape, const AgAttribute &attribute ){ return true; }
  void   AddGroup( const Char_t *name ){ /* nada */ };


  ClassDef(StarNoStacker,1);

};

#endif
