// $Id: StPreVertexMaker.h,v 1.1 2000/02/01 17:12:18 wdeng Exp $
// $Log: StPreVertexMaker.h,v $
// Revision 1.1  2000/02/01 17:12:18  wdeng
// Initial version. This maker reads in tptrack and produces a preliminary primary vertex.
//

#ifndef STAR_StPreVertexMaker
#define STAR_StPreVertexMaker

#ifndef StMaker_H
#include "StMaker.h"
#endif

#include "tables/St_tpt_track_Table.h"
#include "tables/St_evr_evrpar_Table.h"
#include "tables/St_dst_track_Table.h"
#include "tables/St_dst_vertex_Table.h"

class St_evr_evrpar;
class St_tpt_track;
class St_dst_track;
class St_dst_vertex;

class StPreVertexMaker : public StMaker {
  
 private:
  St_evr_evrpar  *m_pre_evrpar;  //!
  
 public: 
  StPreVertexMaker(const char *name="preVertex");
  virtual       ~StPreVertexMaker();
  virtual Int_t  Init();
  virtual Int_t  Make();
  
  ClassDef(StPreVertexMaker, 1)
};
    
#endif
    
