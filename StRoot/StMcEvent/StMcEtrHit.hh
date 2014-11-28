/***************************************************************************
 *
 ***************************************************************************/
#ifndef StMcEtrHit_hh
#define StMcEtrHit_hh

#include "StMcHit.hh"
class g2t_etr_hit_st;

class StMcEtrHit : public StMcHit {
public:
  StMcEtrHit() {}
  StMcEtrHit(g2t_etr_hit_st* pt); 
  ~StMcEtrHit() {}
  
  int layer() const;
  int sector() const;
  void Print(Option_t *option="") const; // *MENU* 
    
private:
    ClassDef(StMcEtrHit,1)
};
ostream&  operator<<(ostream& os, const StMcEtrHit&);
#endif
