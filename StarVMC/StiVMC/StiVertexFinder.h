#ifndef StiVertexFinder_H
#define StiVertexFinder_H 1
#include <vector>
#include "TNamed.h"
#include "StiFactory.h"
class StGenericVertexFinder;
class StEvent;
class StiHit;
class StiVertexFinder : public TNamed {
 public:
  StiVertexFinder(const Char_t * name="");
  virtual ~StiVertexFinder() {}
  Int_t fit(StEvent*);                     // fit the vertex
  StiHit * getVertex(Int_t index);	
  Int_t size() const;	
  void Clear(const Option_t *opt="");
  Factory<StiHit>* HitFactory() {return _hitFactory;}
  const std::vector<StiHit*> *result();
 private:
  Factory<StiHit>*       _hitFactory;
  std::vector<StiHit* >  _result;
  StGenericVertexFinder* mGVF;
  ClassDef(StiVertexFinder,0)
};
#endif
