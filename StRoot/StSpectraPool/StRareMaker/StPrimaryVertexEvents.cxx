#include "StPrimaryVertexEvents.h"
#include "StEventTypes.h"
  ClassImp(StPrimaryVertexEvents)
int StPrimaryVertexEvents::Accept(StEvent* event){
 if (event->primaryVertex()) {
  return 1;
 }
 else {
  return 0;
 }
}

void StPrimaryVertexEvents::Report(){cout << "StPrimaryVertexEvents: require valid primary vertex" << endl;}
