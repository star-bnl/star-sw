/*!\class StMuEmcUtil
\author Alexandre A. P. Suaide

This class has many utilities to convert StEvent format into
StEmcMicroEvent format.
*/
#ifndef StMuEmcUtil_h
#define StMuEmcUtil_h
#include "TObject.h"

class StMuEmcCollection;
class StEmcGeom;
class StEmcCollection;

class StMuEmcUtil : public TObject
{
  protected:
    StEmcGeom          *mGeo[4];		    
    
  public:
                       StMuEmcUtil();
                       ~StMuEmcUtil();
    StMuEmcCollection* getMuEmc(StEmcCollection*);
    StEmcCollection*   getEmc(StMuEmcCollection*);
    void               fillMuEmc(StMuEmcCollection*,StEmcCollection*);
    void               fillEmc(StEmcCollection*,StMuEmcCollection*);
    int               getEndcapId(int,int,int,int,int&);
    int               getEndcapBin(int,int,int&,int&,int&);
              
  ClassDef(StMuEmcUtil,1)
};

#endif
