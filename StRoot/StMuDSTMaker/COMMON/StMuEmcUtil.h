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
    virtual ~StMuEmcUtil();
    StMuEmcCollection* getMuEmc(const StEmcCollection*);
    StEmcCollection*   getEmc(const StMuEmcCollection*);
    void               fillMuEmc(StMuEmcCollection*, const StEmcCollection*);
    void               fillEmc(StEmcCollection*, const StMuEmcCollection*);
    int               getEndcapId(int,int,int,int,int&) const;
    int               getEndcapBin(int,int,int&,int&,int&) const;
              
  ClassDef(StMuEmcUtil,1)
};

#endif
