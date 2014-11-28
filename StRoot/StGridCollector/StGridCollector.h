/*!
 * \class StGridCollector 
 *
 * StGridCollector                                                      
 *                                                                      
 * Class enables the interface to the Grid Collector services.          
 *                                                                      
 * To isolate the implementation details from the STAR Analysis         
 * Framework, it uses the factor pattern.  In keeping with the          
 * convention, this class implements a static method called Create()    
 * for constructing an object of StGridCollector type.  The function    
 * Create() loads the appropriate shared libraries that implement the   
 * real interface.                                                      
 *                                                                      
 * John Wu <John.Wu@nersc.gov>                                          
 */

#ifndef STAR_StGridCollector
#define STAR_StGridCollector

#include "StFileI.h"

class StGridCollector : public StFileI {

protected: // no public access to these functions, no automatic functions
    StGridCollector() : StFileI() {}
    StGridCollector(const StGridCollector&);
    const StGridCollector& operator=(const StGridCollector&);

public:

    virtual ~StGridCollector() {}

    static StGridCollector *Create(const char* sel=0);
    virtual Int_t GetNEvents() = 0;
    virtual void  holdFiles() = 0;
  
    ClassDef(StGridCollector,0)

};
    
#endif
