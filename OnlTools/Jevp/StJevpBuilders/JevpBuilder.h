#ifndef __JEVPBUILDER_H__
#define __JEVPBUILDER_H__

#ifdef __CINT__
typedef unsigned char u_char;
#endif

#include "Jevp/StJevpPlot/JevpPlotSet.h"


// Base class for builders
//
// JevpPlotSet should not have any data related stuff in it
// So put this class in between...  It will just handle the data
// interfaces...
//

#include "DAQ_READER/daqReader.h"
#include "StEvent/StTriggerData.h"

class JevpBuilder : public JevpPlotSet {
 public:
    JevpBuilder(JevpServer *parent=NULL) : JevpPlotSet(parent) 
    {
    } ;
    
    StTriggerData *getStTriggerData(daqReader *rdr);


    ClassDef(JevpBuilder, 1);
};

#endif
