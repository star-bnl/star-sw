#ifndef StOnlineTriggerMonitoring_H
#define StOnlineTriggerMonitoring_H

#include <TObject.h>

class StOnlineTriggerMonitoring {
public:
    void saveTrigger(char* TS="", bool status=true, bool pedestal=true, bool lut=true, bool saveDB = true, char* startrg2 = "startrg2.saved", char* sc3 = "sc3.saved", char* bemcStatusCopy = "./bemcStatus.txt");
    ClassDef(StOnlineTriggerMonitoring, 1)
};

#endif
