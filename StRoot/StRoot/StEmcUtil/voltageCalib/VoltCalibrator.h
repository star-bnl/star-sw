#ifndef VoltCalibrator_H_INCLUDED
#define VoltCalibrator_H_INCLUDED
#include "Stiostream.h"
#include "TObject.h"

class VoltCalibrator
{
 public:

  VoltCalibrator();
  virtual ~VoltCalibrator();
  void setRefFile(const char * name);
  void setGainFile(const char *name);
  void setVoltInputFile(const char *name);
  void setVoltOutputFile(const char *name);
  void process();
  void createTemplates();
 protected:

  /// Reference Gain/Volt File
  const char * refFile;
  /// Requested Relative Gain Change File
  const char * gainFile;
  /// Current Voltage Setting File
  const char * currentVoltFile;
  /// New Voltage Setting File
  const char * newVoltFile;

  ClassDef(VoltCalibrator,1)
    
};
#endif
