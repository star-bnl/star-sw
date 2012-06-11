#ifndef GainVoltCoeffCalculator_H_Included
#define GainVoltCoeffCalculator_H_Included
#include <fstream>
#include <string>
#include <vector>
using std::string;
//yf using std::ifstream;
using std::vector;
#include "TObject.h"
class GainVoltCoeffCalculator;
class GainVoltPmtParameters;

/// This class defines a PMT Gain/Voltage Coefficient calculator.
/// Input:  Raw gain parameters : A list of reference voltage, 5 ADC values for each PMT
/// Output: Gain coefficients for all PMTs.
///
/// The coefficients are calculated assuming a power law
/// g(v) = alpha*v^beta
///
class GainVoltCoeffCalculator
{
public:
  typedef vector<GainVoltPmtParameters*> GVParameters;
  typedef GVParameters::iterator GVP_iterator;
  typedef GVParameters::const_iterator GVP_const_iterator;

  GainVoltCoeffCalculator();
  GainVoltCoeffCalculator(const GainVoltCoeffCalculator&calculator);
  virtual ~GainVoltCoeffCalculator();
  GainVoltCoeffCalculator & operator=(const GainVoltCoeffCalculator&calculator);

  void setIoMode(int mode);
  void process();
  friend std::ostream& operator<<(std::ostream& os, GainVoltCoeffCalculator& object);
  friend std::istream& operator>>(std::istream& is, GainVoltCoeffCalculator& object);
  GVP_iterator begin();
  GVP_iterator end();
    
  GVP_const_iterator begin() const;
  GVP_const_iterator end() const;

protected:
  GVParameters _pmts;
  //ClassDef(GainVoltCoeffCalculator, 1)
};

#endif


