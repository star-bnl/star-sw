#ifndef GainVoltPmtParameters_H_INCLUDED
#define GainVoltPmtParameters_H_INCLUDED
#include <Stiostream.h>
#include <vector>
using std::vector;
#include "PowerLawFit.h"
#include "PmtIdentifier.h"

/// Class used to describe the gain vs bias voltage of a photomultiplier tube.
/// Input parameters consist of experimentally measured relative gain vs
/// bias voltage. Output parameters consist of the gain parameters 
/// obtained with a power law parameterization.
class GainVoltPmtParameters 
{
public:
  GainVoltPmtParameters();
  GainVoltPmtParameters(double refVoltage, double gains[5]);
  GainVoltPmtParameters(int n, double volts[], double gains[]);
  GainVoltPmtParameters(const GainVoltPmtParameters&parameters);
  ~GainVoltPmtParameters();
  GainVoltPmtParameters & operator=(const GainVoltPmtParameters&parameters);

  /// Set the voltages and gains
  void set(double refVolt, double gains[5]);
  /// Set the voltages and gains
  void set(int n, double volts[], double gains[]);

  /// Write obbject to ostream
  friend ostream& operator<<(ostream& os, GainVoltPmtParameters& object);
  /// Read object from istream
  friend istream& operator>>(istream& is, GainVoltPmtParameters& object);
  /// fit the data
  void fit();
  ///Get multiplicative constant
  double getMultConstant() const;
  ///Get exponent of the power law
  double getExponent() const;
  ///Get the gain obtained if the given voltage is applied
  double getGain(double voltage) const;
  ///Get the voltage need to obtain the given relative gain
  double getVoltage(double gain) const;
  ///Get the number of data points available and used for this PMT
  int getNPoints() const;
  ///Print the information of this PMT
  void print();
  ///Get the PMT identifier
  PmtIdentifier & getPmtIdentifier();

  /// Set the default coefficients used in lieu of the fitted coefficients
  /// when there are insufficient or invalid data to perform a fit. 
  static void setDefaults(double multCoefficient, double exponent);

  /// Determine whether the input data are sufficient and valid
  /// to perform a fit to determine the gain coefficients.
  bool isValid() const;

  static int ioMode;
protected:
  PmtIdentifier _id;
  /// multiplicative coefficient
  double _a;
  /// Exponent
  double _b;
  /// HV/Gain data
  map<double,double> _data;
  ///Transient Fit Object
  PowerLawFit<double> _fit;

  /// Default value for PMTs that have incomplete data
  /// and for which a fit cannot be done. 
  /// The multiplicative coefficient "_defaultA" is obtained
  /// a geometric average of the multiplicative coefficients 
  /// of many towers. The exponent "_defaultB" is obtained
  /// as arithmetic average of the exponents of other towers.
  /// These values are NOT calculated internally and must be
  /// externally by the user of this code using the setDefaults()
  /// public method. 
  static double _defaultA;
  static double _defaultB;

};


#endif
