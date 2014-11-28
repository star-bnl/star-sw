


class GainVoltCalibrator
{
public:
  GainVoltCalibrator();
  GainVoltCalibrator(double refVoltage, double gains[5]);
  GainVoltCalibrator(int n, double volts[], double gains[]);
  GainVoltCalibrator(const GainVoltCalibrator&parameters);
  ~GainVoltCalibrator();
  GainVoltCalibrator & operator(const GainVoltCalibrator&parameters);

  void setInputFileName(const char * fileName);
  void setOutputFileName(const char * fileName);
  void process();
  void load();
  vois save();

protected:

  vector<GainVolPmtParameters*> _pmts;
  int status;
}

/// Uses 3 types of files
/// (1) ".adc"     : Raw gain parameters : A list of reference voltage, 5 ADC values for each PMT
/// (2) ".coef"    : Gain vs Voltage Coefficients : List of coefficients for all PMT
/// (3) ".absGain" : Desired Gain List : List of absolute gains
/// (4) ".relGain" : Desired Gain Change : List of current voltage, and need gain change
/// (5) ".volts"   : List of calculate voltages to be applied


/// Generate calibration coefficients
/// Reads raw gain parameters; file type: ".adc"
/// Produces gain voltage coefficients; file type: ".coef"
void GainVoltCalibrator::calculateCalibCoefficient()
{
  cout << "GainVoltCalibrator::generateCalibCoefficient() - INFO - Started" << endl;
  GainVoltCalibrator calibrator;
  calibrator.setInputFileName(inputName);
  calibrator.setOutputFileName(outputName);
  calibrator.load();
  calibrator.process();
  calibrator.save();
  cout << "GainVoltCalibrator::generateCalibCoefficient() - INFO - Started" << endl;
}

/// Calculate voltage estimates on the basis of existing calibration
/// coefficients
/// Reads ".coef"    : Gain vs Voltage Coefficients : List of coefficients for all PMT
/// (3) ".absGain" : Desired Gain List
void calculateVoltageEstimates()
{
  cout << "GainVoltCalibrator::generateCalibCoefficient() - INFO - Started" << endl;
  GainVoltCalibrator calibrator;
  calibrator.setInputFileName(inputName);
  calibrator.setOutputFileName(outputName);
  calibrator.load();
  calibrator.process();
  calibrator.save();
  cout << "GainVoltCalibrator::generateCalibCoefficient() - INFO - Started" << endl;

}