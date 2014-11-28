/// RunCalibrator.C
/// Macro used to perform the calculation of new voltage settings 
/// based on Gain vs HV data, as well as a set of current HV data
/// and required relative gain adjustments.
/// 
/// Input: Three files are needed on input for this macro. They
/// correspond to the relative gain vs HV data, the current HV settings
/// and the required relative gain adjustment.
/// These three files can have arbitrary names (and extension). They
/// are identified with the methods "setRefFile", "setGainFile", and 
/// "setVoltInputFile".
///
/// Output: One file is produced on output. Its name is arbitrary and
/// and must be set with the method "setVoltOutputFile". 
///
/// Note: There are no default files and file names. All four file names
/// (3 input, 1 output) must be set.
///
/// USAGE:
/// The work engine of this macro is the class "VoltCalibrator". This class
/// has the responsibility to open the input/output files, and iterate through
/// the input files, to first generate gain coefficients, and then read the
/// requested gain file, and current HV file to generate a set of new voltages.
/// This is achieved simply by calling the "process" method of the 
/// "VoltageCalibrator" class. e.g. vc.process().
///
/// Refer to the file VoltCalibrator.cxx for reference documentation on the 
/// details of the voltage calibrator.
///
/// \author Claude A Pruneau
///
void RunCalibrator()
{
  // Load the only required shared library.
  gSystem->Load("VoltageCalib");
  // Instantiate the voltage calibrator class 
  VoltCalibrator vc;
  // Select the file that contains the Gain vs HV data
  vc.setRefFile("StRoot/VoltageCalib/data/pmtGainVsHV.dat");
  // Select the file that contains the relative gains to be
  // be achieved
  vc.setGainFile("StRoot/VoltageCalib/data/gainTemplate.dat");
  // Select the file that containe the current voltages
  vc.setVoltInputFile("StRoot/VoltageCalib/data/hvTemplate.dat");
  // Select the file that will contain the new request voltages
  vc.setVoltOutputFile("StRoot/VoltageCalib/data/hvTestOutput.dat");
  // Perform the calculation
  vc.process();
}

