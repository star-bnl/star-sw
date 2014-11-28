///VoltCalibrator.cxx
///
///Implementation of class VoltCalibrator for the calculation of
///PMT high voltage settings.
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
/// Operation:
/// File names must be set with the modifier methods described above.
/// The actual calculation is performed by the "process" method.
/// The calculation first proceeds with the instantiation of a GainVoltCoeffCalculator
/// wich reads in the calibration data and calculate for each PMT the gain 
/// coefficients. (see file GainVoltCoeffCalculator.cxx for documentation on the
/// specifics on that calculation)
/// The GainVoltCoeffCalculator instance is then used to process the calculation
/// of new voltage settings. This first involves reading the current voltage 
/// settings as well as the requested RELATIVE gain adjustments.
///
///\author Claude A Pruneau, Wayne State University
///
#include "VoltCalibrator.h"
#include "GainVoltCoeffCalculator.h"
#include "PmtIdentifier.h"
#include "GainVoltPmtParameters.h"
ClassImp(VoltCalibrator)

VoltCalibrator::VoltCalibrator()
{}

VoltCalibrator::~VoltCalibrator()
{}

void VoltCalibrator::setRefFile(const char * name)
{
  refFile = name;
}

void VoltCalibrator::setGainFile(const char *name)
{
  gainFile = name;
}

void VoltCalibrator::setVoltInputFile(const char *name)
{
  currentVoltFile = name;
}

void VoltCalibrator::setVoltOutputFile(const char *name)
{
  newVoltFile = name;
}

void VoltCalibrator::process()
{
  // open files
  ifstream ref;
  ifstream gain;
  ifstream currentVolt;
  ofstream coeff;
  ofstream newVolt;
  ref.open(refFile);
  gain.open(gainFile);
  currentVolt.open(currentVoltFile);
  newVolt.open(newVoltFile);
  coeff.open("hvGainCoeff.dat");

  // read the reference file, calculate the coefficients
  GainVoltCoeffCalculator calculator;
  ref >> calculator;
  calculator.process();
  coeff << calculator;

  // loop on all channels, read current volt, read requested gain change
  // calculate new voltage, write voltage 
  PmtIdentifier pmtId;
  PmtIdentifier pmtIdCurrent;
  double mygain[4800];
  double g,v,newV,cGain;
  
  // read all gains from file and save in this vector
  while(!gain.eof())
  {
    gain >> pmtId >>g;
    mygain[pmtId._softId-1]=g;
  }
  
  // loop over current voltage database file
  while(!currentVolt.eof())
  {
    currentVolt >> pmtIdCurrent >> v;
	  long soft = pmtIdCurrent._softId;
    long serial = pmtIdCurrent._serial;
    g = mygain[soft-1];
    if(g==0 || g==1) newV = v;
    else
    {
      vector<GainVoltPmtParameters*>::iterator i;
      for (i=calculator.begin();i!=calculator.end();i++)
      {
        PmtIdentifier identifier = (*i)->getPmtIdentifier();
        if(identifier._serial==serial)
        {
          cout <<"PMT found in DB  | ";
      	  cGain = (*i)->getGain(v);
      	  newV  = (*i)->getVoltage(g*cGain);
          goto NEXT;          
        }
      }
      double A = 2.56e-29;
      double B = 10.71;
      cGain = exp(A+B*log(v));
      newV = exp(log(cGain*g)/B-A);
      cout <<"PMT is not in DB | ";
    }
    NEXT:
		cout <<"id = "<<soft<<"  pmtId = "<<pmtIdCurrent<<"  V = "<<v<<"  cGain = "<<cGain<<"  newV = "<<newV<<"  corr = "<<g<<endl;
    newVolt<<pmtIdCurrent<<"\t"<<newV<<endl;
  }
  
  ref.close();
  gain.close();
  currentVolt.close();
  newVolt.close();
  coeff.close();
}

/// A convenience method used to create template files.
/// Do not use/call unless you actually (think) you
/// know what you are doing.
void VoltCalibrator::createTemplates()
{
  // open files
  ifstream ref;
  ofstream gain;
  ofstream volt;
  ref.open(refFile);
  gain.open("gainTemplate.dat");
  volt.open("hvTemplate.dat");
  // read the reference file, calculate the coefficients
  GainVoltCoeffCalculator calculator;
  ref >> calculator;
  vector<GainVoltPmtParameters*>::iterator i;
  for (i=calculator.begin();i!=calculator.end();i++)
    {
      cout << (*i)->getPmtIdentifier() << "\t" << 1 << endl;
      gain << (*i)->getPmtIdentifier() << "\t" << 1 << endl;
      volt << (*i)->getPmtIdentifier() << "\t" << 777 << endl;
    }
  ref.close();
  gain.close();
  volt.close();
}
