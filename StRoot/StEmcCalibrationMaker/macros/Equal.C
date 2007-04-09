////////////////////////////////////////////////////////////////////

class StEmcEqualMaker;
class StEmcMipMaker;
StEmcEqualMaker *equal = 0;

TMemStat memory;

void Equal()
{
  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();
  gSystem->Load("StDbUtilities");
  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");
  gSystem->Load("libgeometry_Tables");
  gSystem->Load("StDaqLib");
  gSystem->Load("StEmcRawMaker");
  gSystem->Load("StEmcADCtoEMaker");

// Load my maker
  gSystem->Load("StEmcCalibrationMaker");
		
  if(equal) { delete equal; equal = 0;}
  equal = new StEmcEqualMaker(); 
  
  // need to configure the maker with the same parameters that
  // were used to create the spectra.
  equal->setDetector(1);
  equal->setRange(700);
  equal->Init();
		
  // load equalization data.
  equal->loadHist("equal/equal.root");
  
  // equalization of the detector based on slopes
  // mode = equalization mode
  //        0   = equalize based on mean and RMS spectrum values (linear)
  //        1   = equalize based only on mean values (linear)
  //        2   = equalize based on mean and RMS spectrum values (log)
  //        3   = equalize based only on mean values (log)
  //        4   = equalize based on exponential fit and compare to a reference spectrum
  //        5   = equalize based on exponential fit and compare slope to a given function
  //              the reference is found as the tower with the
  //              mean ADC value closest to the mean of mean ADC values
  // Only 4 and 5 are fully tested. I never paid too much attention to the others. 
  //
  // deta = size of eta bin in towers
  //
  // et   = true/false. This sets if equalization is done in E or Et.
  //        it only makes difference if deta is too large
  //
  // to tune up the equalization need to change the function StEmcEqualMaker::equalize()
  // because we need to look at the data to check statistics to decide the fit
  // limits and initial guesses. Every run we have something different.
    
  int mode = 5;
  int deta = 2;
  bool et=false;
  equal->equalize(mode, deta, et);
  
  // This saves the equalization contants to a root file. This file can
  // be used to do eta bins MIP calibration or to change the high voltages.
  // the parameters are date, time
             
  equal->saveEqual(20040101,0);
      
}
