St_DataSet *CreateTable() { 
// -----------------------------------------------------------------
// Top/ftpcClusterPars Allocated rows: 1  Used rows: 1  Row size: 184 bytes
//  Table: ftpcClusterPars_st[0]--> ftpcClusterPars_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_ftpcClusterPars")) return 0;
ftpcClusterPars_st row;
St_ftpcClusterPars *tableSet = new St_ftpcClusterPars("ftpcClusterPars",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.gaussFittingFlags         =          0; // 2=time gaussfit, 3=all gaussfit ;
    row.minimumClusterMaxADC      =         15; // cluster minimum peakheight ;
    row.numberOfDriftSteps        =      25600; // # of steps in padtrans integration ;
    row.orderOfDiffusionErrors    =          3; // order of diffusion errors; 
    row.padDiffusionErrors[0]     =      0.015; // function of driftlength up to 2nd order;
    row.padDiffusionErrors[1]     =     0.0003;
    row.padDiffusionErrors[2]     =          0;
    row.timeDiffusionErrors[0]    =     0.0025; // function of driftlength up to 2nd order;
    row.timeDiffusionErrors[1]    =      1e-05;
    row.timeDiffusionErrors[2]    =          0;
    row.lorentzAngleFactor        =          1; // multiplicative lorentz angle correction;
    row.padBadFitError            =      0.095; // additional error for bad-fit clusters;
    row.timeBadFitError           =      0.033; // additional error for bad-fit clusters;
    row.padUnfoldError            =      0.019; // additional error for unfolded clusters;
    row.timeUnfoldError           =     0.0066; // additional error for unfolded clusters;
    row.padFailedFitError         =       0.13; // additional error for failed-fit clusters;
    row.timeFailedFitError        =      0.045; // additional error for failed-fit clusters;
    row.padCutoffClusterError     =        0.2; // additional error for cut-off clusters;
    row.timeCutoffClusterError    =      0.015; // additional error for cut-off clusters;
    row.padSaturatedClusterError  =      0.038; // additional error for saturated clusters;
    row.timeSaturatedClusterError =     0.0088; // additional error for saturated clusters;
    row.twoPadWeightedError       =     0.0171; // additional error 2-pad-cluster, w.mean ;
    row.twoPadGaussError          =    0.01045; // additional error 2-pad-cluster, gaussfit ;
    row.threePadWeightedError     =     0.0152; // additional error 3-pad-cluster, w.mean ;
    row.threePadGaussError        =    0.00057; // additional error 3-pad-cluster, gaussfit ;
    row.zDirectionError           =       0.01; // parameter for error in z-direction ;
    row.normalizedNowPressure     =    1013.25; // normalized air pressure (in hPa) ;
    row.adjustedAirPressureWest   =    1013.25; // temperature adjusted air pressure for FTPC West 
    row.adjustedAirPressureEast   =    1013.25; // temperature adjusted air pressure for FTPC East 
    row.minPressure               =     990.;   // minimum barometric pressure (in hPa);
    row.maxPressure               =    1040.;   // maximum barometric pressure (in hPa);
    row.gasTemperatureWest        =      25.8;  // temperature of gas FTPC East ;
                                                // initially set to default 
    row.gasTemperatureEast        =      25.1;  // temperature of gas FTPC East ;
                                                // initially set to default 
    row.minGasTemperature         =      23.0;  // maximum gas temperature (in centigrade);
    row.maxGasTemperature         =      30.0;  // maximum gas temperature (in centigrade);
    // old settings
    row.maxNumSequences           =       160;  // max number of used sequences
    row.maxNumSeqPeaks            =       160;  // max number of peaks per sequence
    row.maxNumPeaks               =        10;  // max number of unfolded peaks per cluster
    row.maxNumCUC                 =       128;  // max number of cluster under construction
    row.maxLoops                  =       100;  // max loopnumber in unfolding
    row.maxFastLoops              =        30;  // max loopnumber in fast unfolding
    row.unfoldLimit               =      0.01;  // accuracy of unfolding 
    row.unfoldFailedLimit         =      0.50;  // limit for failed unfolding
    row.maxTimelength             =       100;  // max timelength of cluster
    row.maxPadlength              =       100;  // max padlength of cluster
    row.minTimebin                =         3;  // min timebin for cluster search
    
tableSet->AddAt(&row,0);
// ----------------- end of code ---------------
 return (St_DataSet *)tableSet;
}
