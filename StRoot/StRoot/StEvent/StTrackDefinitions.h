#ifndef StTrackDefinitions_hh
#define StTrackDefinitions_hh
/* Numbering scheme for StTrack::mEncodedMethod and dst_track::method
** includes finder scheme + fitting method
** 
** fitting method byte 0-3 (only fit method per track)
** finder scheme bits (4-14) 
** This is a bitmap scheme. Places are reserved for detector trackers.
** Total number of bits reserved (including sign bit) is 12-bits for
** finders (trackers) and first 4-bits for fitting (see below).
**________________________________________________________
**   StTrack::fittingMethod    byte (0:3)
**   Unknown/undefined fitter     0
**   Helix 2 Step                 1   !bending/non-bending plane fits
**   Helix 3D                     2
**   Kalman (local helix)         3 
**   Line 2 Step                  4
**   Line 3D                      5
**   L3 Fitter                    6
**   ITTF Kalman Fit              7
**   Spare                    upto 15 (total of 4 bits)
**_______________________________________________________
**   StTrack::finderMethod     bits                                    
**   SVT Grouper                  4 (LSB) on/off                       
**   SVT Stk                      5                                    
**   SVT Other                    6                                    
**   TPC Standard                 7                                    
**   TPC Other                    8                                    
**   FTPC Conformal               9                                    
**   FTPC Current                10                                    
**   SVT-TPC svm                 11       vector-vector matcher        
**   SVT-TPC est                 12       TPCvector-SVTpoint matcher   
**   SVT-TPC pattern             13      under development             
**   Spare                    14-15 (MSB)                            
**_______________________________________________________
**   StTrack::mEncodedMethod =
**   dst_track.method = fittig_method (4 bits word, 0:3) + 1<< finding_method [+  1<< finding_method2 ..]
**  
*/
/*    Fitting method                 byte[0:3] */
#define kUndefinedFitterIdentifier      0
#define kHelix2StepIdentifier           1
#define kHelix3DIdentifier              2
#define kKalmanFitIdentifier            3
#define kLine2StepIdentifier            4
#define kLine3DIdentifier               5
#define kL3FitIdentifier                6
#define kITKalmanFitIdentifier          7
/* numbering for track finder scheme has to be in synch. with StEnumerations.h */
/* Finder schema                      bit                                      */
#define ksvtGrouperIdentifier           4 
#define ksvtStkIdentifier               5
#define ksvtOtherIdentifier             6
#define ktpcStandardIdentifier          7
#define ktpcOtherIdentifier             8
#define kftpcConformalIdentifier        9
#define kftpcCurrentIdentifier         10
#define ksvtTpcSvmIdentifier           11
#define ksvtTpcEstIdentifier           12
#define ksvtTpcPatternIdentifier       13
#define kl3StandardIdentifier          14


#endif
