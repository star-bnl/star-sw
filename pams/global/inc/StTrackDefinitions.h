
/* Numbering scheme for dst_track::methods, finder scheme
**   Unknown Method          0
**   SVT-Grouper             1
**   SVT-Stk                 2
**   TPC-Standard            3 
**   SVT-TPC svm             4     !vector-vector matcher
**   SVT-TPC est             5     !TPCvector-SVTpoint matcher
*/

/* numbering scheme for track finding types in dst_track.idl*/

#define kUndefinedFinderIdentifier           0
#define kSvtGrouperIdentifier                1
#define kSvtStkIdentifier                    2
#define kTpcStandardIdentifier               3
#define kSvtTpcSvmIdentifier                 4
#define kSvtTpcEstIdentifier                 5

/* Numbering scheme for dst_track::method, finder quality scheme
**   Unknown/Undefined           0
**   SVT-Grouper pass            1
**   SVT-Stk  pass               2
**   SVT-TPC svm  pass           4  
**   SVT-TPC est  pass           5  
*/

/* numbering scheme for track finding quality types in dst_track.idl*/

#define kUndefinedQualityIdentifier           0
#define kGrouperPassIdentifier                1
#define kStkPassIdentifier                    2
#define kSvmPassIdentifier                    3
#define kEstPassIdentifier                    4


/* Numbering scheme for dst_track::methods, fitter scheme
**   Unknown/undefined fitter     0
**   Helix 2 Step                 1   !bending/non-bending plane fits
**   Helix 3D                     2
**   Kalman (local helix)         3 
**   Line 2 Step                  4
**   Line 3D                      5
*/

/* numbering scheme for track fitting types in dst_track.idl*/

#define kUndefinedFitterIdentifier      0
#define kHelix2StepIdentifier           1
#define kHelix3DIdentifier              2
#define kKalmanFitIdentifier            3
#define kLine2StepIdentifier            4
#define kLine3DIdentifier               5


