
/* Numbering scheme for dst_track::methods, finder scheme
** This is a bitmap scheme. Places are reserved for detector trackers.
** Total number of bits reserved (including sign bit) is 12-bits for
** finders (trackers) and 4-bits for fitting (see below). Note that
** fitting occupies the first 4-LSB in the 'short'. 
**                    Bit position   
**   Unknown Method                all bits off      
**   SVT Grouper           5 (LSB) on/off
**   SVT Stk               6
**   SVT Other             7
**   TPC Standard          8
**   TPC Other             9
**   FTPC Conformal        10
**   FTPC Current          11
**   SVT-TPC svm           12      vector-vector matcher
**   SVT-TPC est           13      TPCvector-SVTpoint matcher
**   SVT-TPC pattern       14      under development
**   Spare               15-16 (MSB)
*/

/* Numbering scheme for dst_track::methods, fitter scheme
**   Unknown/undefined fitter     0
**   Helix 2 Step                 1   !bending/non-bending plane fits
**   Helix 3D                     2
**   Kalman (local helix)         3 
**   Line 2 Step                  4
**   Line 3D                      5
**   Spare                    upto 15 (total of 4 bits)
*/

/* numbering scheme for track fitting types in dst_track.idl*/

#define kUndefinedFitterIdentifier      0
#define kHelix2StepIdentifier           1
#define kHelix3DIdentifier              2
#define kKalmanFitIdentifier            3
#define kLine2StepIdentifier            4
#define kLine3DIdentifier               5


