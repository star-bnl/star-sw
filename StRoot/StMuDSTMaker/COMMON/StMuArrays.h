/***************************************************************************
 *
 * $Id: StMuArrays.h,v 1.3 2002/05/20 17:23:31 laue Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 ***************************************************************************/
/** 
    @class StMuArrays 
    Class holding the definitions of the TClonesArrays as static data members.
    There are two sets of TClonesArrays. The 'arrays' for tracks and event information,
    as well as the 'strangeArrays' holding the information that has been copied from the 
    StStrangeMuDst (i.e. V0s, Kinks, etc.)
*/
#ifndef StMuArrays_hh
#define StMuArrays_hh

/// @enum strangeTypes enumeration to to index the strangeArrays
enum strangeTypes {smuEv=0, smuEvMc, smuV0, smuV0mc, smuV0Assoc, smuXi, smuXiMc, smuXiAssoc, smuKink, smuKinkMc, smuKinkAssoc, smuCut};
/// enumeration to to index the arrays
enum muDstTypes {muEvent=0, muPrimary, muGlobal, muOther, muL3, muRich, muState, muAccept, muReject}; 

#define __NARRAYS__ 9
#define __NSTRANGEARRAYS__ 12

class StMuArrays {
 public:
    static char* arrayNames[__NARRAYS__]; ///< names of the TBranches in the TTree/File                        
    static char* arrayTypes[__NARRAYS__]; ///< names of the classes, the TClonesArrays are arrays of this type 
    static int arraySizes[__NARRAYS__];   ///< maximum sizes of the TClonesArrays                              
    static int arrayCounters[__NARRAYS__];///< number of entries in current event, currently not used          
    
    static char* strangeArrayNames[__NSTRANGEARRAYS__]; ///< names of the TBranches in the TTree/File                        
    static char* strangeArrayTypes[__NSTRANGEARRAYS__];	///< names of the classes, the TClonesArrays are arrays of this type 
    static int strangeArraySizes[__NSTRANGEARRAYS__];	///< maximum sizes of the TClonesArrays 
    static int strangeArrayCounters[__NSTRANGEARRAYS__];///< number of entries in current event, currently not used 
};

#endif

/***************************************************************************
 *
 * $Log: StMuArrays.h,v $
 * Revision 1.3  2002/05/20 17:23:31  laue
 * StStrangeCuts added
 *
 * Revision 1.2  2002/05/04 23:56:29  laue
 * some documentation added
 *
 * Revision 1.1  2002/03/08 17:04:17  laue
 * initial revision
 *
 *
 **************************************************************************/
