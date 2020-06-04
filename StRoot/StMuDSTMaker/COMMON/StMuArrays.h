/***************************************************************************
 *
 * $Id: StMuArrays.h,v 1.35 2019/02/21 13:32:54 jdb Exp $
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
#include "Rtypes.h"
/// @enum emcTypes enumeration to to index the emcArrays
//enum emcTypes {muEmc=0};
enum emcTypes {muEmcTow=0, muEmcPrs, muEmcSmde, muEmcSmdp, muEEmcPrs, muEEmcSmdu, muEEmcSmdv};

/// @enum fgtTypes enumeration to to index the fgtArrays
enum fgtTypes {muFgtStrips=0, muFgtClusters, muFgtStripAssociations, muFgtAdcs };

enum fmsTypes {muFmsHit=0, muFmsCluster, muFmsPoint, muFmsInfo};
#ifndef __NO_STRANGE_MUDST__
/// @enum strangeTypes enumeration to to index the strangeArrays
enum strangeTypes {smuEv=0, smuEvMc, smuV0, smuV0Mc, smuV0Assoc, smuXi, smuXiMc, smuXiAssoc, smuKink, smuKinkMc, smuKinkAssoc, smuCut};
#endif
/// @enum MCTypes enumeration to to index the mcArrays
enum MCTypes {MCVertex=0, MCTrack};
/// @enum enumeration to to index the arrays
enum muDstTypes {muEvent=0, muPrimaryVertex, muPrimary, muGlobal, muOther, muL3, muRich, muState, muAccept, muReject, muCovGlobTrack, muCovPrimTrack, mupp2pp, muMtd}; 

/// @enum pmdTypes enumeration to to index the pmdArrays
enum pmdTypes {muPmdHit=0, muCpvHit, muPmdCluster, muCpvCluster}; 

/// @enum Tofr enumeration
// run 5 - dongx
enum tofTypes {muTofHit=0, muTofData, muTofRawData};

/// dongx
enum btofTypes {muBTofHit=0, muBTofRawHit, muBTofHeader};

// jdb
enum etofTypes {muETofDigi=0, muETofHit, muETofHeader};

enum mtdTypes {muMTDHit=0, muMTDRawHit, muMTDHeader};

enum epdTypes {muEpdHit=0};    // MALisa

/// @enum eztTypes enumeration to to index the eztArrays (IUCF-ezTree)
enum eztTypes {muEztHead=0, muEztTrig, muEztETow, muEztESmd,muEztFpd};

enum NARRAYS {
__NARRAYS__        =14,	///< size of the 'regular stuff' arrays, i.e. number of TClonesArrays  (add two more for global and primary track covariance matrices)
#ifndef __NO_STRANGE_MUDST__
__NSTRANGEARRAYS__ =12,	///< size of the strangeness arrays, i.e. number of TClonesArrays  
#endif
__NMCARRAYS__ =2,	///< size of the MCness arrays, i.e. number of TClonesArrays  
__NEMCARRAYS__     =7 ,	///< size of the emc arrays, i.e. number of TClonesArrays  
__NPMDARRAYS__     =4 ,	///< size of the pmd arrays, i.e. number of TClonesArrays  
 __NFMSARRAYS__    =4 ,	///< size of the fms arrays, i.e. number of TClonesArrays  
// run 5 - dongx
__NTOFARRAYS__     =3 ,  ///< size of the tof arrays >
__NBTOFARRAYS__    =3 ,  /// dongx
__NETOFARRAYS__    =3 ,  /// jdb
__NEPDARRAYS__     =1,   /// MALisa
__NMTDARRAYS__    =3,
 __NFGTARRAYS__    =4 ,	///< size of the fgt arrays, i.e. number of TClonesArrays  
__NEZTARRAYS__     =5 ,  ///< size of the ez arrays >
     
/// dongx
#ifndef __NO_STRANGE_MUDST__
__NALLARRAYS__     =  __NARRAYS__+__NSTRANGEARRAYS__+__NMCARRAYS__+__NEMCARRAYS__+__NFMSARRAYS__+__NPMDARRAYS__+__NTOFARRAYS__+__NBTOFARRAYS__+__NETOFARRAYS__+__NEPDARRAYS__+__NMTDARRAYS__+__NFGTARRAYS__+__NEZTARRAYS__
#else
__NALLARRAYS__     =  __NARRAYS__+__NMCARRAYS__+__NEMCARRAYS__+__NFMSARRAYS__+__NPMDARRAYS__+__NTOFARRAYS__+__NBTOFARRAYS__+__NETOFARRAYS__+__NEPDARRAYS__+__NMTDARRAYS__+__NFGTARRAYS__+__NEZTARRAYS__
#endif
};
class StMuArrays {
 public:
 StMuArrays();
 #ifndef __CINT__
    ///< names of the TBranches in the TTree/File 
    static const char*         arrayNames    [__NALLARRAYS__    ];
#ifndef __NO_STRANGE_MUDST__
    static const char** strangeArrayNames; //[__NSTRANGEARRAYS__]
#endif
    static const char** mcArrayNames; //[__NMCARRAYS__]
    static const char**      emcArrayNames;//[__NEMCARRAYS__    ]
    static const char**      pmdArrayNames;//[__NPMDARRAYS__    ]
    static const char**      fmsArrayNames;//[__NFMSARRAYS__    ]
    static const char**      tofArrayNames;//[__NTOFARRAYS__    ]
    static const char**     btofArrayNames;//[__NBTOFARRAYS__   ] // dongx
    static const char**     etofArrayNames;//[__NETOFARRAYS__   ] // jdb
    static const char**     epdArrayNames; //[__NEPDARRAYS__    ] // MALisa
    static const char**      mtdArrayNames;//[__NMTDARRAYS__    ]
    static const char**      fgtArrayNames;//[__NFGTARRAYS__    ]
    static const char**      eztArrayNames;//[__NEZARRAYS__     ]
    
///< names of the classes, the TClonesArrays are arrays of this type
    static const char*   arrayTypes          [__NALLARRAYS__    ];
#ifndef __NO_STRANGE_MUDST__
    static const char**  strangeArrayTypes;//[__NSTRANGEARRAYS__]
#endif
    static const char**  mcArrayTypes;//[__NMCARRAYS__]
    static const char**  emcArrayTypes;//    [__NEMCARRAYS__    ]
    static const char**  pmdArrayTypes;//    [__NPMDARRAYS__    ]
    static const char**  fmsArrayTypes;//    [__NFMSARRAYS__    ]
    static const char**  tofArrayTypes;//    [__NTOFARRAYS__    ]
    static const char**  btofArrayTypes;//   [__NBTOFARRAYS__   ]  // dongx
    static const char**  etofArrayTypes;//   [__NETOFARRAYS__   ]  // jdb
    static const char**  epdArrayTypes;//    [__NEPDARRAYS__    ]  // MALisa
    static const char**  mtdArrayTypes;//    [__NMTDARRAYS__    ]
    static const char**  fgtArrayTypes;//    [__NFGTARRAYS__    ]
    static const char**  eztArrayTypes;//    [__NEZARRAYS__     ]
    
///< maximum sizes of the TClonesArrays
    static int           arraySizes    [__NALLARRAYS__    ];
#ifndef __NO_STRANGE_MUDST__
    static int*   strangeArraySizes;// [__NSTRANGEARRAYS__]
#endif
    static int*   mcArraySizes;// [__NMCARRAYS__]
    static int*       emcArraySizes;// [__NEMCARRAYS__    ]
    static int*       pmdArraySizes;// [__NPMDARRAYS__    ]
    static int*       fmsArraySizes;// [__NFMSARRAYS__    ]
    static int*       tofArraySizes;// [__NTOFARRAYS__    ]
    static int*      btofArraySizes;// [__NBTOFARRAYS__   ]  // dongx
    static int*      etofArraySizes;// [__NETOFARRAYS__   ]  // jdb
    static int*       epdArraySizes;// [__NEPDARRAYS__    ]  // MALisa
    static int*       mtdArraySizes;// [__NMTDARRAYS__     ]
    static int*       fgtArraySizes;// [__NFGTARRAYS__    ]
    static int*       eztArraySizes;// [__NEZARRAYS__     ]
    
///< number of entries in current event, currently not used
    static int        arrayCounters    [__NALLARRAYS__    ];
#ifndef __NO_STRANGE_MUDST__
    static int*strangeArrayCounters;// [__NSTRANGEARRAYS__]
#endif
    static int*mcArrayCounters;// [__NMCARRAYS__]
    static int*    emcArrayCounters;// [__NEMCARRAYS__    ]
    static int*    pmdArrayCounters;// [__NPMDARRAYS__    ]
    static int*    fmsArrayCounters;// [__NFMSARRAYS__    ]
    static int*    tofArrayCounters;// [__NTOFARRAYS__    ]
    static int*   btofArrayCounters;// [__NBTOFARRAYS__   ]  // dongx
    static int*   etofArrayCounters;// [__NETOFARRAYS__   ]  // jdb
    static int*    epdArrayCounters;// [__NEPDARRAYS__    ]  // MALisa
    static int*    mtdArrayCounters;// [__NEZARRAYS__    ]
    static int*    fgtArrayCounters;// [__NFGTARRAYS__    ]
    static int*    eztArrayCounters;// [__NEZARRAYS__    ]
#endif    
    ClassDef(StMuArrays,0)
};

#endif

/***************************************************************************
 *
 * $Log: StMuArrays.h,v $
 * Revision 1.35  2019/02/21 13:32:54  jdb
 * Inclusion of ETOF MuDst code. This code adds support for the full set of ETOF data which includes EtofDigi, EtofHit, EtofHeader. The code essentially copies similar structures from StEvent and additionally rebuilds the maps between Digis and Hits. Accessor methods are added based on the pattern from BTOF to provide access to data at various levels. The code for accessing the PID traits provided by ETOF is also provided
 *
 * Revision 1.34  2018/02/27 04:11:17  jdb
 * Added EPD types
 *
 * Revision 1.33  2015/11/06 17:47:16  jdb
 * Added StMuFmsInfo.{h,cxx} as a new branch for storing event-by-event FMS paramters
 *
 * Revision 1.32  2015/08/28 18:36:03  jdb
 * Added Akios FMS codes
 *
 * Revision 1.31  2013/07/23 11:02:59  jeromel
 * Undo changes (KF and other)
 *
 * Revision 1.29  2013/04/10 19:28:35  jeromel
 * Step back to 04/04 version (van aware) - previous changes may be recoverred
 *
 * Revision 1.27  2013/01/08 22:57:33  sangalin
 * Merged in FGT changes allowing for a variable number of timebins to be read out for each strip.
 *
 * Revision 1.26  2012/11/15 22:26:13  sangalin
 * Added the FGT. Fixed bugs in array offsets for the MTD.
 *
 * Revision 1.25  2012/09/28 22:38:05  tone421
 * Changed array stucture of MTD upon request of the TOF group. MTD arrays now on top level, rather than within __NARRAYS__
 *
 * Revision 1.23  2011/10/17 00:19:13  fisyak
 * Active handing of IdTruth
 *
 * Revision 1.22  2011/05/04 19:51:32  tone421
 * Added MTD infomation
 *
 * Revision 1.21  2011/04/08 01:25:50  fisyak
 * Add branches for MC track and vertex information, add IdTruth to  tracks and vertices, reserve a possiblity to remove Strange MuDst
 *
 * Revision 1.20  2010/05/26 04:25:50  tone421
 * Added StTriggerData arrays in muevent and fixed an issue with PMD arrays being read....
 *
 * Revision 1.19  2010/01/25 03:57:39  tone421
 * Added FMS and Roman pot arrays
 *
 * Revision 1.18  2009/02/20 16:37:44  tone421
 * *** empty log message ***
 *
 * Revision 1.16  2008/03/19 14:51:03  fisyak
 * Add two clone arrays for global and primary track covariance matrices, remove mSigmaDcaD and mSigmaDcaZ
 *
 * Revision 1.15  2005/07/15 21:45:08  mvl
 * Added support for multiple primary vertices (StMuPrimaryVertex). Track Dcas are now calculated with repect to the first vertex in the list (highest rank), but another vertex number can be specified. Tarcks also store the index of the vertex they belong to (StMuTrack::vertexIndex())
 *
 * Revision 1.14  2005/04/12 21:56:29  mvl
 * Changes by Xin Dong for year-5 TOF data format: extra TClonesArray and routines to fill it from StEvent (StTofRawData).
 *
 * Revision 1.13  2004/11/29 15:53:22  mvl
 * Additions by Jan for Fpd ezTree
 *
 * Revision 1.12  2004/10/28 00:11:33  mvl
 * Added stuff to support ezTree mode of MuDstMaker.
 * This is a special mode for fast-online processing of fast-detector data.
 *
 * Revision 1.11  2004/10/19 01:43:05  mvl
 * Changes for splitting Emc and Pmd collections
 *
 * Revision 1.10  2004/07/27 02:35:23  mvl
 * Added access methods for Strangeness Monte-Carlo arrays
 *
 * Revision 1.9  2004/05/04 00:10:28  perev
 * Cleanup
 *
 * Revision 1.8  2004/04/26 00:13:28  perev
 * Cleanup+simplification
 *
 * Revision 1.7  2004/04/09 22:06:35  subhasis
 * after tof createevent fix by Xin
 *
 * Revision 1.6  2004/04/09 03:36:14  jeromel
 * Removed TOF support entirely for now as we need a working version ... Will
 * revisit later.
 *
 * Revision 1.5  2004/04/02 03:24:53  jeromel
 * Changes implements PMD and TOF.  TOF is clearly incomplete.
 *
 * Revision 1.4  2003/01/09 18:59:45  laue
 * initial check in of new EMC classes and the changes required
 *
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
