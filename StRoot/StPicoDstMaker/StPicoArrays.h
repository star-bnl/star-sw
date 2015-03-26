#ifndef StPicoArrays_hh
#define StPicoArrays_hh

enum picoDstTypes {picoEvent=0, picoTrack, picoEmcTrigger, picoMtdTrigger, 
		   picoBTOWHit, picoBTofHit, picoMtdHit,
		   picoEmcPidTraits, picoBTofPidTraits, picoMtdPidTraits};
enum v0Types {picoV0Ks=0, picoV0L, picoV0Lbar};
enum NPICOARRAYS {
__NPICOARRAYS__ = 10,
__NPICOV0ARRAYS__ = 3,
__NALLPICOARRAYS__ = __NPICOARRAYS__+__NPICOV0ARRAYS__
};

class StPicoArrays {
  public:
  StPicoArrays();
///< names of the TBranches in the TTree/File 
  static const char*   picoArrayNames[__NALLPICOARRAYS__];
  static const char**  picoV0ArrayNames; //[__NPICOV0ARRAYS__]

///< names of the classes, the TClonesArrays are arrays of this type
  static const char*   picoArrayTypes[__NALLPICOARRAYS__    ];
  static const char**  picoV0ArrayTypes;//[__NPICOV0ARRAYS__]

///< maximum sizes of the TClonesArrays
  static int           picoArraySizes[__NALLPICOARRAYS__    ];
  static int*          picoV0ArraySizes;// [__NPICOV0ARRAYS__]

///< number of entries in current event, currently not used
  static int           picoArrayCounters[__NALLPICOARRAYS__    ];
  static int*          picoV0ArrayCounters;// [__NPICOV0ARRAYS__] 

};

#endif
