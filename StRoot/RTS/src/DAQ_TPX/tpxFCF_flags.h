#ifndef _TPX_FCF_FLAGS_H_
#define _TPX_FCF_FLAGS_H_

// flag definitions - NEVER CHANGE
#define FCF_ONEPAD              1

#define FCF_DOUBLE_PAD          2	// deconvoluted in the pad direction
#define FCF_MERGED		2	// deconvoluted cluser

#define FCF_DOUBLE_T            4	// deconvoluted in the time direction

#define FCF_FALLING             8	// internal FCF marker...
#define FCF_BIG_CHARGE		8	// charge too big

#define FCF_ROW_EDGE            16      // 0x10 touched end of row
#define FCF_BROKEN_EDGE         32      // 0x20 touches one of the mezzanine/RDO edges i.e. row8
#define FCF_DEAD_EDGE           64      // 0x40 touches a dead pad
#define FCF_IN_DOUBLE           128	// 0x80 one should use the floating point in the union

// used locally for FCF via the gains file
#define FCF_KILLED_PAD		0x0100
#define FCF_NEED_PAD		0x8000

// below are used in Offline only
#define FCF_CHOPPED		256	// 0x100 cluster is chopped from its neighbour: OFFLINE use only

#define FCF_SANITY		512	// 0x200 cluster extents not sane

#endif
