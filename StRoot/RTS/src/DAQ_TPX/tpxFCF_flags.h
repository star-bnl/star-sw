#ifndef _TPX_FCF_FLAGS_H_
#define _TPX_FCF_FLAGS_H_

// flag definitions - NEVER CHANGE
#define FCF_ONEPAD              1

#define FCF_DOUBLE_PAD          2	// offline: merged
#define FCF_MERGED		2

#define FCF_DOUBLE_T            4

#define FCF_FALLING             8	// offline: charge too big!
#define FCF_BIG_CHARGE		8

#define FCF_ROW_EDGE            16      // 0x10 touched end of row
#define FCF_BROKEN_EDGE         32      // 0x20 touches one of the mezzanine edges
#define FCF_DEAD_EDGE           64      // 0x40 touches a dead pad
#define FCF_IN_DOUBLE           128	// 0x80 one should use the floating point in the union
#define FCF_CHOPPED		256	// 0x100 cluster is chopped from its neighbour: OFFLINE use only

#endif
