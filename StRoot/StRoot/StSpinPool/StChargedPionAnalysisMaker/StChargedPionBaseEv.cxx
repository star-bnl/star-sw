// $Id: StChargedPionBaseEv.cxx,v 1.2 2010/09/02 02:46:34 perev Exp $

#include "StChargedPionBaseEv.h"

#include <utility>
using std::make_pair;

ClassImp(StChargedPionBaseEv)

// static member initialization
static std::pair<unsigned int, unsigned int> a[] = {
    make_pair( 96011, 0x00000001),
    make_pair( 96201, 0x00000002),
    make_pair( 96211, 0x00000004),
    make_pair( 96221, 0x00000008),
    make_pair( 96233, 0x00000010),
    make_pair(117001, 0x00000020),
    make_pair(137213, 0x00000040),
    make_pair(137221, 0x00000080),
    make_pair(137222, 0x00000100),
    make_pair(137585, 0x00000200),
    make_pair(137611, 0x00000400),
    make_pair(137622, 0x00000800),
    make_pair(106011, 0x00001000),
    make_pair(106201, 0x00002000),
    make_pair(106211, 0x00004000),
    make_pair(106221, 0x00008000),
    make_pair(106233, 0x00010000),
    make_pair(117402, 0x00020000),
    make_pair(117211, 0x00040000),
    make_pair(117212, 0x00080000),
    make_pair(137262, 0x00100000),
    make_pair(137271, 0x00200000),
    make_pair(137272, 0x00400000),
    make_pair(137273, 0x00800000),
    make_pair(137641, 0x01000000),
    make_pair(137652, 0x02000000),
    // reuse some bits for transverse and first longitudinal running
    make_pair(127212, 0x00080000),
    make_pair(127213, 0x00000040),
    make_pair(117221, 0x00000080),
    make_pair(127221, 0x00000080),
    make_pair(117585, 0x00000200),
    make_pair(127585, 0x00000200),
    make_pair(117611, 0x00000400),
    make_pair(127611, 0x00000400),
    make_pair(117622, 0x00000800),
    make_pair(127622, 0x00000800),
    make_pair(117262, 0x00100000),
    make_pair(127262, 0x00100000),
    make_pair(117271, 0x00200000),
    make_pair(127271, 0x00200000),
    make_pair(117641, 0x01000000),
    make_pair(127641, 0x01000000),
    make_pair(117652, 0x02000000),
    make_pair(127652, 0x02000000)
};

map<unsigned int, unsigned int> 
StChargedPionBaseEv::mTriggerLookup(a, a + sizeof(a)/sizeof(a[0]) );

unsigned int StChargedPionBaseEv::triggerBit(unsigned int trigId) {
    map<unsigned int, unsigned int>::const_iterator it = mTriggerLookup.find(trigId);
    if(it==mTriggerLookup.end()) return 0;
    return it->second;
}

/*****************************************************************************
 * $Log: StChargedPionBaseEv.cxx,v $
 * Revision 1.2  2010/09/02 02:46:34  perev
 * Move variable a[] to static to avoid clash, bug #1993
 *
 * Revision 1.1  2008/07/17 17:06:29  kocolosk
 * big-bang integration StChargedPionMcEvent framework
 *
 *****************************************************************************/
