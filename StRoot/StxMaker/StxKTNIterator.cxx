//StxKTNIterator.cxx

//Stx
#include "StxKTNIterator.h"
static const StxKTNIterator mgEnd;
const StxKTNForwardIterator& StxKTNForwardIterator::end()
{
    return (StxKTNForwardIterator&)mgEnd;
}

const StxKTNIterator& StxKTNIterator::end()
{
    return mgEnd;
}

const StxKTNIterator& StxKTNIterator::rend()
{
    return mgEnd;
}

