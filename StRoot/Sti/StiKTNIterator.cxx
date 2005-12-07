//StiKTNIterator.cxx

//Sti
#include "StiKTNIterator.h"
static const StiKTNIterator mgEnd;
const StiKTNForwardIterator& StiKTNForwardIterator::end()
{
    return (StiKTNForwardIterator&)mgEnd;
}

const StiKTNIterator& StiKTNIterator::end()
{
    return mgEnd;
}

const StiKTNIterator& StiKTNIterator::rend()
{
    return mgEnd;
}

