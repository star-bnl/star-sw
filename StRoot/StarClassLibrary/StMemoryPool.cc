/***************************************************************************
 *
 * $Id: StMemoryPool.cc,v 1.1 1999/11/09 19:29:11 ullrich Exp $
 *
 * Author: Thomas Ullrich, Nov 1999
 ***************************************************************************
 *
 * Description: (see header file)
 *
 ***************************************************************************
 *
 * $Log: StMemoryPool.cc,v $
 * Revision 1.1  1999/11/09 19:29:11  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StMemoryPool.hh"


StMemoryPool::StMemoryPool(unsigned int sz)
    : esize(sz<sizeof(Link*) ? sizeof(Link*) : sz)
{
    head   = 0;
    chunks = 0;
}

StMemoryPool::~StMemoryPool()
{
    Chunk* n = chunks;
    while (n) {
        Chunk* p = n;
        n = n->next;
        delete p;
    }
}

void
StMemoryPool::grow()
{
    Chunk* n = new Chunk;
    n->next  = chunks;
    chunks   = n;
    
    const int nelem = Chunk::size/esize;
    char* start = n->mem;
    char* last  = &start[(nelem-1)*esize];
    
    for (char* p = start; p<last; p+=esize)
        reinterpret_cast<Link*>(p)->next = reinterpret_cast<Link*>(p+esize);
   reinterpret_cast<Link*>(last)->next = 0;
   head = reinterpret_cast<Link*>(start);
}
