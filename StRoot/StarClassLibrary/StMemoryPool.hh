/***************************************************************************
 *
 * $Id: StMemoryPool.hh,v 1.2 1999/11/18 17:55:46 ullrich Exp $
 *
 * Author: Thomas Ullrich, Nov 1999
 ***************************************************************************
 *
 * Description:
 *
 * Idea taken from:
 * B. Stroustrup, The C++ Programming Language, 3ed edition,
 * Chapter 19 (page 570)
 *
 * Should be used if a large number of small objects has to be
 * frequently allocated from heap. It is much more efficient and
 * faster than the standard new/delete operators.
 * Add to the referring class X the following lines:
 *
 * class X {
 * public:
 *    void* operator new(size_t) { return mPool.alloc(); }
 *    void  operator delete(void* p)  { mPool.free(p); }
 *    ...
 * privat:
 *    static StMemoryPool mPool;
 *    ...
 * }
 *
 * StMemoryPool X::mPool(sizeof(X));
 *
 * Note that the class is optimized for speed and compactness
 * not for readability. Only single objects may be created,
 * i.e, new X[100] will not work; however, in these cases the
 * default operators new/delete will be called.
 *
 ***************************************************************************
 *
 * $Log: StMemoryPool.hh,v $
 * Revision 1.2  1999/11/18 17:55:46  ullrich
 * Corrected reference to Stroustrup.
 *
 * Revision 1.1  1999/11/09 19:29:13  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef ST_MEMORY_POOL_HH
#define ST_MEMORY_POOL_HH


class StMemoryPool {
public:
    StMemoryPool(unsigned int n);
    ~StMemoryPool();
                                               
    void* alloc();
    void  free(void*);
                                               
private:
    StMemoryPool();
    StMemoryPool(StMemoryPool&);
    void operator= (StMemoryPool&);
    void grow();
                                               
    struct Link { Link* next; };
    struct Chunk {
        enum {size = 16*1024-16};
        Chunk* next;
        char mem[size];
    };
    Chunk              *chunks;
    Link*              head;
    const unsigned int esize;
};
                                           
                                              
inline void*
StMemoryPool::alloc()
{
   if (head == 0) grow();
   Link* p = head;
   head = p->next;
   return p;
}

inline void
StMemoryPool::free(void* b)
{
   if (b != 0) {
      Link* p = static_cast<Link*>(b);
      p->next = head;
      head = p;
   }
}
#endif
