// implementation of singly linked lists
// especially THitList and TTrackList
// methods:
//	first()		- return slink of first item
//	inf(slink)	- retrun Contents of slink
//	succ(slink)	- return next slink or NULL at end of list
//	append(item)- append item to list
//	tail()		- return contents of last item
//	head()		- return contents of first item
//	size()		- return size of list
//	clear()		- remove all entries (doesnt't delete the entries!)
//	conc(slist)	- append another list

#ifndef L3LIST
#define L3LIST

//#include <stdlib.h>

class slink;
typedef slink* slist_item;

class slink {
public:
  slink* succ;
  void* e;
  slink():succ(0),e(0) {};
  slink(void* a, slink* suc) { e = a; succ = suc; };
//slink* operator++(){ return succ ; } ; 
};

class l3List {
private:
   slink* head;         //head
   slink* tail;         //tail
   slink* current ;     //temporary for looping
   int    count;        //length of List
public:
   l3List():head(0),tail(0),count(0) {};
//*********************************************************
   ~l3List() 
   {
     if (head!=0) {
        register slink* p, *old;
        for(p = head; p; ) {
// get next element before deleting this one
           old = p;
           p = p->succ;
           delete old;
         }
         head=tail=0;
         count=0;
      }
   };
//*********************************************************
   void clear() {
      if (head!=0) {
	 register slink* p, *old;
	 for(p = head; p; ) {
	    // get next element before deleting this one
	    old = p;
	    p = p->succ;
	    delete old;
	 }
	 head=tail=0;
	 count=0;
      }
   }

//*********************************************************
   void* first() { 
      current = head ;
      if ( head ) return head->e;
      else return 0 ;
   };
//*********************************************************
   void* next() { 
      current = current->succ ;
      if ( current == 0 ) return 0 ;
      return current->e;
   };
   //
   slink* append(void* a)
   { count++;
      if (tail) tail = tail->succ = new slink(a,0); 
      else   tail = head = new slink(a,0); 
      return tail;
   };
   //
//*********************************************************
   void remove(slink* item)
   {
      slink* temp;
      count--;
// search succ(el) == item
      if (item == head ) {
         head = item->succ;
         if (head == 0) {
            tail = 0;
            count = 0;
         }
         delete item;
      }
      else
         for(temp = head; temp != 0; temp = temp->succ)
         if (temp->succ == item) {
// found, relink and remove item
            temp->succ = item->succ;
            if (temp->succ == 0) tail = temp;
            delete item;
            break;
       }
   };
   int size() {return count;};

};

#endif
