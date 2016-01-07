#ifndef ABSLIST_H
#define ABSLIST_H
/*
Very simple and reliable doubly-linked list class with clear interface.
The STL library is too hypertrophied regarding containers.
The statements based on iterators seems to be not very clear.
There is no clear semantic sense in the most of them.
The simplest list and simplest operations with it seems to appear
much more pronounced. The list consists of head node and regular nodes.
The head node is a class keeping pointer to the first and to the last
regular node, and the total number of elements.
Each regular node is a class keeping pointers to head node,
to previous node, and to the next node, and also the single "target" object.
The node can be added and removed usually through the interface of
the head class. Nodes can be iterated directly in both directions
with looks like this:

  AbsListNode<T>* an=NULL;  // AbsListNode<T> is the name of node class
  while( (an = list.get_next_node(an)) != NULL)
  {
   // do anything with valid an
   // for example, access to element is an->el
  }

The name of list head class is AbsList<T>, the Abstract List.
Copying the list results in copying all elements (through the operator=).
Deletion of list results in deletion of elements.


Copyright (c) 2003 I. B. Smirnov

The file can be used, copied, modified, and distributed
according to the terms of GNU Lesser General Public License version 2.1
as published by the Free Software Foundation,
and provided that the above copyright notice, this permission notice,
and notices about any modifications of the original text
appear in all copies and in supporting documentation.
The file is provided "as is" without express or implied warranty.
*/

#ifndef DONT_USE_ABSPTR  // in oder to supply some programs without
                         // smart pointers
#include "wcpplib/safetl/AbsPtr.h"
#else
// Some necessary repetitions from AnsPtr.h:
// See AnsPtr.h for details regarding the following:
#define PILF_CONST const
#define PILF_MUTABLE mutable
//#define PILF_CONST
//#define PILF_MUTABLE

#endif

#ifdef USE_REPLACE_ALLOC
#include "wcpplib/safetl/ReplaceAlloc.h"
#endif

//#define DEBUG_ABSLIST  // make some print

template <class T> class AbsList;
template <class T> class AbsListNode;

template <class T>
AbsListNode<T>* glob_insert_before(AbsList<T>&, AbsListNode<T>*, const T& fel);
template <class T>
AbsListNode<T>* glob_insert_after(AbsList<T>&, AbsListNode<T>*, const T& fel);

template <class T> void glob_pilfer(AbsList<T>&, PILF_CONST AbsList<T>&);

#ifndef DONT_USE_ABSPTR
template <class T>
class AbsListNode : public RegPassivePtr {
#else
template <class T>
class AbsListNode {
#endif
 private:
  AbsList<T>* head_node;
  AbsListNode<T>* prev_node;
  AbsListNode<T>* next_node;
  // preservation from copying
  AbsListNode(const AbsListNode<T>& f)
#ifndef DONT_USE_ABSPTR
      : RegPassivePtr()
#endif
        {
    head_node = f.head_node;
    prev_node = f.prev_node;
    next_node = f.next_node;
  }
  AbsListNode<T>& operator=(const AbsListNode<T>& /*f*/) { return *this; }

 public:
  T el;
  inline AbsList<T>* get_head_node(void) const { return head_node; }
  inline AbsListNode<T>* get_prev_node(void) const { return prev_node; }
  inline AbsListNode<T>* get_next_node(void) const { return next_node; }

  // Attention: the following constructor cannot be called directly.
  // It is assumed to be called only from ListNode functions.
 private:
  // The following constructor cannot be called directly.
  // It is assumed to be called only from ListNode functions.
  AbsListNode(AbsList<T>* fhead_node, AbsListNode<T>* fprev_node,
              AbsListNode<T>* fnext_node, const T& fel);

 public:
  void exclude(void);  // exclude this node from list, but don't erase it
  // Then don't forget to delete it (from heap memory, where it normally
  // resides).
  // Really this deletes pointer to head_node, reducing qel in it.
  // It also deletes pointers to prev_node and next_node
  // And ties previous and next elements to each other.
  // There is no the opposite operation - include. There seems to be
  // no necessity in it, since in most of the cases
  // the element itself is included and the node is created dynamically
  // in insert_before, insert_after, etc. functions of AbsList<T>.

  virtual ~AbsListNode()  // excludes from list
      {
    exclude();
    /*
      if(head_node != NULL)
	mcout<<"~AbsListNode(): error: head_node != NULL\n";
      if(prev_node != NULL)
	mcout<<"~AbsListNode(): error: prev_node != NULL\n";
      if(next_node != NULL)
	mcout<<"~AbsListNode(): error: next_node != NULL\n";
      */
  }
  friend AbsListNode<T>* glob_insert_before<>(AbsList<T>&, AbsListNode<T>*,
                                              const T& fel);
  friend AbsListNode<T>* glob_insert_after<>(AbsList<T>&, AbsListNode<T>*,
                                             const T& fel);
  friend void glob_pilfer<>(AbsList<T>&, PILF_CONST AbsList<T>&);

// The following is more logical, but not compiled at some newer compilers,
// alas!
//friend AbsListNode<T>*  AbsList<T>::insert_before(AbsListNode<T>*, const T&
//fel);
//friend AbsListNode<T>*  AbsList<T>::insert_after(AbsListNode<T>*, const T&
//fel);

//friend void AbsList<T>::pilfer(PILF_CONST AbsList<T>&);
#ifndef DONT_USE_ABSPTR
  macro_copy_total(AbsListNode);
#endif
#ifdef USE_REPLACE_ALLOC
  macro_alloc
#endif
};

#ifndef DONT_USE_ABSPTR
template <class T>
class AbsList : public RegPassivePtr
#else
                template <class T>
                class AbsList
#endif
                {
 private:
  PILF_MUTABLE AbsListNode<T>* first_node;
  PILF_MUTABLE AbsListNode<T>* last_node;
  PILF_MUTABLE long qel;

 public:
  AbsList<T>(void) : first_node(NULL), last_node(NULL), qel(0) { ; }
  AbsList<T>(const T& fel) : first_node(NULL), last_node(NULL), qel(0) {
    insert_after(NULL, fel);
  }

  inline AbsListNode<T>* get_first_node(void) const { return first_node; }
  inline AbsListNode<T>* get_last_node(void) const { return last_node; }
  inline AbsListNode<T>* get_next_node(AbsListNode<T>* an) const {
    if (an == NULL)
      return first_node;
    else
      return an->get_next_node();
  }
  inline AbsListNode<T>* get_prev_node(AbsListNode<T>* an) const {
    if (an == NULL)
      return last_node;
    else
      return an->get_prev_node();
  }
  //This function defined above allows simple loops like
  // AbsListNode<T>* an=NULL;
  // while( (an = list.get_next_node(an)) != NULL)
  // { do anything with valid an
  // }

  // two following functions returns the address of the new node
  inline AbsListNode<T>* insert_before(AbsListNode<T>*, const T& fel);
  inline AbsListNode<T>* insert_after(AbsListNode<T>* an, const T& fel);

  // The two following functions do the same as those above and actually called
  // from
  // above. The following are introduced in order to solve some restrictions
  // imposed by formal rules and compilers.
  friend AbsListNode<T>* glob_insert_before<>(AbsList<T>&, AbsListNode<T>*,
                                              const T& fel);
  friend AbsListNode<T>* glob_insert_after<>(AbsList<T>&, AbsListNode<T>*,
                                             const T& fel);
  friend void glob_pilfer<>(AbsList<T>&, PILF_CONST AbsList<T>&);

  // if an == NULL, then only if qel = 0, the insert is made at first place
  // otherwise it is considered as error.
  AbsListNode<T>* push_front(const T& fel) {
    return insert_before(first_node, fel);
  }
  AbsListNode<T>* prepend(const T& fel) { return push_front(fel); }
  AbsListNode<T>* push_back(const T& fel) {
    return insert_after(last_node, fel);
  }
  AbsListNode<T>* append(const T& fel) { return push_back(fel); }

  void erase(AbsListNode<T>* an);  // function to exclude the node
  inline void remove(AbsListNode<T>* an) {
    erase(an);
  }                             // function to exclude the node
  void remove(const T& t);      // function to exclude the first met node
                                // with element equal to t
  void remove_all(const T& t);  // function to exclude all the nodes
                                // with element equal ro t

  inline void clear(void) {
    while (first_node != NULL) {  //mcout<<"erasing first node\n";
      erase(first_node);
    }
    ;
  }
  //inline       AbsListNode& operator[](long n) const ;
  //inline const AbsListNode& operator[](long n) const ;
  long get_qel(void) const { return qel; }
  AbsList(const AbsList<T>& al)
      :
#ifndef DONT_USE_ABSPTR
        RegPassivePtr(),
#endif
        first_node(NULL),
        last_node(NULL),
        qel(0) {
#ifdef DEBUG_ABSLIST
    mcout << "AbsList<T>(const AbsList<T>& al) is working\n";
#endif
    *this = al;
  }
  AbsList(PILF_CONST AbsList<T>& al, Pilfer)
      : first_node(NULL), last_node(NULL), qel(0) {
#ifdef DEBUG_ABSLIST
    mcout << "AbsList<T>(PILF_MUTABLE AbsList<T>& al, Pilfer) is working\n";
#endif
    pilfer(al);
  }
  void pilfer(PILF_CONST AbsList<T>& al);
  AbsList<T>& operator=(const AbsList<T>& al);
  virtual ~AbsList() { clear(); }
  friend void AbsListNode<T>::exclude(void);
//friend template<class T> void AbsListNode<T>::exclude(void);
#ifndef DONT_USE_ABSPTR
  macro_copy_total(AbsList);
#endif
#ifdef USE_REPLACE_ALLOC
  macro_alloc
#endif
};

template <class T>
AbsListNode<T>::AbsListNode(AbsList<T>* fhead_node, AbsListNode<T>* fprev_node,
                            AbsListNode<T>* fnext_node, const T& fel)
    : head_node(fhead_node),
      prev_node(fprev_node),
      next_node(fnext_node),
      el(fel) {
  //mcout<<"Constructor\n";
  if (fprev_node != NULL) {
    if (fprev_node->next_node != fnext_node) {  // for debug
      mcerr << "template<class T> AbsListNode<T>::AbsListNode<T>\n";
      mcerr << "fprev_node->next_node !=  fnext_node\n";
      Iprint2n(mcerr, fprev_node->next_node, fnext_node);
      spexit(mcerr);
    }
    fprev_node->next_node = this;
  }
  if (fnext_node != NULL) {
    if (fnext_node->prev_node != fprev_node) {  // for debug
      mcerr << "template<class T> AbsListNode<T>::AbsListNode<T>\n";
      mcerr << "fnext_node->prev_node !=  fprev_node\n";
      Iprint2n(mcerr, fnext_node->prev_node, fprev_node);
      spexit(mcerr);
    }
    fnext_node->prev_node = this;
  }
}

template <class T>
void AbsListNode<T>::exclude(void)
    // exclude this node from list, but don't erase it
    // Then don't forget to delete it
    {
  if (prev_node != NULL) prev_node->next_node = next_node;
  if (next_node != NULL) next_node->prev_node = prev_node;
  if (head_node != NULL)  // normally cannot be
      {
    if (head_node->qel <= 0) {
      mcerr << "ERROR in template<class T> void AbsListNode<T>::exclude()\n";
      mcerr << "head_node->qel <= 0, contradicts to request to exclude\n";
      Iprintn(mcerr, head_node->qel);
      spexit(mcerr);
    }
    if (head_node->first_node == this) head_node->first_node = next_node;
    if (head_node->last_node == this) head_node->last_node = prev_node;
    head_node->qel--;
  }
  head_node = NULL;
  prev_node = NULL;
  next_node = NULL;
}

template <class T>
AbsListNode<T>* glob_insert_before(AbsList<T>& al, AbsListNode<T>* aln,
                                   const T& fel) {
  AbsListNode<T>* new_aln = NULL;
  if (aln == NULL)  // means to insert at the first place
      {
    if (al.qel == 0)  // no elements, then the addresses should be empty as well
        {
      if (al.first_node != NULL || al.last_node != NULL) {
        mcerr << "ERROR in template<class T> AbsListNode<T>*  "
                 "glob_insert_before\n";
        mcerr << "qel == 0, but first_node != NULL || last_node != NULL \n";
        Iprintn(mcerr, al.first_node);
        Iprintn(mcerr, al.last_node);
        spexit(mcerr);
      }
      al.first_node = new AbsListNode<T>(&al, NULL, NULL, fel);
      al.last_node = al.first_node;
      new_aln = al.first_node;
      al.qel = 1;
    } else {
      mcerr
          << "ERROR in template<class T> AbsListNode<T>*  glob_insert_before\n";
      mcerr << " aln is zero, but the list is not empty\n";
      spexit(mcerr);
    }
  } else {
    if (aln->get_head_node() != &al) {
      mcerr
          << "ERROR in template<class T> AbsListNode<T>*  glob_insert_before\n";
      mcerr << "aln->get_head_node() != this\n";
      Iprint2n(mcerr, aln->get_head_node(), al);
      spexit(mcerr);
    } else {
      if (al.qel <= 0) {
        mcerr << "ERROR in template<class T> AbsListNode<T>*  "
                 "glob_insert_before\n";
        mcerr << "qel <= 0 but aln is not empty\n";
        Iprintn(mcerr, al.qel);
        spexit(mcerr);
      } else {
        new_aln = new AbsListNode<T>(&al, aln->get_prev_node(), aln, fel);
        if (aln == al.first_node) {
          al.first_node = new_aln;
        }
        al.qel++;
      }
    }
  }
  return new_aln;
}

template <class T>
AbsListNode<T>* AbsList<T>::insert_before(AbsListNode<T>* aln, const T& fel) {
  return glob_insert_before<T>(*this, aln, fel);
}
/*
template<class T> AbsListNode<T>* AbsList<T>::insert_before
(AbsListNode<T>* aln, const T& fel)
{
  AbsListNode<T>* new_aln = NULL;
  if(aln == NULL)  // means to insert at the first place
  {
    if(qel == 0)  // no elements, then the addresses should be empty as well
    {
      if(first_node != NULL || last_node != NULL)
      {
	mcerr<<"ERROR in template<class T> void AbsList<T>::insert_before\n";
	mcerr<<"qel == 0, but first_node != NULL || last_node != NULL \n";
	Iprintn(mcerr, first_node);
	Iprintn(mcerr, last_node);
	spexit(mcerr);
      }
      first_node = new AbsListNode<T>(this, NULL, NULL, fel);
      last_node = first_node;
	  new_aln = first_node;
      qel = 1;
    }
    else
    {
      mcerr<<"ERROR in template<class T> void AbsList<T>::insert_before\n";
      mcerr<<" aln is zero, but the list is not empty\n";
      spexit(mcerr);
    }
  }
  else
  {
    if(aln->get_head_node() != this)
    {
      mcerr<<"ERROR in template<class T> void AbsList<T>::insert_before\n";
      mcerr<<"aln->get_head_node() != this\n";
      Iprint2n(mcerr, aln->get_head_node(),  this);
      spexit(mcerr);
    }
    else
    {
      if(qel <= 0)
      {
	mcerr<<"ERROR in template<class T> void AbsList<T>::insert_before\n";
	mcerr<<"qel <= 0 but aln is not empty\n";
	Iprintn(mcerr, qel);
	spexit(mcerr);
      }
      else
      {
	new_aln = new AbsListNode<T>
	  (this, aln->get_prev_node(), aln, fel);
	if(aln == first_node)
	{
	  first_node = new_aln;
	}
	qel++;
      }
    }
  }
  return new_aln;
}
*/

template <class T>
AbsListNode<T>* glob_insert_after(AbsList<T>& al, AbsListNode<T>* aln,
                                  const T& fel) {
  AbsListNode<T>* new_aln = NULL;
  if (aln == NULL)  // no node after which it needs to insert
      {
    if (al.qel == 0)  // empty list, OK
        {
      al.first_node = new AbsListNode<T>(&al, NULL, NULL, fel);
      al.last_node = al.first_node;
      new_aln = al.first_node;
      al.qel = 1;
    } else {
      mcerr << "ERROR in template<class T> AbsListNode<T>* glob_insert_after\n";
      mcerr << " aln is zero, but the list is not empty\n";
      spexit(mcerr);
    }
  } else {
    if (aln->get_head_node() != &al)  // not our node or list
        {
      mcerr
          << "ERROR in template<class T> AbsListNode<T>*  glob_insert_after\n";
      mcerr << "aln->get_heed_node() != this\n";
      Iprint2n(mcerr, aln->get_head_node(), &al);
      spexit(mcerr);
    } else {
      if (al.qel <= 0)  // all ours but empty list - it is not consistent
          {
        mcerr << "ERROR in template<class T> AbsListNode<T>*  "
                 "glob_insert_after\n";
        mcerr << "qel <= 0 but aln is not empty\n";
        Iprintn(mcerr, al.qel);
        spexit(mcerr);
      } else {
        new_aln = new AbsListNode<T>(&al, aln, aln->get_next_node(), fel);
        //if(new_aln->get_next_node().get() == NULL)
        if (aln == al.last_node) {
          al.last_node = new_aln;
        }
        al.qel++;
      }
    }
  }
  return new_aln;
}

template <class T>
AbsListNode<T>* AbsList<T>::insert_after(AbsListNode<T>* aln, const T& fel) {
  return glob_insert_after(*this, aln, fel);
}

/*
template<class T> AbsListNode<T>*  AbsList<T>::insert_after
(AbsListNode<T>* aln, const T& fel)
{
  AbsListNode<T>* new_aln = NULL;
  if(aln == NULL)  // no node after which it needs to insert
  {
    if(qel == 0) // empty list, OK
    {
      first_node = new AbsListNode<T>(this, NULL, NULL, fel);
      last_node = first_node;
	  new_aln = first_node;
      qel = 1;
    }
    else
    {
      mcerr<<"ERROR in template<class T> void AbsList<T>::insert_after\n";
      mcerr<<" aln is zero, but the list is not empty\n";
      spexit(mcerr);
    }
  }
  else
  {
    if(aln->get_head_node() != this)  // not our node or list
    {
      mcerr<<"ERROR in template<class T> void AbsList<T>::insert_after\n";
      mcerr<<"aln->get_heed_node() != this\n";
      Iprint2n(mcerr, aln->get_head_node(),  this);
      spexit(mcerr);
    }
    else
    {
      if(qel <= 0)  // all ours but empty list - it is not consistent
      {
	mcerr<<"ERROR in template<class T> void AbsList<T>::insert_after\n";
	mcerr<<"qel <= 0 but aln is not empty\n";
	Iprintn(mcerr, qel);
	spexit(mcerr);
      }
      else
      {
	new_aln = new AbsListNode<T>
	  (this, aln, aln->get_next_node(), fel);
	//if(new_aln->get_next_node().get() == NULL)
	if(aln  == last_node)
	{
	  last_node = new_aln;
	}
	qel++;
      }
    }
  }
  return new_aln;
}
*/

template <class T> void AbsList<T>::erase(AbsListNode<T>* aln) {
  if (aln->get_head_node() != this) {
    mcerr << "ERROR in template<class T> void AbsList<T>::erase(...)\n";
    mcerr << "aln->get_heed_node() != this\n";
    spexit(mcerr);
  }
  if (qel <= 0)  // empty list - it is not consistent
      {
    mcerr << "ERROR in template<class T> void AbsList<T>::erase(...)\n";
    mcerr << "qel <= 0 before erase \n";
    Iprintn(mcerr, qel);
    spexit(mcerr);
  }

  //AbsListNode<T>* aaln = aln.get();
  //aln->exclude();  now called from delete
  /*
  if(first_node == aln)
  {
    first_node = aln->get_next_node();
  }
  if(last_node == aln)
  {
    last_node = aln->get_prev_node();
  }
  */
  delete aln;
  //qel--; now in exclude();
  //aaln->get_prev_node()->put_next_node(aaln->get_next_node() );
  //aaln->get_next_node()->put_prev_node(aaln->get_prev_node() );
}

template <class T>
void AbsList<T>::remove(const T& t)
    // function to exclude the node
    {
  AbsListNode<T>* an = NULL;
  while ((an = get_next_node(an)) != NULL) {
    if (an->el == t) {
      remove(an);
      break;
    }
  }
}

template <class T>
void AbsList<T>::remove_all(const T& t)
    // function to exclude the node
    {
  AbsListNode<T>* an = NULL;
  while ((an = get_next_node(an)) != NULL) {
    if (an->el == t) {
      remove(an);
    }
  }
}

/*
template<class T> void AbsListNode<T>::exclude(void)
{
  if(prev_node != NULL)
  {
    prev_node->next_node = next_node;
  }
  if(next_node != NULL)
  {
    next_node->prev_node = prev_node;
  }
}
*/

template <class T> AbsList<T>& AbsList<T>::operator=(const AbsList<T>& f) {
#ifdef DEBUG_ABSLIST
  mcout
      << "AbsList<T>& AbsList<T>::operator=(const AbsList<T>& f) is starting\n";
#endif
  clear();
  AbsListNode<T>* aln = get_last_node();
  AbsListNode<T>* faln = f.get_first_node();
  while (faln != NULL) {
    insert_after(aln, faln->el);
    aln = get_last_node();
    faln = faln->get_next_node();
  }
  return *this;
}

template <class T>
void glob_pilfer(AbsList<T>& this_al, PILF_CONST AbsList<T>& al) {
#ifdef DEBUG_ABSLIST
  mcout << "void glob_pilfer(AbsList<T>& this_al, PILF_CONST AbsList<T>& al) "
           "is starting\n";
#endif
  if (this_al.qel != 0) {
    if (al.qel != 0) {
      mcerr << "ERROR in glob_pilfer(...):\n";
      mcerr << "Both the destination and source lists are not empty\n";
      // For explanations why it is dangerous, see similar function
      // of ActivePtr.
      spexit(mcerr);
    } else {
      this_al.clear();
    }
  }
  this_al.first_node = al.first_node;
  this_al.last_node = al.last_node;
  this_al.qel = al.qel;
  AbsListNode<T>* cur_node = this_al.first_node;
  while (cur_node != NULL) {
    cur_node->head_node = &this_al;
    cur_node = cur_node->get_next_node();
  }
  al.first_node = NULL;
  al.last_node = NULL;
  al.qel = 0;
}

template <class T> void AbsList<T>::pilfer(PILF_CONST AbsList<T>& al) {
  glob_pilfer(*this, al);
}

/*
template<class T>
void AbsList<T>::pilfer(PILF_CONST AbsList<T>& al)
{
#ifdef DEBUG_ABSLIST
  mcout<<"void AbsList<T>::pilfer(PILF_CONST AbsList<T>& al) is starting\n";
#endif
  if(qel != 0)
  {
    if(al.qel != 0)
    {
      mcerr<<"ERROR in AbsList::pilfer(...):\n";
      mcerr<<"Both the destination and source lists are not empty\n";
      // For explanations why it is dangerous, see similar function
      // of ActivePtr.
      spexit(mcerr);
    }
    else { clear(); }
  }
  first_node = al.first_node;
  last_node = al.last_node;
  qel = al.qel;
  AbsListNode<T>* cur_node = first_node;
  while(cur_node != NULL)
  {
    cur_node->head_node = this;
    cur_node = cur_node->get_next_node();
  }
  al.first_node = NULL;
  al.last_node = NULL;
  al.qel = 0;
}
*/

template <class T>
void print_AbsList(std::ostream& file, const AbsList<T>& f, int l) {
  mfunnamep("template<class T> void print_AbsList(std::ostream& file, const "
            "AbsList<T>& f, int l)");
  Ifile << "AbsList<T>: qel=" << f.get_qel() << '\n';
  //f.check();
  long n = 0;
  indn.n += 2;
  AbsListNode<T>* aln = NULL;
  while ((aln = f.get_next_node(aln)) != NULL) {
    Ifile << "n=" << n << " el[n]=" << noindent;
    aln->el.print(file, l);
    n++;
  }
  // Another form of the same loop:
  //AbsListNode<T>* aln = f.get_first_node();
  //for( n=0; n<f.get_qel(); n++)
  //{
  //  Ifile<<"n="<<n<<" el[n]="<<noindent; aln->el.print(file, l);
  //  aln = aln->get_next_node();
  //}
  file << yesindent;
  indn.n -= 2;
}

template <class T> void print_AbsList(std::ostream& file, const AbsList<T>& f) {
  mfunnamep("template<class T> void print_AbsList(std::ostream& file, const "
            "AbsList<T>& f)");
  Ifile << "AbsList<T>: qel=" << f.get_qel() << '\n';
  //f.check();
  long n = 0;
  indn.n += 2;
  AbsListNode<T>* aln = NULL;
  while ((aln = f.get_next_node(aln)) != NULL) {
    Ifile << "n=" << n << " el[n]=" << noindent;
    aln->el.print(file);
    n++;
  }
  //AbsListNode<T>* aln = f.get_first_node();
  //for( n=0; n<f.get_qel(); n++)
  //{
  //  Ifile<<"n="<<n<<" el[n]="<<noindent; aln->el.print(file);
  //  aln = aln->get_next_node();
  //}
  file << yesindent;
  indn.n -= 2;
}

template <class T>
    std::ostream& operator<<(std::ostream& file, const AbsListNode<T>& f) {
  mfunnamep("template<class T> std::ostream& operator<<(std::ostream& file, "
            "const AbsListNode<T>& f)");
  Ifile << "AbsListNode<T>:\n";
  indn.n += 2;
#ifdef DONT_USE_ABSPTR
#else
  file << (*(static_cast<const RegPassivePtr*>(&f))) << '\n';
#endif
  file << "Element:\n";
  file << f.el;
  indn.n -= 2;
  return file;
}

template <class T>
    std::ostream& operator<<(std::ostream& file, const AbsList<T>& f) {
  mfunnamep("template<class T> std::ostream& operator<<(std::ostream& file, "
            "const AbsList<T>& f)");
  Ifile << "AbsList<T>: qel=" << f.get_qel() << '\n';
  //f.check();
  long n = 0;
  indn.n += 2;
  AbsListNode<T>* aln = NULL;
  while ((aln = f.get_next_node(aln)) != NULL) {
    Ifile << "n=" << n << " el[n]=" << aln->el << '\n';
    n++;
  }
  //AbsListNode<T>* aln = f.get_first_node();
  //for( n=0; n<f.get_qel(); n++)
  //{
  //  Ifile<<"n="<<n<<" el[n]="<<aln->el<<'\n';
  //  aln = aln->get_next_node();
  //  //Iprintn(mcout, aln);
  //}
  file << yesindent;
  indn.n -= 2;
  return file;
}

#endif
