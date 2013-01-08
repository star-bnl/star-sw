#include "wcpplib/geometry/volume_int.h"

/* 
Copyright (c) 2004 I. B. Smirnov


The file can be used, copied, modified, and distributed
according to the terms of GNU Lesser General Public License version 2.1
as published by the Free Software Foundation,
and provided that the above copyright notice, this permission notice, 
and notices about any modifications of the original text 
appear in all copies and in supporting documentation.
The file is provided "as is" without express or implied warranty.
*/

absvol* absvol_with_internals::Gavol(void) const {
  mcerr<<"ERROR in absvol*  absvol_with_internals::Gavol(void) const:\n"
       <<"this function is not defined\n";
  spexit(mcerr);
  return NULL;
}

absvol_with_internals::absvol_with_internals(const absvol_with_internals& f): 
  absref(f), sh_manip_absvol(f), avwi(f.avwi), id(f.id) {

}

DynLinArr< manip_absvol * > absvol_with_internals::Gamanip_embed(void) const {
  mfunnamep("DynLinArr< manip_absvol * > absvol_with_internals::Gamanip_embed(void) const");
  long q = avwi.get_qel();
  DynLinArr< manip_absvol * > internal(q);
  if (q == 0) return internal;
  long n = 0;
  AbsListNode< ActivePtr< absvol_with_internals > >* an=NULL;
  //AbsListNode< ActivePtr< absvol_with_internals,
  //  CopyDefinition_1< absvol_with_internals > > >* an=NULL;
  while ((an = avwi.get_next_node(an)) != NULL) { 
    internal[n] = (manip_absvol*) an->el.get();
    n++;
  }
  return internal;
}
  
AbsListNode< ActivePtr< absvol_with_internals > >* 
//AbsListNode< ActivePtr< absvol_with_internals,
//  CopyDefinition_1< absvol_with_internals > > >* 
absvol_with_internals::get_Node(long fid) {
  mfunnamep("...absvol_with_internals::get_Node(long fid)"); 
  AbsListNode< ActivePtr< absvol_with_internals > >* an=NULL;
  while ((an = avwi.get_next_node(an)) != NULL) { 
    if (an->el.get()->id == fid) return an;
  }
  return NULL;
}

long absvol_with_internals::get_q_id(long fid) {
  mfunnamep("long absvol_with_internals::get_q_id(long fid)");
  long q = 0;
  if (id == fid) q++;
  q += get_q_id_serv(fid);
  return q;
}

long absvol_with_internals::get_q_id_serv(long fid) {
  mfunnamep("long absvol_with_internals::get_q_id_serv(long fid)");
  long q = 0;
  AbsListNode< ActivePtr< absvol_with_internals > >* an=NULL;
  while ((an = avwi.get_next_node(an)) != NULL) { 
    q += an->el.get()->get_q_id_serv(fid);
  }
  return q;
}

absvol_with_internals* absvol_with_internals::get_ptr(long fid) {
  mfunnamep("...absvol_with_internals::get_ptr(long fid)"); 
  //mcout<<"absvol_with_internals::get_ptr: fid="<<fid
  //     <<" id="<<id<<" this="<<this<<'\n';
  //this->print(mcout, 10);
  if (id == fid) return this;
  AbsListNode< ActivePtr< absvol_with_internals > >* ptr = get_Node(fid);
  if (ptr != NULL) return ptr->el.get();
  return NULL;
}

macro_copy_body_not_defined(absvol_with_internals)

/*
absvol_with_internals* absvol_with_internals::copy(void) const {
  return new absvol_with_internals(*this);
} 
absvol* absvol_with_internals::copy(void) const {
  return new absvol_with_internals(*this);
}  
absvol_with_internals* absvol_with_internals::copy1(void) const {
  return new absvol_with_internals(*this);
}  
*/

void absvol_with_internals::print(ostream& file, int l) const {
  Ifile<<"absvol_with_internals::print(l="<<l<<"):\n";
  Iprintn(file, avwi.get_qel());
  sh_manip_absvol::m_print(file, l); 
}

