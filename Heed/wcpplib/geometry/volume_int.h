#ifndef VOLUME_INT_H
#define VOLUME_INT_H
#include "wcpplib/geometry/volume.h"
/*

Volume with internals.
Normally it is not necessary.
It was made for interface with fortran.


Copyright (c) 2004 Igor B. Smirnov

The file can be used, copied, modified, and distributed
according to the terms of GNU Lesser General Public License version 2.1
as published by the Free Software Foundation,
and provided that the above copyright notice, this permission notice, 
and notices about any modifications of the original text 
appear in all copies and in supporting documentation.
The file is provided "as is" without express or implied warranty.
*/


class absvol_with_internals;
class absvol_with_internals: public sh_manip_absvol
			     //			     public absvol
{public:
  AbsList< ActivePtr< absvol_with_internals > > avwi;
  //AbsList< ActivePtr< absvol_with_internals, 
  //  CopyDefinition_1< absvol_with_internals > > > avwi;
  // Here the alternative name of copy function (copy1) is used
  // since the initial one (copy) is inherented from manip_absvol

  virtual absvol* Gavol(void) const ;
protected:
  virtual void get_components(ActivePtr<absref_transmit>& aref_tran)
    {
      sh_manip_absvol::get_components(aref_tran);
    }
public:
  virtual DynLinArr< manip_absvol * > Gamanip_embed(void) const ;
  absvol_with_internals(void):id(0) {;}
  absvol_with_internals(const absvol_with_internals& f);
  absvol_with_internals
  (const sh_manip_absvol& shmav, long fid): sh_manip_absvol(shmav), id(fid){}
  absvol_with_internals
  (const abssyscoor& f, long fid): sh_manip_absvol(f), id(fid){}
  absvol_with_internals
  (const sh_manip_absvol& shmav,
   const AbsList< ActivePtr< absvol_with_internals > >& favwi, long fid):
   //const AbsList< ActivePtr< absvol_with_internals,
   //CopyDefinition_1< absvol_with_internals >  > >& favwi, long fid):
    sh_manip_absvol(shmav), avwi(favwi), id(fid) {;}
  long id;  // unique id code, should be unique for all system
  // Only for internal nodes:
  AbsListNode< ActivePtr< absvol_with_internals > >* get_Node(long fid);
  //AbsListNode< ActivePtr< absvol_with_internals,
  //  CopyDefinition_1< absvol_with_internals > > >* get_Node(long fid);
  // For all classes including this:
  absvol_with_internals* get_ptr(long fid);
  long get_q_id(long fid); // function for debug: 
  // get the number of elements with this id.
  // should be 0 or 1
  // assistance function:
  long get_q_id_serv(long fid);
 
  macro_copy_header(absvol_with_internals);
  //macro_copy_total_zero(absvol_with_internals);
  //virtual absvol_with_internals* copy(void) const =0;
  //virtual absvol* copy(void) const =0;
  //virtual absvol_with_internals* copy(void) const =0;
  virtual void print(ostream& file, int l) const ;
  virtual ~absvol_with_internals() {}
};



#endif
