#include "wcpplib/geometry/volume.h"
/*
Copyright (c) 2000 Igor B. Smirnov

The file can be used, copied, modified, and distributed
according to the terms of GNU Lesser General Public License version 2.1
as published by the Free Software Foundation,
and provided that the above copyright notice, this permission notice,
and notices about any modifications of the original text
appear in all copies and in supporting documentation.
The file is provided "as is" without express or implied warranty.
*/

manip_absvol_eid::manip_absvol_eid(void) {
  amvol.put(NULL);
  nembed = -1;
}

void manip_absvol_eid::print(std::ostream& file, int l) const {
  if (l < 0) return;
  char s[1000];
  amvol->m_chname(s);
  Ifile << s << ", nembed=" << nembed << '\n';
  file.flush();
}

absvol* manip_absvol_treeid::G_lavol() const {
  return qeid > 0 ? eid[qeid - 1].amvol->Gavol() : 0;
}

void manip_absvol_treeid::down_absref(absref* f) {
  for (int n = qeid - 1; n >= 1; n--)
    eid[n].amvol->down_absref(f);
}

void manip_absvol_treeid::up_absref(absref* f) {
  for (int n = 1; n < qeid; ++n)
    eid[n].amvol->up_absref(f);
}

int manip_absvol_treeid::check_manip_absvol_registered(manip_absvol* amvol) {
  for (int n = 0; n < qeid; ++n) {
    if (eid[n].amvol.get() == amvol) return 1;
  }
  return 0;
}

int manip_absvol_treeid::check_absvol_registered(absvol* avol) {
  for (int n = 0; n < qeid; ++n) {
    if (eid[n].amvol->Gavol() == avol) return 1;
  }
  return 0;
}

int operator==(manip_absvol_treeid& tid1, manip_absvol_treeid& tid2) {
  // checks total route
  // first to make fast rejection
  if (tid1.qeid != tid2.qeid) return 0;
  if (tid1.G_laeid()->nembed != tid2.G_laeid()->nembed) return 0;
  if (tid1.G_lamvol() != tid2.G_lamvol()) return 0;
  // more thoroughly
  for (int n = 0; n < tid1.qeid - 1; ++n) {
    // the last is checked
    if (tid1.eid[n].amvol.get() != tid2.eid[n].amvol.get()) return 0;
  }
  return 1;
}

void manip_absvol_treeid::print(std::ostream& file, int l) const {
  if (l < 0) return;
  if (qeid <= 0) {
    Ifile << "no volume defined, qeid=" << qeid << '\n';
  } else {
    if (l > 0) {
      for (int n = 0; n < qeid - 1; ++n) {
        Ifile << "n=" << n;
        eid[n].print(file, 0);
      }
    }
    Ifile << "n=" << qeid - 1;
    eid[qeid - 1].print(file, 0);
  }
  file.flush();
}
// ********  absvol   *******
DynLinArr<manip_absvol*> absvol::Gamanip_embed(void) const {
  return DynLinArr<manip_absvol*>();
}

int absvol::find_embed_vol(const point& fpt, const vec& dir,
                           manip_absvol_treeid* atid) const {
  if (check_point_inside(fpt, dir) == 0) return 0;
  const int s = atid->qeid;
  DynLinArr<manip_absvol*> aman = Gamanip_embed();
  if (aman.get_qel() > 0 && atid->qeid >= pqamvol) {
    mcerr << "absvol::find_embed_vol:\n"
          << "    aman->get_qel() > 0 && atid->qeid == pqamvol\n"
          << "    atid->qeid=" << atid->qeid << '\n';
    mcerr << "    Increase pqamvol\n";
    exit(1);
  }

  for (int n = 0; n < aman.get_qel(); ++n) {
    atid->eid[atid->qeid].nembed = n;  // for next
    const int i = aman[n]->m_find_embed_vol(fpt, dir, atid);
    if (i == 1) {
      if (s < atid->qeid) break;
      Imcout << "absvol::find_embed_vol:\n";
      Imcout << "    Warning: contradiction between "
             << " i==1 and s == fnamvol\n";
    }
  }
  return 1;
}

int absvol::range(trajestep& fts, int s_ext, int& sb,
                  manip_absvol_eid* faeid) const {
  faeid->amvol.put(NULL);
  faeid->nembed = -1;
  if (s_ext == 0) {
    sb = 1;
    return range_ext(fts, 0);
  }
  int s = range_ext(fts, 1);
  if (s == 1) {
    sb = 1;
  } else {
    sb = 0;
  }
  DynLinArr<manip_absvol*> aman = Gamanip_embed();
  for (int n = 0; n < aman.get_qel(); ++n) {
    if (aman[n]->m_range_ext(fts, 0) == 1) {
      sb = 2;
      faeid->amvol.put(aman[n]);
      faeid->nembed = n;
    }
  }
  if (sb == 1 || sb == 2) return 1;
  return 0;
}

void absvol::print(std::ostream& file, int l) const {
  if (l <= 0) return;
  char s[1000];
  chname(s);
  Ifile << "absvol::print(l=" << l << "): name=" << s << '\n';
  --l;
  if (l > 0) {
    DynLinArr<manip_absvol*> embed = Gamanip_embed();
    indn.n += 2;
    if (embed.get_qel() > 0) {
      Ifile << "The following volumes are embraced, q=" << embed.get_qel()
            << '\n';
      indn.n += 2;
      for (int n = 0; n < embed.get_qel(); ++n) {
        Ifile << "n=" << n << '\n';
        indn.n += 2;
        embed[n]->m_print(file, l);
        indn.n -= 2;
      }
      indn.n -= 2;
    } else {
      Ifile << "None of embraced volumes\n";
    }
    indn.n -= 2;
  }
  file.flush();
}

macro_copy_body_not_defined(absvol)

// *********  manip_absvol  *********
int manip_absvol::m_check_point_inside(const point& fpt,
                                       const vec& fdir) const {
  const abssyscoor* asc = Gasc();
  const absvol* avol = Gavol();
  if (asc != NULL) {
    point pt = fpt;
    vec dir = fdir;
    pt.up(asc);
    dir.up(asc);
    return avol->check_point_inside(pt, dir);
  }
  return avol->check_point_inside(fpt, fdir);
}

int manip_absvol::m_find_embed_vol(const point& fpt, const vec& fdir,
                                   manip_absvol_treeid* atid) const {
  mfunname("int manip_absvol::m_find_embed_vol(...)");
  absvol* avol = Gavol();
  point pt = fpt;
  up_absref(&pt);
  vec dir = fdir;
  up_absref(&dir);
  atid->eid[atid->qeid++].amvol.put((manip_absvol*)this);  // to kill const
  int s = atid->qeid;
  int iret = avol->find_embed_vol(pt, dir, atid);
  if (iret == 0) {
    if (atid->qeid < s) {
      mcerr << "manip_absvol::m_find_embed_vol: should never happen\n";
      exit(1);
    }
    atid->qeid--;
    return 0;
  } else {
    if (atid->qeid < s) {
      mcerr << "manip_absvol::m_find_embed_vol: should never happen\n";
      exit(1);
    }
    return 1;
  }
}

int manip_absvol::m_range(trajestep& fts, int s_ext, int& sb,
                          manip_absvol_eid* faeid) const {
  trajestep ts(fts);
  up_absref(&ts);
  absvol* avol = Gavol();

  int s = avol->range(ts, s_ext, sb, faeid);
  if (s == 1) {
    down_absref(&ts);
    fts = ts;
  }
  return s;
}

int manip_absvol::m_range_ext(trajestep& fts, int s_ext) const {
  trajestep ts(fts);
  up_absref(&ts);
  absvol* avol = Gavol();
  int s = avol->range_ext(ts, s_ext);
  if (s == 1) {
    down_absref(&ts);
    fts = ts;
  }
  return s;
}

void manip_absvol::m_chname(char* nm) const {
  strcpy(nm, "mvol->");
  Gavol()->chname(&nm[6]);
}

void manip_absvol::m_print(std::ostream& file, int l) const {
  if (l <= 0) return;
  char s[1000];
  m_chname(s);
  Ifile << "manip_absvol::m_print(l=" << l << "): " << s << '\n';
  --l;
  if (l > 0) {
    indn.n += 2;
    const abssyscoor* asys = Gasc();
    if (asys != NULL)
      asys->print(file, l - 1);
    else
      mcout << "manip_absvol::m_print: system==NULL\n";
    absvol* avol = Gavol();
    if (avol != NULL)
      avol->print(file, l - 1);
    else
      mcout << "manip_absvol::m_print: avol==NULL\n";
    indn.n -= 2;
  }
  file.flush();

}

macro_copy_body_not_defined(manip_absvol)

// *********  sh_manip_absvol  *********
void sh_manip_absvol::get_components(ActivePtr<absref_transmit>& aref_tran) {
  aref_ptr[0] = &csys;
  aref_tran.pass(new absref_transmit(1, aref_ptr));
}

const abssyscoor* sh_manip_absvol::Gasc(void) const { return &csys; }

sh_manip_absvol::sh_manip_absvol(void) : csys() {}
sh_manip_absvol::sh_manip_absvol(sh_manip_absvol& f)
    : absref(f), manip_absvol(f), csys(f.csys) {}
sh_manip_absvol::sh_manip_absvol(const sh_manip_absvol& f)
    : absref(f), manip_absvol(f), csys(f.csys) {}
sh_manip_absvol::sh_manip_absvol(const abssyscoor& f) : csys(f) {}

sh_manip_absvol::sh_manip_absvol(const point& fc, const basis& fbas,
                                 const String& fname)
    : csys(fc, fbas, fname) {}

void sh_manip_absvol::m_chname(char* nm) const {
  strcpy(nm, "mvol->");
  Gavol()->chname(&nm[6]);
}

macro_copy_body_not_defined(sh_manip_absvol)

void sh_manip_absvol::m_print(std::ostream& file, int l) const {
  if (l <= 0) return;
  char s[1000];
  m_chname(s);
  Ifile << "sh_manip_absvol::m_print(l=" << l << "): " << s << '\n';
  if (l > 1) {
    indn.n += 2;
    Ifile << "csys=" << noindent << csys;
    absvol* avol = Gavol();
    if (avol != NULL)
      avol->print(file, l - 1);
    else
      mcout << "manip_absvol::m_print: avol==NULL\n";
    indn.n -= 2;
  }
  file.flush();
}
