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

namespace Heed {

absvol* manip_absvol_treeid::G_lavol() const {
  return eid.empty() ? nullptr : eid.back()->Gavol();
}

void manip_absvol_treeid::down_absref(absref* f) {
  const int qeid = eid.size();
  for (int n = qeid - 1; n >= 1; n--) eid[n]->down_absref(f);
}

void manip_absvol_treeid::up_absref(absref* f) {
  const int qeid = eid.size();
  for (int n = 1; n < qeid; ++n) eid[n]->up_absref(f);
}

int manip_absvol_treeid::check_manip_absvol_registered(manip_absvol* amvol) {
  for (auto vol : eid) if (vol == amvol) return 1;
  return 0;
}

int manip_absvol_treeid::check_absvol_registered(absvol* avol) {
  for (auto vol : eid) if (vol->Gavol() == avol) return 1;
  return 0;
}

int operator==(manip_absvol_treeid& tid1, manip_absvol_treeid& tid2) {
  // First a quick check.
  if (tid1.eid.size() != tid2.eid.size()) return 0;
  // Check the last volume.
  if (tid1.G_lamvol() != tid2.G_lamvol()) return 0;
  // Check all volumes.
  const int qeid = tid1.eid.size();
  for (int n = 0; n < qeid - 1; ++n) {
    if (tid1.eid[n] != tid2.eid[n]) return 0;
  }
  return 1;
}

void manip_absvol_treeid::print(std::ostream& file, int l) const {
  if (l < 0) return;
  if (eid.empty()) {
    Ifile << "no volume defined\n";
  } else {
    // TODO!
    /*
    const int qeid = eid.size();
    if (l > 0) {
      for (int n = 0; n < qeid - 1; ++n) {
        Ifile << "n=" << n;
        eid[n]->print(file, 0);
      }
    }
    Ifile << "n=" << qeid - 1;
    eid.back()->print(file, 0);
    */
  }
  file.flush();
}

// ******** absvol ********
std::vector<manip_absvol*> absvol::Gamanip_embed() const {
  return std::vector<manip_absvol*>();
}

int absvol::find_embed_vol(const point& fpt, const vec& dir,
                           manip_absvol_treeid* atid) const {
  if (check_point_inside(fpt, dir) == 0) return 0;
  const unsigned int s = atid->eid.size();
  std::vector<manip_absvol*> aman = Gamanip_embed();
  const int qaman = aman.size();
  for (int n = 0; n < qaman; ++n) {
    const int i = aman[n]->m_find_embed_vol(fpt, dir, atid);
    if (i == 1) {
      // TODO!
      if (s < atid->eid.size()) break;
      Imcout << "absvol::find_embed_vol:\n";
      Imcout << "    Warning: contradiction between "
             << " i==1 and s == fnamvol\n";
    }
  }
  return 1;
}

int absvol::range(trajestep& fts, int s_ext, int& sb,
                  manip_absvol*& faeid) const {
  faeid = nullptr;
  if (s_ext == 0) {
    sb = 1;
    return range_ext(fts, 0);
  }
  sb = range_ext(fts, 1) == 1 ? 1 : 0;
  std::vector<manip_absvol*> aman = Gamanip_embed();
  const int qaman = aman.size();
  for (int n = 0; n < qaman; ++n) {
    if (aman[n]->m_range_ext(fts, 0) == 1) {
      sb = 2;
      faeid = aman[n];
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
    std::vector<manip_absvol*> embed = Gamanip_embed();
    indn.n += 2;
    const int qembed = embed.size();
    if (qembed > 0) {
      Ifile << "The following volumes are embraced, q=" << embed.size() << '\n';
      indn.n += 2;
      for (int n = 0; n < qembed; ++n) {
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

absvol* absvol::copy() const {
  mcerr << "absvol::copy() not defined\n";
  spexit(mcerr);
  return NULL;
}                   

// *********  manip_absvol  *********
int manip_absvol::m_check_point_inside(const point& fpt,
                                       const vec& fdir) const {
  const abssyscoor* asc = Gasc();
  const absvol* avol = Gavol();
  if (asc) {
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
  // TODO!
  atid->eid.resize(atid->eid.size() + 1);
  atid->eid.back() = (manip_absvol*)this;
  unsigned int s = atid->eid.size();
  int iret = avol->find_embed_vol(pt, dir, atid);
  if (iret == 0) {
    if (atid->eid.size() < s) {
      mcerr << "manip_absvol::m_find_embed_vol: should never happen\n";
      exit(1);
    }
    atid->eid.pop_back();
    return 0;
  } else {
    if (atid->eid.size() < s) {
      mcerr << "manip_absvol::m_find_embed_vol: should never happen\n";
      exit(1);
    }
    return 1;
  }
}

int manip_absvol::m_range(trajestep& fts, int s_ext, int& sb,
                          manip_absvol*& faeid) const {
  trajestep ts(fts);
  up_absref(&ts);
  absvol* avol = Gavol();

  const int s = avol->range(ts, s_ext, sb, faeid);
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
    if (asys)
      asys->print(file, l - 1);
    else
      mcout << "manip_absvol::m_print: system==NULL\n";
    absvol* avol = Gavol();
    if (avol)
      avol->print(file, l - 1);
    else
      mcout << "manip_absvol::m_print: avol==NULL\n";
    indn.n -= 2;
  }
  file.flush();
}

manip_absvol* manip_absvol::copy() const {
  mcerr << "manip_absvol_copy() not defined\n";
  spexit(mcerr);
  return NULL;
}                   

// *********  sh_manip_absvol  *********
absref_transmit sh_manip_absvol::get_components() {
  aref_ptr[0] = &csys;
  return absref_transmit(1, aref_ptr);
}

const abssyscoor* sh_manip_absvol::Gasc() const { return &csys; }

sh_manip_absvol::sh_manip_absvol() : csys() {}
sh_manip_absvol::sh_manip_absvol(sh_manip_absvol& f)
    : absref(f), manip_absvol(f), csys(f.csys) {}
sh_manip_absvol::sh_manip_absvol(const sh_manip_absvol& f)
    : absref(f), manip_absvol(f), csys(f.csys) {}
sh_manip_absvol::sh_manip_absvol(const abssyscoor& f) : csys(f) {}

sh_manip_absvol::sh_manip_absvol(const point& fc, const basis& fbas,
                                 const std::string& fname)
    : csys(fc, fbas, fname) {}

void sh_manip_absvol::m_chname(char* nm) const {
  strcpy(nm, "mvol->");
  Gavol()->chname(&nm[6]);
}

sh_manip_absvol* sh_manip_absvol::copy() const {
  mcerr << "sh_manip_absvol_copy() not defined\n";
  spexit(mcerr);
  return NULL;
}                   

void sh_manip_absvol::m_print(std::ostream& file, int l) const {
  if (l <= 0) return;
  char s[1000];
  m_chname(s);
  Ifile << "sh_manip_absvol::m_print(l=" << l << "): " << s << '\n';
  if (l > 1) {
    indn.n += 2;
    Ifile << "csys=" << noindent << csys;
    absvol* avol = Gavol();
    if (avol)
      avol->print(file, l - 1);
    else
      mcout << "manip_absvol::m_print: avol==NULL\n";
    indn.n -= 2;
  }
  file.flush();
}
}
