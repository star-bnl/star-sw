/*
 * StDbFieldI.h
 *
 * Created on Aug 20, 2007
 *
 * Author: Zhengqiang Liang (Wayne State University)
 *            Valeri Fine (Brookhaven National Laboratory)
 *            Jerome Lauret (Brookhaven National Laboratory)
 *            
 *
 * This file is part of the UCM project funded under an SBIR
 * Copyright (c) 2007-2008 STAR Collaboration - Brookhaven National Laboratory
 *
 * @(#)cpp/api:$Id: RecordList.h,v 1.1 2010/03/30 20:05:36 fine Exp $
 *
 *
 *
 * This is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * STAR Scheduler is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with STAR Scheduler; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */
 
#ifndef RECORDLIST_H
#define RECORDLIST_H
#include <vector>
namespace TxLogging {
class Iterator;
class StRecord;
class RecordList : public std::vector<StRecord*> {
   public:
      RecordList();
      RecordList(const RecordList &me);
      RecordList &operator=(const RecordList &me);
      virtual ~RecordList();
      virtual void Print();
      void Clear();
      TxLogging::Iterator iterator();
};
typedef std::vector<StRecord*>::iterator RecordIterator;
}
#endif
