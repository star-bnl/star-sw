/*
 * TxModuleFactory.h
 *
 * Created on Aug 20, 2007
 *
 * Author: Zhengqiang Liang (Wayne State University)
 *            Valeri Fine (Brookhaven National Laboratory)
 *            Jerome Lauret (Brookhaven National Laboratory)
 *            Stephen Tramer (Tech-X Corp.)
 *            
 *
 * This file is part of the UCM project funded under an SBIR
 * Copyright (c) 2007-2008 STAR Collaboration - Brookhaven National Laboratory
 *
 * @(#)cpp/api:$Id: TxModuleFactory.h,v 1.1 2008/12/08 21:10:32 fine Exp $
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
 
#ifndef TXMODULEFACTORY_H
#define TXMODULEFACTORY_H

#include <string>

using namespace std;

class TxModule;

/**
 * This class provides an interface to get and create a generic module object.
 */
class TxModuleFactory {
 public:
  /**
   * Creates a module with the specified storage type which connects
   * to the provided location.
   * @param storageType
   */
  static TxModule* createModule(const std::string& storageType,
				const std::string& location);

  /**
   * Creates a module from the specified type/location string.
   * @param type The location and type of module to connect to.
   */
  static TxModule* createModule(const std::string& type);

 private:
  /**
   * @internal
   * TxModuleFactory is intended to be a static class.
   */
  TxModuleFactory();
};


#endif
