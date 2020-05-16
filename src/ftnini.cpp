//------------------------------GPL---------------------------------------//
// This file is part of FtnIni.
//
// (c) 2020 Zachary Cobell
//
// FtnIni is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// FtnIni is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with FtnIni.  If not, see <http://www.gnu.org/licenses/>.
//------------------------------------------------------------------------//
#include <iostream>
#include <limits>
#include <typeinfo>

#include "inifile.h"

extern "C" {
void* ftnini_init(const char* filename, int& ierr);
void ftnini_deinit(void* ptr);
int ftnini_getString(void* ptr, const char* section, const char* name);
int ftnini_getDouble(void* ptr, const char* section, const char* name,
                     double& value);
int ftnini_getFloat(void* ptr, const char* section, const char* name,
                    float& value);
int ftnini_getInteger(void* ptr, const char* section, const char* name,
                      int& value);
int ftnini_getBool(void* ptr, const char* section, const char* name,
                   bool& value);
void ftnini_errorString(void* ptr);
void c2f_copyStringToFortran(const char*, int n);
}

void* ftnini_init(const char* filename, int& ierr) {
  IniFile* reader = new IniFile(filename);
  if (reader->read() != 0) {
    ierr = 1;
    delete reader;
    return nullptr;
  }
  ierr = 0;
  return reinterpret_cast<void*>(reader);
}

void ftnini_deinit(void* ptr) {
  if (ptr) {
    IniFile* reader = reinterpret_cast<IniFile*>(ptr);
    delete reader;
  }
}

template <typename T>
int ftnini_get(void* ptr, const char* section, const char* name, T& value) {
  if (ptr) {
    IniFile* reader = reinterpret_cast<IniFile*>(ptr);
    if (!reader->initialized()) return 1;
    int ierr = reader->get<T>(section, name, value);
    if (ierr != 0) {
      return reader->error();
    } else {
      return 0;
    }
  } else {
    return 1;
  }
}

int ftnini_getString(void* ptr, const char* section, const char* name) {
  if (ptr) {
    IniFile* reader = reinterpret_cast<IniFile*>(ptr);
    if (!reader->initialized()) return 1;
    std::string v;
    int ierr = reader->get<std::string>(section, name, v);
    if (ierr != 0) {
      return reader->error();
    } else {
      c2f_copyStringToFortran(v.c_str(), v.size());
      return 0;
    }
  } else {
    return 1;
  }
}

int ftnini_getDouble(void* ptr, const char* section, const char* name,
                     double& value) {
  return ftnini_get<double>(ptr, section, name, value);
}

int ftnini_getFloat(void* ptr, const char* section, const char* name,
                    float& value) {
  return ftnini_get<float>(ptr, section, name, value);
}

int ftnini_getInteger(void* ptr, const char* section, const char* name,
                      int& value) {
  return ftnini_get<int>(ptr, section, name, value);
}

int ftnini_getBool(void* ptr, const char* section, const char* name,
                   bool& value) {
  return ftnini_get<bool>(ptr, section, name, value);
}

void ftnini_errorString(void* ptr) {
  if (ptr) {
    IniFile* reader = reinterpret_cast<IniFile*>(ptr);
    std::cerr << "[ERROR]: " << reader->errorString() << std::endl;
  } else {
    std::cerr << "[ERROR]: Uninitialized pointer passed to ftnini_errorString."
              << std::endl;
  }
  return;
}
