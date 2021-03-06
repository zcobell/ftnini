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
int ftnini_getLongDouble(void* ptr, const char* section, const char* name,
                         long double& value);
int ftnini_getShort(void* ptr, const char* section, const char* name,
                    short& value, bool& ok);
int ftnini_getInteger(void* ptr, const char* section, const char* name,
                      int& value, bool& ok);
int ftnini_getLong(void* ptr, const char* section, const char* name,
                   long& value, bool& ok);
int ftnini_getLongLong(void* ptr, const char* section, const char* name,
                       long long& value, bool& ok);
int ftnini_getUnsigned(void* ptr, const char* section, const char* name,
                       unsigned& value, bool& ok);
int ftnini_getUnsignedLong(void* ptr, const char* section, const char* name,
                           unsigned long& value, bool& ok);
int ftnini_getUnsignedLongLong(void* ptr, const char* section, const char* name,
                               unsigned long long& value, bool& ok);
int ftnini_getBool(void* ptr, const char* section, const char* name,
                   bool& value);
void ftnini_errorString(void* ptr);
void c2f_copyStringToFortran(const char*,
                             int n);  // Fortran function in ftnini.F90
}

/**
 * @brief Initialization function called from Fortran
 * @param filename name of the file to read
 * @param ierr error code
 * @return pointer to created object
 */
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

/**
 * @brief Deconstruction function called from Fortran
 * @param ptr pointer to IniFile object
 */
void ftnini_deinit(void* ptr) {
  if (ptr) {
    IniFile* reader = reinterpret_cast<IniFile*>(ptr);
    delete reader;
  }
}

/**
 * @brief Templatized function to get a value from the IniFile object
 * @param ptr pointer to IniFile object
 * @param section ini file section
 * @param name ini file name
 * @param value returned value
 * @param[optional] ok true if there is a possible loss of precision
 * @return error code
 */
template <typename T>
int ftnini_get(void* ptr, const char* section, const char* name, T& value,
               bool& ok) {
  if (ptr) {
    IniFile* reader = reinterpret_cast<IniFile*>(ptr);
    if (!reader->initialized()) return 1;
    int ierr = reader->get<T>(section, name, value, ok);
    if (ierr != 0) {
      return reader->error();
    } else {
      return 0;
    }
  } else {
    std::cerr
        << "[WARNING]: Uninitialized pointer passed to ftnini_get function"
        << std::endl;
    return 1;
  }
}

/**
 * @brief Function go get a string value and copy to the Fortran memory buffer
 * @param ptr pointer to IniFile object
 * @param section ini file section
 * @param name ini file name
 * @return error code
 */
int ftnini_getString(void* ptr, const char* section, const char* name) {
  if (ptr) {
    bool ok;
    IniFile* reader = reinterpret_cast<IniFile*>(ptr);
    if (!reader->initialized()) return 1;
    std::string v;
    int ierr = reader->get<std::string>(section, name, v, ok);
    if (ierr != 0) {
      return reader->error();
    } else {
      c2f_copyStringToFortran(v.c_str(), v.size());
      return 0;
    }
  } else {
    std::cerr
        << "[WARNING]: Uninitialized pointer passed to ftnini_get function"
        << std::endl;
    return 1;
  }
}

/**
 * @brief Function called from Fortran to get a double (i.e. REAL(8) )
 * @param ptr pointer to IniFile object
 * @param section ini file section
 * @param name ini file name
 * @param value returned value
 * @return error code
 */
int ftnini_getDouble(void* ptr, const char* section, const char* name,
                     double& value) {
  bool ok;
  return ftnini_get(ptr, section, name, value, ok);
}

/**
 * @brief Function called from Fortran to get a long double (i.e. REAL(16) )
 * @param ptr pointer to IniFile object
 * @param section ini file section
 * @param name ini file name
 * @param value returned value
 * @return error code
 */
int ftnini_getLongDouble(void* ptr, const char* section, const char* name,
                         long double& value) {
  bool ok;
  return ftnini_get(ptr, section, name, value, ok);
}

/**
 * @brief Function called from Fortran to get a float (i.e. REAL(4) )
 * @param ptr pointer to IniFile object
 * @param section ini file section
 * @param name ini file name
 * @param value returned value
 * @return error code
 */
int ftnini_getFloat(void* ptr, const char* section, const char* name,
                    float& value) {
  bool ok;
  return ftnini_get(ptr, section, name, value, ok);
}

/**
 * @brief Function called from Fortran to get a short integer
 * @param ptr pointer to IniFile object
 * @param section ini file section
 * @param name ini file name
 * @param value returned value
 * @param[optional] ok true if there is a potential loss in precision
 * @return error code
 */
int ftnini_getShort(void* ptr, const char* section, const char* name,
                    short& value, bool& ok) {
  return ftnini_get(ptr, section, name, value, ok);
}

/**
 * @brief Function called from Fortran to get an integer
 * @param ptr pointer to IniFile object
 * @param section ini file section
 * @param name ini file name
 * @param value returned value
 * @param[optional] ok true if there is a potential loss in precision
 * @return error code
 */
int ftnini_getInteger(void* ptr, const char* section, const char* name,
                      int& value, bool& ok) {
  return ftnini_get(ptr, section, name, value, ok);
}

/**
 * @brief Function called from Fortran to get a long integer
 * @param ptr pointer to IniFile object
 * @param section ini file section
 * @param name ini file name
 * @param value returned value
 * @param[optional] ok true if there is a potential loss in precision
 * @return error code
 */
int ftnini_getLong(void* ptr, const char* section, const char* name,
                   long& value, bool& ok) {
  return ftnini_get(ptr, section, name, value, ok);
}

/**
 * @brief Function called from Fortran to get a long long integer
 * @param ptr pointer to IniFile object
 * @param section ini file section
 * @param name ini file name
 * @param value returned value
 * @param[optional] ok true if there is a potential loss in precision
 * @return error code
 */
int ftnini_getLongLong(void* ptr, const char* section, const char* name,
                       long long& value, bool& ok) {
  return ftnini_get(ptr, section, name, value, ok);
}

/**
 * @brief Function called from Fortran to get an unsigned integer
 * @param ptr pointer to IniFile object
 * @param section ini file section
 * @param name ini file name
 * @param value returned value
 * @param[optional] ok true if there is a potential loss in precision
 * @return error code
 */
int ftnini_getUnsigned(void* ptr, const char* section, const char* name,
                       unsigned& value, bool& ok) {
  return ftnini_get(ptr, section, name, value, ok);
}

/**
 * @brief Function called from Fortran to get an unsigned long integer
 * @param ptr pointer to IniFile object
 * @param section ini file section
 * @param name ini file name
 * @param value returned value
 * @param[optional] ok true if there is a potential loss in precision
 * @return error code
 */
int ftnini_getLongUnsigned(void* ptr, const char* section, const char* name,
                           unsigned long& value, bool& ok) {
  return ftnini_get(ptr, section, name, value, ok);
}

/**
 * @brief Function called from Fortran to get an unsigned long long integer
 * @param ptr pointer to IniFile object
 * @param section ini file section
 * @param name ini file name
 * @param value returned value
 * @param[optional] ok true if there is a potential loss in precision
 * @return error code
 */
int ftnini_getLongLongUnsigned(void* ptr, const char* section, const char* name,
                               unsigned long long& value, bool& ok) {
  return ftnini_get(ptr, section, name, value, ok);
}

/**
 * @brief Function called from Fortran to get a bool (i.e. LOGICAL )
 * @param ptr pointer to IniFile object
 * @param section ini file section
 * @param name ini file name
 * @param value returned value
 * @return error code
 */
int ftnini_getBool(void* ptr, const char* section, const char* name,
                   bool& value) {
  bool ok;
  return ftnini_get(ptr, section, name, value, ok);
}

/**
 * @brief Function to return the error string
 * @param ptr pointer to IniFile object
 */
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
