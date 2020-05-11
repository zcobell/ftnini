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

#include "INIReader.h"

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
void ftnini_errorString();
void c2f_copyStringToFortran(const char*, int n);
}

static std::string c_last_error_string = "none";
static int c_last_error_code = 0;

void* ftnini_init(const char* filename, int& ierr) {
  INIReader* reader = new INIReader(filename);
  if (reader->ParseError() != 0) {
    c_last_error_code = 1;
    c_last_error_string =
        std::string("Could not initialize file from") + filename;
    ierr = 1;
    return nullptr;
  }
  c_last_error_code = 0;
  ierr = 0;
  return reinterpret_cast<void*>(reader);
}

void ftnini_deinit(void* ptr) {
  if (ptr) {
    INIReader* reader = reinterpret_cast<INIReader*>(ptr);
    delete reader;
  }
}

int ftnini_getString(void* ptr, const char* section, const char* name) {
  const std::string default_string = "___UNKNOWN___";
  INIReader* reader = reinterpret_cast<INIReader*>(ptr);
  std::string v = reader->Get(section, name, default_string);
  if (v == default_string) {
    c_last_error_code = 1;
    c_last_error_string =
        std::string("Could not read string from ") + section + " --> " + name;
    return 1;
  } else {
    c2f_copyStringToFortran(v.c_str(), v.size());
    c_last_error_code = 0;
    return 0;
  }
}

int ftnini_getDouble(void* ptr, const char* section, const char* name,
                     double& value) {
  const double default_value = -std::numeric_limits<double>::max();
  INIReader* reader = reinterpret_cast<INIReader*>(ptr);
  double v = reader->GetReal(section, name, default_value);
  if (v == default_value) {
    c_last_error_code = 1;
    c_last_error_string =
        std::string("Could not read double from ") + section + " --> " + name;
    return 1;
  } else {
    value = v;
    c_last_error_code = 0;
    return 0;
  }
}

int ftnini_getFloat(void* ptr, const char* section, const char* name,
                    float& value) {
  const float default_value = -std::numeric_limits<float>::max();
  INIReader* reader = reinterpret_cast<INIReader*>(ptr);
  double v = reader->GetFloat(section, name, default_value);
  if (v == default_value) {
    c_last_error_code = 1;
    c_last_error_string =
        std::string("Could not read float from ") + section + " --> " + name;
    return 1;
  } else {
    value = v;
    c_last_error_code = 0;
    return 0;
  }
}

int ftnini_getInteger(void* ptr, const char* section, const char* name,
                      int& value) {
  const int default_value = -std::numeric_limits<int>::max();
  INIReader* reader = reinterpret_cast<INIReader*>(ptr);
  double v = reader->GetInteger(section, name, default_value);
  if (v == default_value) {
    c_last_error_code = 1;
    c_last_error_string =
        std::string("Could not read int from ") + section + " --> " + name;
    return 1;
  } else {
    value = v;
    return 0;
  }
}

int ftnini_getBool(void* ptr, const char* section, const char* name,
                   bool& value) {
  INIReader* reader = reinterpret_cast<INIReader*>(ptr);
  if (reader->contains(section, name)) {
    value = reader->GetBoolean(section, name, false);
    c_last_error_code = 0;
    return 0;
  } else {
    c_last_error_code = 1;
    c_last_error_string =
        std::string("Could not read bool from ") + section + " --> " + name;
    return 1;
  }
}

void ftnini_errorString() {
  if (c_last_error_code != 0) {
    std::cout << "FtnIni ERROR Code " << c_last_error_code << ": "
              << c_last_error_string << std::endl;
  } else {
    std::cout << "FtnIni: No error" << std::endl;
  }
  return;
}
