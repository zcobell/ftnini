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
#ifndef INIFILE_H
#define INIFILE_H

#include <fstream>
#include <iostream>
#include <string>

#include "boost/property_tree/ini_parser.hpp"
#include "boost/property_tree/ptree.hpp"

/**
 * @brief The IniFile class
 * @author Zachary Cobell
 */
class IniFile {
 public:
  enum ErrorCode { NoError, FileNotFound, FileParseError, KeyNotFound };

  /**
   * @brief Constructor
   * @param filename path to the ini file to read
   */
  IniFile(const std::string &filename)
      : m_initialized(false), m_filename(filename) {}

  /**
   * @brief Constructor
   * @param filename path to the ini file to read
   */
  IniFile(const char *filename) : m_initialized(false), m_filename(filename) {}

  /**
   * @brief Function to read the ini file into the boost::property_tree object
   * @return error code
   */
  int read() {
    std::ifstream f(this->m_filename);
    if (f.bad()) {
      this->setError(FileNotFound);
      return this->error();
    }
    try {
      boost::property_tree::read_ini(this->m_filename, this->m_iniFile);
    } catch (const boost::property_tree::ptree_error &e) {
      std::cerr << "[ERROR]: " << e.what() << std::endl;
      this->setError(FileParseError);
      return this->error();
    } catch (const boost::property_tree::ini_parser::ini_parser_error &e) {
      std::cerr << "[ERROR]: " << e.what() << std::endl;
      this->setError(FileParseError);
      return this->error();
    } catch (...) {
      std::cerr << "[ERROR]: could not read the specified ini file"
                << std::endl;
      this->setError(FileParseError);
      return this->error();
    }
    this->m_initialized = true;
    this->setError(NoError);
    return this->error();
  }

  /**
   * @brief Templatized function to get the value out of the
   * boost::property_tree
   * @param section ini file section
   * @param key ini file key
   * @param value returned value
   * @return error code
   */
  template <typename T>
  int get(const std::string &section, const std::string &key, T &value) {
    if (this->contains<T>(section, key)) {
      value = this->m_iniFile.get<T>(section + "." + key);
      this->setError(NoError);
      return this->error();
    } else {
      this->setError<T>(KeyNotFound, section, key);
      return this->error();
    }
  }

  /**
   * @brief Templatized function to put the value into a boost::property_tree
   * @param section ini file section
   * @param key ini file key
   * @param value placed value
   * @return error code
   */
  template <typename T>
  int put(const std::string &section, const std::string &key, const T &value) {
    this->m_iniFile.put<T>(section + "." + key, value);
    this->setError(NoError);
    return this->error();
  }

  /**
   * @brief Templatized function to check if the value exists for a given type
   * @param section ini file section
   * @param key ini file key
   * @return error code
   */
  template <typename T>
  bool contains(const std::string &section, const std::string &key) {
    return this->m_iniFile.get_optional<T>(section + "." + key)
        .is_initialized();
  }

  /**
   * @brief Filename used for the ini file
   * @return filename
   */
  std::string filename() const { return this->m_filename; }

  /**
   * @brief Function to return the error string associated with the most recent
   * error
   * @return string containing the error
   */
  std::string errorString() const { return this->m_errorString; }

  /**
   * @brief Function to return the most recent error code as an integer
   * @return error code integer
   */
  int error() const { return static_cast<int>(this->m_errorCode); }

  bool initialized() const;

 private:
  template <typename T>
  struct Typestring {
    static std::string name() { return typeid(T).name(); }
  };
  template <>
  struct Typestring<int> {
    static std::string name() { return "integer"; }
  };
  template <>
  struct Typestring<float> {
    static std::string name() { return "float"; }
  };
  template <>
  struct Typestring<double> {
    static std::string name() { return "double"; }
  };
  template <>
  struct Typestring<bool> {
    static std::string name() { return "bool"; }
  };
  template <>
  struct Typestring<unsigned> {
    static std::string name() { return "unsigned"; }
  };

  /**
   * @brief Templatized function to set the error with section and name
   * parameters
   * @param code error code to set
   * @param section ini file section
   * @param key ini file key
   */
  template <typename T>
  void setError(const ErrorCode &code, const std::string &section,
                const std::string &key) {
    this->m_errorCode = code;
    switch (this->m_errorCode) {
      case NoError:
        this->m_errorString = "No error";
        break;
      case KeyNotFound:
        this->m_errorString = "The specified key pair " + section + "-->" +
                              key + " was not found for type " +
                              Typestring<T>::name() + ".";
        break;
      default:
        this->m_errorString =
            "An unknown error was specified for " + section + "-->" + key + ".";
        break;
    }
  }

  /**
   * @brief Sets an error code and associated string
   * @param code error code
   */
  void setError(const ErrorCode &code) {
    this->m_errorCode = code;
    switch (this->m_errorCode) {
      case NoError:
        this->m_errorString = "No error";
        break;
      case FileNotFound:
        this->m_errorString =
            "The specfified file " + this->m_filename + " was not found.";
        break;
      case FileParseError:
        this->m_errorString =
            "The Boost library returned an error code during parsing of " +
            this->m_filename + ".";
        break;
      default:
        this->m_errorString = "An unknown error was specified";
        break;
    }
  }

  bool m_initialized;
  const std::string m_filename;
  boost::property_tree::ptree m_iniFile;
  std::string m_errorString;
  ErrorCode m_errorCode;
};

bool IniFile::initialized() const { return m_initialized; }

#endif  // INIFILE_H
