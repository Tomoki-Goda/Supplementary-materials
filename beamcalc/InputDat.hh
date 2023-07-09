//-------------------------------------------------------BEGINHEADER--
// This file is part of HepLib.
//
// Copyright 2009-2016 Sebastian Sapeta.
//
// This software is provided as a snapshot of the ongoing HepLib
// research project. As such, you may not redistribute it without the
// authors' permission.
//
// If you use HepLib as part of your scientific work, you should
// discuss and agree with the HepLib authors how best to acknowledge
// HepLib in your work (e.g. whether through a reference, or through
// joint authorship with the HepLib authors). 
//
// To help guide HepLib's proper use in its current early stage of
// development, a condition of use of HepLib is that any results that
// you obtain with it must be shown to the HepLib authors before
// they are made public.
//
//-------------------------------------------------------ENDHEADER----
//
// todo:
//   * description should keep the order of options
// issues:
//
//   * there is some problem with reading options having underscore in them
//
#ifndef __INPUTDAT_HH__
#define __INPUTDAT_HH__

#include <cstdlib>
#include <string>
#include <map>
#include <vector>
#include <iostream>
#include <fstream>
#include <sstream>
#include <algorithm>
#include <typeinfo>
#include <quadmath.h>


using namespace std;

// Class for reading options from file.
//
// The input file is assumed in the form
//
//   [option_name] value
//
// Any number of lines is allowed. The options and their values are stored in
// the map. To retrieve them one used a template function 'value' with an
// appropriate type
//
//   inputdat->value<int>("ptmin0")
//
class InputDat {

  public:
  // minimal constructor, useful for tests
  InputDat() {}

  // proper constructor
  InputDat(string filename) : _filename(filename) {
    _read_options();
  }

  // case with multiple values
  vector<string> value_list (const string& name) {
    //if (typeid(T) == typeid(vector<string>)) { // for the future?
    vector<string> result;
    stringstream valuestr;
    string value;
    valuestr << _options["["+name+"]"];
    if (name.rfind("+") == string::npos) {
        cerr << "ERROR: Function value_list cannot be used in this context" 
	     << endl;
    }
    while (true) {
      valuestr >> value;
      if (value == "end") break;
      result.push_back(value);
    }
    return result;
  }

  // return value corresponding to a given option name
  template<class T> T value (const string& name) {

    string optval = "-999"; //default option value
    // set iterator to the key='name'
    // if there is no entry corresponding entry, throw a warning and keep the
    // default vale for this option; otherwise, assign the value from card file
    // the warning for particular options can be switched off with the function 
    // warning_off()
    map<string,string>::iterator it=_options.find("["+name+"]");
    if( it == _options.end())  {
      if (find(_warningoff.begin(),_warningoff.end(),name)==_warningoff.end()) {
        cerr << "WARNING: There is no option corresponding to the name '" 
             << name << "'" << endl;;
        cerr << "         (set to its default value = " <<optval<< ")." << endl;
      }
    } else{ 
      optval = (*it).second;
    }
    T result;
    istringstream istream(optval);
    //istringstream istream(_options["["+name+"]"]);
    istream >> result;
    return result;
  }

  // return list of all options and values
  string description() {
    stringstream sstr;
    sstr <<  "# Input file: " << _filename << endl;
    for (map<string,string>::iterator it=_options.begin();
         it != _options.end();it++) 
      sstr <<  "# " << (*it).first << " " << (*it).second << endl;
    return sstr.str();
  }

  // return the name of the card file
  string filename () const { return _filename;}

  // manually set a value for a given option
  template<class T> void set_option(string name, T value ) {
      stringstream stringvalue;
      stringvalue << value;
      _options["["+name+"]"]=stringvalue.str();
  }

  // disable warning for this option
  void warning_off(string opt) {
    _warningoff.push_back(opt); 
  }

  private:
  string _filename;
  map<string, string> _options;
  vector<string> _warningoff;
  
  // read options from file and save them in this class
  void _read_options() {

    ifstream indata;
    indata.open(_filename.c_str());
    if(!indata) {
       cerr << "ERROR: file could not be opened" << endl;
       exit(1);
    }
     // the idea of the code below is the following: we read a string and check
     // if it starts with "["; if it does not, we assume that this string is
     // part of a comment; if, on the contrary, the string read to "name" starts
     // with "[", we read the next string into "value" 
     while(true) {
      string name = "null", value = "null";
      stringstream valstr;
      indata >> name;
      if (name.compare(0,1,"[") == 0) {
        // option with multiple values
        if (name.find("+]") != string::npos)  {
          while (true) {
            indata >> value; 
	    valstr << value << " ";
            if (value == "end") break;
	  }
	  value = valstr.str();
        // option with a single value 
	} else { indata >> value; }
      }

      //cout << valdata.str() << endl;
      if (!indata.good()) break;
      if (value.compare("null") != 0) { _options[name]=value; }
    }
    indata.close();
  }
};

#endif // __INPUTDAT_HH__
