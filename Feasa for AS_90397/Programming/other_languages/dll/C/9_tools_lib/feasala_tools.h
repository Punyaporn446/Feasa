#ifndef FEASATOOLS_H
#define FEASATOOLS_H

/********************************************************
*  Copyright 2019 Feasa Enterprises Ltd
*  Led Analyser Tools Library
********************************************************/

typedef int (WINAPI*tFeasaCom_ParseInt16)(char * StringToParse, char Parameter);
typedef int (WINAPI*tFeasaCom_ParseInt32)(char * StringToParse, char Parameter);
typedef int (WINAPI*tFeasaCom_ParseFloat)(char * StringToParse, char Parameter);
typedef int (WINAPI*tFeasaCom_ParseRGBI)(char * AnalyserResponse, unsigned char * Red, unsigned char * Green, unsigned char * Blue, int * Intensity);
typedef int (WINAPI*tFeasaCom_ParseHSI)(char * AnalyserResponse, float * Hue, int * Saturation, int * Intensity);
typedef int (WINAPI*tFeasaCom_Parsexy)(char * AnalyserResponse, float * x, float * y);
typedef int (WINAPI*tFeasaCom_Parseuv)(char * AnalyserResponse, float * u, float * v);
typedef int (WINAPI*tFeasaCom_ParseCCT)(char * AnalyserResponse, int * CCT, float * delta);
typedef int (WINAPI*tFeasaCom_ParseWavelength)(char * AnalyserResponse, __int16 * Wavelength);

#endif //FEASATOOLS_H