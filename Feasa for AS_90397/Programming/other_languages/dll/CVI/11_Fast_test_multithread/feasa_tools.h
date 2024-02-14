#ifndef FEASATOOLS_H
#define FEASATOOLS_H

/********************************************************
*  Copyright 2019 Feasa Enterprises Ltd
*  Led Analyser Dynamic Library
********************************************************/

	extern short __stdcall Feasa_Parse_Int16(const char * StringToParse, char Parameter);
	extern int __stdcall Feasa_Parse_Int32(const char * StringToParse, char Parameter);
	extern float __stdcall Feasa_Parse_Float(const char * StringToParse, char Parameter);

	extern int __stdcall Feasa_Parse_RGBI(const char * AnalyserResponse, unsigned char * Red, unsigned char * Green, unsigned char * Blue, int * Intensity);
	extern int __stdcall Feasa_Parse_RGBI_All(const char * AnalyserResponse, unsigned char * RedValues, unsigned char * GreenValues, unsigned char * BlueValues, int * IntensityValues);
	extern int __stdcall Feasa_Parse_HSI(const char * AnalyserResponse, float * Hue, int * Saturation, int * Intensity);
	extern int __stdcall Feasa_Parse_HSI_All(const char * AnalyserResponse, float * HueValues, int * SaturationValues, int * IntensityValues);
	extern int __stdcall Feasa_Parse_xy(const char * AnalyserResponse, float * x, float * y);
	extern int __stdcall Feasa_Parse_xy_All(const char * AnalyserResponse, float * xValues, float * yValues);
	extern int __stdcall Feasa_Parse_uv(const char * AnalyserResponse, float * u, float * v);
	extern int __stdcall Feasa_Parse_uv_All(const char * AnalyserResponse, float * uValues, float * vValues);
	extern int __stdcall Feasa_Parse_CCT(const char * AnalyserResponse, int * CCT, float * delta);
	extern int __stdcall Feasa_Parse_CCT_All(const char * AnalyserResponse, int * CCTValues, float * deltaValues);
	extern int __stdcall Feasa_Parse_Wavelength(const char * AnalyserResponse, float * Wavelength);
	extern int __stdcall Feasa_Parse_Wavelength_All(const char * AnalyserResponse, float * WavelengthValues);
	extern int __stdcall Feasa_Parse_WI(const char * AnalyserResponse, float * Wavelength, int * Intensity);
	extern int __stdcall Feasa_Parse_WI_All(const char * AnalyserResponse, float * WavelengthValues, int * IntensityValues);
	extern int __stdcall Feasa_Parse_Intensity(const char * AnalyserResponse, int * Intensity);
	extern int __stdcall Feasa_Parse_Intensity_All(const char * AnalyserResponse, int * IntensityValues);

	extern int __stdcall Feasa_Parse_Spectrum(const char * Response, float * WavelengthValues, double * IntensityValues);

#endif //FEASATOOLS_H
