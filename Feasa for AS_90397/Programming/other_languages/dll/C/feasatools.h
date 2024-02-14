#ifndef FEASATOOLS_H
#define FEASATOOLS_H

/********************************************************
*  Copyright 2019 Feasa Enterprises Ltd
*  Led Analyser Dynamic Library
********************************************************/

//------------------------------------------------------------------
// DLL TYPES
//------------------------------------------------------------------
typedef __int16(__stdcall *tFeasa_Parse_Int16)(const char * StringToParse, char Parameter);
typedef int(__stdcall *tFeasa_Parse_Int32)(const char * StringToParse, char Parameter);
typedef float(__stdcall *tFeasa_Parse_Float)(const char * StringToParse, char Parameter);

typedef int(__stdcall *tFeasa_Parse_RGBI)(const char * Response, unsigned char * Red, unsigned char * Green, unsigned char * Blue, int * Intensity);
typedef int(__stdcall *tFeasa_Parse_RGBI_All)(const char * Response, unsigned char * RedValues, unsigned char * GreenValues, unsigned char * BlueValues, int * IntensityValues);
typedef int(__stdcall *tFeasa_Parse_HSI)(const char * Response, float * Hue, int * Saturation, int * Intensity);
typedef int(__stdcall *tFeasa_Parse_HSI_All)(const char * Response, float * HueValues, int * SaturationValues, int * IntensityValues);
typedef int(__stdcall *tFeasa_Parse_xy)(const char * Response, float * x, float * y);
typedef int(__stdcall *tFeasa_Parse_xy_All)(const char * Response, float * xValues, float * yValues);
typedef int(__stdcall *tFeasa_Parse_uv)(const char * Response, float * u, float * v);
typedef int(__stdcall *tFeasa_Parse_uv_All)(const char * Response, float * uValues, float * vValues);
typedef int(__stdcall *tFeasa_Parse_CCT)(const char * Response, int * CCT, float * delta);
typedef int(__stdcall *tFeasa_Parse_CCT_All)(const char * Response, int * CCTValues, float * deltaValues);
typedef int(__stdcall *tFeasa_Parse_Wavelength)(const char * Response, float * Wavelength);
typedef int(__stdcall *tFeasa_Parse_Wavelength_All)(const char * Response, float * WavelengthValues);
typedef int(__stdcall *tFeasa_Parse_WI)(const char * Response, float * Wavelength, int * Intensity);
typedef int(__stdcall *tFeasa_Parse_WI_All)(const char * Response, float * WavelengthValues, int * IntensityValues);
typedef int(__stdcall *tFeasa_Parse_Intensity)(const char * Response, int * Intensity);
typedef int(__stdcall *tFeasa_Parse_Intensity_All)(const char * Response, int * IntensityValues);

typedef int(__stdcall *tFeasa_Parse_Spectrum)(const char * Response, float * WavelengthValues, double * IntensityValues);

//------------------------------------------------------------------
// EXTERN
//------------------------------------------------------------------

extern tFeasa_Parse_Int16 Feasa_Parse_Int16;
extern tFeasa_Parse_Int32 Feasa_Parse_Int32;
extern tFeasa_Parse_Float Feasa_Parse_Float;

extern tFeasa_Parse_RGBI Feasa_Parse_RGBI;
extern tFeasa_Parse_RGBI_All Feasa_Parse_RGBI_All;
extern tFeasa_Parse_HSI Feasa_Parse_HSI;
extern tFeasa_Parse_HSI_All Feasa_Parse_HSI_All;
extern tFeasa_Parse_xy Feasa_Parse_xy;
extern tFeasa_Parse_xy_All Feasa_Parse_xy_All;
extern tFeasa_Parse_uv Feasa_Parse_uv;
extern tFeasa_Parse_uv_All Feasa_Parse_uv_All;
extern tFeasa_Parse_CCT Feasa_Parse_CCT;
extern tFeasa_Parse_CCT_All Feasa_Parse_CCT_All;
extern tFeasa_Parse_Wavelength Feasa_Parse_Wavelength;
extern tFeasa_Parse_Wavelength_All Feasa_Parse_Wavelength_All;
extern tFeasa_Parse_WI Feasa_Parse_WI;
extern tFeasa_Parse_WI_All Feasa_Parse_WI_All;
extern tFeasa_Parse_Intensity Feasa_Parse_Intensity;
extern tFeasa_Parse_Intensity_All Feasa_Parse_Intensity_All;

extern tFeasa_Parse_Spectrum Feasa_Parse_Spectrum;

int FeasaTools_Load(int is64bit, char * PathToDLL);
void FeasaTools_UnLoad();
void FormatDecimal(char * buffer);

#endif